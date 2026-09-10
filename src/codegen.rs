use crate::ast::{
    is_constructor_field, is_static_member, member_modifiers, member_visibility, Modifier,
};
use crate::ast::{Expr, Module, Node, Stmt, Visitor};
use crate::modules::{MemberImports, Program};
use crate::reachable::{self, FunctionRef};
use crate::token::Token;
use crate::ty::{Type, TypeKind};
use crate::typecheck::TraitCallSite;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Write;

/// Types inferred by the type checker, keyed by AST node address.
///
/// The type checker and the code generator both borrow the *same* `Module`, so these
/// addresses stay valid and let codegen re-use inferred information (most importantly
/// closure parameter types, which are not written anywhere in the AST).
pub type ExprTypes = HashMap<*const Expr, Type>;

/// The type arguments each generic call site resolved to, keyed by AST node.
pub type CallInstantiations = HashMap<*const Expr, Vec<TypeKind>>;
pub type TraitCallSites = HashMap<*const Expr, TraitCallSite>;
pub use crate::typecheck::FunctionRefs;

#[derive(Clone)]
struct ExplicitImpl {
    module: String,
    trait_type: TypeKind,
    target: TypeKind,
}

/// Every distinct instantiation of each generic function, keyed by module then name.
pub use crate::typecheck::Instantiations;

/// The CppCodeGenerator traverses the AST and produces C++ source code.
pub struct CppCodeGenerator {
    output: String,
    indent_level: usize,
    expr_types: ExprTypes,
    function_refs: FunctionRefs,
    /// True while generating the body of a `main` that was declared as returning `void`,
    /// so bare `return;` statements can be lowered to `return 0;`.
    in_synthesised_int_main: bool,
    /// Imported name -> module name, for the module currently being generated.
    imports: HashMap<String, String>,
    member_imports: MemberImports,
    /// The module currently being generated. The program itself is `main`.
    current_module: String,
    /// Names of functions declared in the module currently being generated, so calls to
    /// them can be mangled to match their definitions.
    local_functions: HashSet<String>,
    /// Headers requested by `@include("..")`, gathered across every module.
    extra_includes: BTreeSet<String>,
    /// Type arguments resolved at each generic call site.
    call_instantiations: CallInstantiations,
    trait_call_sites: TraitCallSites,
    explicit_impls: Vec<ExplicitImpl>,
    in_impl_method: bool,
    /// Which specialisations of each generic function need emitting.
    instantiations: Instantiations,
    /// Type parameter bindings for the specialisation currently being emitted, applied
    /// by `translate_type` so every type position in the body is substituted at once.
    current_substitution: HashMap<String, TypeKind>,
    /// Functions reachable from the program's own code. Anything in an imported module
    /// that is not in here is skipped, so importing a module only costs what you use.
    /// Empty means "emit everything", which is what single-module generation does.
    live_functions: Option<HashSet<FunctionRef>>,
}

impl CppCodeGenerator {
    /// Creates a new C++ code generator.
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            expr_types: ExprTypes::new(),
            function_refs: FunctionRefs::new(),
            in_synthesised_int_main: false,
            call_instantiations: CallInstantiations::new(),
            trait_call_sites: TraitCallSites::new(),
            explicit_impls: Vec::new(),
            in_impl_method: false,
            instantiations: Instantiations::new(),
            current_substitution: HashMap::new(),
            imports: HashMap::new(),
            member_imports: MemberImports::new(),
            current_module: "main".to_string(),
            local_functions: HashSet::new(),
            extra_includes: BTreeSet::new(),
            live_functions: None,
        }
    }

    /// Creates a code generator that can consult the types inferred by the type checker.
    pub fn with_types(expr_types: ExprTypes) -> Self {
        Self {
            expr_types,
            ..Self::new()
        }
    }

    /// Supplies the generic instantiations worked out by the type checker.
    pub fn set_instantiations(
        &mut self,
        call_instantiations: CallInstantiations,
        instantiations: Instantiations,
    ) {
        self.call_instantiations = call_instantiations;
        self.instantiations = instantiations;
    }

    pub fn set_trait_call_sites(&mut self, sites: TraitCallSites) {
        self.trait_call_sites = sites;
    }

    pub fn set_function_refs(&mut self, refs: FunctionRefs) {
        self.function_refs = refs;
    }

    /// The C++ symbol for one specialisation of a generic function.
    ///
    /// The type arguments are folded into the name, so `identity<int>` and
    /// `identity<string>` become separate functions.
    fn mangled_generic(module: &str, name: &str, arguments: &[TypeKind]) -> String {
        let mut mangled = Self::mangled(module, name);
        for argument in arguments {
            mangled.push('_');
            mangled.push_str(&Self::type_tag(argument));
        }
        mangled
    }

    /// The C++ name to call for `expr`, accounting for any generic specialisation the
    /// type checker resolved at this call site.
    fn call_name(&self, expr: &Expr, module: &str, name: &str) -> String {
        match self.call_instantiations.get(&(expr as *const Expr)) {
            Some(arguments) => {
                // Inside a specialisation the recorded type arguments may still mention
                // the enclosing function's type parameters, so resolve them first: the
                // `identity(x)` in `wrapper<int>` must call `identity_int`.
                let resolved: Vec<TypeKind> = arguments
                    .iter()
                    .map(|argument| self.resolve_type_argument(argument))
                    .collect();

                Self::mangled_generic(module, name, &resolved)
            }
            None => Self::mangled(module, name),
        }
    }

    /// The emitted name of a method at this call site. Generic method type arguments
    /// are recorded by the type checker and resolved against any enclosing class or
    /// method specialisation here.
    fn method_call_name(&self, call: &Expr, name: &str) -> String {
        let mut emitted = Self::ident(name);
        if let Some(arguments) = self.call_instantiations.get(&(call as *const Expr)) {
            for argument in arguments {
                emitted.push('_');
                emitted.push_str(&Self::type_tag(&self.resolve_type_argument(argument)));
            }
        }
        emitted
    }

    /// Method-specific argument lists needed for the active class specialisation.
    fn method_specialisations(
        &self,
        class_name: &str,
        class_generics: &[Token],
        method_name: &str,
    ) -> Vec<Vec<TypeKind>> {
        let key = format!("method {}.{}", class_name, method_name);
        let Some(instances) = self
            .instantiations
            .get(&self.current_module)
            .and_then(|module| module.get(&key))
        else {
            return Vec::new();
        };
        let class_arguments: Vec<TypeKind> = class_generics
            .iter()
            .map(|generic| {
                self.current_substitution
                    .get(&generic.lexeme)
                    .cloned()
                    .unwrap_or(TypeKind::GenericParam(generic.lexeme.clone()))
            })
            .collect();

        instances
            .iter()
            .filter(|arguments| {
                arguments.len() >= class_arguments.len()
                    && arguments[..class_arguments.len()] == class_arguments
            })
            .map(|arguments| arguments[class_arguments.len()..].to_vec())
            .collect()
    }

    /// Applies the active specialisation to a type argument.
    fn resolve_type_argument(&self, kind: &TypeKind) -> TypeKind {
        let resolved = match kind {
            TypeKind::GenericParam(name) | TypeKind::User(_, name) => {
                match self.current_substitution.get(name) {
                    Some(bound) if !Self::names_itself(name, bound) => bound.clone(),
                    _ => kind.clone(),
                }
            }
            _ => {
                Type::new(Token::dummy("<type>"), kind.clone())
                    .apply_substitution(&self.current_substitution)
                    .kind
            }
        };
        Self::owned_type_kind(
            &self.current_module,
            &self.imports,
            &self.member_imports,
            &resolved,
        )
    }

    /// Normalises a type's module owner for codegen.
    ///
    /// The parser initially records plain names (`Person`, `Box<int>`) with an empty
    /// module. If the type checker leaves one unresolved, treat it as local to the
    /// current module so generated type names match generated class declarations.
    fn type_module<'a>(&'a self, module: &'a str) -> &'a str {
        if module.is_empty() {
            &self.current_module
        } else {
            self.imports
                .get(module)
                .map(String::as_str)
                .unwrap_or(module)
        }
    }

    fn type_identity(&self, module: &str, name: &str) -> (String, String) {
        if module.is_empty() {
            if let Some(imported) = self.member_imports.get(name) {
                return imported.clone();
            }
        }
        (self.type_module(module).to_string(), name.to_string())
    }

    /// Every instantiation of `name` needed in the module being generated.
    fn instantiations_of(&self, name: &str) -> Vec<Vec<TypeKind>> {
        self.instantiations
            .get(&self.current_module)
            .or_else(|| self.instantiations.get(""))
            .and_then(|module| module.get(name))
            .cloned()
            .unwrap_or_default()
    }

    /// Writes one function definition under an already-decided C++ name.
    fn write_function(
        &mut self,
        cpp_name: &str,
        params: &[Box<Stmt>],
        body: &[Box<Stmt>],
        return_type: &Type,
        is_main: bool,
    ) {
        // C++ requires `main` to return `int`. Cardamom allows `fn main()` (a `void`
        // return), so synthesise the `int` and the trailing `return 0;` for that case.
        let void_main = is_main && return_type.kind == TypeKind::Void;
        let ret_type = if void_main {
            "int".to_string()
        } else {
            self.translate_type(return_type)
        };
        let param_str = self.translate_params(params);

        self.writeln(&format!("{} {}({})", ret_type, cpp_name, param_str));
        self.output.push_str(&self.indent());
        self.output.push_str("{\n");
        self.indent_level += 1;

        let old_in_main = self.in_synthesised_int_main;
        self.in_synthesised_int_main = void_main;
        for s in body {
            s.accept(self);
        }
        if void_main {
            self.writeln("return 0;");
        }
        self.in_synthesised_int_main = old_in_main;

        self.indent_level -= 1;
        self.writeln("}");
    }

    /// Whether a substitution binds `name` to a type that is just `name` again.
    fn names_itself(name: &str, bound: &TypeKind) -> bool {
        matches!(bound, TypeKind::GenericParam(other) | TypeKind::User(_, other) if other == name)
    }

    /// The C++ name of one specialisation of a generic class.
    fn mangled_class(module: &str, name: &str, arguments: &[TypeKind]) -> String {
        let mut mangled = Self::mangled(module, name);
        for argument in arguments {
            mangled.push('_');
            mangled.push_str(&Self::type_tag(argument));
        }
        mangled
    }

    /// A short identifier-safe spelling of a type, for use in a mangled name.
    fn type_tag(kind: &TypeKind) -> String {
        match kind {
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::String => "string".to_string(),
            TypeKind::Void => "void".to_string(),
            TypeKind::User(module, name) => {
                if module.is_empty() || module == "main" {
                    name.clone()
                } else {
                    format!("{}_{}", module.replace('.', "_"), name)
                }
            }
            TypeKind::GenericParam(name) => name.clone(),
            TypeKind::Array(inner, _) => format!("{}arr", Self::type_tag(&inner.kind)),
            TypeKind::Reference(inner) => format!("ref{}", Self::type_tag(&inner.kind)),
            TypeKind::MutRef(inner) => format!("mut{}", Self::type_tag(&inner.kind)),
            TypeKind::Tuple(elements) => {
                let parts: Vec<String> = elements.iter().map(|e| Self::type_tag(&e.kind)).collect();
                format!("tup{}", parts.join("_"))
            }
            TypeKind::Function(params, ret) => {
                let parts: Vec<String> = params.iter().map(|p| Self::type_tag(&p.kind)).collect();
                format!("fn{}_to_{}", parts.join("_"), Self::type_tag(&ret.kind))
            }
            TypeKind::GenericInstance(module, name, arguments) => {
                let mut tag = if module.is_empty() || module == "main" {
                    name.clone()
                } else {
                    format!("{}_{}", module.replace('.', "_"), name)
                };
                for argument in arguments {
                    tag.push('_');
                    tag.push_str(&Self::type_tag(&argument.kind));
                }
                tag
            }
            TypeKind::Module(name) => name.clone(),
        }
    }

    fn generalise_impl_kind(kind: &TypeKind, generics: &[String]) -> TypeKind {
        match kind {
            TypeKind::User(_, name) if generics.contains(name) => {
                TypeKind::GenericParam(name.clone())
            }
            TypeKind::GenericInstance(module, name, arguments) => TypeKind::GenericInstance(
                module.clone(),
                name.clone(),
                arguments
                    .iter()
                    .map(|argument| {
                        Type::new(
                            argument.name.clone(),
                            Self::generalise_impl_kind(&argument.kind, generics),
                        )
                    })
                    .collect(),
            ),
            TypeKind::Array(inner, depth) => TypeKind::Array(
                Box::new(Type::new(
                    inner.name.clone(),
                    Self::generalise_impl_kind(&inner.kind, generics),
                )),
                *depth,
            ),
            _ => kind.clone(),
        }
    }

    fn match_impl_type(
        pattern: &TypeKind,
        actual: &TypeKind,
        substitutions: &mut HashMap<String, TypeKind>,
    ) -> bool {
        pattern.match_pattern(actual, substitutions)
    }

    fn impl_key(trait_type: &TypeKind, target: &TypeKind) -> String {
        format!("impl {} for {}", trait_type, target)
    }

    fn owned_type_kind(
        module: &str,
        imports: &HashMap<String, String>,
        member_imports: &MemberImports,
        kind: &TypeKind,
    ) -> TypeKind {
        let owner = |name: &str| {
            if name.is_empty() {
                module.to_string()
            } else {
                imports
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.to_string())
            }
        };
        let identity = |m: &str, name: &str| {
            if m.is_empty() {
                if let Some(imported) = member_imports.get(name) {
                    return imported.clone();
                }
            }
            (owner(m), name.to_string())
        };
        let nested = |ty: &Type| Type {
            kind: Self::owned_type_kind(module, imports, member_imports, &ty.kind),
            ..ty.clone()
        };
        match kind {
            TypeKind::User(m, name) => {
                let (owner, name) = identity(m, name);
                TypeKind::User(owner, name)
            }
            TypeKind::GenericInstance(m, name, arguments) => {
                let (owner, name) = identity(m, name);
                TypeKind::GenericInstance(owner, name, arguments.iter().map(nested).collect())
            }
            TypeKind::Array(inner, depth) => TypeKind::Array(Box::new(nested(inner)), *depth),
            TypeKind::Reference(inner) => TypeKind::Reference(Box::new(nested(inner))),
            TypeKind::MutRef(inner) => TypeKind::MutRef(Box::new(nested(inner))),
            TypeKind::Function(params, ret) => {
                TypeKind::Function(params.iter().map(nested).collect(), Box::new(nested(ret)))
            }
            TypeKind::Tuple(elements) => TypeKind::Tuple(elements.iter().map(nested).collect()),
            _ => kind.clone(),
        }
    }

    fn impl_specialisations(
        &self,
        trait_type: &TypeKind,
        target: &TypeKind,
        generics: &[Token],
    ) -> Vec<HashMap<String, TypeKind>> {
        if generics.is_empty() {
            return vec![HashMap::new()];
        }
        self.instantiations
            .get(&self.current_module)
            .and_then(|module| module.get(&Self::impl_key(trait_type, target)))
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|arguments| {
                generics
                    .iter()
                    .map(|generic| generic.lexeme.clone())
                    .zip(arguments)
                    .collect()
            })
            .collect()
    }

    fn impl_name(module: &str, trait_type: &TypeKind, target: &TypeKind, method: &str) -> String {
        format!(
            "{}_impl_{}_{}_{}",
            Self::mangled(module, "trait"),
            Self::type_tag(trait_type),
            Self::type_tag(target),
            Self::ident(method)
        )
    }

    /// Generate C++ code for a whole program: every imported module, then the program
    /// itself, in dependency order so definitions precede their uses.
    pub fn generate_program(&mut self, program: &Program) -> String {
        self.live_functions = Some(reachable::analyse(program, &self.function_refs));
        self.explicit_impls = program
            .modules
            .iter()
            .flat_map(|module| {
                module
                    .ast
                    .statements
                    .iter()
                    .filter_map(move |stmt| match &**stmt {
                        Stmt::Impl {
                            trait_type,
                            target,
                            generics,
                            ..
                        } => {
                            let generic_names: Vec<String> = generics
                                .iter()
                                .map(|generic| generic.lexeme.clone())
                                .collect();
                            Some(ExplicitImpl {
                                module: module.name.clone(),
                                trait_type: Self::owned_type_kind(
                                    &module.name,
                                    &module.imports,
                                    &module.member_imports,
                                    &Self::generalise_impl_kind(&trait_type.kind, &generic_names),
                                ),
                                target: Self::owned_type_kind(
                                    &module.name,
                                    &module.imports,
                                    &module.member_imports,
                                    &Self::generalise_impl_kind(&target.kind, &generic_names),
                                ),
                            })
                        }
                        _ => None,
                    })
            })
            .collect();

        let mut body = String::new();

        // Generic functions in an earlier dependency can be specialised with a class
        // from a later, unrelated module. Forward-declare every concrete class across
        // the whole program before emitting any module prototypes.
        let forward_declarations = self.capture_output(|gen| {
            for module in &program.modules {
                gen.current_module = module.name.clone();
                gen.imports = module.imports.clone();
                gen.member_imports = module.member_imports.clone();
                for stmt in &module.ast.statements {
                    if matches!(&**stmt, Stmt::Class { .. }) {
                        gen.write_class_forward_declaration(stmt);
                    }
                }
            }
            gen.writeln("");
        });
        body.push_str(&forward_declarations);

        for module in &program.modules {
            self.current_module = module.name.clone();
            self.imports = module.imports.clone();
            self.member_imports = module.member_imports.clone();
            self.local_functions = module
                .ast
                .statements
                .iter()
                .filter_map(|stmt| match &**stmt {
                    Stmt::Function { name, .. } => Some(name.lexeme.clone()),
                    _ => None,
                })
                .collect();

            let module_code = self.capture_output(|gen| {
                if gen.current_module != "main" {
                    gen.writeln(&format!("// --- module {} ---", gen.current_module));
                }

                // C++ needs declarations before uses, and Cardamom does not, so the
                // module is emitted in phases: class bodies (so the types exist),
                // then function prototypes, then all the definitions.
                let classes: Vec<&Box<Stmt>> = module
                    .ast
                    .statements
                    .iter()
                    .filter(|stmt| matches!(&***stmt, Stmt::Class { .. }))
                    .collect();
                let classes = Self::order_classes_by_dependencies(&classes, &gen.current_module);

                for class in &classes {
                    gen.write_class_declaration(class);
                }

                gen.write_function_prototypes(&module.ast);
                gen.write_impl_prototypes(&module.ast);
            });

            body.push_str(&module_code);
        }

        // Only emit bodies after every class is complete. A generic function in one
        // module may be specialised with a class from an unrelated later module.
        for module in &program.modules {
            self.current_module = module.name.clone();
            self.imports = module.imports.clone();
            self.member_imports = module.member_imports.clone();
            self.local_functions = module
                .ast
                .statements
                .iter()
                .filter_map(|stmt| match &**stmt {
                    Stmt::Function { name, .. } => Some(name.lexeme.clone()),
                    _ => None,
                })
                .collect();

            let definitions = self.capture_output(|gen| {
                let classes: Vec<&Box<Stmt>> = module
                    .ast
                    .statements
                    .iter()
                    .filter(|stmt| matches!(&***stmt, Stmt::Class { .. }))
                    .collect();
                let classes = Self::order_classes_by_dependencies(&classes, &gen.current_module);
                for class in &classes {
                    gen.write_class_definitions(class);
                }
                for stmt in &module.ast.statements {
                    if !matches!(&**stmt, Stmt::Class { .. }) {
                        stmt.accept(gen);
                    }
                }
            });
            body.push_str(&definitions);
        }

        // A C++ program needs an entry point even when the source has none.
        let root = program.root();
        if !Self::has_main(&root.ast) {
            self.current_module = root.name.clone();
            let fallback = self.capture_output(|gen| {
                gen.writeln("int main()");
                gen.writeln("{");
                gen.indent_level += 1;
                gen.writeln("return 0;");
                gen.indent_level -= 1;
                gen.writeln("}");
            });
            body.push_str(&fallback);
        }

        // Includes are written last because `@include` requests are discovered while
        // generating the bodies above.
        self.output.clear();
        self.write_includes();
        self.output.push_str(&body);
        self.output.clone()
    }

    /// Generate C++ code from a single Module AST, with no imports.
    pub fn generate(&mut self, module: &Module) -> String {
        let body = self.capture_output(|gen| {
            gen.write_function_prototypes(module);
            module.accept(gen);

            if !Self::has_main(module) {
                gen.writeln("int main()");
                gen.writeln("{");
                gen.indent_level += 1;
                gen.writeln("return 0;");
                gen.indent_level -= 1;
                gen.writeln("}");
            }
        });

        self.output.clear();
        self.write_includes();
        self.output.push_str(&body);
        self.output.clone()
    }

    /// The C++ symbol for a function declared in `module`.
    ///
    /// Module functions are prefixed so that two modules can both define `len` without
    /// colliding. The program's own functions keep their names, and `main` is always
    /// `main` so the linker can find it.
    fn mangled(module: &str, name: &str) -> String {
        if name == "main" {
            name.to_string()
        } else if module == "main" {
            Self::ident(name)
        } else {
            format!("cardamom_{}_{}", module, name)
        }
    }

    /// C++ keywords that a Cardamom program is free to use as ordinary names.
    ///
    /// Emitting them verbatim produces code that will not compile, so they are given a
    /// suffix. The mapping is applied everywhere an identifier is emitted, so uses and
    /// declarations stay consistent.
    ///
    /// `this` is deliberately absent: Cardamom's `this` means exactly what C++'s does,
    /// so it is passed through rather than renamed.
    const CPP_KEYWORDS: &'static [&'static str] = &[
        "alignas",
        "alignof",
        "and",
        "and_eq",
        "asm",
        "auto",
        "bitand",
        "bitor",
        "bool",
        "break",
        "case",
        "catch",
        "char",
        "char8_t",
        "char16_t",
        "char32_t",
        "class",
        "compl",
        "concept",
        "const",
        "consteval",
        "constexpr",
        "constinit",
        "const_cast",
        "continue",
        "co_await",
        "co_return",
        "co_yield",
        "decltype",
        "default",
        "delete",
        "do",
        "double",
        "dynamic_cast",
        "else",
        "enum",
        "explicit",
        "export",
        "extern",
        "false",
        "float",
        "for",
        "friend",
        "goto",
        "if",
        "inline",
        "int",
        "long",
        "mutable",
        "namespace",
        "new",
        "noexcept",
        "not",
        "not_eq",
        "nullptr",
        "operator",
        "or",
        "or_eq",
        "private",
        "protected",
        "public",
        "register",
        "reinterpret_cast",
        "requires",
        "return",
        "short",
        "signed",
        "sizeof",
        "static",
        "static_assert",
        "static_cast",
        "struct",
        "switch",
        "template",
        "thread_local",
        "throw",
        "true",
        "try",
        "typedef",
        "typeid",
        "typename",
        "union",
        "unsigned",
        "using",
        "virtual",
        "void",
        "volatile",
        "wchar_t",
        "while",
        "xor",
        "xor_eq",
    ];

    /// Renders a Cardamom identifier as a safe C++ identifier.
    fn ident(name: &str) -> String {
        if Self::CPP_KEYWORDS.contains(&name) {
            format!("{}_", name)
        } else {
            name.to_string()
        }
    }

    /// Whether the type checker inferred `expr` to be a string.
    fn is_string_expr(&self, expr: &Expr) -> bool {
        matches!(
            self.expr_types.get(&(expr as *const Expr)).map(|t| &t.kind),
            Some(TypeKind::String)
        )
    }

    /// Works out which methods can be marked `const` in the generated C++.
    ///
    /// A method is const unless it writes to a member of `this` or calls another
    /// method on `this` that is not itself const. The second condition makes this a
    /// fixpoint: assume every method is const, then repeatedly drop the ones that turn
    /// out not to be, until nothing changes.
    fn const_methods(methods: &[Box<Stmt>]) -> HashSet<String> {
        let mut candidates: HashSet<String> = methods
            .iter()
            .filter_map(|method| match &**method {
                // A static method has no `this`, so `const` does not apply.
                Stmt::Function {
                    name, modifiers, ..
                } if !is_static_member(modifiers) => Some(name.lexeme.clone()),
                _ => None,
            })
            .collect();

        loop {
            let mut changed = false;

            for method in methods {
                let Stmt::Function { name, body, .. } = &**method else {
                    continue;
                };
                if !candidates.contains(&name.lexeme) {
                    continue;
                }

                let mutates = {
                    let mut detector = MutationDetector {
                        const_methods: &candidates,
                        mutates: false,
                    };
                    for stmt in body {
                        detector.walk_stmt(stmt);
                    }
                    detector.mutates
                };

                if mutates {
                    candidates.remove(&name.lexeme);
                    changed = true;
                }
            }

            if !changed {
                return candidates;
            }
        }
    }

    /// Orders classes so a class used as a field is complete before its owner is
    /// defined. Forward declarations alone are insufficient for by-value fields.
    fn order_classes_by_dependencies<'b>(
        classes: &[&'b Box<Stmt>],
        current_module: &str,
    ) -> Vec<&'b Box<Stmt>> {
        fn type_dependencies(ty: &Type, current_module: &str, output: &mut HashSet<String>) {
            match &ty.kind {
                TypeKind::User(module, name) => {
                    if module.is_empty() || module == current_module {
                        output.insert(name.clone());
                    }
                }
                TypeKind::GenericInstance(module, name, arguments) => {
                    if module.is_empty() || module == current_module {
                        output.insert(name.clone());
                    }
                    for argument in arguments {
                        type_dependencies(argument, current_module, output);
                    }
                }
                TypeKind::Array(inner, _)
                | TypeKind::Reference(inner)
                | TypeKind::MutRef(inner) => type_dependencies(inner, current_module, output),
                TypeKind::Function(params, return_type) => {
                    for param in params {
                        type_dependencies(param, current_module, output);
                    }
                    type_dependencies(return_type, current_module, output);
                }
                TypeKind::Tuple(elements) => {
                    for element in elements {
                        type_dependencies(element, current_module, output);
                    }
                }
                _ => {}
            }
        }

        fn visit(
            index: usize,
            dependencies: &[Vec<usize>],
            state: &mut [u8],
            ordered: &mut Vec<usize>,
        ) {
            if state[index] == 2 {
                return;
            }
            // Leave cycles in a deterministic source-based order; a recursive by-value
            // layout will then be rejected by C++ until Cardamom diagnoses it directly.
            if state[index] == 1 {
                return;
            }
            state[index] = 1;
            for dependency in &dependencies[index] {
                visit(*dependency, dependencies, state, ordered);
            }
            state[index] = 2;
            ordered.push(index);
        }

        let names: HashMap<String, usize> = classes
            .iter()
            .enumerate()
            .filter_map(|(index, stmt)| match &***stmt {
                Stmt::Class { name, .. } => Some((name.lexeme.clone(), index)),
                _ => None,
            })
            .collect();
        let dependencies: Vec<Vec<usize>> = classes
            .iter()
            .map(|stmt| {
                let mut found = HashSet::new();
                if let Stmt::Class { fields, .. } = &***stmt {
                    for field in fields {
                        if let Stmt::Variable { type_, .. } = &**field {
                            type_dependencies(type_, current_module, &mut found);
                        }
                    }
                }
                found
                    .into_iter()
                    .filter_map(|name| names.get(&name).copied())
                    .collect()
            })
            .collect();

        let mut state = vec![0; classes.len()];
        let mut ordered = Vec::with_capacity(classes.len());
        for index in 0..classes.len() {
            visit(index, &dependencies, &mut state, &mut ordered);
        }
        ordered.into_iter().map(|index| classes[index]).collect()
    }

    /// Emits forward declarations for every concrete specialisation of a class.
    fn write_class_forward_declaration(&mut self, stmt: &Stmt) {
        self.for_each_class_specialisation(stmt, Self::write_class_forward_declaration_body);
    }

    fn write_class_forward_declaration_body(&mut self, _stmt: &Stmt, class_name: &str) {
        self.writeln(&format!("class {};", class_name));
    }

    /// Emits the `class X { .. };` block: fields, method prototypes, constructor.
    pub fn write_class_declaration(&mut self, stmt: &Stmt) {
        self.for_each_class_specialisation(stmt, Self::write_class_declaration_body);
    }

    /// Emits everything that must follow the class body, for every specialisation.
    pub fn write_class_definitions(&mut self, stmt: &Stmt) {
        self.for_each_class_specialisation(stmt, Self::write_class_definitions_body);
    }

    /// Runs `write` once per instantiation of a generic class, with its type parameters
    /// bound; a non-generic class is written once with no substitution.
    fn for_each_class_specialisation(&mut self, stmt: &Stmt, write: fn(&mut Self, &Stmt, &str)) {
        let Stmt::Class { name, generics, .. } = stmt else {
            return;
        };

        if generics.is_empty() {
            let class_name = Self::mangled(&self.current_module, &name.lexeme);
            write(self, stmt, &class_name);
            return;
        }

        for arguments in self.instantiations_of(&format!("class {}", name.lexeme)) {
            let substitution: HashMap<String, TypeKind> = generics
                .iter()
                .map(|g| g.lexeme.clone())
                .zip(arguments.iter().cloned())
                .collect();

            let previous = std::mem::replace(&mut self.current_substitution, substitution);
            let class_name = Self::mangled_class(&self.current_module, &name.lexeme, &arguments);
            write(self, stmt, &class_name);
            self.current_substitution = previous;
        }
    }

    fn write_class_declaration_body(&mut self, stmt: &Stmt, class_name: &str) {
        if let Stmt::Class {
            name: source_class_name,
            generics: class_generics,
            modifier: _,
            fields,
            methods,
            ..
        } = stmt
        {
            let const_methods = Self::const_methods(methods);

            self.writeln(&format!("class {} {{", class_name));
            self.indent_level += 1;

            // C++ still groups members by access specifier, so bucket the members here.
            // Because visibility and `static` are independent in the AST, a `private
            // static` member lands in the private block with a `static` prefix, which
            // the old section-based representation could not express.
            for visibility in [Modifier::Public, Modifier::Protected, Modifier::Private] {
                let members: Vec<&Stmt> = fields
                    .iter()
                    .chain(methods.iter())
                    .map(|m| &**m)
                    // An explicit `constructor()` contributes its body to the real C++
                    // constructor below; it is not also a method named `constructor`.
                    .filter(|m| {
                        !matches!(m, Stmt::Function { .. })
                            || !member_modifiers(m).contains(&Modifier::Constructor)
                    })
                    .filter(|m| member_visibility(member_modifiers(m)) == visibility)
                    .collect();

                if members.is_empty() {
                    continue;
                }

                self.writeln(match visibility {
                    Modifier::Public => "public:",
                    Modifier::Protected => "protected:",
                    _ => "private:",
                });
                self.indent_level += 1;

                for member in members {
                    let prefix = if is_static_member(member_modifiers(member)) {
                        "static "
                    } else {
                        ""
                    };

                    match member {
                        Stmt::Variable {
                            name: field_name,
                            type_,
                            ..
                        } => {
                            let cpp_type = self.translate_type(type_);
                            self.writeln(&format!(
                                "{}{} {};",
                                prefix,
                                cpp_type,
                                Self::ident(&field_name.lexeme)
                            ));
                        }
                        Stmt::Function {
                            name: method_name,
                            params,
                            return_type,
                            generics: method_generics,
                            ..
                        } => {
                            let specialisations = if method_generics.is_empty() {
                                vec![Vec::new()]
                            } else {
                                self.method_specialisations(
                                    &source_class_name.lexeme,
                                    class_generics,
                                    &method_name.lexeme,
                                )
                            };
                            for arguments in specialisations {
                                let previous = self.current_substitution.clone();
                                for (generic, argument) in
                                    method_generics.iter().zip(arguments.iter())
                                {
                                    self.current_substitution
                                        .insert(generic.lexeme.clone(), argument.clone());
                                }
                                let ret_type = self.translate_type(return_type);
                                let param_str = self.translate_params(params);
                                let suffix = if const_methods.contains(&method_name.lexeme) {
                                    " const"
                                } else {
                                    ""
                                };
                                let mut emitted_name = Self::ident(&method_name.lexeme);
                                for argument in &arguments {
                                    emitted_name.push('_');
                                    emitted_name.push_str(&Self::type_tag(argument));
                                }
                                self.writeln(&format!(
                                    "{}{} {}({}){};",
                                    prefix, ret_type, emitted_name, param_str, suffix
                                ));
                                self.current_substitution = previous;
                            }
                        }
                        _ => {}
                    }
                }

                self.indent_level -= 1;
            }

            // Constructor parameters are declared as fields, so the constructor takes
            // them in declaration order and initialises the matching members.
            let ctor_params: Vec<Box<Stmt>> = fields
                .iter()
                .filter(|f| is_constructor_field(member_modifiers(f)))
                .cloned()
                .collect();

            self.writeln("public:");
            self.indent_level += 1;
            let ctor_param_str = self.translate_params(&ctor_params);
            let mut init_list = String::new();
            for param in &ctor_params {
                if let Stmt::Variable {
                    name: field_name, ..
                } = &**param
                {
                    if !init_list.is_empty() {
                        init_list.push_str(", ");
                    }
                    let field = Self::ident(&field_name.lexeme);
                    write!(&mut init_list, "{}({})", field, field).unwrap();
                }
            }
            let separator = if init_list.is_empty() { "" } else { " : " };
            self.writeln(&format!(
                "{}({}){}{}",
                class_name, ctor_param_str, separator, init_list
            ));
            self.writeln("{");
            self.indent_level += 1;
            // If the class body supplied an explicit constructor method, emit its
            // statements here instead of relying solely on the synthesised initializer.
            if let Some(ctor_method) = methods
                .iter()
                .find(|m| member_modifiers(m).contains(&Modifier::Constructor))
            {
                if let Stmt::Function { body, .. } = &**ctor_method {
                    for s in body {
                        s.accept(self);
                    }
                }
            }

            // Non-constructor fields with an initialiser are assigned in the body so the
            // generated code matches the declaration order in the source.
            for field in fields {
                if let Stmt::Variable {
                    name: field_name,
                    initialiser: Some(init),
                    modifiers,
                    ..
                } = &**field
                {
                    if is_constructor_field(modifiers) || is_static_member(modifiers) {
                        continue;
                    }
                    self.output.push_str(&self.indent());
                    self.output
                        .push_str(&format!("this->{} = ", Self::ident(&field_name.lexeme)));
                    init.accept(self);
                    self.output.push_str(";\n");
                }
            }
            self.indent_level -= 1;
            self.writeln("}");
            self.indent_level -= 1;

            self.indent_level -= 1;
            self.writeln("};");
        }
    }

    /// Static data members and out-of-line method definitions.
    fn write_class_definitions_body(&mut self, stmt: &Stmt, class_name: &str) {
        if let Stmt::Class {
            name: source_class_name,
            generics: class_generics,
            fields,
            methods,
            ..
        } = stmt
        {
            let const_methods = Self::const_methods(methods);

            // Static data members need a definition outside the class body.
            for field in fields {
                if let Stmt::Variable {
                    name: field_name,
                    type_,
                    initialiser,
                    modifiers,
                    ..
                } = &**field
                {
                    if !is_static_member(modifiers) {
                        continue;
                    }
                    let cpp_type = self.translate_type(type_);
                    self.output.push_str(&self.indent());
                    self.output.push_str(&format!(
                        "{} {}::{}",
                        cpp_type,
                        class_name,
                        Self::ident(&field_name.lexeme)
                    ));
                    if let Some(init) = initialiser {
                        self.output.push_str(" = ");
                        init.accept(self);
                    }
                    self.output.push_str(";\n");
                }
            }

            // Method bodies are emitted out of line so they can refer to the whole class.
            for method in methods {
                if member_modifiers(method).contains(&Modifier::Constructor) {
                    continue;
                }
                if let Stmt::Function {
                    name: method_name,
                    params,
                    body,
                    return_type,
                    generics: method_generics,
                    ..
                } = &**method
                {
                    let specialisations = if method_generics.is_empty() {
                        vec![Vec::new()]
                    } else {
                        self.method_specialisations(
                            &source_class_name.lexeme,
                            class_generics,
                            &method_name.lexeme,
                        )
                    };
                    for arguments in specialisations {
                        let previous = self.current_substitution.clone();
                        for (generic, argument) in method_generics.iter().zip(arguments.iter()) {
                            self.current_substitution
                                .insert(generic.lexeme.clone(), argument.clone());
                        }
                        let ret_type = self.translate_type(return_type);
                        let param_str = self.translate_params(params);
                        let suffix = if const_methods.contains(&method_name.lexeme) {
                            " const"
                        } else {
                            ""
                        };
                        let mut emitted_name = Self::ident(&method_name.lexeme);
                        for argument in &arguments {
                            emitted_name.push('_');
                            emitted_name.push_str(&Self::type_tag(argument));
                        }
                        self.writeln(&format!(
                            "{} {}::{}({}){}",
                            ret_type, class_name, emitted_name, param_str, suffix
                        ));
                        self.writeln("{");
                        self.indent_level += 1;
                        for s in body {
                            s.accept(self);
                        }
                        self.indent_level -= 1;
                        self.writeln("}");
                        self.current_substitution = previous;
                    }
                }
            }
        }
    }

    /// Whether a function of the module being generated needs to be emitted at all.
    fn is_live(&self, name: &str) -> bool {
        match &self.live_functions {
            Some(live) => live.contains(&(self.current_module.clone(), name.to_string())),
            // Single-module generation has no call graph, so nothing is dropped.
            None => true,
        }
    }

    fn write_includes(&mut self) {
        // Headers the generated code always needs, plus anything `@include` asked for.
        let mut includes: BTreeSet<String> = BTreeSet::new();
        for base in ["<functional>", "<string>", "<tuple>", "<vector>"] {
            includes.insert(base.to_string());
        }
        includes.extend(self.extra_includes.iter().cloned());

        for include in includes {
            self.writeln(&format!("#include {}", include));
        }
        self.writeln("");
    }

    /// If `object` is an imported module, returns the C++ symbol for `name` in it.
    fn resolve_module_function(&self, object: &Expr, name: &Token) -> Option<(String, String)> {
        let Expr::Variable { name: object_name } = object else {
            return None;
        };

        let module_name = self.imports.get(&object_name.lexeme)?;
        Some((module_name.clone(), name.lexeme.clone()))
    }

    fn has_main(module: &Module) -> bool {
        module
            .statements
            .iter()
            .any(|stmt| matches!(&**stmt, Stmt::Function { name, .. } if name.lexeme == "main"))
    }

    fn write_impl_prototypes(&mut self, module: &Module) {
        for stmt in &module.statements {
            let Stmt::Impl {
                trait_type,
                target,
                generics,
                methods,
                ..
            } = &**stmt
            else {
                continue;
            };
            let generic_names: Vec<String> = generics
                .iter()
                .map(|generic| generic.lexeme.clone())
                .collect();
            let trait_pattern = Self::owned_type_kind(
                &self.current_module,
                &self.imports,
                &self.member_imports,
                &Self::generalise_impl_kind(&trait_type.kind, &generic_names),
            );
            let target_pattern = Self::owned_type_kind(
                &self.current_module,
                &self.imports,
                &self.member_imports,
                &Self::generalise_impl_kind(&target.kind, &generic_names),
            );
            for substitution in self.impl_specialisations(&trait_pattern, &target_pattern, generics)
            {
                let previous =
                    std::mem::replace(&mut self.current_substitution, substitution.clone());
                let resolved_trait = Type::new(trait_type.name.clone(), trait_pattern.clone())
                    .apply_substitution(&substitution)
                    .kind;
                let resolved_target = Type::new(target.name.clone(), target_pattern.clone())
                    .apply_substitution(&substitution)
                    .kind;
                for method in methods {
                    if let Stmt::Function {
                        name,
                        params,
                        return_type,
                        ..
                    } = &**method
                    {
                        let mut parameters = format!("const {}& self", self.translate_type(target));
                        let rest = self.translate_params(params);
                        if !rest.is_empty() {
                            parameters.push_str(", ");
                            parameters.push_str(&rest);
                        }
                        self.writeln(&format!(
                            "{} {}({});",
                            self.translate_type(return_type),
                            Self::impl_name(
                                &self.current_module,
                                &resolved_trait,
                                &resolved_target,
                                &name.lexeme,
                            ),
                            parameters
                        ));
                    }
                }
                self.current_substitution = previous;
            }
        }
    }

    fn write_function_prototypes(&mut self, module: &Module) {
        let mut wrote_any = false;
        for stmt in &module.statements {
            if let Stmt::Function {
                name,
                params,
                return_type,
                modifiers,
                generics,
                ..
            } = &**stmt
            {
                // `main` needs no prototype, `extern` functions are supplied by the
                // user, and generic functions have no single concrete signature.
                if name.lexeme == "main"
                    || modifiers.contains(&Modifier::Extern)
                    || !self.is_live(&name.lexeme)
                {
                    continue;
                }

                // A generic function needs one prototype per specialisation, each with
                // its type parameters bound.
                if !generics.is_empty() {
                    for arguments in self.instantiations_of(&name.lexeme) {
                        let substitution = generics
                            .iter()
                            .map(|g| g.lexeme.clone())
                            .zip(arguments.iter().cloned())
                            .collect();

                        let previous =
                            std::mem::replace(&mut self.current_substitution, substitution);
                        let ret_type = self.translate_type(return_type);
                        let param_str = self.translate_params(params);
                        self.current_substitution = previous;

                        let cpp_name =
                            Self::mangled_generic(&self.current_module, &name.lexeme, &arguments);
                        self.writeln(&format!("{} {}({});", ret_type, cpp_name, param_str));
                        wrote_any = true;
                    }
                    continue;
                }

                let ret_type = self.translate_type(return_type);
                let param_str = self.translate_params(params);
                let cpp_name = Self::mangled(&self.current_module, &name.lexeme);
                self.writeln(&format!("{} {}({});", ret_type, cpp_name, param_str));
                wrote_any = true;
            }
        }
        if wrote_any {
            self.writeln("");
        }
    }

    /// Renders a parameter list (`int a, std::string b`) for a function or method.
    fn translate_params(&self, params: &[Box<Stmt>]) -> String {
        let mut param_str = String::new();
        for param in params.iter() {
            if let Stmt::Variable { name, type_, .. } = &**param {
                if !param_str.is_empty() {
                    param_str.push_str(", ");
                }
                write!(
                    &mut param_str,
                    "{} {}",
                    self.translate_type(type_),
                    Self::ident(&name.lexeme)
                )
                .unwrap();
            }
        }
        param_str
    }

    /// Returns a string of indentation based on the current indent level.
    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    /// Writes a line with the current indent.
    fn writeln(&mut self, line: &str) {
        let _ = writeln!(self.output, "{}{}", self.indent(), line);
    }

    /// A helper to temporarily capture output generated by a closure.
    fn capture_output<F: FnOnce(&mut Self)>(&mut self, f: F) -> String {
        let original = self.output.clone();
        self.output.clear();
        f(self);
        let captured = self.output.clone();
        self.output = original;
        captured
    }

    /// Translates a source language Type into a corresponding C++ type.
    fn translate_type(&self, ty: &Type) -> String {
        match &ty.kind {
            TypeKind::Int => "int".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::String => "std::string".to_string(),
            TypeKind::Void => "void".to_string(),
            // A type parameter reaches codegen as a plain user type, because the parser
            // cannot tell `T` from a class name. Inside a specialisation the active
            // substitution is what distinguishes them.
            TypeKind::User(module, name) => match self.current_substitution.get(name) {
                Some(bound) if !Self::names_itself(name, bound) => {
                    self.translate_type(&Type::new(ty.name.clone(), bound.clone()))
                }
                _ => {
                    let (owner, base) = self.type_identity(module, name);
                    Self::mangled(&owner, &base)
                }
            },
            TypeKind::Array(inner, depth) => {
                let mut inner_type = self.translate_type(inner);
                for _ in 0..*depth {
                    inner_type = format!("std::vector<{}>", inner_type);
                }
                inner_type
            }
            // Function values (closures and function pointers) become `std::function`, so
            // they can hold both plain functions and capturing lambdas.
            TypeKind::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|p| self.translate_type(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "std::function<{}({})>",
                    self.translate_type(ret),
                    params_str
                )
            }
            // Borrows are real C++ references: `&T` is read-only, `#T` allows writing
            // through it. The type checker rejects assignment through a `const T&`, so
            // the two agree about what is allowed.
            TypeKind::Reference(inner) => format!("const {}&", self.translate_type(inner)),
            TypeKind::MutRef(inner) => format!("{}&", self.translate_type(inner)),
            TypeKind::Tuple(elements) => {
                let elements_str = elements
                    .iter()
                    .map(|e| self.translate_type(e))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("std::tuple<{}>", elements_str)
            }
            // A generic class is monomorphised, so `Box<int>` names the specialised
            // class `Box_int` rather than a C++ template instantiation.
            TypeKind::GenericInstance(module, name, args) => {
                let arguments: Vec<TypeKind> = args
                    .iter()
                    .map(|a| self.resolve_type_argument(&a.kind))
                    .collect();
                let (owner, base) = self.type_identity(module, name);
                Self::mangled_class(&owner, &base, &arguments)
            }
            // Inside a specialisation, a type parameter stands for its bound type.
            TypeKind::GenericParam(name) => match self.current_substitution.get(name) {
                Some(bound) => self.translate_type(&Type::new(ty.name.clone(), bound.clone())),
                None => name.clone(),
            },
            // Modules are namespaces rather than values, so this is unreachable for a
            // well-typed program; the type checker rejects using a module as a value.
            TypeKind::Module(name) => format!("/* module {} */ void", name),
        }
    }

    /// Only class-method `this` is a pointer in C++; Cardamom borrows lower
    /// to C++ references, whose member accesses still use `.`.
    fn member_access_operator(&self, object: &Expr) -> &'static str {
        match object {
            Expr::Variable { name } if name.lexeme == "this" && !self.in_impl_method => "->",
            _ => ".",
        }
    }

    fn trait_impl_name(&self, call: &Expr, receiver: &TypeKind) -> Option<String> {
        let site = self.trait_call_sites.get(&(call as *const Expr))?;
        let concrete_trait = site
            .trait_type
            .apply_substitution(&self.current_substitution)
            .kind;
        let concrete = self.resolve_type_argument(receiver);
        self.explicit_impls.iter().find_map(|implementation| {
            let mut subs = HashMap::new();
            if Self::match_impl_type(&implementation.target, &concrete, &mut subs)
                && Self::match_impl_type(&implementation.trait_type, &concrete_trait, &mut subs)
            {
                Some(Self::impl_name(
                    &implementation.module,
                    &concrete_trait,
                    &concrete,
                    &site.method,
                ))
            } else {
                None
            }
        })
    }

    fn write_operand_value(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable { name } if name.lexeme == "this" && !self.in_impl_method => {
                self.output.push_str("(*this)")
            }
            Expr::Reference { object } | Expr::MutReference { object } => {
                self.write_operand_value(object)
            }
            Expr::Grouping { expression } => {
                self.output.push('(');
                self.write_operand_value(expression);
                self.output.push(')');
            }
            _ => expr.accept(self),
        }
    }

    fn write_operator_call(
        &mut self,
        expr: &Expr,
        op: &Token,
        receiver: &Expr,
        rhs: Option<&Expr>,
    ) -> bool {
        let Some(site) = self.trait_call_sites.get(&(expr as *const Expr)).cloned() else {
            return false;
        };
        let mut receiver_kind = &self
            .expr_types
            .get(&(receiver as *const Expr))
            .expect("checked operator receiver")
            .kind;
        while let TypeKind::Reference(inner) | TypeKind::MutRef(inner) = receiver_kind {
            receiver_kind = &inner.kind;
        }
        let concrete = self.resolve_type_argument(receiver_kind);
        let builtin = match rhs {
            Some(rhs) => {
                let mut kind = &self
                    .expr_types
                    .get(&(rhs as *const Expr))
                    .expect("checked right operand")
                    .kind;
                while let TypeKind::Reference(inner) | TypeKind::MutRef(inner) = kind {
                    kind = &inner.kind;
                }
                crate::operators::builtin_binary(
                    &op.kind,
                    &concrete,
                    &self.resolve_type_argument(kind),
                )
                .is_some()
            }
            None => crate::operators::builtin_unary(&op.kind, &concrete).is_some(),
        };
        let implementation = self.trait_impl_name(expr, receiver_kind);
        // Sequence operands left-to-right, exactly once. Borrow temporaries rather
        // than copying them, and keep both alive through the dispatch.
        self.output
            .push_str("([&]() { const auto& _crdm_op_left = ");
        self.write_operand_value(receiver);
        self.output.push_str("; ");
        if let Some(rhs) = rhs {
            self.output.push_str("const auto& _crdm_op_right = ");
            self.write_operand_value(rhs);
            self.output.push_str("; ");
        }
        self.output.push_str("return ");
        if builtin {
            if rhs.is_some() {
                self.output.push_str("_crdm_op_left ");
                self.output.push_str(&op.lexeme);
                self.output.push_str(" _crdm_op_right");
            } else {
                self.output.push_str(&op.lexeme);
                self.output.push_str("_crdm_op_left");
            }
            self.output.push_str("; }())");
            return true;
        }
        use crate::token::TokenKind;
        let comparison = matches!(
            op.kind,
            TokenKind::EqEq
                | TokenKind::Neq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Lte
                | TokenKind::Gte
        );
        if matches!(op.kind, TokenKind::Neq | TokenKind::Lte | TokenKind::Gte) {
            self.output.push('!');
        }
        if comparison {
            self.output.push_str("static_cast<bool>(");
        }
        if let Some(name) = implementation {
            self.output.push_str(&name);
            self.output.push_str("(_crdm_op_left");
            if rhs.is_some() {
                self.output.push_str(", _crdm_op_right");
            }
        } else {
            self.output.push_str("_crdm_op_left.");
            self.output.push_str(&Self::ident(&site.method));
            self.output.push('(');
            if rhs.is_some() {
                self.output.push_str("_crdm_op_right");
            }
        }
        self.output.push(')');
        if comparison {
            self.output.push(')');
        }
        self.output.push_str("; }())");
        true
    }

    fn write_compound_operator(&mut self, expr: &Expr) -> bool {
        let Some(site) = self.trait_call_sites.get(&(expr as *const Expr)).cloned() else {
            return false;
        };
        let receiver = &self
            .expr_types
            .get(&(expr as *const Expr))
            .expect("checked compound assignment")
            .kind;
        let implementation = self.trait_impl_name(expr, receiver);
        self.output.push_str("([&]() { auto& _crdm_op_left = ");
        let value = match expr {
            Expr::Assignment { name, value, .. } => {
                self.output.push_str(&Self::ident(&name.lexeme));
                value
            }
            Expr::MemberAssignment {
                object,
                name,
                value,
                ..
            }
            | Expr::StaticAssignment {
                object,
                name,
                value,
                ..
            } => {
                object.accept(self);
                self.output
                    .push_str(if matches!(expr, Expr::StaticAssignment { .. }) {
                        "::"
                    } else {
                        self.member_access_operator(object)
                    });
                self.output.push_str(&Self::ident(&name.lexeme));
                value
            }
            Expr::IndexAssignment {
                object,
                index,
                value,
                ..
            } => {
                object.accept(self);
                self.output.push('[');
                index.accept(self);
                self.output.push(']');
                value
            }
            _ => unreachable!("compound operator on non-assignment"),
        };
        self.output.push_str("; const auto& _crdm_op_right = ");
        value.accept(self);
        self.output.push_str("; _crdm_op_left = ");
        if let Some(name) = implementation {
            self.output.push_str(&name);
            self.output.push_str("(_crdm_op_left, _crdm_op_right)");
        } else {
            self.output.push_str("_crdm_op_left.");
            self.output.push_str(&Self::ident(&site.method));
            self.output.push_str("(_crdm_op_right)");
        }
        self.output.push_str("; return _crdm_op_left; }())");
        true
    }

    fn write_call_arguments(&mut self, arguments: &[Box<Expr>]) {
        for (i, arg) in arguments.iter().enumerate() {
            arg.accept(self);
            if i < arguments.len() - 1 {
                self.output.push_str(", ");
            }
        }
    }

    /// `call` is the enclosing call expression, needed to resolve a generic
    /// specialisation at this site.
    fn visit_member_call(
        &mut self,
        call: &Expr,
        object: &Expr,
        name: &Token,
        arguments: &[Box<Expr>],
    ) {
        // `io.println(x)` is a free function call in the generated C++, not a method.
        if let Some((module, function)) = self.resolve_module_function(object, name) {
            let cpp_name = self.call_name(call, &module, &function);
            self.output.push_str(&cpp_name);
            self.output.push('(');
            self.write_call_arguments(arguments);
            self.output.push(')');
            return;
        }

        let receiver_kind = self
            .expr_types
            .get(&(object as *const Expr))
            .map(|ty| &ty.kind);
        let receiver_kind = match receiver_kind {
            Some(TypeKind::Reference(inner)) | Some(TypeKind::MutRef(inner)) => Some(&inner.kind),
            other => other,
        };
        let is_array = matches!(receiver_kind, Some(TypeKind::Array(_, _)));
        let is_string = matches!(receiver_kind, Some(TypeKind::String));

        if let Some(name) = receiver_kind.and_then(|kind| self.trait_impl_name(call, kind)) {
            self.output.push_str(&name);
            self.output.push('(');
            object.accept(self);
            if !arguments.is_empty() {
                self.output.push_str(", ");
            }
            self.write_call_arguments(arguments);
            self.output.push(')');
            return;
        }

        match (name.lexeme.as_str(), is_array, is_string) {
            ("push", true, _) => {
                object.accept(self);
                self.output.push_str(".push_back(");
                self.write_call_arguments(arguments);
                self.output.push(')');
            }
            ("pop", true, _) => {
                object.accept(self);
                self.output.push_str(".pop_back()");
            }
            ("len", true, _) | ("len", _, true) => {
                // `.size()` is unsigned; cast so it can be mixed with `int` arithmetic and
                // comparisons without signed/unsigned surprises.
                self.output.push_str("static_cast<int>(");
                object.accept(self);
                self.output.push_str(".size())");
            }
            ("charAt", _, true) => {
                self.output.push_str("std::string(1, ");
                object.accept(self);
                self.output.push_str(".at(");
                self.write_call_arguments(arguments);
                self.output.push_str("))");
            }
            ("charCodeAt", _, true) => {
                self.output
                    .push_str("static_cast<int>(static_cast<unsigned char>(");
                object.accept(self);
                self.output.push_str(".at(");
                self.write_call_arguments(arguments);
                self.output.push_str(")))");
            }
            _ => {
                object.accept(self);
                // `this` is a pointer in class methods; explicit impls lower it to the
                // free function's `self` reference instead.
                self.output.push_str(self.member_access_operator(object));
                self.output
                    .push_str(&self.method_call_name(call, &name.lexeme));
                self.output.push('(');
                self.write_call_arguments(arguments);
                self.output.push(')');
            }
        }
    }
}

/// Implement the Visitor trait to generate C++ code.
/// Each method converts the corresponding AST node into its C++ representation.
impl Visitor for CppCodeGenerator {
    // Module
    fn visit_module(&mut self, module: &Module) {
        for stmt in &module.statements {
            stmt.accept(self);
        }
    }

    // Statements
    fn visit_block(&mut self, stmt: &Stmt) {
        if let Stmt::Block { statements } = stmt {
            self.writeln("{");
            self.indent_level += 1;
            for s in statements {
                s.accept(self);
            }
            self.indent_level -= 1;
            self.writeln("}");
        }
    }

    fn visit_expression(&mut self, stmt: &Stmt) {
        if let Stmt::Expression { expression } = stmt {
            // Intrinsics emit raw text (or nothing at all), so they supply their own
            // punctuation rather than being terminated like an expression.
            if let Expr::Intrinsic { .. } = &**expression {
                let code = self.capture_output(|gen| expression.accept(gen));
                if !code.is_empty() {
                    self.writeln(&code);
                }
                return;
            }

            self.output.push_str(&self.indent());
            expression.accept(self);
            self.output.push_str(";");
            self.output.push('\n');
        }
    }

    fn visit_if(&mut self, stmt: &Stmt) {
        if let Stmt::If {
            condition,
            then_branch,
            else_branch,
        } = stmt
        {
            self.output.push_str(&self.indent());
            self.output.push_str("if (");
            condition.accept(self);
            self.output.push_str(") ");
            match &**then_branch {
                Stmt::Block { .. } => then_branch.accept(self),
                _ => {
                    self.output.push('\n');
                    self.indent_level += 1;
                    then_branch.accept(self);
                    self.indent_level -= 1;
                }
            }
            if let Some(else_branch) = else_branch {
                self.output.push_str(&self.indent());
                self.output.push_str("else ");
                match &**else_branch {
                    Stmt::Block { .. } => else_branch.accept(self),
                    _ => {
                        self.output.push('\n');
                        self.indent_level += 1;
                        else_branch.accept(self);
                        self.indent_level -= 1;
                    }
                }
            }
        }
    }

    fn visit_while(&mut self, stmt: &Stmt) {
        if let Stmt::While { condition, body } = stmt {
            self.output.push_str(&self.indent());
            self.output.push_str("while (");
            condition.accept(self);
            self.output.push_str(") ");
            body.accept(self);
        }
    }

    fn visit_for(&mut self, stmt: &Stmt) {
        if let Stmt::For {
            initialiser,
            condition,
            increment,
            body,
        } = stmt
        {
            self.output.push_str(&self.indent());
            self.output.push_str("for (");
            if let Some(init) = initialiser {
                let init_code = self.capture_output(|gen| {
                    init.accept(gen);
                });
                self.output.push_str(&init_code);
            } else {
                self.output.push_str("; ");
            }
            if let Some(cond) = condition {
                let cond_code = self.capture_output(|gen| {
                    cond.accept(gen);
                });
                self.output.push_str(&cond_code);
            }
            self.output.push_str("; ");
            if let Some(inc) = increment {
                let inc_code = self.capture_output(|gen| {
                    inc.accept(gen);
                });
                self.output.push_str(&inc_code);
            }
            self.output.push_str(") ");
            body.accept(self);
        }
    }

    fn visit_return(&mut self, stmt: &Stmt) {
        if let Stmt::Return { token: _, value } = stmt {
            self.output.push_str(&self.indent());
            self.output.push_str("return");
            if let Some(expr) = value {
                self.output.push(' ');
                expr.accept(self);
            } else if self.in_synthesised_int_main {
                // `return;` inside a `void` main becomes `return 0;` in the generated `int main`.
                self.output.push_str(" 0");
            }
            self.output.push_str(";");
            self.output.push('\n');
        }
    }

    fn visit_break(&mut self, stmt: &Stmt) {
        if let Stmt::Break { token: _ } = stmt {
            self.writeln("break;");
        }
    }

    fn visit_continue(&mut self, stmt: &Stmt) {
        if let Stmt::Continue { token: _ } = stmt {
            self.writeln("continue;");
        }
    }

    fn visit_variable(&mut self, stmt: &Stmt) {
        if let Stmt::Variable {
            name,
            initialiser,
            type_,
            modifiers: _,
            derived: _,
        } = stmt
        {
            let cpp_type = self.translate_type(type_);
            self.output.push_str(&self.indent());
            self.output.push_str(&cpp_type);
            self.output.push(' ');
            self.output.push_str(&Self::ident(&name.lexeme));
            if let Some(init) = initialiser {
                self.output.push_str(" = ");
                init.accept(self);
            }
            self.output.push_str(";");
            self.output.push('\n');
        }
    }

    fn visit_function(&mut self, stmt: &Stmt) {
        if let Stmt::Function {
            name,
            params,
            body,
            return_type,
            modifiers,
            generics,
            ..
        } = stmt
        {
            if modifiers.contains(&Modifier::Extern) {
                // `fn extern` declares a function implemented outside Cardamom; the user
                // supplies the definition, so emit nothing here.
                return;
            }

            // Skip functions of imported modules that the program never reaches.
            if name.lexeme != "main" && !self.is_live(&name.lexeme) {
                return;
            }

            // A generic function has no code of its own: it is emitted once per
            // instantiation, with its type parameters bound to concrete types.
            if !generics.is_empty() {
                for arguments in self.instantiations_of(&name.lexeme) {
                    let substitution = generics
                        .iter()
                        .map(|g| g.lexeme.clone())
                        .zip(arguments.iter().cloned())
                        .collect();

                    let previous = std::mem::replace(&mut self.current_substitution, substitution);
                    let cpp_name =
                        Self::mangled_generic(&self.current_module, &name.lexeme, &arguments);
                    self.write_function(&cpp_name, params, body, return_type, false);
                    self.current_substitution = previous;
                }
                return;
            }

            // C++ requires `main` to return `int`. Cardamom allows `fn main()` (i.e. a
            // `void` return), so synthesise the `int` return type and the trailing
            // `return 0;` for that case.
            let cpp_name = Self::mangled(&self.current_module, &name.lexeme);
            let is_main = name.lexeme == "main";
            self.write_function(&cpp_name, params, body, return_type, is_main);
        }
    }

    fn visit_intrinsic(&mut self, expr: &Expr) {
        if let Expr::Intrinsic { name, arguments } = expr {
            // String literals keep their quotes in the token, but an intrinsic wants the
            // contents: the raw C++, or the header name.
            let literal = |arg: &Box<Expr>| match &**arg {
                Expr::Literal { value } => Some(
                    value
                        .lexeme
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string(),
                ),
                _ => None,
            };

            match name.lexeme.as_str() {
                // Splice the raw C++ in verbatim. This is what lets a standard library
                // function written in Cardamom bottom out in a real implementation.
                "cpp" => {
                    if let Some(code) = arguments.first().and_then(literal) {
                        self.output.push_str(&code);
                    }
                }
                // Record a header for the generated file; nothing is emitted inline.
                "include" => {
                    if let Some(header) = arguments.first().and_then(literal) {
                        self.extra_includes.insert(header);
                    }
                }
                _ => {}
            }
        }
    }

    fn visit_import(&mut self, _stmt: &Stmt) {
        // Imports produce no code of their own; the standard library definitions they
        // make reachable are emitted on demand by `use_std_function`.
    }

    fn visit_class(&mut self, stmt: &Stmt) {
        // Single-module generation emits both halves together; `generate_program`
        // interleaves them with function prototypes instead.
        self.write_class_declaration(stmt);
        self.write_class_definitions(stmt);
    }

    // Expression visitors
    fn visit_binary(&mut self, expr: &Expr) {
        if let Expr::Binary { left, op, right } = expr {
            if self.write_operator_call(expr, op, left, Some(right)) {
                return;
            }
            self.output.push('(');

            // C++ string literals (including grouped literals) must compare by
            // value, not pointer identity, and + must concatenate them.
            if self.is_string_expr(left) {
                self.output.push_str("std::string(");
                left.accept(self);
                self.output.push(')');
            } else {
                left.accept(self);
            }

            self.output.push(' ');
            self.output.push_str(&op.lexeme);
            self.output.push(' ');
            right.accept(self);
            self.output.push(')');
        }
    }

    fn visit_unary(&mut self, expr: &Expr) {
        if let Expr::Unary { op, right } = expr {
            if self.write_operator_call(expr, op, right, None) {
                return;
            }
            self.output.push_str(&op.lexeme);
            right.accept(self);
        }
    }

    fn visit_literal(&mut self, expr: &Expr) {
        if let Expr::Literal { value } = expr {
            self.output.push_str(&value.lexeme);
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) {
        if let Expr::Grouping { expression } = expr {
            self.output.push('(');
            expression.accept(self);
            self.output.push(')');
        }
    }

    fn visit_variable_expr(&mut self, expr: &Expr) {
        if let Expr::Variable { name } = expr {
            if let Some((module, name)) = self.function_refs.get(&(expr as *const Expr)) {
                self.output.push_str(&Self::mangled(module, name));
            } else if self.in_impl_method && name.lexeme == "this" {
                self.output.push_str("self");
            } else {
                self.output.push_str(&Self::ident(&name.lexeme));
            }
        }
    }

    fn visit_assignment(&mut self, expr: &Expr) {
        if self.write_compound_operator(expr) {
            return;
        }
        if let Expr::Assignment { name, value, op } = expr {
            self.output.push_str(&Self::ident(&name.lexeme));
            self.output.push(' ');
            self.output.push_str(&op.lexeme);
            self.output.push(' ');
            value.accept(self);
        }
    }

    fn visit_call(&mut self, expr: &Expr) {
        if let Expr::Call {
            callee,
            paren: _,
            arguments,
        } = expr
        {
            if let Some((module, name)) = self.function_refs.get(&(&**callee as *const Expr)) {
                self.output.push_str(&self.call_name(expr, module, name));
                self.output.push('(');
                self.write_call_arguments(arguments);
                self.output.push(')');
                return;
            }
            if let Expr::MemberAccess { object, name } = &**callee {
                self.visit_member_call(expr, object, name, arguments);
                return;
            }

            // A call to a function of the module being generated has to use the same
            // mangled name its definition was given, including any specialisation.
            if let Expr::Variable { name } = &**callee {
                if !self.expr_types.contains_key(&(&**callee as *const Expr))
                    && self.local_functions.contains(&name.lexeme)
                {
                    let cpp_name = self.call_name(expr, &self.current_module, &name.lexeme);
                    self.output.push_str(&cpp_name);
                    self.output.push('(');
                    self.write_call_arguments(arguments);
                    self.output.push(')');
                    return;
                }
            }

            callee.accept(self);
            self.output.push('(');
            self.write_call_arguments(arguments);
            self.output.push(')');
        }
    }

    fn visit_generic_call(&mut self, expr: &Expr) {
        if let Expr::GenericCall {
            callee,
            paren: _,
            arguments,
            generics: _,
        } = expr
        {
            // The type arguments are already baked into the specialisation's name, so
            // nothing of them survives into the generated C++.
            if let Some((module, name)) = self.function_refs.get(&(&**callee as *const Expr)) {
                self.output.push_str(&self.call_name(expr, module, name));
                self.output.push('(');
                self.write_call_arguments(arguments);
                self.output.push(')');
                return;
            }
            if let Expr::MemberAccess { object, name } = &**callee {
                self.visit_member_call(expr, object, name, arguments);
                return;
            }

            if let Expr::Variable { name } = &**callee {
                if !self.expr_types.contains_key(&(&**callee as *const Expr))
                    && self.local_functions.contains(&name.lexeme)
                {
                    let cpp_name = self.call_name(expr, &self.current_module, &name.lexeme);
                    self.output.push_str(&cpp_name);
                    self.output.push('(');
                    self.write_call_arguments(arguments);
                    self.output.push(')');
                    return;
                }
            }

            callee.accept(self);
            self.output.push('(');
            for (i, arg) in arguments.iter().enumerate() {
                arg.accept(self);
                if i < arguments.len() - 1 {
                    self.output.push_str(", ");
                }
            }
            self.output.push(')');
        }
    }

    fn visit_member_access(&mut self, expr: &Expr) {
        if let Expr::MemberAccess { object, name } = expr {
            object.accept(self);
            self.output.push_str(self.member_access_operator(object));
            self.output.push_str(&Self::ident(&name.lexeme));
        }
    }

    fn visit_static_access(&mut self, expr: &Expr) {
        if let Expr::StaticAccess { object, name } = expr {
            object.accept(self);
            self.output.push_str("::");
            self.output.push_str(&Self::ident(&name.lexeme));
        }
    }

    fn visit_index(&mut self, expr: &Expr) {
        if let Expr::Index {
            object,
            index,
            token: _,
        } = expr
        {
            object.accept(self);
            self.output.push('[');
            index.accept(self);
            self.output.push(']');
        }
    }

    fn visit_cast(&mut self, expr: &Expr) {
        if let Expr::Cast { object, type_ } = expr {
            self.output.push('(');
            self.output.push_str(&self.translate_type(type_));
            self.output.push_str(") ");
            object.accept(self);
        }
    }

    fn visit_class_init(&mut self, expr: &Expr) {
        if let Expr::ClassInit {
            name, arguments, ..
        } = expr
        {
            // Classes have value semantics, so `new Person(..)` is a constructor call.
            // A generic class resolves to whichever specialisation was inferred here.
            let class_name = match self.expr_types.get(&(expr as *const Expr)).map(|t| &t.kind) {
                Some(TypeKind::GenericInstance(module, base, arguments)) => {
                    let kinds: Vec<TypeKind> = arguments
                        .iter()
                        .map(|a| self.resolve_type_argument(&a.kind))
                        .collect();
                    Self::mangled_class(self.type_module(module), base, &kinds)
                }
                Some(TypeKind::User(module, class_name)) => {
                    Self::mangled(self.type_module(module), class_name)
                }
                _ => Self::mangled(&self.current_module, &name.lexeme),
            };
            self.output.push_str(&class_name);
            self.output.push('(');
            for (i, arg) in arguments.iter().enumerate() {
                arg.accept(self);
                if i < arguments.len() - 1 {
                    self.output.push_str(", ");
                }
            }
            self.output.push(')');
        }
    }

    fn visit_reference(&mut self, expr: &Expr) {
        if let Expr::Reference { object } = expr {
            // Borrowing is implicit in C++: a reference binds directly to the object,
            // so `&x` and `#x` both emit just `x`.
            object.accept(self);
        }
    }

    fn visit_mut_reference(&mut self, expr: &Expr) {
        if let Expr::MutReference { object } = expr {
            object.accept(self);
        }
    }

    fn visit_closure(&mut self, expr: &Expr) {
        if let Expr::Closure {
            name: _,
            parameters,
            param_types,
            body,
            return_type,
        } = expr
        {
            // A closure is an *expression*, so it has to be emitted inline. Emitting a
            // named function definition here produced C++ that could never compile;
            // a lambda is both valid in expression position and able to capture.
            //
            // Parameter types are usually inferred from the closure's expected type, so
            // prefer the type checker's result and fall back to any explicit annotations.
            let inferred_params = match self.expr_types.get(&(expr as *const Expr)).map(|t| &t.kind)
            {
                Some(TypeKind::Function(params, _)) => Some(params.clone()),
                _ => None,
            };

            let mut params_str = String::new();
            for (i, param) in parameters.iter().enumerate() {
                let param_type = inferred_params
                    .as_ref()
                    .and_then(|params| params.get(i))
                    .or_else(|| param_types.get(i))
                    .map(|ty| self.translate_type(ty))
                    .unwrap_or_else(|| "auto".to_string());
                if i > 0 {
                    params_str.push_str(", ");
                }
                write!(
                    &mut params_str,
                    "{} {}",
                    param_type,
                    Self::ident(&param.lexeme)
                )
                .unwrap();
            }

            let ret_type = self.translate_type(return_type);
            // Capture by value so the lambda stays valid after the enclosing scope ends
            // (closures can be returned from functions).
            self.output
                .push_str(&format!("[=]({}) -> {} ", params_str, ret_type));

            let body_code = self.capture_output(|gen| match &**body {
                Stmt::Block { .. } => body.accept(gen),
                _ => {
                    gen.writeln("{");
                    gen.indent_level += 1;
                    body.accept(gen);
                    gen.indent_level -= 1;
                    gen.writeln("}");
                }
            });
            // The body was rendered as statements, so drop the leading indent and the
            // trailing newline to keep it inside the surrounding expression.
            self.output.push_str(body_code.trim_start());
            while self.output.ends_with('\n') {
                self.output.pop();
            }
        }
    }

    fn visit_array(&mut self, expr: &Expr) {
        // For arrays, generate an initializer list for std::vector.
        if let Expr::Array { elements, .. } = expr {
            self.output.push('{');
            for (i, element) in elements.iter().enumerate() {
                element.accept(self);
                if i < elements.len() - 1 {
                    self.output.push_str(", ");
                }
            }
            self.output.push('}');
        }
    }

    fn visit_tuple(&mut self, expr: &Expr) {
        if let Expr::Tuple { elements } = expr {
            // Represent a tuple as a comma-separated list enclosed in parentheses.
            self.output.push('(');
            for (i, element) in elements.iter().enumerate() {
                element.accept(self);
                if i < elements.len() - 1 {
                    self.output.push_str(", ");
                }
            }
            self.output.push(')');
        }
    }

    fn visit_member_assignment(&mut self, expr: &Expr) {
        if self.write_compound_operator(expr) {
            return;
        }
        if let Expr::MemberAssignment {
            object,
            name,
            value,
            op,
        } = expr
        {
            object.accept(self);
            self.output.push_str(self.member_access_operator(object));
            self.output.push_str(&Self::ident(&name.lexeme));
            self.output.push(' ');
            self.output.push_str(&op.lexeme);
            self.output.push(' ');
            value.accept(self);
        }
    }

    fn visit_static_assignment(&mut self, expr: &Expr) {
        if self.write_compound_operator(expr) {
            return;
        }
        if let Expr::StaticAssignment {
            object,
            name,
            value,
            op,
        } = expr
        {
            object.accept(self);
            self.output.push_str("::");
            self.output.push_str(&Self::ident(&name.lexeme));
            self.output.push(' ');
            self.output.push_str(&op.lexeme);
            self.output.push(' ');
            value.accept(self);
        }
    }

    fn visit_index_assignment(&mut self, expr: &Expr) {
        if self.write_compound_operator(expr) {
            return;
        }
        if let Expr::IndexAssignment {
            object,
            index,
            value,
            op,
            token: _,
        } = expr
        {
            object.accept(self);
            self.output.push('[');
            index.accept(self);
            self.output.push(']');
            self.output.push(' ');
            self.output.push_str(&op.lexeme);
            self.output.push(' ');
            value.accept(self);
        }
    }

    fn visit_impl(&mut self, stmt: &Stmt) {
        let Stmt::Impl {
            trait_type,
            target,
            generics,
            methods,
            ..
        } = stmt
        else {
            return;
        };
        let generic_names: Vec<String> = generics
            .iter()
            .map(|generic| generic.lexeme.clone())
            .collect();
        let trait_pattern = Self::owned_type_kind(
            &self.current_module,
            &self.imports,
            &self.member_imports,
            &Self::generalise_impl_kind(&trait_type.kind, &generic_names),
        );
        let target_pattern = Self::owned_type_kind(
            &self.current_module,
            &self.imports,
            &self.member_imports,
            &Self::generalise_impl_kind(&target.kind, &generic_names),
        );
        for substitution in self.impl_specialisations(&trait_pattern, &target_pattern, generics) {
            let previous = std::mem::replace(&mut self.current_substitution, substitution.clone());
            let resolved_trait = Type::new(trait_type.name.clone(), trait_pattern.clone())
                .apply_substitution(&substitution)
                .kind;
            let resolved_target = Type::new(target.name.clone(), target_pattern.clone())
                .apply_substitution(&substitution)
                .kind;
            for method in methods {
                let Stmt::Function {
                    name,
                    params,
                    return_type,
                    body,
                    ..
                } = &**method
                else {
                    continue;
                };
                let mut parameters = format!("const {}& self", self.translate_type(target));
                let rest = self.translate_params(params);
                if !rest.is_empty() {
                    parameters.push_str(", ");
                    parameters.push_str(&rest);
                }
                self.writeln(&format!(
                    "{} {}({})",
                    self.translate_type(return_type),
                    Self::impl_name(
                        &self.current_module,
                        &resolved_trait,
                        &resolved_target,
                        &name.lexeme,
                    ),
                    parameters
                ));
                self.writeln("{");
                self.indent_level += 1;
                let old = self.in_impl_method;
                self.in_impl_method = true;
                for statement in body {
                    statement.accept(self);
                }
                self.in_impl_method = old;
                self.indent_level -= 1;
                self.writeln("}");
            }
            self.current_substitution = previous;
        }
    }

    fn visit_extension(&mut self, stmt: &Stmt) {
        if let Stmt::Extension { target, methods } = stmt {
            let target_name = target.name.lexeme.clone();
            for method in methods {
                if let Stmt::Function {
                    name,
                    params,
                    return_type,
                    body,
                    ..
                } = &**method
                {
                    let ret_type = self.translate_type(return_type);
                    let mut param_str = String::new();
                    // For extension methods, add the extended type as the first parameter.
                    write!(&mut param_str, "{}& self", self.translate_type(target)).unwrap();
                    if !params.is_empty() {
                        param_str.push_str(", ");
                    }
                    for (i, param) in params.iter().enumerate() {
                        if let Stmt::Variable {
                            name: param_name,
                            type_,
                            ..
                        } = &**param
                        {
                            let t = self.translate_type(type_);
                            write!(&mut param_str, "{} {}", t, Self::ident(&param_name.lexeme))
                                .unwrap();
                            if i < params.len() - 1 {
                                param_str.push_str(", ");
                            }
                        }
                    }
                    // Generate the function prototype (and body, if desired).
                    self.writeln(&format!(
                        "{} {}_extension_{}({})",
                        ret_type, target_name, name.lexeme, param_str
                    ));
                    // Optionally, generate a stub body:
                    self.writeln("{");
                    self.indent_level += 1;
                    for s in body {
                        s.accept(self);
                    }
                    self.indent_level -= 1;
                    self.writeln("}");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typecheck::TypeChecker;
    use crate::utils::symtable::SymbolTable;
    use std::io::Write;
    use std::process::{Command, Stdio};

    /// Loads, type checks and generates a fixture together with its imports.
    fn generate_fixture(relative_path: &str) -> String {
        let filename = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), relative_path);
        let program =
            crate::modules::load(std::path::Path::new(&filename)).unwrap_or_else(|errors| {
                panic!("`{}` should load: {} error(s)", relative_path, errors.len())
            });

        let root = program.root().name.clone();
        let mut exports: HashMap<String, crate::typecheck::ModuleExports> = HashMap::new();
        let mut expr_types = ExprTypes::new();
        let mut function_refs = FunctionRefs::new();
        let mut call_instantiations = CallInstantiations::new();
        let mut trait_call_sites = TraitCallSites::new();
        let mut instantiations = Instantiations::new();
        let mut generic_call_sites: Vec<crate::typecheck::GenericCallSite> = Vec::new();
        let mut function_generics = crate::typecheck::FunctionGenerics::new();

        for module in &program.modules {
            let mut symtable = SymbolTable::new();
            let mut checker = TypeChecker::new(
                &mut symtable,
                module.path.to_string_lossy().to_string(),
                module.source.clone(),
            );
            checker.set_module_exports(exports.clone());
            checker.set_module_name(module.name.clone());
            checker.set_is_library(module.name != root);
            checker.check_module(&module.ast);
            assert_eq!(
                checker.error_count(),
                0,
                "`{}` should typecheck (module `{}`)",
                relative_path,
                module.name
            );

            exports.insert(module.name.clone(), checker.exports(&module.ast));
            expr_types.extend(checker.expr_types.clone());
            function_refs.extend(checker.function_refs.clone());
            call_instantiations.extend(checker.call_instantiations.clone());
            trait_call_sites.extend(checker.trait_call_sites.clone());
            generic_call_sites.extend(checker.generic_call_sites.clone());

            for (module_name, functions) in checker.instantiations.clone() {
                instantiations
                    .entry(module_name)
                    .or_default()
                    .extend(functions);
            }
            for (module_name, functions) in checker.function_generics.clone() {
                function_generics
                    .entry(module_name)
                    .or_default()
                    .extend(functions);
            }
        }

        crate::typecheck::expand_instantiations(
            &mut instantiations,
            &generic_call_sites,
            &function_generics,
        );

        let mut generator = CppCodeGenerator::with_types(expr_types);
        generator.set_function_refs(function_refs);
        generator.set_instantiations(call_instantiations, instantiations);
        generator.set_trait_call_sites(trait_call_sites);
        generator.generate_program(&program)
    }

    /// Compiles generated C++, returning the path to the binary.
    fn compile_cpp(code: &str, label: &str) -> String {
        let cpp_path = format!("/tmp/cardamom_{}_{}.cpp", label, std::process::id());
        let bin_path = format!("/tmp/cardamom_{}_{}", label, std::process::id());
        std::fs::write(&cpp_path, code).expect("generated C++ should be writable");

        let compile = Command::new("g++")
            .arg(&cpp_path)
            .arg("-o")
            .arg(&bin_path)
            .output()
            .expect("g++ should run");

        assert!(
            compile.status.success(),
            "`{}` should generate compilable C++:\n{}",
            label,
            String::from_utf8_lossy(&compile.stderr)
        );

        let _ = std::fs::remove_file(cpp_path);
        bin_path
    }

    /// Every fixture in `tests/pass` must lower to C++ that actually compiles.
    #[test]
    fn pass_fixtures_generate_compilable_cpp() {
        let fixtures = std::fs::read_dir(format!("{}/tests/pass", env!("CARGO_MANIFEST_DIR")))
            .expect("tests/pass should be readable");

        for entry in fixtures {
            let path = entry.expect("directory entry should be readable").path();
            if path.extension().and_then(|e| e.to_str()) != Some("crdm") {
                continue;
            }
            let stem = path.file_stem().unwrap().to_string_lossy().to_string();
            let relative = format!("tests/pass/{}", path.file_name().unwrap().to_string_lossy());

            let code = generate_fixture(&relative);
            let bin_path = compile_cpp(&code, &format!("pass_{}", stem));
            let _ = std::fs::remove_file(bin_path);
        }
    }

    #[test]
    fn bf_codegen_compiles_and_runs_input_echo() {
        let code = generate_fixture("bf.crdm");
        let bin_path = compile_cpp(&code, "bf");

        let mut child = Command::new(&bin_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("generated bf binary should run");
        child
            .stdin
            .as_mut()
            .expect("stdin should be open")
            .write_all(b",.\nA\n")
            .expect("stdin write should succeed");
        let output = child.wait_with_output().expect("run should finish");
        assert!(
            output.status.success(),
            "generated bf binary should exit cleanly"
        );

        // `bf.crdm` writes cells with `io.print`, so the echoed byte is not followed by
        // a newline of its own.
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.ends_with("A"),
            "generated bf binary should echo one input byte, got `{}`",
            stdout
        );

        let _ = std::fs::remove_file(bin_path);
    }

    /// A generic function is emitted once per instantiation actually used, and not at
    /// all for type arguments the program never asks for.
    #[test]
    fn generics_emit_only_the_instantiations_used() {
        let code = generate_fixture("tests/pass/generic_3.crdm");

        assert!(
            code.contains("cardamom_util_firstOr_int"),
            "an int instantiation should be emitted"
        );
        assert!(
            code.contains("cardamom_util_firstOr_string"),
            "a string instantiation should be emitted"
        );

        // `identity` is only ever reached through `echo<string>`.
        assert!(
            code.contains("cardamom_util_identity_string"),
            "a transitively required instantiation should be emitted:\n{}",
            code
        );
        assert!(
            !code.contains("cardamom_util_identity_int"),
            "an unused instantiation should not be emitted:\n{}",
            code
        );

        // Nothing generic should survive into the generated C++.
        assert!(
            !code.contains("template"),
            "monomorphisation should leave no templates:\n{}",
            code
        );
    }

    /// Importing a module must not drag in the functions the program never calls.
    #[test]
    fn unused_module_functions_are_not_emitted() {
        let code = generate_fixture("tests/pass/import_1.crdm");

        assert!(
            code.contains("cardamom_math_pow"),
            "a called function should be emitted"
        );
        assert!(
            !code.contains("cardamom_math_sqrt"),
            "an uncalled function should not be emitted:\n{}",
            code
        );
        assert!(
            !code.contains("<cmath>"),
            "an uncalled function should not drag in its includes:\n{}",
            code
        );
    }
}

/// Detects whether a method body mutates the object it is called on.
///
/// Used to decide if a method can be `const` in the generated C++.
struct MutationDetector<'a> {
    /// Methods currently believed to be const, for resolving calls on `this`.
    const_methods: &'a HashSet<String>,
    mutates: bool,
}

impl<'a> MutationDetector<'a> {
    /// Whether an expression ultimately refers to `this`.
    fn targets_this(expr: &Expr) -> bool {
        match expr {
            Expr::Variable { name } => name.lexeme == "this",
            Expr::MemberAccess { object, .. } | Expr::Index { object, .. } => {
                Self::targets_this(object)
            }
            Expr::Grouping { expression } => Self::targets_this(expression),
            _ => false,
        }
    }

    fn walk_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => self.walk_expr(expression),
            Stmt::Block { statements } => statements.iter().for_each(|s| self.walk_stmt(s)),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(condition);
                self.walk_stmt(then_branch);
                if let Some(else_branch) = else_branch {
                    self.walk_stmt(else_branch);
                }
            }
            Stmt::While { condition, body } => {
                self.walk_expr(condition);
                self.walk_stmt(body);
            }
            Stmt::For {
                initialiser,
                condition,
                increment,
                body,
            } => {
                if let Some(initialiser) = initialiser {
                    self.walk_stmt(initialiser);
                }
                if let Some(condition) = condition {
                    self.walk_expr(condition);
                }
                if let Some(increment) = increment {
                    self.walk_expr(increment);
                }
                self.walk_stmt(body);
            }
            Stmt::Return {
                value: Some(value), ..
            } => self.walk_expr(value),
            Stmt::Variable {
                initialiser: Some(initialiser),
                ..
            } => self.walk_expr(initialiser),
            _ => {}
        }
    }

    fn walk_expr(&mut self, expr: &Expr) {
        match expr {
            // Any write reaching `this` rules the method out.
            Expr::MemberAssignment { object, value, .. } => {
                if Self::targets_this(object) {
                    self.mutates = true;
                }
                self.walk_expr(value);
            }
            Expr::IndexAssignment {
                object,
                index,
                value,
                ..
            } => {
                if Self::targets_this(object) {
                    self.mutates = true;
                }
                self.walk_expr(object);
                self.walk_expr(index);
                self.walk_expr(value);
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                // Calling a non-const method on `this` mutates it transitively.
                if let Expr::MemberAccess { object, name } = &**callee {
                    if Self::targets_this(object) && !self.const_methods.contains(&name.lexeme) {
                        self.mutates = true;
                    }
                }
                self.walk_expr(callee);
                arguments.iter().for_each(|a| self.walk_expr(a));
            }
            Expr::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            Expr::Unary { right, .. } => self.walk_expr(right),
            Expr::Grouping { expression } => self.walk_expr(expression),
            Expr::Array { elements, .. } | Expr::Tuple { elements } => {
                elements.iter().for_each(|e| self.walk_expr(e))
            }
            Expr::Assignment { value, .. } | Expr::StaticAssignment { value, .. } => {
                self.walk_expr(value)
            }
            Expr::MemberAccess { object, .. }
            | Expr::StaticAccess { object, .. }
            | Expr::Cast { object, .. }
            | Expr::Reference { object }
            | Expr::MutReference { object } => self.walk_expr(object),
            Expr::Index { object, index, .. } => {
                self.walk_expr(object);
                self.walk_expr(index);
            }
            Expr::ClassInit { arguments, .. } | Expr::GenericCall { arguments, .. } => {
                arguments.iter().for_each(|a| self.walk_expr(a))
            }
            Expr::Closure { body, .. } => self.walk_stmt(body),
            Expr::Intrinsic { .. } | Expr::Literal { .. } | Expr::Variable { .. } => {}
        }
    }
}
