//! Type-erased shared borrows: two pointers, one shared table per implementation.

use super::*;
use crate::typecheck::DynamicCast;
use std::collections::BTreeMap;

impl CppCodeGenerator {
    // Escape punctuation and underscores so e.g. `module.T` and `module_T`
    // cannot collide. Canonical spellings ignore import aliases and source tokens.
    fn dynamic_ident(identity: &str) -> String {
        identity
            .bytes()
            .map(|byte| {
                if byte.is_ascii_alphanumeric() {
                    (byte as char).to_string()
                } else {
                    format!("_{byte:02x}")
                }
            })
            .collect()
    }

    pub(super) fn dynamic_name(trait_type: &TypeKind) -> String {
        format!(
            "cardamom_dynamic_{}",
            Self::dynamic_ident(&trait_type.to_string())
        )
    }

    /// Checker metadata already has canonical owners and concrete types. Import
    /// aliases and generic bindings at the emission site must not rename them.
    pub(super) fn with_canonical_type_names(&mut self, emit: impl FnOnce(&mut Self)) {
        let imports = std::mem::take(&mut self.imports);
        let members = std::mem::take(&mut self.member_imports);
        let substitution = std::mem::take(&mut self.current_substitution);
        emit(self);
        self.imports = imports;
        self.member_imports = members;
        self.current_substitution = substitution;
    }

    fn dynamic_table_name(&self, cast: &DynamicCast) -> String {
        format!(
            "{}_for_{}_{}_table",
            Self::dynamic_name(&cast.trait_type.kind),
            Self::dynamic_ident(&cast.concrete.kind.to_string()),
            cast.implementation
                .as_ref()
                .map(|module| format!("impl_{}", Self::dynamic_ident(module)))
                .unwrap_or("structural".to_string()),
        )
    }

    fn dynamic_params(&self, method: &crate::typecheck::ExportedMethod) -> String {
        method
            .params
            .iter()
            .enumerate()
            .map(|(i, ty)| format!("{} arg{i}", self.translate_type(ty)))
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Signatures can mention forward-declared classes; method bodies wait until
    /// every class is complete, including classes from later modules.
    pub(super) fn write_dynamic_declarations(&mut self) {
        for descriptor in self.dynamic_traits.values().cloned().collect::<Vec<_>>() {
            let name = Self::dynamic_name(&descriptor.trait_type.kind);
            self.writeln(&format!("struct {name}_vtable {{"));
            self.indent_level += 1;
            for (method_name, method) in &descriptor.methods {
                let rest = self.dynamic_params(method);
                let rest = if rest.is_empty() {
                    rest
                } else {
                    format!(", {rest}")
                };
                self.writeln(&format!(
                    "{} (*slot_{})(const void*{rest});",
                    self.translate_type(&method.return_type),
                    Self::ident(method_name)
                ));
            }
            self.indent_level -= 1;
            self.writeln("};");
            self.writeln(&format!("struct {name} {{"));
            self.indent_level += 1;
            self.writeln("const void* _cardamom_data;");
            self.writeln(&format!("const {name}_vtable* _cardamom_vtable;"));
            for (method_name, method) in &descriptor.methods {
                self.writeln(&format!(
                    "{} {}({}) const;",
                    self.translate_type(&method.return_type),
                    Self::ident(method_name),
                    self.dynamic_params(method)
                ));
            }
            self.indent_level -= 1;
            self.writeln("};");
            self.writeln("");
        }
    }

    pub(super) fn write_dynamic_definitions(&mut self) {
        for descriptor in self.dynamic_traits.values().cloned().collect::<Vec<_>>() {
            let name = Self::dynamic_name(&descriptor.trait_type.kind);
            for (method_name, method) in &descriptor.methods {
                let args = (0..method.params.len())
                    .map(|i| format!(", arg{i}"))
                    .collect::<String>();
                self.writeln(&format!(
                    "{} {name}::{}({}) const {{",
                    self.translate_type(&method.return_type),
                    Self::ident(method_name),
                    self.dynamic_params(method)
                ));
                self.indent_level += 1;
                self.writeln(&format!(
                    "return _cardamom_vtable->slot_{}(_cardamom_data{args});",
                    Self::ident(method_name)
                ));
                self.indent_level -= 1;
                self.writeln("}");
            }
        }
        // AST tokens are not type identity. Deduplicate casts by canonical names,
        // including the selected implementation's module, and emit in stable order.
        let tables: BTreeMap<String, DynamicCast> = self
            .dynamic_casts
            .values()
            .map(|cast| (self.dynamic_table_name(cast), cast.clone()))
            .collect();
        for (table, cast) in tables {
            let descriptor = self.dynamic_traits[&cast.trait_type.kind.to_string()].clone();
            let handle = Self::dynamic_name(&cast.trait_type.kind);
            for (method_name, method) in &descriptor.methods {
                let slot = Self::ident(method_name);
                let rest = self.dynamic_params(method);
                let rest = if rest.is_empty() {
                    rest
                } else {
                    format!(", {rest}")
                };
                self.writeln(&format!(
                    "static {} {table}_{slot}(const void* data{rest}) {{",
                    self.translate_type(&method.return_type)
                ));
                self.indent_level += 1;
                self.writeln(&format!(
                    "const auto& self = *static_cast<const {}*>(data);",
                    self.translate_type(&cast.concrete)
                ));
                let args = (0..method.params.len())
                    .map(|i| format!("arg{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let call = match &cast.implementation {
                    Some(module) => {
                        let impl_name = Self::impl_name(
                            module,
                            &cast.trait_type.kind,
                            &cast.concrete.kind,
                            method_name,
                        );
                        let rest = if args.is_empty() {
                            args
                        } else {
                            format!(", {args}")
                        };
                        format!("{impl_name}(self{rest})")
                    }
                    None => format!("self.{slot}({args})"),
                };
                self.writeln(&format!("return {call};"));
                self.indent_level -= 1;
                self.writeln("}");
            }
            let entries = descriptor
                .methods
                .iter()
                .map(|(method, _)| format!("&{table}_{}", Self::ident(method)))
                .collect::<Vec<_>>()
                .join(", ");
            self.writeln(&format!(
                "static const {handle}_vtable {table} = {{{entries}}};"
            ));
            self.writeln("");
        }
    }

    pub(super) fn write_dynamic_cast(&mut self, expr: &Expr, object: &Expr) -> bool {
        if let Some(cast) = self.dynamic_casts.get(&(expr as *const Expr)).cloned() {
            self.extra_includes.insert("<memory>".to_string());
            self.output.push_str(&format!(
                "{}{{std::addressof(",
                Self::dynamic_name(&cast.trait_type.kind)
            ));
            object.accept(self);
            self.output
                .push_str(&format!("), &{}}}", self.dynamic_table_name(&cast)));
            return true;
        }
        // Identity casts do not rebuild the handle or take its address.
        if self
            .expr_types
            .get(&(expr as *const Expr))
            .is_some_and(|ty| ty.kind.dynamic_trait().is_some())
        {
            object.accept(self);
            return true;
        }
        false
    }
}
