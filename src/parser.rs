use crate::ast::*;
use crate::token::*;
use crate::errors::*;
use crate::ty::*;

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub had_error: bool,
    pub source: String,
    pub filename: String,

    pub errors: usize,

    current_modifier: Modifier,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String, filename: String) -> Parser {
        Parser {
            tokens,
            current: 0,
            had_error: false,
            source,
            filename,
            errors: 0,
            current_modifier: Modifier::None,
        }
    }

    pub fn parse(&mut self) -> Result<Module, Error> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            let statement = self.declaration();
            if let Err(e) = statement {
                eprintln!("{}", e.to_string());
                self.synchronize();
            } else {
                statements.push(Box::new(statement?));
            }
        }

        Ok(Module { statements })
    }

    pub fn declaration(&mut self) -> Result<Stmt, Error> {
        if self.current_modifier != Modifier::None {
            return Err(self.error(String::from("Unexpected modifier.")));
        }

        if self.match_token(TokenKind::Public) {
            self.current_modifier = Modifier::Public;
        } else if self.match_token(TokenKind::Private) {
            self.current_modifier = Modifier::Private;
        } else if self.match_token(TokenKind::Protected) {
            self.current_modifier = Modifier::Protected;
        } else if self.match_token(TokenKind::Static) {
            self.current_modifier = Modifier::Static;
        }

        if self.match_token(TokenKind::Fn) {
            return self.function_declaration("function");
        } else if self.match_token(TokenKind::Let) {
            return self.variable_declaration();
        } else if self.match_token(TokenKind::Const) {
            return self.variable_declaration();
        } else if self.match_token(TokenKind::Class) {
            return self.class_declaration();
        } else if self.current_modifier != Modifier::None {
            return Err(self.error(String::from("Unexpected modifier.")));
        } else if self.match_token(TokenKind::Import) {
            return self.import_declaration();
        }

        self.statement()
    }

    pub fn statement(&mut self) -> Result<Stmt, Error> {
        if self.match_token(TokenKind::If) {
            return self.if_statement();
        } else if self.match_token(TokenKind::While) {
            return self.while_statement();
        } else if self.match_token(TokenKind::For) {
            return self.for_statement();
        } else if self.match_token(TokenKind::Return) {
            return self.return_statement();
        } else if self.match_token(TokenKind::Break) {
            return self.break_statement();
        } else if self.match_token(TokenKind::Continue) {
            return self.continue_statement();
        } else if self.match_token(TokenKind::LBrace) {
            return Ok(Stmt::Block { statements: self.block()?.into_iter().map(Box::new).collect() });
        }

        self.expression_statement()
    }

    pub fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenKind::RBrace, "Expected '}' after block.")?;
        Ok(statements)
    }

    pub fn if_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenKind::LParen, "Expected '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RParen, "Expected ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_token(TokenKind::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If { condition: Box::new(condition), then_branch, else_branch })
    }

    pub fn while_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenKind::LParen, "Expected '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RParen, "Expected ')' after while condition.")?;

        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition: Box::new(condition), body } )
    }

    pub fn for_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenKind::LParen, "Expected '(' after 'for'.")?;

        let initialiser = if self.match_token(TokenKind::Semicolon) {
            None
        } else if self.match_token(TokenKind::Let) {
            Some(self.variable_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after loop condition.")?;

        let increment = if !self.check(TokenKind::RParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::RParen, "Expected ')' after for clauses.")?;

        let body = Box::new(self.statement()?);

        Ok(Stmt::For { initialiser: initialiser.map(Box::new), condition: condition.map(Box::new), increment: increment.map(Box::new), body })
    }

    pub fn return_statement(&mut self) -> Result<Stmt, Error> {
        let token = self.previous();
        let value = if !self.check(TokenKind::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after return value.")?;
        Ok(Stmt::Return { token, value: value.map(Box::new) })
    }

    pub fn break_statement(&mut self) -> Result<Stmt, Error> {
        let token = self.previous();
        self.consume(TokenKind::Semicolon, "Expected ';' after 'break'.")?;
        Ok(Stmt::Break { token })
    }

    pub fn continue_statement(&mut self) -> Result<Stmt, Error> {
        let token = self.previous();
        self.consume(TokenKind::Semicolon, "Expected ';' after 'continue'.")?;
        Ok(Stmt::Continue { token })
    }

    pub fn variable_declaration(&mut self) -> Result<Stmt, Error> {
        let mut modifiers = vec![];
        if self.current_modifier != Modifier::None {
            modifiers.push(self.current_modifier.clone());
            self.current_modifier = Modifier::None;
        }

        let mut derived = Vec::new();

        let is_const = self.previous().kind == TokenKind::Const;
        if is_const {
            derived.push(Derived::Const);
        }

        while self.match_token(TokenKind::Amp) || self.match_token(TokenKind::Hash) || self.match_token(TokenKind::Mul) {
            if self.previous().kind == TokenKind::Amp {
                derived.push(Derived::Ref);
            } else if self.previous().kind == TokenKind::Hash {
                derived.push(Derived::MutRef);
            } else if self.previous().kind == TokenKind::Mul {
                derived.push(Derived::Ptr);
            }
        }

        let name = self.consume(TokenKind::Identifier, "Expected variable name.")?;
        self.consume(TokenKind::Colon, "Expected ':' after const declaration.")?;

        let type_ = self.type_expression()?;

        let initialiser = if is_const {
            self.consume(TokenKind::Eq, "Expected '=' after const declaration.")?;
            Some(self.expression()?)
        } else if self.match_token(TokenKind::Eq) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after variable declaration.")?;
        Ok(Stmt::Variable { name, type_, initialiser: initialiser.map(Box::new), derived, modifiers })
    }

    pub fn function_declaration(&mut self, kind: &str) -> Result<Stmt, Error> {
        let mut function_modifier = Vec::new();

        if self.current_modifier != Modifier::None {
            function_modifier.push(self.current_modifier.clone());
            self.current_modifier = Modifier::None;
        }

        let name = if kind == "function" {
            self.consume(TokenKind::Identifier, "Expected function name.")?
        } else {
            Token::new(TokenKind::Identifier, "".to_string(), 0, Span::new(0, 0))
        };

        let mut generics = Vec::new();

        if self.match_token(TokenKind::Lt) {
            loop {
                generics.push(self.consume(TokenKind::Identifier, "Expected generic name.")?);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }

            self.consume(TokenKind::Gt, "Expected '>' after generics.")?;
        }

        self.consume(TokenKind::LParen, &format!("Expected '(' after {} name.", kind))?;
        let mut params = Vec::new();

        if !self.check(TokenKind::RParen) {
            loop {
                let mut derived = Vec::new();

                while self.match_token(TokenKind::Amp) || self.match_token(TokenKind::Hash) || self.match_token(TokenKind::Mul) {
                    if self.previous().kind == TokenKind::Amp {
                        derived.push(Derived::Ref);
                    } else if self.previous().kind == TokenKind::Hash {
                        derived.push(Derived::MutRef);
                    } else if self.previous().kind == TokenKind::Mul {
                        derived.push(Derived::Ptr);
                    }
                }

                let param_name = self.consume(TokenKind::Identifier, "Expected parameter name.")?;
                self.consume(TokenKind::Colon, "Expected ':' after parameter name.")?;
                let type_ = self.type_expression()?;
                params.push(Box::new(Stmt::Variable { name: param_name, type_, initialiser: None, derived, modifiers: vec![] }));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RParen, "Expected ')' after parameters.")?;

        let return_type: Type;
        if self.match_token(TokenKind::Arrow) {
            return_type = self.type_expression()?;
        } else {
            return_type = Type {
                attributes: vec![],
                name: Token::new(TokenKind::Identifier, "void".to_string(), self.previous().line, self.previous().span),
                derived: vec![],
                generics: vec![],
                kind: TypeKind::Void,
                is_constructor: false,
            };
        }

        self.consume(TokenKind::LBrace, "Expected '{' before function body.")?;
        let body = self.block()?;

        Ok(Stmt::Function { name, params: params.into_iter().collect(), body: body.into_iter().map(Box::new).collect(), return_type, modifiers: function_modifier, generics })
    }

    pub fn class_declaration(&mut self) -> Result<Stmt, Error> {
        let mut class_modifier = Vec::new();

        if self.current_modifier != Modifier::None {
            class_modifier.push(self.current_modifier.clone());
            self.current_modifier = Modifier::None;
        }

        let name = self.consume(TokenKind::Identifier, "Expected class name.")?;

        let mut generics = Vec::new();

        if self.match_token(TokenKind::Lt) {
            loop {
                generics.push(self.consume(TokenKind::Identifier, "Expected generic name.")?);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }

            self.consume(TokenKind::Gt, "Expected '>' after generics.")?;
        }

        let mut public_methods: Vec<Box<Stmt>> = Vec::new();
        let mut private_methods: Vec<Box<Stmt>> = Vec::new();
        let mut protected_methods: Vec<Box<Stmt>> = Vec::new();
        let mut static_methods: Vec<Box<Stmt>> = Vec::new();
        let mut public_fields: Vec<Box<Stmt>> = Vec::new();
        let mut private_fields: Vec<Box<Stmt>> = Vec::new();
        let mut protected_fields: Vec<Box<Stmt>> = Vec::new();
        let mut static_fields: Vec<Box<Stmt>> = Vec::new();

        let derived = Vec::new();

        if self.match_token(TokenKind::LParen) {
            if !self.check(TokenKind::RParen) {
                loop {
                    if self.match_token(TokenKind::Public) {
                        let param_name = self.consume(TokenKind::Identifier, "Expected parameter name.")?;
                        self.consume(TokenKind::Colon, "Expected ':' after parameter name.")?;
                        let mut type_ = self.type_expression()?;
                        type_.is_constructor = true;
                        let modifiers = vec![Modifier::Public, Modifier::Constructor];
                        let param = Stmt::Variable { name: param_name, type_, initialiser: None, derived: derived.clone(), modifiers };
                        public_fields.push(Box::new(param));
                    } else if self.match_token(TokenKind::Private) {
                        let param_name = self.consume(TokenKind::Identifier, "Expected parameter name.")?;
                        self.consume(TokenKind::Colon, "Expected ':' after parameter name.")?;
                        let mut type_ = self.type_expression()?;
                        type_.is_constructor = true;
                        let modifiers = vec![Modifier::Private, Modifier::Constructor];
                        let param = Stmt::Variable { name: param_name, type_, initialiser: None, derived: derived.clone(), modifiers };
                        private_fields.push(Box::new(param));
                    } else if self.match_token(TokenKind::Protected) {
                        let param_name = self.consume(TokenKind::Identifier, "Expected parameter name.")?;
                        self.consume(TokenKind::Colon, "Expected ':' after parameter name.")?;
                        let mut type_ = self.type_expression()?;
                        type_.is_constructor = true;
                        let modifiers = vec![Modifier::Protected, Modifier::Constructor];
                        let param = Stmt::Variable { name: param_name, type_, initialiser: None, derived: derived.clone(), modifiers };
                        protected_fields.push(Box::new(param));
                    } else {
                        let param_name = self.consume(TokenKind::Identifier, "Expected parameter name.")?;
                        self.consume(TokenKind::Colon, "Expected ':' after parameter name.")?;
                        let mut type_ = self.type_expression()?;
                        type_.is_constructor = true;
                        let modifiers = vec![Modifier::Private, Modifier::Constructor];
                        let param = Stmt::Variable { name: param_name, type_, initialiser: None, derived: derived.clone(), modifiers };
                        private_fields.push(Box::new(param));
                    }

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }

                self.consume(TokenKind::RParen, "Expected ')' after parameters.")?;

                self.consume(TokenKind::LBrace, "Expected '{' before class body.")?;

                let mut accessors_defined: Vec<TokenKind> = vec![];
                while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                    if self.match_token(TokenKind::Public)
                        || self.match_token(TokenKind::Private)
                        || self.match_token(TokenKind::Protected)
                        || self.match_token(TokenKind::Static) {
                        accessors_defined.clear();
                        accessors_defined.push(self.previous().kind);

                        self.consume(TokenKind::Colon, "Expected ':' after accessor.")?;

                        while !self.check(TokenKind::RBrace) && !self.is_at_end()
                            && !self.check(TokenKind::Public)
                            && !self.check(TokenKind::Private)
                            && !self.check(TokenKind::Protected)
                            && !self.check(TokenKind::Static)
                        {
                            let name = self.consume(TokenKind::Identifier, "Expected field or function name.")?;

                            if self.match_token(TokenKind::Colon) {
                                let type_ = self.type_expression()?;
                                
                                if self.match_token(TokenKind::Eq) {
                                    let initialiser = Some(self.expression()?);
                                    self.consume(TokenKind::Semicolon, "Expected ';' after field declaration.")?;

                                    let mut field = Stmt::Variable { name, type_, initialiser: initialiser.map(Box::new), derived: derived.clone(), modifiers: vec![] };
                                    if accessors_defined[0] == TokenKind::Public {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Public);
                                        }
                                        public_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Private {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Private);
                                        }
                                        private_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Protected {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Protected);
                                        }
                                        protected_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Static {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Static);
                                            modifiers.push(Modifier::Public);
                                        }
                                        static_fields.push(Box::new(field));
                                    }
                                } else {
                                    self.consume(TokenKind::Semicolon, "Expected ';' after field declaration.")?;

                                    let mut field = Stmt::Variable { name, type_, initialiser: None, derived: derived.clone(), modifiers: vec![] };

                                    if accessors_defined[0] == TokenKind::Public {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Public);
                                        }
                                        public_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Private {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Private);
                                        }
                                        private_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Protected {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Protected);
                                        }
                                        protected_fields.push(Box::new(field));
                                    } else if accessors_defined[0] == TokenKind::Static {
                                        if let Stmt::Variable { ref mut modifiers, .. } = field {
                                            modifiers.push(Modifier::Static);
                                            modifiers.push(Modifier::Public);
                                        }
                                        static_fields.push(Box::new(field));
                                    }
                                }
                            } else {
                                let mut method = self.function_declaration("method")?;
                                
                                if let Stmt::Function { name: ref mut n, ref mut modifiers, .. } = method {
                                    *n = name;

                                    if accessors_defined[0] == TokenKind::Public {
                                        modifiers.push(Modifier::Public);
                                        public_methods.push(Box::new(method));
                                    } else if accessors_defined[0] == TokenKind::Private {
                                        modifiers.push(Modifier::Private);
                                        private_methods.push(Box::new(method));
                                    } else if accessors_defined[0] == TokenKind::Protected {
                                        modifiers.push(Modifier::Protected);
                                        protected_methods.push(Box::new(method));
                                    } else if accessors_defined[0] == TokenKind::Static {
                                        modifiers.push(Modifier::Static);
                                        static_methods.push(Box::new(method));
                                    }
                                }
                            }
                        }
                    }
                }

                self.consume(TokenKind::RBrace, "Expected '}' after class body.")?;

                return Ok(Stmt::Class {
                    name,
                    generics,
                    modifier: class_modifier,
                    public_methods,
                    private_methods,
                    protected_methods,
                    static_methods,
                    public_fields,
                    private_fields,
                    protected_fields,
                    static_fields,
                });
            } else {
                self.consume(TokenKind::RParen, "Expected ')' after parameters.")?;
            }

            self.consume(TokenKind::LBrace, "Expected '{' before class body.")?;

            while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                if self.match_token(TokenKind::Public) || self.match_token(TokenKind::Private) || self.match_token(TokenKind::Protected) || self.match_token(TokenKind::Static) {
                    let accessors_defined = vec![self.previous().kind];

                    self.consume(TokenKind::Colon, "Expected ':' after accessor.")?;

                    while !self.check(TokenKind::RBrace) && !self.is_at_end() && !self.match_token(TokenKind::Public) && !self.match_token(TokenKind::Private) && !self.match_token(TokenKind::Protected) && !self.match_token(TokenKind::Static) {
                        let name = self.consume(TokenKind::Identifier, "Expected field or function name.")?;

                        if self.match_token(TokenKind::Colon) {
                            let type_ = self.type_expression()?;

                            let initialiser = if self.match_token(TokenKind::Eq) {
                                Some(self.expression()?)
                            } else {
                                None
                            };

                            self.consume(TokenKind::Semicolon, "Expected ';' after field declaration.")?;

                            let mut field = Stmt::Variable { name, type_, initialiser: initialiser.map(Box::new), derived: derived.clone(), modifiers: vec![] };

                            if accessors_defined[0] == TokenKind::Public {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Public);
                                }
                                public_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Private {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Private);
                                }
                                private_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Protected {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Protected);
                                }
                                protected_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Static {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Static);
                                    modifiers.push(Modifier::Public);
                                }
                                static_fields.push(Box::new(field));
                            }
                        } else {
                            let mut method = self.function_declaration("method")?;
                                    
                            if let Stmt::Function { name: ref mut n, ref mut modifiers, .. } = method {
                                *n = name;

                                if accessors_defined[0] == TokenKind::Public {
                                    modifiers.push(Modifier::Public);
                                    public_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Private {
                                    modifiers.push(Modifier::Private);
                                    private_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Protected {
                                    modifiers.push(Modifier::Protected);
                                    protected_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Static {
                                    modifiers.push(Modifier::Static);
                                    static_methods.push(Box::new(method));
                                }
                            }
                        }
                    }
                }
            }

            self.consume(TokenKind::RBrace, "Expected '}' after class body.")?;

            return Ok(Stmt::Class { name, generics, modifier: class_modifier, public_methods, private_methods, protected_methods, static_methods, public_fields, private_fields, protected_fields, static_fields });
        } else {
            self.consume(TokenKind::LBrace, "Expected '{' before class body.")?;

            while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                if self.match_token(TokenKind::Public) || self.match_token(TokenKind::Private) || self.match_token(TokenKind::Protected) || self.match_token(TokenKind::Static) {
                    let accessors_defined = vec![self.previous().kind];

                    self.consume(TokenKind::Colon, "Expected ':' after accessor.")?;

                    while !self.check(TokenKind::RBrace) && !self.is_at_end() && !self.match_token(TokenKind::Public) && !self.match_token(TokenKind::Private) && !self.match_token(TokenKind::Protected) && !self.match_token(TokenKind::Static) {
                        let name = self.consume(TokenKind::Identifier, "Expected field or function name.")?;

                        if self.match_token(TokenKind::Colon) {
                            let type_ = self.type_expression()?;

                            let initialiser = if self.match_token(TokenKind::Eq) {
                                Some(self.expression()?)
                            } else {
                                None
                            };

                            self.consume(TokenKind::Semicolon, "Expected ';' after field declaration.")?;

                            let mut field = Stmt::Variable { name, type_, initialiser: initialiser.map(Box::new), derived: derived.clone(), modifiers: vec![] };

                            if accessors_defined[0] == TokenKind::Public {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Public);
                                }
                                public_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Private {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Private);
                                }
                                private_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Protected {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Protected);
                                }
                                protected_fields.push(Box::new(field));
                            } else if accessors_defined[0] == TokenKind::Static {
                                if let Stmt::Variable { ref mut modifiers, .. } = field {
                                    modifiers.push(Modifier::Static);
                                    modifiers.push(Modifier::Public);
                                }
                                static_fields.push(Box::new(field));
                            }
                        } else {
                            let mut method = self.function_declaration("method")?;
                                    
                            if let Stmt::Function { name: ref mut n, ref mut modifiers, .. } = method {
                                *n = name;

                                if accessors_defined[0] == TokenKind::Public {
                                    modifiers.push(Modifier::Public);
                                    public_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Private {
                                    modifiers.push(Modifier::Private);
                                    private_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Protected {
                                    modifiers.push(Modifier::Protected);
                                    protected_methods.push(Box::new(method));
                                } else if accessors_defined[0] == TokenKind::Static {
                                    modifiers.push(Modifier::Static);
                                    static_methods.push(Box::new(method));
                                }
                            }
                        }
                    }
                }
            }

            self.consume(TokenKind::RBrace, "Expected '}' after class body.")?;

            return Ok(Stmt::Class { name, generics, modifier: class_modifier, public_methods, private_methods, protected_methods, static_methods, public_fields, private_fields, protected_fields, static_fields });
        }
    }

    pub fn type_expression(&mut self) -> Result<Type, Error> {
        let mut attributes: Vec<Attribute> = Vec::new();
        let mut derived = Vec::new();
        let mut generics = Vec::new();
        let mut kind: TypeKind;
        let name: Token;

        while self.match_token(TokenKind::Amp) || self.match_token(TokenKind::Hash) || self.match_token(TokenKind::Mul) {
            if self.previous().kind == TokenKind::Amp {
                derived.push(Derived::Ref);
            } else if self.previous().kind == TokenKind::Hash {
                derived.push(Derived::MutRef);
            } else if self.previous().kind == TokenKind::Mul {
                derived.push(Derived::Ptr);
            }
        }

        // Array handling
        if self.check(TokenKind::LBracket) {
            let mut clone: Vec<Vec<Derived>> = vec![];
            let mut depth: usize = 0;
    
            while self.match_token(TokenKind::LBracket) {
                depth += 1;
                clone.push(Vec::new());
                clone[depth-1].push(Derived::Array);

                while self.match_token(TokenKind::Amp) || self.match_token(TokenKind::Hash) || self.match_token(TokenKind::Mul) {
                    if self.previous().kind == TokenKind::Amp {
                        clone[depth-1].push(Derived::Ref);
                    } else if self.previous().kind == TokenKind::Hash {
                        clone[depth-1].push(Derived::MutRef);
                    } else if self.previous().kind == TokenKind::Mul {
                        clone[depth-1].push(Derived::Ptr);
                    }
                }
            }

            if self.match_token(TokenKind::Fn) {
                let mut generics = Vec::new();
    
                if self.match_token(TokenKind::Lt) {
                    loop {
                        generics.push(self.type_expression()?);
    
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
    
                    self.consume(TokenKind::Gt, "Expected '>' after function generics.")?;
                }
    
                self.consume(TokenKind::LParen, "Expected '(' before function parameters.")?;
    
                let mut parameters = Vec::new();
    
                if !self.check(TokenKind::RParen) {
                    loop {
                        parameters.push(self.type_expression()?);
    
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }
    
                self.consume(TokenKind::RParen, "Expected ')' after function parameters.")?;
                self.consume(TokenKind::Arrow, "Expected '->' before function return type.")?;
    
                let return_type = self.type_expression()?;
                let name = Token::new(TokenKind::Fn, "".to_string(), 0, Span::new(0, 0));
    
                kind = TypeKind::Function(parameters, Box::new(return_type));

                for _ in 0..depth {
                    self.consume(TokenKind::RBracket, "Unmatched ']' in type expression.")?;
                }

                for c in clone.iter().rev() {
                    kind = self.apply_derived(kind, &c, &attributes, &name, depth);
                }

                kind = self.apply_derived(kind, &derived, &attributes, &name, depth);
    
                let function_type = Type {
                    attributes: attributes.clone(),
                    name: name.clone(),
                    derived: derived.clone(),
                    generics: generics.into_iter().map(Box::new).collect(),
                    kind,
                    is_constructor: false
                };

                return Ok(function_type);
            } else {
                name = self.consume(TokenKind::Identifier, "Expected type name.")?;
    
                let mut inner = TypeKind::from_name(&name.lexeme.clone());

                for _ in 0..depth {
                    self.consume(TokenKind::RBracket, "Unmatched ']' in type expression.")?;
                }

                for c in clone.iter().rev() {
                    inner = self.apply_derived(inner, &c, &attributes, &name, depth);
                }

                kind = self.apply_derived(inner, &derived, &attributes, &name, depth);
            }
        } else if self.match_token(TokenKind::Fn) {
            let mut generics = Vec::new();
    
            if self.match_token(TokenKind::Lt) {
                loop {
                    generics.push(self.type_expression()?);
    
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
    
                self.consume(TokenKind::Gt, "Expected '>' after function generics.")?;
            }
    
            self.consume(TokenKind::LParen, "Expected '(' before function parameters.")?;
    
            let mut parameters = Vec::new();
    
            if !self.check(TokenKind::RParen) {
                loop {
                    parameters.push(self.type_expression()?);
    
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }
    
            self.consume(TokenKind::RParen, "Expected ')' after function parameters.")?;
            self.consume(TokenKind::Arrow, "Expected '->' before function return type.")?;
    
            let return_type = self.type_expression()?;
            let name = Token::new(TokenKind::Fn, "".to_string(), 0, Span::new(0, 0));
    
            kind = TypeKind::Function(parameters, Box::new(return_type));
    
            kind = self.apply_derived(kind, &derived, &attributes, &name, 0);
    
            let function_type = Type {
                attributes: attributes.clone(),
                name: name.clone(),
                derived: derived.clone(),
                generics: generics.into_iter().map(Box::new).collect(),
                kind,
                is_constructor: false,
            };

            return Ok(function_type); 
        } else if self.match_token(TokenKind::LParen) {
            // Tuple handling
            let mut tuple = Vec::new();

            if !self.check(TokenKind::RParen) {
                loop {
                    tuple.push(self.type_expression()?);

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }

            self.consume(TokenKind::RParen, "Expected ')' after tuple type.")?;

            kind = TypeKind::Tuple(tuple.clone());

            let mut lexeme = String::from("(");

            for e in tuple.iter() {
                lexeme.push_str(&e.name.lexeme);
                lexeme.push_str(", ");
            }

            lexeme = lexeme.trim_end_matches(", ").to_string() + ")";

            name = Token::new(TokenKind::Identifier, lexeme, tuple.last().unwrap().name.line, tuple.last().unwrap().name.span.clone());
        } else {
            // check if it is a reference, pointer, mutref
            // if it is, set kind to be Ref(Pointer(MutRef(...))) etc...
            name = self.consume(TokenKind::Identifier, "Expected type name.")?;
            kind = TypeKind::from_name(&name.lexeme.clone());
            kind = self.apply_derived(kind, &derived, &attributes, &name, 0);
        }

        if self.match_token(TokenKind::Lt) {
            loop {
                generics.push(self.type_expression()?);
    
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
    
            self.consume(TokenKind::Gt, "Expected '>' after type generics.")?;
            attributes.push(Attribute::Class);
    
            return Ok(Type {
                attributes,
                name,
                generics: generics.into_iter().map(Box::new).collect(),
                derived,
                kind,
                is_constructor: false,
            });
        }
    
        Ok(Type {
            attributes,
            name,
            generics: vec![],
            derived,
            kind,
            is_constructor: false,
        })
    }

    pub fn import_declaration(&mut self) -> Result<Stmt, Error> {
        let path = Expr::Literal { value: self.consume(TokenKind::String, "Expected string after 'import'.")? };
        self.consume(TokenKind::As, "Expected 'as' after import path.")?;
        let alias = self.consume(TokenKind::Identifier, "Expected identifier after 'as'.")?;

        self.consume(TokenKind::Semicolon, "Expected ';' after import statement.")?;
        Ok(Stmt::Import { path: Box::new(path), alias })
    }

    pub fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ';' after expression.")?;
        Ok(Stmt::Expression { expression: Box::new(expr) })
    }

    pub fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    pub fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.logical_or()?;

        if self.match_token(TokenKind::Eq) || self.match_token(TokenKind::PlusEq) || self.match_token(TokenKind::MinusEq) || self.match_token(TokenKind::MulEq) || self.match_token(TokenKind::DivEq) || self.match_token(TokenKind::ModEq) || self.match_token(TokenKind::AmpEq) || self.match_token(TokenKind::PipeEq) || self.match_token(TokenKind::CaretEq) || self.match_token(TokenKind::LShiftEq) || self.match_token(TokenKind::RShiftEq) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable { name } = expr {
                return Ok(Expr::Assignment { name, value: Box::new(value), op: equals });
            } else if let Expr::MemberAccess { object, name } = expr {
                return Ok(Expr::MemberAssignment { object, name, value: Box::new(value), op: equals });
            } else if let Expr::StaticAccess { object, name } = expr {
                return Ok(Expr::StaticAssignment { object, name, value: Box::new(value), op: equals });
            } else if let Expr::Index { object, index, token } = expr {
                return Ok(Expr::IndexAssignment { object, index, value: Box::new(value), op: equals, token });
            }

            return Err(self.error(String::from("Invalid assignment target.")))
        }

        Ok(expr)
    }

    pub fn logical_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.logical_and()?;

        while self.match_token(TokenKind::Or) {
            let op = self.previous().clone();
            let right = self.logical_and()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn logical_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.nullish_coalesce()?;

        while self.match_token(TokenKind::And) {
            let op = self.previous().clone();
            let right = self.nullish_coalesce()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn nullish_coalesce(&mut self) -> Result<Expr, Error> {
        let mut expr = self.bitwise_or()?;

        while self.match_token(TokenKind::QuestionQuestion) {
            let op = self.previous().clone();
            let right = self.bitwise_or()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn bitwise_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.bitwise_xor()?;

        while self.match_token(TokenKind::Pipe) {
            let op = self.previous().clone();
            let right = self.bitwise_xor()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn bitwise_xor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.bitwise_and()?;

        while self.match_token(TokenKind::Caret) {
            let op = self.previous().clone();
            let right = self.bitwise_and()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn bitwise_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.match_token(TokenKind::Amp) {
            let op = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.match_token(TokenKind::EqEq) || self.match_token(TokenKind::Neq) {
            let op = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.shift()?;

        while self.match_token(TokenKind::Lt) || self.match_token(TokenKind::Lte) || self.match_token(TokenKind::Gt) || self.match_token(TokenKind::Gte) {
            let op = self.previous().clone();
            let right = self.shift()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn shift(&mut self) -> Result<Expr, Error> {
        let mut expr = self.addition()?;

        while self.match_token(TokenKind::LShift) || self.match_token(TokenKind::RShift) {
            let op = self.previous().clone();
            let right = self.addition()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn addition(&mut self) -> Result<Expr, Error> {
        let mut expr = self.multiplication()?;

        while self.match_token(TokenKind::Plus) || self.match_token(TokenKind::Minus) {
            let op = self.previous().clone();
            let right = self.multiplication()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn multiplication(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_token(TokenKind::Mul) || self.match_token(TokenKind::Div) || self.match_token(TokenKind::Mod) {
            let op = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::Bang) || self.match_token(TokenKind::Minus) || self.match_token(TokenKind::Tilde) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary { op, right: Box::new(right) });
        }

        self.reference()
    }

    pub fn reference(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::Amp) {
            let right = self.reference()?;
            return Ok(Expr::Reference { object: Box::new(right) });
        }

        self.mutreference()
    }

    pub fn mutreference(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::Hash) {
            let right = self.mutreference()?;
            return Ok(Expr::MutReference { object: Box::new(right) });
        }

        self.dereference()
    }

    pub fn dereference(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::Mul) {
            let right = self.dereference()?;
            return Ok(Expr::Dereference { object: Box::new(right) });
        }

        self.cast()
    }

    pub fn cast(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::As) {
            let type_ = self.type_expression()?;
            let right = self.cast()?;
            return Ok(Expr::Cast { object: Box::new(right), type_ });
        }

        self.index()
    }

    pub fn index(&mut self) -> Result<Expr, Error> {
        let mut expr = self.member_access()?;

        while self.match_token(TokenKind::LBracket) {
            let index = self.expression()?;
            let token = self.previous();
            self.consume(TokenKind::RBracket, "Expected ']' after index.")?;
            expr = Expr::Index { object: Box::new(expr), index: Box::new(index), token };
        }

        Ok(expr)
    }

    pub fn member_access(&mut self) -> Result<Expr, Error> {
        let mut expr = self.call()?;

        while self.match_token(TokenKind::Dot) || self.match_token(TokenKind::StaticAccess) {
            let is_static = self.previous().kind == TokenKind::StaticAccess;
            let name = self.consume(TokenKind::Identifier, "Expected property name after '.' or '::'.")?;
            
            if is_static {
                expr = Expr::StaticAccess { object: Box::new(expr), name };
            } else {
                expr = Expr::MemberAccess { object: Box::new(expr), name };
            }
        }

        Ok(expr)
    }

    pub fn call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        let mut generics: Vec<Type> = Vec::new();
        loop {
            let mut is_generic = false;
            if self.match_token(TokenKind::Lt) {
                is_generic = true;
                generics = Vec::new();
                loop {
                    generics.push(self.type_expression()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }

                self.consume(TokenKind::Gt, "Expected '>' after generic parameters.")?;
            }
            if self.match_token(TokenKind::LParen) {
                let paren = self.previous();
                let mut arguments = Vec::new();

                if !self.check(TokenKind::RParen) {
                    loop {
                        arguments.push(self.expression()?);

                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                }

                self.consume(TokenKind::RParen, "Expected ')' after arguments.")?;

                // Handle generic parameters
                if is_generic {
                    expr = Expr::GenericCall {
                        callee: Box::new(expr),
                        paren,
                        arguments: arguments.into_iter().map(Box::new).collect(),
                        generics: generics.clone(),
                    };
                } else {
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        paren,
                        arguments: arguments.into_iter().map(Box::new).collect(),
                    };
                }
            } else if self.match_token(TokenKind::Dot) {
                let name = self.consume(TokenKind::Identifier, "Expected property name after '.'.")?;
                expr = Expr::MemberAccess { object: Box::new(expr), name };
            } else if self.match_token(TokenKind::LBracket) {
                let index = self.expression()?;
                let token = self.previous();
                self.consume(TokenKind::RBracket, "Expected ']' after index.")?;
                expr = Expr::Index { token, object: Box::new(expr), index: Box::new(index) };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub fn primary(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenKind::Integer) || self.match_token(TokenKind::Float) || self.match_token(TokenKind::String) {
            return Ok(Expr::Literal { value: self.previous().clone() });
        }

        if self.match_token(TokenKind::Identifier) {
            return Ok(Expr::Variable { name: self.previous().clone() });
        }

        if self.match_token(TokenKind::LParen) {
            let expr = self.expression()?;
            if self.match_token(TokenKind::Comma) {
                let mut elements = vec![expr];

                loop {
                    elements.push(self.expression()?);

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }

                self.consume(TokenKind::RParen, "Expected ')' after expression.")?;
                return Ok(Expr::Tuple { elements: elements.into_iter().map(Box::new).collect() });
            }
            self.consume(TokenKind::RParen, "Expected ')' after expression.")?;
            return Ok(Expr::Grouping { expression: Box::new(expr) });
        }

        if self.match_token(TokenKind::LBracket) {
            let mut elements = Vec::new();

            if !self.check(TokenKind::RBracket) {
                loop {
                    elements.push(self.expression()?);

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
            }

            self.consume(TokenKind::RBracket, "Expected ']' after array elements.")?;
            return Ok(Expr::Array { elements: elements.into_iter().map(Box::new).collect() });
        }

        if self.match_token(TokenKind::New) {
            return self.class_init();
        }

        if self.match_token(TokenKind::Pipe){
            return self.closure(true);
        }

        if self.match_token(TokenKind::Or) {
            return self.closure(false);
        }

        if self.errors == 0 {
            Err(self.error(String::from("Expected expression.")))
        } else {
            Err(self.error_with_notes(
                String::from("Expected expression."),
                vec![Note::new(
                    String::from("This error was caused by a previous error."),
                    self.peek().line,
                    self.peek().span,
                    self.filename.clone(),
                )]
            ))
        }
    }

    pub fn class_init(&mut self) -> Result<Expr, Error> {
        let name = self.consume(TokenKind::Identifier, "Expected class name.")?;
        self.consume(TokenKind::LParen, "Expected '(' after class name.")?;
        let mut arguments = Vec::new();

        if !self.check(TokenKind::RParen) {
            loop {
                arguments.push(self.expression()?);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RParen, "Expected ')' after arguments.")?;
        Ok(Expr::ClassInit { name, arguments: arguments.into_iter().map(Box::new).collect() })
    }

    pub fn closure(&mut self, fields: bool) -> Result<Expr, Error> {
        let mut parameters = Vec::new();
        let mut param_types = Vec::new();

        let mut name = self.previous();

        let mut lexeme = String::new();

        if fields {
            if !self.check(TokenKind::Pipe) {
                loop {
                    parameters.push(self.consume(TokenKind::Identifier, "Expected parameter name.")?);
    
                    lexeme += &self.previous().lexeme;

                    if self.match_token(TokenKind::Colon) {
                        lexeme += &self.previous().lexeme;
                        let ty = self.type_expression()?;
                        param_types.push(ty.clone());

                        lexeme += &ty.name.lexeme;
                    }
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    lexeme += &self.previous().lexeme;
                }
            }

            self.consume(TokenKind::Pipe, "Expected '|' after closure parameters.")?;
            lexeme += &self.previous().lexeme;
        }

        let return_type = self.type_expression()?;
        lexeme += &return_type.name.lexeme;

        self.consume(TokenKind::Arrow, "Expected '->' before closure body.")?;        

        let body = Box::new(self.statement()?);

        name.lexeme = lexeme;

        Ok(Expr::Closure { name, parameters, param_types, body, return_type })
    }

    pub fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, Error> {
        if self.check(kind) {
            return Ok(self.advance());
        }

        Err(self.error(message.to_string()))
    }

    pub fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous().clone()
    }

    pub fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            return true;
        }

        false
    }

    pub fn check(&mut self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().kind == kind
    }

    pub fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EndOfFile
    }

    pub fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenKind::Fn | TokenKind::Let | TokenKind::Const | TokenKind::Class | TokenKind
                ::Import | TokenKind::If | TokenKind::While | TokenKind::For | TokenKind::Return | TokenKind::Break | TokenKind::Continue => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn error(&mut self, message: String) -> Error {
        self.had_error = true;
        self.errors += 1;
        let mut e = Error::new(message, self.peek().line, self.peek().span, self.filename.clone());
        e.add_source(self.source.clone());
        e
    }

    fn error_with_notes(&mut self, message: String, notes: Vec<Note>) -> Error {
        self.had_error = true;
        self.errors += 1;
        let mut e = Error::new(message, self.peek().line, self.peek().span, self.filename.clone());
        e.add_source(self.source.clone());

        for note in notes {
            e.add_note(note);
        }
        e
    }

    fn apply_derived(
        &self,
        kind: TypeKind,
        derived: &[Derived],
        attributes: &Vec<Attribute>,
        name: &Token,
        depth: usize
    ) -> TypeKind {
        let mut current_kind = kind;
        let mut depth_c = depth;
        for deriv in derived.iter().rev() {
            current_kind = match deriv {
                Derived::Ref => TypeKind::Reference(Box::new(Type {
                    attributes: attributes.clone(),
                    name: name.clone(),
                    derived: vec![Derived::Ref],
                    generics: Vec::new(),
                    kind: current_kind,
                    is_constructor: false,
                })),
                Derived::MutRef => TypeKind::MutRef(Box::new(Type {
                    attributes: attributes.clone(),
                    name: name.clone(),
                    derived: vec![Derived::MutRef],
                    generics: Vec::new(),
                    kind: current_kind,
                    is_constructor: false,
                })),
                Derived::Ptr => TypeKind::Pointer(Box::new(Type {
                    attributes: attributes.clone(),
                    name: name.clone(),
                    derived: vec![Derived::Ptr],
                    generics: Vec::new(),
                    kind: current_kind,
                    is_constructor: false,
                })),
                Derived::Array => {
                    depth_c -= 1;
                    TypeKind::Array(Box::new(Type {
                        attributes: attributes.clone(),
                        name: name.clone(),
                        derived: vec![Derived::Array],
                        generics: Vec::new(),
                        kind: current_kind,
                        is_constructor: false,
                    }), depth-depth_c)
                }
                _ => current_kind,
            };
        }
        current_kind
    }
}