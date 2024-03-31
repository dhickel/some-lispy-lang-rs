use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{Debug, format};
use std::ops::Deref;
use std::rc::Rc;
use ahash::{AHashMap, HashSet, HashSetExt};
use lasso::Spur;
use crate::eval;
use crate::eval::class_loader::ClassLoader;
use crate::eval::environment::Environment;
use crate::lang::types::Type;
use crate::parse::ast_nodes::{AstNode, DefClassData, DefFuncData, DirectInst, DefLambdaData, EvalResult, ExprNode, Field, FuncArg, InstArgs, LambdaValue, LitNode, ObjectValue, FuncCallData};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::{ast_nodes, Mod};


fn is_literal(name: &Spur, value: &AstNode) -> Result<(), String> {
    if !matches!(value, LiteralNode(_)) {
        Err(format!("Attempted to assign non literal value to{:?}", name))
    } else { Ok(()) }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub obj_type: Type,
    pub value: Rc<LitNode>,
    pub is_set: bool,
    pub private: bool,
    pub dynamic: bool,
    pub mutable: bool,
}


impl Binding {
    pub fn new_binding_ast(
        value: &AstNode,
        mods: &Option<Vec<Mod>>,
        priv_override: bool,
        is_set: bool,
    ) -> Result<Binding, String> {
        if let LiteralNode(lit) = &value {
            Self::new_binding_lit(lit, mods, priv_override, is_set)
        } else { Err(format!("Attempted to bind non literal object: {:?}", value)) }
    }

    pub fn new_binding_lit(
        value: &Rc<LitNode>,
        mods: &Option<Vec<Mod>>,
        priv_override: bool,
        is_set: bool,
    ) -> Result<Binding, String> {
        let mut mutable = false;
        let mut dynamic = false;
        let mut private = true;

        if let Some(mods) = mods {
            for m in mods {
                if *m == Mod::Mutable { mutable = true; }
                if *m == Mod::Dynamic { dynamic = true; }
                if *m == Mod::Public { private = false; }
            }
        }

        let obj_type = value.value().get_type();
        if dynamic {
            Ok(Binding {
                obj_type,
                value: Rc::clone(value),
                private: if priv_override { false } else { private },
                dynamic: true,
                mutable: true,
                is_set,
            })
        } else if mutable {
            Ok(Binding {
                obj_type,
                value: Rc::clone(value),
                private: if priv_override { false } else { private },
                dynamic: false,
                mutable: true,
                is_set,
            })
        } else {
            Ok(Binding {
                obj_type,
                value: Rc::clone(value),
                private: if priv_override { false } else { private },
                dynamic: false,
                mutable: false,
                is_set,
            })
        }
    }

    pub fn update_binding(value: &AstNode, dynamic: bool, mutable: bool, private: bool) -> Result<Binding, String> {
        let obj_type: Type;
        if let LiteralNode(lit) = value {
            obj_type = lit.value().get_type();

            Ok(Binding { obj_type, value: Rc::clone(&lit), private, dynamic, mutable, is_set: true })
        } else {
            return Err(format!("Attempted to bind non literal object: {:?}", value));
        }
    }
}


pub trait ObjectAccess {
    fn get_field(&self, name: &Spur) -> Result<Rc<LitNode>, String>;
    fn get_method(&self, name: &Spur) -> Result<LitNode, String>;
    fn set_field(&mut self, name: &Spur, value: &AstNode) -> Result<Rc<LitNode>, String>;
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    immutable: bool,
    data: AHashMap<Spur, Binding>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructMetaData {
    immutable: bool,
    data: Option<AHashMap<Spur, Binding>>,
    arity: u8,
}


impl StructMetaData {
    pub fn new_declaration(fields: Option<Vec<Field>>) -> Result<StructMetaData, String> {
        if let Some(fields) = fields {
            let mut immutable = true;
            let mut arity = fields.len();
            let mut data = AHashMap::<Spur, Binding>::with_capacity(arity);

            for field in fields {
                let binding: Binding = Binding::new_binding_ast(&AstNode::new_nil_lit(), &field.modifiers, true, false)?;
                immutable = false;
                data.insert(field.name, binding);
            }
            Ok(StructMetaData { immutable, data: Some(data), arity: arity as u8 })
        } else { Ok(StructMetaData { immutable: true, data: None, arity: 0 }) }
    }

    pub fn new_instance(
        &self,
        name: Spur,
        args: Option<Vec<InstArgs>>,
    ) -> Result<AstNode, String> {
        if self.arity == 0 && args.is_none() {
            let data = StructData { immutable: true, data: AHashMap::with_capacity(0) };
            return Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Struct(data)))));
        }

        if let Some(args) = args {
            if args.len() as u8 != self.arity {
                return Err(format!(
                    "Expected {} instance arguments but found {} for:{:?}",
                    self.arity, args.len(), &name));
            }

            let mut inst_data = self.data.clone().unwrap();
            for arg in args {
                let field = inst_data.get(&arg.name);
                if let Some(field) = field {
                    // TODO type matching
                    let binding = Binding::update_binding(&arg.value, field.private, field.dynamic, field.mutable)?;
                    inst_data.insert(arg.name, binding);
                } else {
                    return Err(format!("Instance argument not found: {:?} for struct: {:?}", &arg.name, &name));
                }
            }
            let data = StructData { immutable: self.immutable, data: inst_data };
            Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Struct(data)))))
        } else {
            Err(format!("Expected instance arguments but found none for:{:?}", name))
        }
    }
}


impl ObjectAccess for StructData {
    fn get_field(&self, name: &Spur) -> Result<Rc<LitNode>, String> {
        if let Some(found) = self.data.get(name) {
            Ok(Rc::clone(&found.value))
        } else { Err(format!("Could not locate field: {:?}", name)) }
    }

    fn get_method(&self, name: &Spur) -> Result<LitNode, String> {
        todo!()
    }


    fn set_field(&mut self, name: &Spur, value: &AstNode) -> Result<Rc<LitNode>, String> {
        is_literal(name, value)?;

        let found = self.data.get_mut(name);
        match found {
            None => Err(format!("Struct field not found: {:?}", name)),
            Some(data) => {
                if let LiteralNode(lit) = value {
                    data.value = Rc::clone(lit);
                    Ok(Rc::clone(lit))
                } else { Err(format!("Attempted to reassign immutable field: {:?}", name)) }
            }
        }
    }
}


impl EvalResult for StructData {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { format!("Struct: {:?}", self.data) }
    fn equal_to(&self, other: &LitNode) -> bool {
        match other {
            LitNode::Struct(val) => { val.data == self.data }
            _ => false
        }
    }
    fn get_type(&self) -> Type { Type::Struct }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassMetaData {
    arity: u8,
    data: AHashMap<Spur, Binding>,
    methods: AHashMap<Spur, Rc<DefLambdaData>>,
    def: DefClassData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassData {
    env: Rc<RefCell<Environment>>,
    methods: AHashMap<Spur, Rc<DefLambdaData>>,
    fin: Option<AstNode>,
}


impl ClassMetaData {
    pub fn new_declaration(
        def: DefClassData,
    ) -> Result<ClassMetaData, String> {
        let arity: usize;
        let mut field_map: AHashMap::<Spur, Binding>;

        if let Some(fields) = &def.fields {
            arity = fields.len();
            field_map = AHashMap::<Spur, Binding>::with_capacity(arity);
            for field in fields {
                let binding: Binding = Binding::new_binding_ast(&AstNode::new_nil_lit(), &field.modifiers, false, false)?;
                field_map.insert(field.name, binding);
            }
        } else {
            arity = 0;
            field_map = AHashMap::<Spur, Binding>::with_capacity(0);
        }

        let mut method_map: AHashMap::<Spur, Rc<DefLambdaData>>;
        if let Some(methods) = &def.methods {
            method_map = AHashMap::<Spur, Rc<DefLambdaData>>::with_capacity(methods.len());
            for method in methods {
                method_map.insert(method.name, Rc::clone(&method.lambda));
            }
        } else { method_map = AHashMap::<Spur, Rc<DefLambdaData>>::with_capacity(0); }

        Ok(ClassMetaData { data: field_map, methods: method_map, arity: arity as u8, def })
    }

    pub fn new_instance(
        &self,
        args: Option<Vec<InstArgs>>,
        init: &Option<Vec<FuncArg>>,
        calling_env: &Rc<RefCell<Environment>>,
        loader: &RefCell<ClassLoader>,
    ) -> Result<AstNode, String> {
        let field_arity = match &self.def.fields {
            None => 0,
            Some(fields) => fields.len()
        };
        let env = Environment::new();
        env.borrow_mut().unlock();

        // Map default values into environment, false if needs set
        for f in self.def.fields.iter().flatten() {
            if f.default_value.is_some() {
                let evaled = eval::interpreter::eval_node(&env, loader, f.default_value.as_ref().unwrap())?;
                let value = match evaled.as_ref() {
                    LiteralNode(lit) => Rc::clone(lit),
                    _ => { return Err(format!("Default value for field: {:?} is not a literal", f.name)); }
                };

                let binding = Binding::new_binding_lit(&value, &f.modifiers, false, true)?;
                env.borrow_mut().create_binding(f.name, binding)?;
            } else {
                let value = Rc::new(ast_nodes::NIL_LIT);
                let binding = Binding::new_binding_lit(&value, &f.modifiers, false, false)?;
                env.borrow_mut().create_binding(f.name, binding)?;
            }
        }

        // run pre-init
        if self.def.pre_init.is_some() {
            eval::interpreter::eval_node(&env, loader, self.def.pre_init.as_ref().unwrap())?;
        }

        // run init func, or direct field instances

        if let Some(args) = init {
            let init_func = self.get_init_for_arg(args)?;
            let init_env = Environment::of_nested(Rc::clone(&env));
            init_env.borrow_mut().unlock();
            if let Some(params) = &init_func.parameters {
                eval::interpreter::map_param_to_env(
                    calling_env,
                    loader,
                    params,
                    args,
                    &init_env)?;
            }
            eval::interpreter::eval_node(&init_env, loader, &init_func.body)?;
            println!("init env: {:?}", &init_env);
            println!("final env: {:?}", &env);
        } else if let Some(args) = args {
            for arg in args {
                let eval_arg = eval::interpreter::eval_node(&env, loader, &arg.value)?;
                env.borrow_mut().update_binding(&arg.name, eval_arg.deref())?;
            }
        } else if field_arity != 0 {
            return Err("Expected init or direct field arguments".to_string());
        }

        env.borrow_mut().lock();

        if self.def.post_init.is_some() {
            eval::interpreter::eval_node(&env, loader, self.def.post_init.as_ref().unwrap())?;
        }

        if !env.borrow().validate_bindings() {
            return Err("Failed to set all values".to_string());
        }

        if self.def.validate.is_some() {
            let validate = self.def.validate.as_ref().unwrap();
            match validate {
                AstNode::ExpressionNode(expr) => match expr.as_ref() {
                    ExprNode::MultiExpr(multi) => {
                        for m in multi {
                            if let LiteralNode(lit) = eval::interpreter::eval_node(&env, loader, m)?.deref() {
                                if !lit.value().as_bool() {
                                    return Err(format!("Validation failed for expr: {:?}", m));
                                }
                            } else { return Err("Expected validation to return literal node".to_string()); }
                        }
                    }
                    _ => {
                        if let LiteralNode(lit) = eval::interpreter::eval_node(&env, loader, validate)?.deref() {
                            if !lit.value().as_bool() {
                                return Err(format!("Validation failed for expr: {:?}", expr));
                            }
                        } else { return Err("Expected validation to return literal node".to_string()); }
                    }
                },
                _ => { return Err("Expected expression node in validation".to_string()); }
            }
        }


        let class_data = ClassData { env, methods: self.methods.clone(), fin: self.def.fin.clone() };
        Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Class(class_data)))))
    }

    fn get_init_for_arg(&self, args: &Vec<FuncArg>) -> Result<&DefLambdaData, String> {
        for init in self.def.init.iter().flatten() {
            if let Some(init_params) = &init.parameters {
                if init_params.len() == args.len() {
                    return Ok(init);
                }
            } else if init.parameters.is_none() && args.is_empty() {
                return Ok(init);
            }
        }
        Err("Failed to find valid init function".to_string())
    }
}


impl ObjectAccess for ClassData {
    fn get_field(&self, name: &Spur) -> Result<Rc<LitNode>, String> {
        self.env.borrow().get_literal(name)
    }


    fn get_method(&self, name: &Spur) -> Result<LitNode, String> {
        if let Some(method) = self.methods.get(name) {
            Ok(LitNode::Lambda(LambdaValue { env: Rc::clone(&self.env), def: Rc::clone(&method) }))
        } else { Err(format!("Failed to find method: {:?}", name)) }
    }


    fn set_field(&mut self, name: &Spur, value: &AstNode) -> Result<Rc<LitNode>, String> {
        let result = self.env.borrow_mut().update_binding(name, value)?;
        if let LiteralNode(lit) = result {
            Ok(Rc::clone(&lit))
        } else { Err("Non literal, should happen".to_string()) }
    }
}



