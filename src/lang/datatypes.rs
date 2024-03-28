use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use ahash::AHashMap;
use lasso::Spur;
use crate::eval::environment::Environment;
use crate::lang::types::Type;
use crate::parse::ast_nodes::{AstNode, DefFuncData, DefLambdaData, EvalResult, Field, FuncArg, InstArgs, LambdaValue, LitNode, ObjectValue};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;


fn is_literal(name: &Spur, value: &AstNode) -> Result<(), String> {
    if !matches!(value, LiteralNode(_)) {
        Err(format!("Attempted to assign non literal value to{:?}", name))
    } else { Ok(()) }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub obj_type: Type,
    pub value: Rc<LitNode>,
    pub dynamic: bool,
    pub mutable: bool,
}


impl Binding {
    pub fn new_binding(value: &AstNode, mods: &Option<Vec<Mod>>) -> Result<Binding, String> {
        let mut is_mutable = false;
        let mut is_dynamic = false;

        if let Some(mods) = mods {
            for m in mods {
                if *m == Mod::Mutable { is_mutable = true; }
                if *m == Mod::Dynamic { is_dynamic = true; }
            }
        }

        let obj_type: Type;
        if let LiteralNode(lit) = &value {
            obj_type = lit.value().get_type();

            if is_dynamic {
                Ok(Binding { obj_type, value: Rc::clone(&lit), dynamic: true, mutable: true })
            } else if is_mutable {
                Ok(Binding { obj_type, value: Rc::clone(&lit), dynamic: false, mutable: true })
            } else {
                Ok(Binding { obj_type, value: Rc::clone(&lit), dynamic: false, mutable: false })
            }
        } else {
            return Err(format!("Attempted to bind non literal object: {:?}", value));
        }
    }

    pub fn replace_binding(value: &AstNode, dynamic: bool, mutable: bool) -> Result<Binding, String> {
        let obj_type: Type;
        if let LiteralNode(lit) = value {
            obj_type = lit.value().get_type();

            Ok(Binding { obj_type, value: Rc::clone(&lit), dynamic, mutable })
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
                let binding: Binding = Binding::new_binding(&AstNode::new_nil_lit(), &field.modifiers)?;
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
                    let binding = Binding::replace_binding(&arg.value, field.dynamic, field.mutable)?;
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
    methods: AHashMap<Spur, Rc<DefLambdaData>>
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassData {
    env: Rc<RefCell<Environment>>,
    methods: AHashMap<Spur, Rc<DefLambdaData>>,
}





impl ClassMetaData {
    pub fn new_declaration(fields: Option<Vec<Field>>, methods: Option<Vec<DefFuncData>>,
    ) -> Result<ClassMetaData, String> {
        let arity: usize;
        let mut field_map: AHashMap::<Spur, Binding>;
        if let Some(fields) = fields {
            arity = fields.len();
            field_map = AHashMap::<Spur, Binding>::with_capacity(arity);
            
            for field in fields {
                let binding: Binding = Binding::new_binding(&AstNode::new_nil_lit(), &field.modifiers)?;
                field_map.insert(field.name, binding);
            }
        } else {
            arity = 0;
            field_map = AHashMap::<Spur, Binding>::with_capacity(0);
        }

        let mut method_map: AHashMap::<Spur, Rc<DefLambdaData>>;
        if let Some(methods) = methods {
            method_map = AHashMap::<Spur, Rc<DefLambdaData>>::with_capacity(methods.len());
            
            for method in methods {
                method_map.insert(method.name, Rc::clone(&method.lambda));
            }
        } else { method_map = AHashMap::<Spur, Rc<DefLambdaData>>::with_capacity(0); }

        Ok(ClassMetaData { data: field_map, methods: method_map, arity: arity as u8 })
    }

    pub fn new_instance(&self, name: &Spur, args: Option<Vec<InstArgs>>,
    ) -> Result<AstNode, String> {
        let mut methods =
            AHashMap::<Spur, Rc<DefLambdaData>>::with_capacity(self.methods.len());

        for (key, value) in &self.methods {
            methods.insert(*key, Rc::clone(&value));
        }


        if self.arity != 0 && !args.is_none() {
            if let Some(args) = args {
                if args.len() as u8 != self.arity {
                    return Err(format!("Invalid amount of instance args expect :{}, found: {}", self.arity, args.len()));
                }

                if args.len() as u8 != self.arity {
                    return Err(format!("Expected {} instance arguments but found {} for:{:?}", self.arity, args.len(), &name));
                }

                let mut inst_data = self.data.clone();
                for arg in args {
                    let field = inst_data.get(&arg.name);
                    
                    if let Some(field) = field {
                        // TODO type matching
                        let binding = Binding::replace_binding(&arg.value, field.dynamic, field.mutable)?;
                        inst_data.insert(arg.name, binding);
                    } else {
                        return Err(format!("Instance argument not found: {:?} for struct: {:?}", &arg.name, &name));
                    }
                }
                
                let env = Environment::of_fields(inst_data);
                let class_data = ClassData { env, methods };
                Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Class(class_data)))))
            } else {
                Err(format!("Expected instance arguments but found none for:{:?}", name))
            }
        } else if self.arity != 0 && args.is_none() {
            Err(format!("Expected instance arguments but found none for:{:?}", name))
        } else {
            let env = Environment::of_fields(AHashMap::<Spur, Binding>::with_capacity(0));
            let class_data = ClassData { env, methods };
            Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Class(class_data)))))
        }
    }
}


impl ObjectAccess for ClassData{
    fn get_field(&self, name: &Spur) -> Result<Rc<LitNode>, String> {
        self.env.borrow().get_literal(name)
    }

    fn get_method(&self, name: &Spur) -> Result<LitNode, String> {
        if let Some(method) = self.methods.get(name) {
             Ok(LitNode::Lambda( LambdaValue{ env: Rc::clone(&self.env), def: Rc::clone(&method)}))
        } else {
            Err(format!("Failed to find method: {:?}", name))
        }
    }


    fn set_field(&mut self, name: &Spur, value: &AstNode) -> Result<Rc<LitNode>, String> {
        let result = self.env.borrow_mut().update_binding(name, value)?;
        if let LiteralNode(lit) = result {
            Ok(Rc::clone(&lit))
        } else {
            Err("Non literal, should happen".to_string())
        }
    }
}

