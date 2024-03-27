use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use ahash::AHashMap;
use crate::lang::types::Type;
use crate::parse::ast_nodes::{ AstNode, EvalResult, Field, FuncArg, InstArgs, LitNode, ObjectValue};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;


fn is_literal(name: &str, value: &AstNode) -> Result<(), String> {
    if !matches!(value, LiteralNode(_)) {
        Err(format!("Attempted to assign non literal value to{}", name))
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
    fn get_field(&self, name: &str) -> Result<Rc<LitNode>, String>;
    fn get_method(&self, name: &str) -> Result<Rc<LitNode>, String>;
    fn set_field(&mut self, name: &str, value: &AstNode) -> Result<Rc<LitNode>, String>;
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    immutable: bool,
    data: AHashMap<String, Binding>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructMetaData {
    immutable: bool,
    data: Option<AHashMap<String, Binding>>,
    arity: u8,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ClassData {}


impl StructMetaData {
    pub fn new_declaration(fields: Option<Vec<Field>>) -> Result<StructMetaData, String> {
        if let Some(fields) = fields {
            let mut immutable = true;
            let mut arity = fields.len();
            let mut data = AHashMap::<String, Binding>::with_capacity(arity);

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
        name: String,
        args: Option<Vec<InstArgs>>,
    ) -> Result<AstNode, String> {
        if self.arity == 0 && args.is_none() {
            let data = StructData { immutable: true, data: AHashMap::with_capacity(0) };
            return Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Struct(data)))));
        }

        if let Some(args) = args {
            if args.len() as u8 != self.arity {
                return Err(format!(
                    "Expected {} instance arguments but found {} for:{}",
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
                    return Err(format!("Instance argument not found: {} for struct: {}", &arg.name, &name));
                }
            }
            let data = StructData { immutable: self.immutable, data: inst_data };
            Ok(LiteralNode(Rc::new(LitNode::Object(ObjectValue::Struct(data)))))
        } else {
            Err(format!("Expected instance arguments but found none for:{}", name))
        }
    }
}


impl ObjectAccess for StructData {
    fn get_field(&self, name: &str) -> Result<Rc<LitNode>, String> {
        if let Some(found) = self.data.get(name) {
            Ok(Rc::clone(&found.value))
        } else { Err(format!("Could not locate field: {}", name)) }
    }

    fn get_method(&self, name: &str) -> Result<Rc<LitNode>, String> {
        todo!()
    }


    fn set_field(&mut self, name: &str, value: &AstNode) -> Result<Rc<LitNode>, String> {
        is_literal(name, value)?;

        let found = self.data.get_mut(name);
        match found {
            None => Err(format!("Struct field not found: {}", name)),
            Some(data) => {
                if let LiteralNode(lit) = value {
                    data.value = Rc::clone(lit);
                    Ok(Rc::clone(lit))
                } else { Err(format!("Attempted to reassign immutable field: {}", name)) }
            }
        }
    }
}


impl ObjectAccess for ClassData {
    fn get_field(&self, name: &str) -> Result<Rc<LitNode>, String> {
        todo!()
    }

    fn get_method(&self, name: &str) -> Result<Rc<LitNode>, String> {
        todo!()
    }


    fn set_field(&mut self, name: &str, value: &AstNode) -> Result<Rc<LitNode>, String> {
        todo!()
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
