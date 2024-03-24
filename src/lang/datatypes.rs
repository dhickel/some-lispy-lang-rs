use std::cell::RefCell;
use std::rc::Rc;
use ahash::AHashMap;
use crate::lang::types::Type;
use crate::parse::ast_nodes::{AST_TRUE_LIT, AstNode};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;


fn is_literal(name: &String, value: &AstNode) -> Result<(), String> {
    if !matches!(value, LiteralNode(_)) {
        Err(format!("Attempted to assign non literal value to{}", name))
    } else { Ok(()) }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub obj_type: Type,
    pub value: Rc<AstNode>,
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

        if let LiteralNode(lit) = value {
            if is_dynamic {
                Ok(Binding { obj_type: lit.value().get_type(), value: Rc::new(value.clone()), dynamic: true, mutable: true })
            } else if is_mutable {
                Ok(Binding { obj_type: lit.value().get_type(), value: Rc::new(value.clone()), dynamic: false, mutable: true })
            } else {
                Ok(Binding { obj_type: lit.value().get_type(), value: Rc::new(value.clone()), dynamic: false, mutable: false })
            }
        } else { Err("Binding did not eval to literal".to_string()) }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    data: AHashMap<String, Rc<RefCell<Binding>>>
}

impl StructData {
    pub fn get(&self, name: &String) -> Option<Rc<RefCell<Binding>>> {
        let found = self.data.get(name);
        found.map(|data| Rc::clone(&data))
    }

    pub fn set(&self, name: &String, value: &AstNode) -> Result<AstNode, String> {
        is_literal(name, value)?;

        let found = self.data.get(name);
        match found {
            None => Err(format!("Struct field not found: {}", name)),
            Some(data) => {
                let mut data = data.borrow_mut();
                if data.mutable {
                    data.value = value.clone().into();
                    Ok(AST_TRUE_LIT)
                } else {
                    Err(format!("Attempted to reassign immutable field: {}", name))
                }
            }
        }
    }
}