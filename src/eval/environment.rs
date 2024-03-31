use std::cell::RefCell;

use std::rc::Rc;
use ahash::AHashMap;
use lasso::Spur;
use crate::eval::class_loader::{ClassDef, ClassLoader};
use crate::lang::datatypes::Binding;

use crate::parse::ast_nodes::{AstNode, LitNode};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::util;


#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: AHashMap<Spur, Binding>,
    locked: bool,
}


impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: None,
            bindings: AHashMap::with_capacity(50),
            locked: true,
        }))
    }

    pub fn of_nested(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: Some(env),
            bindings: AHashMap::with_capacity(10),
            locked: true,
        }))
    }

    pub fn of_fields(field_map: AHashMap<Spur, Binding>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: None,
            bindings: field_map,
            locked: true,
        }))
    }

    pub fn unlock(&mut self) { self.locked = false; }
    pub fn lock(&mut self) { self.locked = true; }

    pub fn validate_bindings(&self) -> bool {
        let valid = self.bindings.values().all(|binding| binding.is_set);
        if !valid { return false; }

        if let Some(p_env) = &self.parent {
            p_env.borrow().validate_bindings()
        } else {
            true
        }
    }

    pub fn depth(&self, mut i: usize) -> usize {
        i += self.bindings.len();
        if self.parent.is_some() {
            self.parent.as_ref().unwrap().borrow().depth(i)
        } else {
            i
        }
    }


    pub fn get_literal(&self, name: &Spur) -> Result<Rc<LitNode>, String> {
        if let Some(found) = self.bindings.get(name) {
            Ok(Rc::clone(&found.value))
        } else if let Some(p_env) = &self.parent {
            p_env.borrow().get_literal(name)
        } else { Err("Couldnt find binding".to_string()) }
    }


    pub fn create_binding(&mut self, name: Spur, binding: Binding) -> Result<AstNode, String> {
        if let Some(_) = self.bindings.get(&name) {
            Err("Binding already exists".to_string())
        } else {
            self.bindings.insert(name, binding);
            Ok(AstNode::new_bool_lit(true))
        }
    }

    pub fn update_binding<'a>(&mut self, name: &Spur, value: &'a AstNode) -> Result<AstNode, String> {
        if !matches!(value, LiteralNode(_)) {
            return Err(format!("Attempted to assign non literal value to{:?}", name));
        }

        println!("updating: {:?}", util::SCACHE.resolve(name));
        if let Some(binding) = self.bindings.get_mut(name) {
            let mut data = binding;

            if data.mutable || !self.locked {
                if let LiteralNode(lit) = &value {
                    data.is_set = true;
                    data.value = Rc::clone(&lit);
                    Ok(AstNode::new_bool_lit(true))
                } else { Err(format!("Attempted to bind non literal object: {:?}", value)) }
            } else { Err(format!("Attempted to reassign immutable binding{:?}", name)) }
        } else if let Some(p_env) = &mut self.parent {
            p_env.borrow_mut().update_binding(name, value)
        } else {
            Err(format!("Failed to find binding to update for: {:?}", name))
        }
    }
}






