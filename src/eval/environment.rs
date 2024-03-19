use std::cell::RefCell;
use std::rc::Rc;
use ahash::AHashMap;
use crate::parse::ast_nodes::{AstNode, LitNode};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: AHashMap<String, Binding>,
}

impl Environment {
    pub fn new() -> Rc<Environment> {
        Rc::new(Environment {
            parent: None,
            bindings: AHashMap::with_capacity(50),
        })
    }

    pub fn of(env: Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment {
            parent: Some(env),
            bindings: AHashMap::with_capacity(10),
        })
    }


    pub fn get_binding(&self, name: &String) -> Option<&Binding> {
        let found = &self.bindings.get(name);
        if found.is_some() {
            *found
        } else if let Some(parent) = &self.parent {
            parent.get_binding(name)
        } else { None }
    }

    pub fn get_literal(&self, name: &String) -> Option<&LitNode> {
        if let Some(found) = &self.bindings.get(name) {
            return Some(&found.value);
        } else if let Some(p_env) = &self.parent {
            p_env.get_literal(name)
        } else { None }
    }

    pub fn create_binding(&mut self, name: String, binding: Binding) -> Result<(), String> {
        if let Some(existing) = self.bindings.get(&name) {
            Err(format!("Binding already exists for: {}", name))
        } else {
            self.bindings.insert(name, binding);
            Ok(())
        }
    }

    pub fn update_binding(&mut self, name: String, value: LitNode) -> Result<&LitNode, String> {
        if let Some(binding) = self.bindings.get_mut(&name) {
            binding.value = value;
            Ok(&binding.value)
        } else {
            Err(format!("Failed to find binding to update for: {}", name))
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub obj_type: String,
    pub value: LitNode,
    pub dynamic: bool,
    pub mutable: bool,
}

impl Binding {
    pub fn new_binding(value: AstNode, mods: Option<Vec<Mod>>) -> Result<Binding, String> {
        let mut is_mutable = false;
        let mut is_dynamic = false;

        if let Some(mods) = mods {
            for m in mods {
                if m == Mod::Mutable { is_mutable = true; }
                if m == Mod::Dynamic { is_dynamic = true; }
            }
        }

        if let LiteralNode(lit) = value {
            if is_dynamic {
                Ok(Binding { obj_type: lit.value().get_type(), value: lit, dynamic: true, mutable: true })
            } else if (is_mutable) {
                Ok(Binding { obj_type: lit.value().get_type(), value: lit, dynamic: false, mutable: true })
            } else {
                Ok(Binding { obj_type: lit.value().get_type(), value: lit, dynamic: false, mutable: false })
            }
        } else { Err("Binding did not eval to literal".to_string()) }
    }
}




