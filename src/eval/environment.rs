use std::cell::RefCell;
use std::ops::Deref;

use std::rc::Rc;
use ahash::AHashMap;

use crate::parse::ast_nodes::{AST_TRUE_LIT, AstNode, LitNode};
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: AHashMap<String, Rc<RefCell<Binding>>>,
}


impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: None,
            bindings: AHashMap::with_capacity(50),
        }))
    }

    pub fn of(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: Some(env),
            bindings: AHashMap::with_capacity(10),
        }))
    }

    // pub fn print_bindings(&self) {
    //     for i in self.bindings.iter() {
    //         println!("{}:{:?}", i.0, i.1.obj_type)
    //     }
    //     if let Some(p_env) = &self.parent {
    //         p_env.borrow().print_bindings();
    //     }
    // }

    pub fn depth(&self, mut i: usize) -> usize {
        i += self.bindings.len();
        if self.parent.is_some() {
            self.parent.as_ref().unwrap().borrow().depth(i)
        } else {
            i
        }
    }


    pub fn get_literal(&self, name: &String) -> Option<Rc<RefCell<Binding>>> {
        if let Some(found) = self.bindings.get(name) {
            Some(Rc::clone(&found))
        } else if let Some(p_env) = &self.parent {
            p_env.borrow().get_literal(name).clone()
        } else { None }
    }


    pub fn create_binding(&mut self, name: String, binding: Binding) -> Result<AstNode, String> {
        if let Some(_) = self.bindings.get(&name) {
            Err(format!("Binding already exists for: {}", name))
        } else {
            self.bindings.insert(name, Rc::new(RefCell::new(binding)));
            Ok(AST_TRUE_LIT)
        }
    }

    pub fn update_binding(&mut self, name: &String, value: AstNode) -> Result<AstNode, String> {
        if let Some(binding) = self.bindings.get_mut(name) {
            if binding.borrow().mutable {
                binding.borrow_mut().value = value;
                Ok(AST_TRUE_LIT)
            } else {
                Err(format!("Attempted to reassign immutable binding{}", name))
            }
        } else if let Some(p_env) = &mut self.parent {
            p_env.borrow_mut().update_binding(name, value)
        } else {
            Err(format!("Failed to find binding to update for: {}", name))
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub obj_type: String,
    pub value: AstNode,
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
                Ok(Binding { obj_type: lit.value().get_type(), value: value.clone(), dynamic: true, mutable: true })
            } else if is_mutable {
                Ok(Binding { obj_type: lit.value().get_type(), value: value.clone(), dynamic: false, mutable: true })
            } else {
                Ok(Binding { obj_type: lit.value().get_type(), value: value.clone(), dynamic: false, mutable: false })
            }
        } else { Err("Binding did not eval to literal".to_string()) }
    }
}




