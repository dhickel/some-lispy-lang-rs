use std::cell::RefCell;

use std::rc::Rc;
use ahash::AHashMap;
use crate::eval::class_loader::{ClassDef, ClassLoader};
use crate::lang::datatypes::Binding;

use crate::parse::ast_nodes::{AST_TRUE_LIT, AstNode, LitNode};
use crate::parse::ast_nodes::AstNode::LiteralNode;


#[derive(Clone, Debug)]
pub struct Context<'a> {
    pub env: &'a Rc<RefCell<Environment>>,
    pub class_loader: &'a ClassLoader,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: AHashMap<String, Binding>,
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


    pub fn get_literal(&self, name: &String) -> Option<Rc<AstNode>> {
        if let Some(found) = self.bindings.get(name) {
            Some(Rc::clone(&found.value))
        } else if let Some(p_env) = &self.parent {
            p_env.borrow().get_literal(name)
        } else { None }
    }


    pub fn create_binding(&mut self, name: String, binding: Binding) -> Result<AstNode, String> {
        if let Some(_) = self.bindings.get(&name) {
            Err(format!("Binding already exists for: {}", name))
        } else {
            self.bindings.insert(name, binding);
            Ok(AST_TRUE_LIT)
        }
    }

    pub fn update_binding<'a>(&mut self, name: &String, value: &'a AstNode) -> Result<AstNode, String> {
        if !matches!(value, LiteralNode(_)) {
            return Err(format!("Attempted to assign non literal value to{}", name));
        }
        if let Some(binding) = self.bindings.get_mut(name) {
            let mut data = binding;
            if data.mutable {
                data.value = Rc::new(value.clone());
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






