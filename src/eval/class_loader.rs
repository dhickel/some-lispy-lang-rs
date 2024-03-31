use ahash::AHashMap;
use lasso::Spur;
use crate::lang::datatypes::{ClassMetaData, StructMetaData};
use crate::parse::ast_nodes::{DefClassData, LambdaValue};
use crate::parse::util;


#[derive(Debug, Clone, PartialEq)]
pub enum ClassDef {
    Struct(StructMetaData),
    Class(ClassMetaData),
}

#[derive(Debug)]
pub struct ClassLoader {
    class_defs: AHashMap<Spur, ClassDef>,
}

impl Default for ClassLoader {
    fn default() -> Self {
        ClassLoader { class_defs: AHashMap::<Spur, ClassDef>::with_capacity(50) }
    }
}


impl ClassLoader {
    pub fn new_class_def(&mut self, name: Spur, class_def: ClassDef) -> Result<(), String> {
        if self.class_defs.contains_key(&name) {
            return Err(format!("Attempted to redeclare existing class: {:?}", util::SCACHE.resolve(&name)));
        }
        self.class_defs.insert(name, class_def);
        Ok(())
    }

    pub fn get_class_def(&self, name: &Spur) -> Result<&ClassDef, String> {
        if let Some(found) = self.class_defs.get(name) {
            return Ok(found);
        } else {  
            println!("{:?}", self.class_defs);
            println!("{:?}", name);
            Err(format!("Definition not loaded for: {:?}", util::SCACHE.resolve(&name)))
        }
    }
}