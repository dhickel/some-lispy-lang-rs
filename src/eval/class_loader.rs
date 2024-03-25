use ahash::AHashMap;
use crate::lang::datatypes::StructMetaData;

#[derive(Debug, Clone, PartialEq)]
pub enum ClassDef {
    Struct(StructMetaData),
    Class,
}

#[derive(Debug)]
pub struct ClassLoader {
    class_defs: AHashMap<String, ClassDef>,
}

impl Default for ClassLoader {
    fn default() -> Self {
        ClassLoader { class_defs: AHashMap::<String, ClassDef>::with_capacity(50) }
    }
}


impl ClassLoader {
    pub fn new_class_def(&mut self, name: String, class_def: ClassDef) -> Result<(), String> {
        if self.class_defs.contains_key(&name) {
            return Err(format!("Attempted to redeclare existing class: {}", &name));
        }
        self.class_defs.insert(name, class_def);
        Ok(())
    }

    pub fn get_class_def(&self, name: &str) -> Result<&ClassDef, String> {
        if let Some(found) = self.class_defs.get(name) {
            return Ok(found);
        } else { Err(format!("Definition not loaded for: {}", name)) }
    }
}