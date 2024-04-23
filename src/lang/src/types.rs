use ahash::AHashMap;
use crate::util::{IString, SCACHE};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unresolved,
    Integer,
    Float,
    Boolean,
    Array(Box<Type>),
    String,
    Pair,
    Nil,
    Quote,
    Object(ObjType),
    Lambda(FuncType),
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjType {
    pub super_types: Vec<Type>,
    pub name: IString,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub return_type: Box<Type>,
    pub arg_typ: Vec<Type>,
}


#[derive(Debug)]
pub struct TypeTable {
    type_defs: AHashMap<Type, u16>,
    type_ids: Vec<Type>,
    type_names: AHashMap<IString, u16>,
    pub nil: u16,
    pub bool: u16,
    pub int: u16,
    pub float: u16,
    pub string: u16,
    pub pair: u16,
}


impl Default for TypeTable {
    fn default() -> Self {
        let mut type_defs = AHashMap::<Type, u16>::with_capacity(50);
        let mut type_ids = Vec::<Type>::with_capacity(50);
        let mut type_names = AHashMap::<IString, u16>::with_capacity(50);


        let nil = type_ids.len() as u16;
        type_ids.push(Type::Nil);
        type_defs.insert(Type::Nil, nil);
        type_names.insert(SCACHE.const_nil, nil);

        let bool = type_ids.len() as u16;
        type_ids.push(Type::Boolean);
        type_defs.insert(Type::Boolean, bool);
        type_names.insert(SCACHE.const_bool, bool);

        let int = type_ids.len() as u16;
        type_ids.push(Type::Integer);
        type_defs.insert(Type::Integer, int);
        type_names.insert(SCACHE.const_int, int);

        let float = type_ids.len() as u16;
        type_ids.push(Type::Float);
        type_defs.insert(Type::Float, float);
        type_names.insert(SCACHE.const_float, float);

        let string = type_ids.len() as u16;
        type_ids.push(Type::String);
        type_defs.insert(Type::String, string);
        type_names.insert(SCACHE.const_string, string);

        let pair = type_ids.len() as u16;
        type_ids.push(Type::Pair);
        type_defs.insert(Type::Pair, pair);
        type_names.insert(SCACHE.const_pair, pair);

        TypeTable {
            type_defs,
            type_ids,
            type_names,
            nil,
            bool,
            int,
            float,
            string,
            pair,
        }
    }
}


impl TypeTable {
    pub fn get_or_define_type(&mut self, typ: Type) -> u16 {
        if typ == Type::Unresolved {
            panic!("Passed unresolved type to define_type");
        }
        if self.type_defs.contains_key(&typ) {
            return *self.type_defs.get(&typ).unwrap();
        }

        let id = self.type_ids.len();

        match &typ {
            Type::Array(data) => { self.type_names.insert(SCACHE.intern("Array".to_string()), id as u16); }// FIXMe name
            Type::Object(data) => { self.type_names.insert(data.name, id as u16); }
            Type::Lambda(data) => todo!(),
            _ => panic!(),
        }

        if id > u16::MAX as usize { panic!("Exceeded maximum type definitions (65,535)"); }
        self.type_ids.push(typ.clone());
        self.type_defs.insert(typ, id as u16);
        id as u16
    }

    pub fn get_type_id(&self, typ: &Type) -> u16 {
        return *self.type_defs.get(typ).expect("Invalid Type");
    }

    pub fn get_type_by_id(&self, id: u16) -> &Type {
        unsafe {
            self.type_ids.get_unchecked(id as usize)
        }
    }

    pub fn get_type_by_name(&self, i_string: IString) -> Option<&Type> {
        return if let Some(id) = self.type_names.get(&i_string) {
            Some(self.get_type_by_id(*id))
        } else { None };
    }
}
