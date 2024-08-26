use ahash::AHashMap;
use crate::token::Def;
use crate::types::Type::Unresolved;
use crate::util::{IString, SCACHE};


#[derive(Debug)]
pub enum TypeError {
    CheckError(String)
}


impl Into<String> for TypeError {
    fn into(self) -> String {
        match self { TypeError::CheckError(str) => str }
    }
}


pub enum TypeState {
    Unresolved,
    Valid,
    Invalid,
    Error(TypeError),
}


impl From<TypeError> for TypeState {
    fn from(value: TypeError) -> Self {
        Self::Error(value)
    }
}


impl From<bool> for TypeState {
    fn from(value: bool) -> Self {
        if value {
            Self::Valid
        } else {
            Self::Invalid
        }
    }
}


pub trait TypeCheck {
    fn get_type_state(&self) -> TypeState;
    fn do_types_match(&self, typ: &Type) -> Result<bool, TypeError>;
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unresolved(UnresolvedType),
    Integer,
    Float,
    Boolean,
    Array(Box<Type>),
    String,
    Tuple,
    Nil,
    Void,
    Quote,
    Object(ObjType),
    Lambda(FuncType),
}


impl Default for Type { fn default() -> Self { Unresolved(UnresolvedType::Unknown) } }


#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum UnresolvedType {
    #[default]
    Unknown,
    Identifier(IString),
    Ignore,
    Any,
}


impl Into<Type> for UnresolvedType { fn into(self) -> Type { Unresolved(self) } }


impl Type {
    pub fn parse_type_from_string(name: IString) -> Type {
        let name_str = SCACHE.resolve(name);

        match name_str {
            "Int" => Type::Integer,
            "Float" => Type::Float,
            "Bool" => Type::Boolean,
            "String" => Type::String,
            "void" => Type::Void,
            "Nil" => Type::Nil,
            "Tuple" => Type::Tuple,
            "Array" => Type::Array(Box::new(UnresolvedType::Unknown.into())),
            "Fn" => Type::Lambda(FuncType::default()),
            "()" => Type::Nil,
            _ => UnresolvedType::Identifier(name).into()
        }
    }
}


impl TypeCheck for Type {
    fn get_type_state(&self) -> TypeState {
        match self {
            Type::Unresolved(_) => TypeState::Unresolved,
            Type::Lambda(func_type) => {
                match func_type.match_returns(&Type::Unresolved(UnresolvedType::Unknown)) { // FIXME? idk tf is going on with all of this
                    Ok(is_unresolved) if is_unresolved => return TypeState::Invalid,
                    Err(err) => return TypeState::from(err),
                    _ => {}
                }

                match func_type.param_types.iter().any(|t| matches!(*t, Type::Unresolved(_))) {
                    true => TypeState::Invalid,
                    false => TypeState::Valid
                }
            }
            Type::Array(arr_type) => arr_type.get_type_state(),
            _ => TypeState::Valid
        }
    }

    fn do_types_match(&self, typ: &Type) -> Result<bool, TypeError> {
        match self {
            Type::Lambda(self_type) => {
                if let Type::Lambda(test_type) = typ {
                    Ok(self_type.match_returns(&test_type.rtn_type)?
                        && self_type.match_parameters(&test_type.param_types)?)
                } else {
                    Ok(false)
                }
            }
            Type::Array(self_type) => {
                if let Type::Array(test_type) = typ {
                    self_type.do_types_match(test_type)
                } else {
                    Ok(false)
                }
            }
            other => Ok(*other == *typ)
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjType {
    pub super_types: Vec<Type>,
    pub name: IString,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub rtn_type: Box<Type>,
    pub param_types: Vec<Type>,
}


impl Default for FuncType {
    fn default() -> Self {
        Self {
            rtn_type: Box::new(UnresolvedType::Unknown.into()),
            param_types: vec![],
        }
    }
}


impl FuncType {
    pub fn init_params_unresolved(&mut self, param_count: usize) -> Result<(), TypeError> {
        if !self.param_types.is_empty() {
            Err(TypeError::CheckError("Attempted to reinitialize existing parameters to Unresolved".to_string()))
        } else {
            self.param_types = std::iter::repeat(UnresolvedType::Unknown.into()).take(param_count).collect();
            Ok(())
        }
    }


    pub fn add_param_type(&mut self, typ: Type) {
        self.param_types.push(typ)
    }


    pub fn set_param_type(&mut self, index: usize, typ: Type) -> Result<(), TypeError> {
        if index >= self.param_types.len() {
            Err(TypeError::CheckError(
                format!("Invalid parameter index: {}, params length: {}",
                    index, self.param_types.len()))
            )
        } else if {
            let p_type = &self.param_types[index];
            !matches!(*p_type, Type::Unresolved(_)) && *p_type != typ
        } {
            Err(TypeError::CheckError(
                format!("Attempted to reassign parameter type, Existing: {:?}, New: {:?}",
                    self.param_types[index], typ))
            )
        } else {
            self.param_types[index] = typ;
            Ok(())
        }
    }

    pub fn set_return_type(&mut self, rtn_type: Type) -> Result<(), TypeError> {
        if matches!(*self.rtn_type, Type::Unresolved(_)) {
            self.rtn_type = Box::new(rtn_type);
            Ok(())
        } else if *self.rtn_type == rtn_type {
            Ok(())
        } else {
            Err(TypeError::CheckError(
                format!("Attempted to reassign return type, Existing: {:?}, New: {:?}",
                    self.rtn_type, rtn_type))
            )
        }
    }


    fn match_parameters(&self, params: &[Type]) -> Result<bool, TypeError> {
        if params.len() != self.param_types.len() { return Ok(false); }

        for (self_param, test_param) in self.param_types.iter().zip(params.iter()) {
            if matches!(*self_param, Type::Unresolved(_)) || matches!(*test_param, Type::Unresolved(_)) {
                return Err(TypeError::CheckError(
                    format!("Unresolved parameter detected in type comparison, Type Param: {:?}, Test Param: {:?}",
                        self_param, test_param))
                );
            }
            if self_param != test_param { return Ok(false); }
        }
        Ok(true)
    }


    fn match_returns(&self, rtn_type: &Type) -> Result<bool, TypeError> {
        if matches!(*self.rtn_type,  Type::Unresolved(_)) || matches!(*rtn_type, Type::Unresolved(_)) {
            return Err(TypeError::CheckError(
                format!("Unresolved parameter detected in type comparison, Type Param: {:?}, Test Param: {:?}",
                    self.rtn_type, rtn_type))
            );
        }
        Ok(&*self.rtn_type == rtn_type)
    }


    pub fn is_predicate(&self) -> Result<bool, TypeError> {
        self.match_returns(&Type::Boolean)
    }


    pub fn is_nil(&self) -> Result<bool, TypeError> {
        self.match_returns(&Type::Nil)
    }


    pub fn is_predicate_of(&self, params: &[Type]) -> Result<bool, TypeError> {
        if !self.match_returns(&Type::Boolean)? {
            Ok(false)
        } else { self.match_parameters(params) }
    }


    // TODO should something with no params still be a consumer?
    pub fn is_consumer_of(&self, params: &[Type]) -> Result<bool, TypeError> {
        if !self.is_nil()? {
            return Ok(false);
        } else { self.match_parameters(params) }
    }


    pub fn is_supplier_of(&self, rtn_type: &Type) -> Result<bool, TypeError> {
        Ok(self.param_types.is_empty() && self.match_returns(rtn_type)?)
    }


    pub fn is_function_of(&self, params: &[Type], rtn_type: &Type) -> Result<bool, TypeError> {
        Ok(self.match_parameters(params)? && self.match_returns(rtn_type)?)
    }
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
    pub void: u16,
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

        let void = type_ids.len() as u16;
        type_ids.push(Type::Void);
        type_defs.insert(Type::Void, void);
        type_names.insert(SCACHE.const_void, void);

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
        type_ids.push(Type::Tuple);
        type_defs.insert(Type::Tuple, pair);
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
            void,
        }
    }
}


impl TypeTable {
    pub fn get_or_define_type(&mut self, typ: &Type) -> u16 {
        if matches!(typ, Type::Unresolved(_)) {
            panic!("Passed unresolved type to define_type");
        }
        if self.type_defs.contains_key(&typ) {
            return *self.type_defs.get(&typ).unwrap();
        }

        let id = self.type_ids.len();


        // match &typ {
        //     Type::Array(data) => {}{ self.type_names.insert(SCACHE.intern("".to_string()), id as u16); }
        //     Type::Object(data) => { self.type_names.insert(data.name, id as u16); }
        //     Type::Lambda(data) => todo!(),
        //     _ => panic!(),
        // }

        if id > u16::MAX as usize { panic!("Exceeded maximum type definitions (65,535)"); }
        self.type_ids.push(typ.clone());
        self.type_defs.insert(typ.clone(), id as u16);
        id as u16
    }

    pub fn get_type_id(&self, typ: &Type) -> u16 {
        return *self.type_defs.get(typ).unwrap_or_else(|| panic!("Invalid Type{:?}", typ));
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
