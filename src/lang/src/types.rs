use std::ops::Deref;
use ahash::AHashMap;
use intmap::IntMap;
use crate::util::{IString, SCACHE};


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TypeId(u16);


impl TypeId {
    pub fn as_usize(&self) -> usize { self.0 as usize }
}


impl From<u16> for TypeId { fn from(value: u16) -> Self { Self(value) } }


impl From<usize> for TypeId {
    fn from(value: usize) -> Self {
        if value > u16::MAX as usize {
            panic!("Fatal<internal>: From call on usize > u16 TypeId")
        }
        Self(value as u16)
    }
}

// 
// impl Deref for TypeId {
//     type Target = u16;
// 
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }


#[derive(Debug)]
pub enum TypeError {
    CheckError(String),
    Resolution(String),
    InvalidOperation(String),
    TypeOverflow,
}


impl Into<String> for TypeError {
    fn into(self) -> String {
        match self {
            TypeError::CheckError(str) | TypeError::Resolution(str)
            | TypeError::InvalidOperation(str) => str,
            TypeError::TypeOverflow => format!("{:?}", self).to_string()
        }
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
        } else { Self::Invalid }
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
    Tuple(Vec<Type>),
    Nil,
    Quote,
    Object(ObjType),
    Lambda(FuncType),
}


impl Type {
    pub fn compatible_with(&self, other: &Self) -> bool {
        match self {
            Type::Unresolved(_) => {
                panic!(
                    "Fatal<internal>: Type compatability check on called with unresolved types, \
                    this state should not be reached"
                )
            }
            Type::Integer | Type::Float | Type::Boolean | Type::Array(_) | Type::String | Type::Nil => self == other,
            Type::Quote => todo!("Quote compatibility checks"),
            Type::Tuple(self_types) => {
                if let Type::Tuple(pther_types) = other {
                    self_types.iter().zip(pther_types.iter())
                        .all(|(s, o)| s.compatible_with(o))
                } else { false }
            }
            Type::Object(_) => todo!("Object compatibility checks"),
            Type::Lambda(self_func) => {
                if let Type::Lambda(other_func) = other {
                    if !self_func.rtn_type.compatible_with(&other_func.rtn_type) { return false; }
                    self_func.param_types.iter().zip(other_func.param_types.iter())
                        .all(|(s, o)| s.compatible_with(o))
                } else { false }
            }
        }
    }

    pub fn primitive_to_type_id(&self) -> TypeId {
        match self {
            Type::Integer => TypeTable::INT,
            Type::Float => TypeTable::FLOAT,
            Type::Boolean => TypeTable::BOOL,
            _ => panic!("Fatal<internal>: Attempted to call primitive type_id of non-primitive")
        }
    }
}


impl Default for Type { fn default() -> Self { Type::Unresolved(UnresolvedType::Unknown) } }


#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum UnresolvedType {
    #[default]
    Unknown,
    Identifier(IString),
}


impl Into<Type> for UnresolvedType { fn into(self) -> Type { Type::Unresolved(self) } }


impl Type {
    pub fn parse_type_from_string(name: IString) -> Type {
        let name_str = SCACHE.resolve(name);
        // TODO add _ and * type modifiers
        match name_str {
            "Int" => Type::Integer,
            "Float" => Type::Float,
            "Bool" => Type::Boolean,
            "String" => Type::String,
            "Nil" => Type::Nil,
            //"Tuple" => Type::Tuple,
            "Array" => Type::Array(Box::new(UnresolvedType::Unknown.into())),
            "Fn" => Type::Lambda(FuncType::default()),
            "()" => Type::Nil,
            _ => UnresolvedType::Identifier(name).into()
        }
    }

    pub fn expr_type(&self) -> &Type {
        match self {
            Type::Lambda(func_type) => &func_type.rtn_type,
            _ => self
        }
    }

    pub fn is_resolved(&self) -> bool {
        match self {
            Type::Unresolved(_) => false,
            Type::Integer | Type::Float | Type::Boolean | Type::String | Type::Nil => true,
            Type::Array(inner) => inner.is_resolved(),
            Type::Lambda(func_type) => func_type.is_resolved(),
            Type::Tuple(_) => todo!("Implement Tuples"),
            Type::Quote => todo!("Implement Quotes"),
            Type::Object(_) => todo!("Implement Objects"),
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

    pub fn are_params_resolved(&self) -> bool {
        self.param_types.iter().all(|p| p.is_resolved())
    }

    pub fn is_return_resolved(&self) -> bool {
        !matches!(*self.rtn_type, Type::Unresolved(_))
    }

    pub fn is_resolved(&self) -> bool {
        self.are_params_resolved() && self.is_return_resolved()
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


    // FIXME we may not want to use these matches here and use to type compatibility call to check these
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
    type_map: Vec<Type>,
    type_enum_id_map: AHashMap<Type, TypeId>,
    type_string_id_map: IntMap<TypeId>,
}


impl Default for TypeTable {
    fn default() -> Self {
        let mut type_map = Vec::<Type>::with_capacity(100);
        let mut type_enum_id_map = AHashMap::<Type, TypeId>::with_capacity(100);
        let mut type_string_id_map = IntMap::<TypeId>::with_capacity(100);

        type_map.push(Type::Nil);
        type_enum_id_map.insert(Type::Nil, Self::NIL);
        type_string_id_map.insert(SCACHE.const_nil.into(), Self::NIL);

        type_map.push(Type::Boolean);
        type_enum_id_map.insert(Type::Boolean, Self::BOOL);
        type_string_id_map.insert(SCACHE.const_bool.into(), Self::BOOL);

        type_map.push(Type::Integer);
        type_enum_id_map.insert(Type::Integer, Self::INT);
        type_string_id_map.insert(SCACHE.const_int.into(), Self::INT);

        type_map.push(Type::Float);
        type_enum_id_map.insert(Type::Float, Self::FLOAT);
        type_string_id_map.insert(SCACHE.const_float.into(), Self::FLOAT);

        type_map.push(Type::String);
        type_enum_id_map.insert(Type::String, Self::STRING);
        type_string_id_map.insert(SCACHE.const_string.into(), Self::STRING);

        TypeTable { type_map, type_enum_id_map, type_string_id_map }
    }
}


pub struct TypeEntry {
    type_id: Option<TypeId>,
    typ: Type,
}


impl TypeEntry {
    pub fn typ_id(&self) -> Option<TypeId> { self.type_id }
    pub fn typ(&self) -> &Type { &self.typ }
}


// TODO clean up the structure and remove all the cloning, I assume type system
//  will be fully redone at some point anyway
impl TypeTable {
    pub const NIL: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const INT: TypeId = TypeId(2);
    pub const FLOAT: TypeId = TypeId(3);
    pub const STRING: TypeId = TypeId(4);

    fn get_entry(&self, type_id: TypeId) -> TypeEntry {
        TypeEntry { type_id: Some(type_id), typ: self.type_map[type_id.as_usize()].clone() }
    }

    fn resolve_type_name(&self, name: IString) -> Option<TypeId> {
        self.type_string_id_map.get(name.into()).copied()
    }


    // Only call from definition statements, and on variant of internal types (Arrays/Lambdas atm)
    fn define_new_type(&mut self, typ: &Type) -> Result<TypeEntry, TypeError> {
        if self.type_enum_id_map.contains_key(&typ) {
            return Err(
                TypeError::InvalidOperation("Fatal<Internal>: Attempted to redefine existing type".to_string())
            );
        }

        let idx = {
            let len = self.type_map.len();
            if len > u16::MAX as usize {
                return Err(TypeError::TypeOverflow);
            } else { TypeId::from(len) }
        };

        self.type_enum_id_map.insert(typ.clone(), idx);

        if let Type::Object(obj_typ) = &typ {
            self.type_string_id_map.insert(obj_typ.name.into(), idx);
        }

        self.type_map.push(typ.clone());

        Ok(TypeEntry { type_id: Some(idx), typ: self.type_map[idx.as_usize()].clone() })
    }

    pub fn resolve_type(&mut self, typ: &Type) -> Result<(bool, Option<TypeEntry>), TypeError> {
        if typ.is_resolved() {
            return Ok((true, Some(self.get_entry(*self.type_enum_id_map.get(typ).unwrap()))));
        }


        // Insert new types should only happen on functions and arrays. As these are variations
        //  of built-in types. All other definitions must occur via definition statements;
        match typ {
            Type::Array(inner) => {
                if let (true, Some(type_entry)) = self.resolve_type(inner)? {
                    let resolved_type = Type::Array(Box::new(type_entry.typ));
                    if let Some(existing) = self.type_enum_id_map.get(&resolved_type) {
                        Ok((true, Some(TypeEntry { type_id: Some(*existing), typ: typ.clone() })))
                    } else { Ok((true, Some(self.define_new_type(typ)?))) }
                } else { Ok((false, None)) }
            }

            Type::Lambda(func_type) => {
                let mut resolved_type = FuncType::default();
                if !func_type.are_params_resolved() {
                    for param in func_type.param_types.iter() {
                        if !param.is_resolved() {
                            if let (true, Some(entry)) = self.resolve_type(param)? {
                                resolved_type.param_types.push(entry.typ);
                            } else { resolved_type.param_types.push(param.clone()) }
                        }
                    }
                } else { resolved_type.param_types = func_type.param_types.clone() }

                if !func_type.is_return_resolved() {
                    if let Ok((true, Some(entry))) = self.resolve_type(&func_type.rtn_type) {
                        resolved_type.rtn_type = Box::new(entry.typ)
                    }
                } else { resolved_type.rtn_type = func_type.rtn_type.clone() }

                let resolved_type = Type::Lambda(resolved_type);

                if resolved_type.is_resolved() {
                    if let Some(existing) = self.type_enum_id_map.get(&resolved_type) {
                        Ok((true, Some(TypeEntry { type_id: Some(*existing), typ: resolved_type })))
                    } else { Ok((true, Some(self.define_new_type(&resolved_type)?))) }
                } else { Ok((false, Some(TypeEntry { type_id: None, typ: resolved_type }))) }
            }

            Type::Unresolved(unresolved) => {
                match unresolved {
                    UnresolvedType::Identifier(i_str) => {
                        if let Some(type_id) = self.type_string_id_map.get(i_str.value.into()) {
                            Ok((true, Some(TypeEntry { type_id: Some(*type_id), typ: typ.clone() })))
                        } else { Ok((false, None)) }
                    }
                    _ => panic!("Fatal<Internal>: Unknown types should not be resolved directly")
                }
            }
            Type::Quote => todo!("Implement Quote"),
            Type::Object(_) => todo!("Implement Objects"),
            Type::Tuple(_) => todo!("Implement Tuples"),
            Type::Nil | Type::Boolean | Type::Integer | Type::Float | Type::String => panic!("Should already exist"),
        }
    }


    pub fn get_type_id(&self, typ: &Type) -> TypeId {
        return *self.type_enum_id_map.get(typ).unwrap_or_else(|| panic!());
    }

    pub fn get_type_by_id(&self, id: TypeId) -> &Type {
        unsafe { self.type_map.get_unchecked(id.as_usize()) }
    }

    pub fn type_id_compatible(&self, src_type: TypeId, dst_type: TypeId) -> bool {
        let src = &self.type_map[src_type.as_usize()];
        let dst = &self.type_map[dst_type.as_usize()];
        dst.compatible_with(src)
    }

    pub fn get_type_by_name(&self, i_string: IString) -> Option<&Type> {
        return if let Some(id) = self.type_string_id_map.get(i_string.into()) {
            Some(self.get_type_by_id(*id))
        } else { None };
    }
}
