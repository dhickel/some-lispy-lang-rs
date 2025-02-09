use std::ops::{Add, Deref};
use ahash::AHashMap;
use intmap::IntMap;
use crate::util::{IString, SCACHE};
use crate::ast::Symbol;

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

#[derive(Debug)]
pub enum TypeError {
    CheckError(String),
    Resolution(String),
    InvalidOperation(String),
    TypeOverflow,
}

// impl Into<String> for TypeError {
//     fn into(self) -> String {
//         match self {
//             TypeError::CheckError(str) | TypeError::Resolution(str) | TypeError::InvalidOperation(str) => str,
//             TypeError::TypeOverflow => format!("{:?}", self).to_string()
//         }
//     }
// }


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
    fn do_types_match(&self, typ: &LangType) -> Result<bool, TypeError>;
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LangType {
    Primitive(PrimitiveType),
    Composite(CompositeType),
    Custom(CustomType),
    Unresolved(UnresolvedType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Nil
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompositeType {
    Function(FunctionType),
    Array(Box<LangType>),
    Tuple(Vec<LangType>),
    String,
    Quote,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CustomType {
    Class,
    Record,
    Struct,
    Enum,
    Interface
}



impl LangType {
    pub fn compatible_with(&self, other: &Self) -> bool {
        match self {
            LangType::Unresolved(_) => {
                panic!(
                    "Fatal<Internal>: Type compatability check on called with unresolved types, \
                    this state should not be reached"
                )
            }
            LangType::Integer | LangType::Float | LangType::Boolean | LangType::Array(_) | LangType::String | LangType::Nil => self == other,
            LangType::Quote => todo!("Quote compatibility checks"),
            LangType::Tuple(self_types) => {
                if let LangType::Tuple(pther_types) = other {
                    self_types.iter().zip(pther_types.iter()).all(|(s, o)| s.compatible_with(o))
                } else { false }
            }
            LangType::Object(_) => todo!("Object compatibility checks"),
            LangType::Lambda(self_func) => {
                if let LangType::Lambda(other_func) = other {
                    if !self_func.rtn_type.compatible_with(&other_func.rtn_type) { return false; }
                    self_func.param_types.iter().zip(other_func.param_types.iter()).all(|(s, o)| s.compatible_with(o))
                } else { false }
            }
        }
    }

    pub fn size(&self) -> usize {
        match self {
            LangType::Unresolved(_) => { panic!("Fatal<internal>: Size called on unresolved type") }
            LangType::Integer => 8,
            LangType::Float => 8,
            LangType::Boolean => 1,
            LangType::Array(_) => { todo!("Array size not implemented") }
            LangType::String => 8,
            LangType::Tuple(vals) => vals.iter().fold(0, |prior, val| prior.add(val.size())),
            LangType::Nil => 0, // FIXME should this be 1 byte?
            LangType::Quote => { todo!("Quote not implemented") }
            LangType::Object(obj) => { todo!("Object not implemented") }
            LangType::Lambda(func) => { todo!("Lambda not implemented") }
        }
    }
}


impl Default for LangType { fn default() -> Self { LangType::Unresolved(UnresolvedType::Undefined) } }


#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum UnresolvedType {
    #[default]
    //FIXME: this need properly documented and is kinda of hacky. Panics can be induced if undefined
    // types are in non-optional position, currently these are used on lambda params/returns, since
    // they can either be defined on the symbol or in the lambda sig. But lambdas passed as arguments,
    // can have their type inferred if that path is take, or it can be require to put into the sig,
    // which would make it easier to run a validation pass on.
    Undefined,
    Identifier(IString),
}


impl Into<LangType> for UnresolvedType { fn into(self) -> LangType { LangType::Unresolved(self) } }


impl LangType {
    pub fn parse_type_from_string(name: IString) -> LangType {
        let name_str = SCACHE.resolve(name);
        // TODO add _ and * type modifiers
        match name_str {
            "Int" => LangType::Integer,
            "Float" => LangType::Float,
            "Bool" => LangType::Boolean,
            "String" => LangType::String,
            "Nil" => LangType::Nil,
            //"Tuple" => Type::Tuple,
            "Array" => LangType::Array(Box::new(UnresolvedType::Undefined.into())),
            "Fn" => LangType::Lambda(FunctionType::default()),
            "()" => LangType::Nil,
            _ => UnresolvedType::Identifier(name).into()
        }
    }

    pub fn expr_type(&self) -> &LangType {
        match self {
            LangType::Lambda(func_type) => &func_type.rtn_type,
            _ => self
        }
    }

    pub fn is_resolved(&self) -> bool {
        match self {
            LangType::Unresolved(_) => false,
            LangType::Integer | LangType::Float | LangType::Boolean | LangType::String | LangType::Nil => true,
            LangType::Array(inner) => inner.is_resolved(),
            LangType::Lambda(func_type) => func_type.is_resolved(),
            LangType::Tuple(_) => todo!("Implement Tuples"),
            LangType::Quote => todo!("Implement Quotes"),
            LangType::Object(_) => todo!("Implement Objects"),
        }
    }
}


impl TypeCheck for LangType {
    fn get_type_state(&self) -> TypeState {
        match self {
            LangType::Unresolved(_) => TypeState::Unresolved,
            LangType::Lambda(func_type) => {
                match func_type.does_return_match(&LangType::Unresolved(UnresolvedType::Undefined)) { // FIXME? idk tf is going on with all of this
                    Ok(is_unresolved) if is_unresolved => return TypeState::Invalid,
                    Err(err) => return TypeState::from(err),
                    _ => {}
                }

                match func_type.param_types.iter().any(|t| matches!(*t, LangType::Unresolved(_))) {
                    true => TypeState::Invalid,
                    false => TypeState::Valid
                }
            }
            LangType::Array(arr_type) => arr_type.get_type_state(),
            _ => TypeState::Valid
        }
    }

    fn do_types_match(&self, typ: &LangType) -> Result<bool, TypeError> {
        match self {
            LangType::Lambda(self_type) => {
                if let LangType::Lambda(test_type) = typ {
                    Ok(self_type.does_return_match(&test_type.rtn_type)? && self_type.does_params_match(&test_type.param_types)?)
                } else {
                    Ok(false)
                }
            }
            LangType::Array(self_type) => {
                if let LangType::Array(test_type) = typ {
                    self_type.do_types_match(test_type)
                } else {
                    Ok(false)
                }
            }
            other => Ok(other.compatible_with(typ))
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjType {
    pub super_types: Vec<LangType>,
    pub name: Symbol,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub rtn_type: Box<LangType>,
    pub param_types: Vec<LangType>,
}


impl Default for FunctionType {
    fn default() -> Self {
        Self {
            rtn_type: Box::new(UnresolvedType::Undefined.into()),
            param_types: vec![],
        }
    }
}


impl FunctionType {
    pub fn init_params_unresolved(&mut self, param_count: usize) -> Result<(), TypeError> {
        if !self.param_types.is_empty() {
            Err(TypeError::CheckError("Attempted to reinitialize existing parameters to Unresolved".to_string()))
        } else {
            self.param_types = std::iter::repeat(UnresolvedType::Undefined.into()).take(param_count).collect();
            Ok(())
        }
    }

    pub fn are_params_resolved(&self) -> bool {
        self.param_types.iter().all(|p| p.is_resolved())
    }

    pub fn is_return_resolved(&self) -> bool {
        !matches!(*self.rtn_type, LangType::Unresolved(_))
    }

    pub fn is_resolved(&self) -> bool {
        self.are_params_resolved() && self.is_return_resolved()
    }

    pub fn add_param_type(&mut self, typ: LangType) {
        self.param_types.push(typ)
    }

    pub fn set_param_type(&mut self, index: usize, typ: LangType) -> Result<(), TypeError> {
        if index >= self.param_types.len() {
            Err(TypeError::CheckError(
                format!("Invalid parameter index: {}, params length: {}",
                        index, self.param_types.len()))
            )
        } else if {
            let p_type = &self.param_types[index];
            !matches!(*p_type, LangType::Unresolved(_)) && *p_type != typ
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

    pub fn set_return_type(&mut self, rtn_type: LangType) -> Result<(), TypeError> {
        if matches!(*self.rtn_type, LangType::Unresolved(_)) {
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

    fn does_params_match(&self, params: &[LangType]) -> Result<bool, TypeError> {
        if params.len() != self.param_types.len() { return Ok(false); }

        for (self_param, test_param) in self.param_types.iter().zip(params.iter()) {
            if matches!(*self_param, LangType::Unresolved(_)) || matches!(*test_param, LangType::Unresolved(_)) {
                return Err(TypeError::CheckError(
                    format!("Unresolved parameter detected in type comparison, Type Param: {:?}, Test Param: {:?}",
                            self_param, test_param))
                );
            }

            if !self_param.compatible_with(test_param) { return Ok(false); }
        }
        Ok(true)
    }


    fn does_return_match(&self, rtn_type: &LangType) -> Result<bool, TypeError> {
        if matches!(*self.rtn_type,  LangType::Unresolved(_)) || matches!(*rtn_type, LangType::Unresolved(_)) {
            return Err(TypeError::CheckError(
                format!("Unresolved parameter detected in type comparison, Type Param: {:?}, Test Param: {:?}",
                        self.rtn_type, rtn_type))
            );
        }

        Ok(self.rtn_type.compatible_with(rtn_type))
    }


    pub fn is_predicate(&self) -> Result<bool, TypeError> {
        self.does_return_match(&LangType::Boolean)
    }


    pub fn is_nil(&self) -> Result<bool, TypeError> {
        self.does_return_match(&LangType::Nil)
    }


    pub fn is_predicate_of(&self, params: &[LangType]) -> Result<bool, TypeError> {
        if !self.does_return_match(&LangType::Boolean)? {
            Ok(false)
        } else { self.does_params_match(params) }
    }


    // TODO should something with no params still be a consumer?
    pub fn is_consumer_of(&self, params: &[LangType]) -> Result<bool, TypeError> {
        if !self.is_nil()? {
            Ok(false)
        } else { self.does_params_match(params) }
    }


    pub fn is_supplier_of(&self, rtn_type: &LangType) -> Result<bool, TypeError> {
        Ok(self.param_types.is_empty() && self.does_return_match(rtn_type)?)
    }


    pub fn is_function_of(&self, params: &[LangType], rtn_type: &LangType) -> Result<bool, TypeError> {
        Ok(self.does_params_match(params)? && self.does_return_match(rtn_type)?)
    }
}


#[derive(Debug)]
pub struct TypeTable {
    type_map: Vec<LangType>,
    type_enum_id_map: AHashMap<LangType, TypeId>,
    type_string_id_map: IntMap<TypeId>,
}


impl Default for TypeTable {
    fn default() -> Self {
        let mut type_map = Vec::<LangType>::with_capacity(100);
        let mut type_enum_id_map = AHashMap::<LangType, TypeId>::with_capacity(100);
        let mut type_string_id_map = IntMap::<TypeId>::with_capacity(100);

        type_map.push(LangType::Nil);
        type_enum_id_map.insert(LangType::Nil, Self::NIL);
        type_string_id_map.insert(SCACHE.const_nil.into(), Self::NIL);

        type_map.push(LangType::Boolean);
        type_enum_id_map.insert(LangType::Boolean, Self::BOOL);
        type_string_id_map.insert(SCACHE.const_bool.into(), Self::BOOL);

        type_map.push(LangType::Integer);
        type_enum_id_map.insert(LangType::Integer, Self::INT);
        type_string_id_map.insert(SCACHE.const_int.into(), Self::INT);

        type_map.push(LangType::Float);
        type_enum_id_map.insert(LangType::Float, Self::FLOAT);
        type_string_id_map.insert(SCACHE.const_float.into(), Self::FLOAT);

        type_map.push(LangType::String);
        type_enum_id_map.insert(LangType::String, Self::STRING);
        type_string_id_map.insert(SCACHE.const_string.into(), Self::STRING);

        TypeTable { type_map, type_enum_id_map, type_string_id_map }
    }
}


pub struct TypeEntry {
    id: Option<TypeId>,
    typ: LangType,
}


impl TypeEntry {
    pub fn id(&self) -> Option<TypeId> { self.id }
    pub fn typ(&self) -> &LangType { &self.typ }
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
        TypeEntry { id: Some(type_id), typ: self.type_map[type_id.as_usize()].clone() }
    }

    fn resolve_type_name(&self, name: IString) -> Option<TypeId> {
        self.type_string_id_map.get(name.into()).copied()
    }


    // Only call from definition statements, and on variant of internal types (Arrays/Lambdas atm) 

    fn define_new_type(&mut self, typ: &LangType) -> Result<TypeEntry, TypeError> {
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

        if let LangType::Object(obj_typ) = &typ { self.type_string_id_map.insert(obj_typ.name.name().into(), idx); }

        self.type_map.push(typ.clone());

        Ok(TypeEntry { id: Some(idx), typ: self.type_map[idx.as_usize()].clone() })
    }

    pub fn resolve_type(&mut self, typ: &LangType) -> Result<(bool, Option<TypeEntry>), TypeError> {


        // Insert new types should only happen on functions and arrays. As these are variations
        //  of built-in types. All other definitions must occur via definition statements;
        match typ {
            LangType::Array(inner) => {
                if let (true, Some(type_entry)) = self.resolve_type(inner)? {
                    let resolved_type = LangType::Array(Box::new(type_entry.typ));
                    if let Some(existing) = self.type_enum_id_map.get(&resolved_type) {
                        Ok((true, Some(TypeEntry { id: Some(*existing), typ: typ.clone() })))
                    } else { Ok((true, Some(self.define_new_type(typ)?))) }
                } else { Ok((false, None)) }
            }

            LangType::Lambda(func_type) => {
                let mut resolved_type = FunctionType::default();
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

                let resolved_type = LangType::Lambda(resolved_type);

                if resolved_type.is_resolved() {
                    if let Some(existing) = self.type_enum_id_map.get(&resolved_type) {
                        Ok((true, Some(TypeEntry { id: Some(*existing), typ: resolved_type })))
                    } else { Ok((true, Some(self.define_new_type(&resolved_type)?))) }
                } else { Ok((false, Some(TypeEntry { id: None, typ: resolved_type }))) }
            }

            LangType::Unresolved(unresolved) => {
                match unresolved {
                    UnresolvedType::Identifier(i_str) => {
                        if let Some(type_id) = self.type_string_id_map.get(i_str.value.into()) {
                            Ok((true, Some(TypeEntry { id: Some(*type_id), typ: typ.clone() })))
                        } else { Ok((false, None)) }
                    }
                    _ => panic!("Fatal<Internal>: Unknown types should not be resolved directly")
                }
            }
            LangType::Quote => todo!("Implement Quote"),
            LangType::Object(_) => todo!("Implement Objects"),
            LangType::Tuple(_) => todo!("Implement Tuples"),
            LangType::Nil | LangType::Boolean | LangType::Integer | LangType::Float | LangType::String => panic!("Should already exist"),
        }
    }


    pub fn get_type_id(&self, typ: &LangType) -> Option<TypeId> {
        self.type_enum_id_map.get(typ).map_or_else(|| None, |t| Some(*t))
    }


    pub fn get_type_by_id(&self, id: TypeId) -> &LangType {
        unsafe { self.type_map.get_unchecked(id.as_usize()) }
    }


    pub fn type_id_compatible(&self, src_type: TypeId, dst_type: TypeId) -> bool {
        let src = &self.type_map[src_type.as_usize()];
        let dst = &self.type_map[dst_type.as_usize()];
        dst.compatible_with(src)
    }


    pub fn get_type_by_name(&self, i_string: IString) -> Option<&LangType> {
        self.type_string_id_map.get(i_string.into()).map(|id| self.get_type_by_id(*id))
    }


    pub fn get_type_and_id_by_name(&self, i_string: IString) -> Option<(&LangType, TypeId)> {
        self.type_string_id_map.get(i_string.into()).map(|id| (self.get_type_by_id(*id), *id))
    }
}
