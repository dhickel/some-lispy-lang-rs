use std::any::Any;
use std::ops::{Add, Deref};
use ahash::AHashMap;
use intmap::IntMap;
use crate::util::{IString, SCACHE};
use crate::ast::Symbol;
use crate::PrimType::Bool;

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

// TODO add a way to have build in interfaces that do more on the back end, like from type
//  we can then check that when casting

// FIXME need ot calculate things like operation return values (unless we are going to require explicit casts)

impl LangType {
    const U8: LangType = LangType::Primitive(PrimitiveType::U8);
    const U16: LangType = LangType::Primitive(PrimitiveType::U16);
    const U32: LangType = LangType::Primitive(PrimitiveType::U32);
    const U64: LangType = LangType::Primitive(PrimitiveType::U64);
    const I32: LangType = LangType::Primitive(PrimitiveType::I32);
    const I64: LangType = LangType::Primitive(PrimitiveType::I64);
    const F32: LangType = LangType::Primitive(PrimitiveType::F32);
    const F64: LangType = LangType::Primitive(PrimitiveType::F64);
    const BOOL: LangType = LangType::Primitive(PrimitiveType::Bool);
    const STRING: LangType = LangType::Composite(CompositeType::String);
    const NIL: LangType = LangType::Primitive(PrimitiveType::Nil);
    const QUOTE: LangType = LangType::Composite(CompositeType::Quote);
    const UNDEFINED: LangType = LangType::Unresolved(UnresolvedType::Undefined);


    //FIXME this will need to take into account interfaces adn the like on non primtiives
    pub fn can_cast_to(&self, target: &LangType) -> bool {
        match (self, target) {
            (LangType::Primitive(from), LangType::Primitive(to)) => {
                // NOTE: Higher precedence can't safely convert to lower precedence
                //  Lower to higher is usually safe, this may need ot be re-approached
                from.get_precedence() <= to.get_precedence()
            }
            (LangType::Composite(from), LangType::Composite(to)) => {
                from == to
            }
            (LangType::Custom(from), LangType::Custom(to)) => false, // FIXME
            _ => false
        }
    }

    pub fn get_stack_size(&self) -> usize {
        match self {
            LangType::Primitive(prim) => {
                match prim {
                    PrimitiveType::U8 | PrimitiveType::Bool => 1,
                    PrimitiveType::U16 => 2,
                    PrimitiveType::U32 | PrimitiveType::I32 | PrimitiveType::F32 => 4,
                    PrimitiveType::U64 | PrimitiveType::I64 | PrimitiveType::F64 => 8,
                    PrimitiveType::Nil => 0,
                }
            }
            LangType::Composite(comp) => {
                //TODO: add the ability for small arrays to be stack allocated and maybe some flags?
                8
            }
            LangType::Custom(_) => 8, // TODO:: more logic Here
            LangType::Unresolved(ur) => {
                panic!("Fatal<Internal>: get_stack_size() call on unresolved type: {:?}", ur)
            }
        }
    }

    pub fn parse_type_from_string(name: IString) -> LangType {
        let name_str = SCACHE.resolve(name);
        // TODO add _ and * type modifiers
        match name_str {
            "U8" => LangType::U8,
            "U16" => LangType::U16,
            "U32" => LangType::U32,
            "U64" => LangType::U64,
            "I32" => LangType::I32,
            "I64" => LangType::I64,
            "F32" => LangType::F32,
            "F64" => LangType::F64,
            "Bool" => LangType::BOOL,
            "String" => LangType::STRING,
            "Nil" => LangType::NIL,
            //"Tuple" => Type::Tuple,
            // "Array" => LangType::Composite(CompositeType::Array(Box::new(LangType::UNDEFINED))),
            // "Fn" => LangType::Composite(CompositeType::Function(FunctionType::default())),
            "()" => LangType::Primitive(PrimitiveType::Nil),
            _ => UnresolvedType::Identifier(name).into()
        }
    }


    pub fn is_resolved(&self) -> bool {
        match self {
            LangType::Primitive(_) => true,
            LangType::Composite(comp_type) => {
                match comp_type {
                    CompositeType::Function(func_type) => func_type.is_resolved(),
                    CompositeType::Array(arr_type) => arr_type.is_resolved(),
                    CompositeType::Tuple(tup_type) => tup_type.iter().all(|t| t.is_resolved()),
                    CompositeType::String => true,
                    CompositeType::Quote => true,
                }
            }
            LangType::Custom(cust_type) => cust_type.is_resolved(),
            LangType::Unresolved(_) => false,
        }
    }


    pub fn compatible_with(&self, other: &LangType) -> bool {
        self.can_cast_to(other) // TODO: This may need different logic from cast check
    }
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
    Nil,
}

impl PrimitiveType {
    fn get_precedence(&self) -> u8 {
        match self {
            PrimitiveType::Nil => 0,
            PrimitiveType::Bool => 1,
            PrimitiveType::U8 => 2,
            PrimitiveType::U16 => 3,
            PrimitiveType::U32 => 4,
            PrimitiveType::U64 => 5,
            PrimitiveType::I32 => 6,
            PrimitiveType::I64 => 7,
            PrimitiveType::F32 => 8,
            PrimitiveType::F64 => 9,
        }
    }
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
    Interface,
}

impl CustomType {
    pub fn is_resolved(&self) -> bool {
        false // FIXME: make actually resolve
    }
    
    pub fn name(&self) -> IString {
        todo!()
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum UnresolvedType {
    #[default]
    //FIXME: this need properly documented and is kinda of hacky. Panics can be induced if undefined
    // types are in non-optional position, currently these are used on lambda params/returns, since
    // they can either be defined on the symbol or in the lambda sig. But lambdas passed as arguments,
    // can have their type inferred if that path is taken, or it can be require to put into the sig,
    // which would make it easier to run a validation pass on.
    Undefined,
    Identifier(IString),
}


impl Into<LangType> for UnresolvedType { fn into(self) -> LangType { LangType::Unresolved(self) } }


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
                        index, self.param_types.len())))
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


    // pub fn is_predicate(&self) -> Result<bool, TypeError> {
    //     self.does_return_match(&LangType::Boolean)
    // }
    // 
    // 
    // pub fn is_nil(&self) -> Result<bool, TypeError> {
    //     self.does_return_match(&LangType::Nil)
    // }
    // 
    // 
    // pub fn is_predicate_of(&self, params: &[LangType]) -> Result<bool, TypeError> {
    //     if !self.does_return_match(&LangType::Boolean)? {
    //         Ok(false)
    //     } else { self.does_params_match(params) }
    // }
    // 
    // 
    // // TODO should something with no params still be a consumer?
    // pub fn is_consumer_of(&self, params: &[LangType]) -> Result<bool, TypeError> {
    //     if !self.is_nil()? {
    //         Ok(false)
    //     } else { self.does_params_match(params) }
    // }
    // 
    // 
    // pub fn is_supplier_of(&self, rtn_type: &LangType) -> Result<bool, TypeError> {
    //     Ok(self.param_types.is_empty() && self.does_return_match(rtn_type)?)
    // }
    // 
    // 
    // pub fn is_function_of(&self, params: &[LangType], rtn_type: &LangType) -> Result<bool, TypeError> {
    //     Ok(self.does_params_match(params)? && self.does_return_match(rtn_type)?)
    // }
}


#[derive(Debug)]
pub struct TypeTable {
    type_id_table: Vec<LangType>,
    type_enum_map: AHashMap<LangType, TypeId>,
    type_name_map: IntMap<TypeId>, // FIXME: I dont think this is needed
}


impl Default for TypeTable {
    fn default() -> Self {
        let mut type_id_table = Vec::<LangType>::with_capacity(100);
        let mut type_enum_map = AHashMap::<LangType, TypeId>::with_capacity(100);
        let mut type_name_map = IntMap::<TypeId>::with_capacity(100);

        type_id_table.push(LangType::NIL);
        type_enum_map.insert(LangType::NIL, Self::NIL);
        type_name_map.insert(SCACHE.NIL.into(), Self::NIL);

        type_id_table.push(LangType::BOOL);
        type_enum_map.insert(LangType::BOOL, Self::BOOL);
        type_name_map.insert(SCACHE.BOOL.into(), Self::BOOL);

        type_id_table.push(LangType::U8);
        type_enum_map.insert(LangType::U8, Self::U8);
        type_name_map.insert(SCACHE.U8.into(), Self::U8);

        type_id_table.push(LangType::U16);
        type_enum_map.insert(LangType::U16, Self::U16);
        type_name_map.insert(SCACHE.U16.into(), Self::U16);

        type_id_table.push(LangType::U32);
        type_enum_map.insert(LangType::U32, Self::U32);
        type_name_map.insert(SCACHE.U32.into(), Self::U32);

        type_id_table.push(LangType::U64);
        type_enum_map.insert(LangType::U64, Self::U64);
        type_name_map.insert(SCACHE.U64.into(), Self::U64);

        type_id_table.push(LangType::I32);
        type_enum_map.insert(LangType::I32, Self::I32);
        type_name_map.insert(SCACHE.I32.into(), Self::I32);

        type_id_table.push(LangType::I64);
        type_enum_map.insert(LangType::I64, Self::I64);
        type_name_map.insert(SCACHE.I64.into(), Self::I64);

        type_id_table.push(LangType::F32);
        type_enum_map.insert(LangType::F32, Self::F32);
        type_name_map.insert(SCACHE.F32.into(), Self::F32);

        type_id_table.push(LangType::F64);
        type_enum_map.insert(LangType::F64, Self::F64);
        type_name_map.insert(SCACHE.F64.into(), Self::F64);
        
        TypeTable { type_id_table, type_enum_map, type_name_map }
    }
}


pub struct TypeEntry {
    id: Option<TypeId>,
    typ: LangType,
}


impl TypeEntry {
    pub fn id(&self) -> Option<TypeId> { self.id }
    pub fn lang_type(&self) -> &LangType { &self.typ }
}


// TODO clean up the structure and remove all the cloning, I assume type system
//  will be fully redone at some point anyway  

impl TypeTable {
    pub const NIL: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const STRING: TypeId = TypeId(2);
    pub const U8: TypeId = TypeId(3);
    pub const U16: TypeId = TypeId(4);
    pub const U32: TypeId = TypeId(5);
    pub const U64: TypeId = TypeId(6);
    pub const I32: TypeId = TypeId(7);
    pub const I64: TypeId = TypeId(8);
    pub const F32: TypeId = TypeId(9);
    pub const F64: TypeId = TypeId(10);

    fn get_entry(&self, type_id: TypeId) -> TypeEntry {
        TypeEntry { id: Some(type_id), typ: self.type_id_table[type_id.as_usize()].clone() }
    }

    fn resolve_type_name(&self, name: IString) -> Option<TypeId> {
        self.type_name_map.get(name.into()).copied()
    }


    // Only call from definition statements, and on variant of internal types (Arrays/Lambdas atm) 

    fn define_new_type(&mut self, typ: &LangType) -> Result<TypeEntry, TypeError> {
        if self.type_enum_map.contains_key(&typ) {
            return Err(
                TypeError::InvalidOperation("Fatal<Internal>: Attempted to redefine existing type".to_string())
            );
        }

        let idx = {
            let len = self.type_id_table.len();
            if len > u16::MAX as usize {
                return Err(TypeError::TypeOverflow);
            } else { TypeId::from(len) } // len instead of len -1, type hasn't been pushed yet
        };

        self.type_enum_map.insert(typ.clone(), idx);

        if let LangType::Custom(cust_type) = &typ { 
            self.type_name_map.insert(cust_type.name().into(), idx); }

        self.type_id_table.push(typ.clone());

        Ok(TypeEntry { id: Some(idx), typ: self.type_id_table[idx.as_usize()].clone() })
    }

    pub fn resolve_type(&mut self, typ: &LangType) -> Result<(bool, Option<TypeEntry>), TypeError> {


        // Insert new types should only happen on functions and arrays. As these are variations
        //  of built-in types. All other definitions must occur via definition statements;
        match typ {
            LangType::Array(inner) => {
                if let (true, Some(type_entry)) = self.resolve_type(inner)? {
                    let resolved_type = LangType::Array(Box::new(type_entry.typ));
                    if let Some(existing) = self.type_enum_map.get(&resolved_type) {
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
                    if let Some(existing) = self.type_enum_map.get(&resolved_type) {
                        Ok((true, Some(TypeEntry { id: Some(*existing), typ: resolved_type })))
                    } else { Ok((true, Some(self.define_new_type(&resolved_type)?))) }
                } else { Ok((false, Some(TypeEntry { id: None, typ: resolved_type }))) }
            }

            LangType::Unresolved(unresolved) => {
                match unresolved {
                    UnresolvedType::Identifier(i_str) => {
                        if let Some(type_id) = self.type_name_map.get(i_str.value.into()) {
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
        self.type_enum_map.get(typ).map_or_else(|| None, |t| Some(*t))
    }


    pub fn get_type_by_id(&self, id: TypeId) -> &LangType {
        unsafe { self.type_id_table.get_unchecked(id.as_usize()) }
    }


    pub fn type_id_compatible(&self, src_type: TypeId, dst_type: TypeId) -> bool {
        let src = &self.type_id_table[src_type.as_usize()];
        let dst = &self.type_id_table[dst_type.as_usize()];
        dst.compatible_with(src)
    }


    pub fn get_type_by_name(&self, i_string: IString) -> Option<&LangType> {
        self.type_name_map.get(i_string.into()).map(|id| self.get_type_by_id(*id))
    }


    pub fn get_type_and_id_by_name(&self, i_string: IString) -> Option<(&LangType, TypeId)> {
        self.type_name_map.get(i_string.into()).map(|id| (self.get_type_by_id(*id), *id))
    }
}
