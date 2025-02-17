use std::process::id;
use intmap::IntMap;
use sha2::digest::typenum::private::IsEqualPrivate;
use crate::util::{IString, SCache, SCACHE};
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

impl From<TypeId> for usize {
    fn from(value: TypeId) -> Self { value.0 as usize }
}

#[derive(Debug)]
pub enum TypeError {
    CheckError(String),
    Resolution(String),
    InvalidOperation(String),
    TypeOverflow,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LangType {
    Primitive(PrimitiveType),
    Composite(CompositeType),
    Custom(CustomType),
    Undefined,
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
    const UNDEFINED: LangType = LangType::Undefined;


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
            _ => LangType::Custom(CustomType { identifier: name, is_resolved: false }),
        }
    }

    pub fn is_resolved(&self) -> bool {
        match self {
            LangType::Primitive(_) => true,
            LangType::Composite(comp_type) => {
                match comp_type {
                    CompositeType::Function(func_type) => func_type.is_resolved(),
                    CompositeType::Array(arr_type) => arr_type.is_resolved(),
                    CompositeType::Tuple(types) => types.iter().any(|t| t.is_resolved()),
                    CompositeType::String | CompositeType::Quote => true,
                }
            }
            LangType::Custom(custom_type) => custom_type.is_resolved,
            LangType::Undefined => panic!("Fatal<Internal>: is_resolved called on undefined type,
                compiler should not be resolving undefined type and must enforce their limited use, or per-infer them")
        }
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
pub struct CustomType {
    pub identifier: IString,
    pub is_resolved: bool,
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
            rtn_type: Box::new(LangType::UNDEFINED),
            param_types: vec![],
        }
    }
}

impl FunctionType {
    pub fn init_params_undefined(&mut self, param_count: usize) -> Result<(), TypeError> {
        if !self.param_types.is_empty() {
            Err(TypeError::CheckError("Attempted to reinitialize existing parameters to Unresolved".to_string()))
        } else {
            self.param_types = std::iter::repeat(LangType::UNDEFINED).take(param_count).collect();
            Ok(())
        }
    }

    pub fn are_params_resolved(&self) -> bool { self.param_types.iter().all(|p| p.is_resolved()) }

    pub fn is_return_resolved(&self) -> bool { self.rtn_type.is_resolved() }

    pub fn is_resolved(&self) -> bool { self.are_params_resolved() && self.is_return_resolved() }

    pub fn add_param_type(&mut self, typ: LangType) { self.param_types.push(typ) }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeMeta {}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEntry {
    id: TypeId,
    lang_type: LangType,
    meta_data: Option<TypeMeta>,
}

impl TypeEntry {
    pub fn id(&self) -> TypeId { self.id }

    pub fn lang_type(&self) -> &LangType { &self.lang_type }

    pub fn new(id: TypeId, lang_type: LangType) -> TypeEntry {
        TypeEntry { id, lang_type, meta_data: None }
    }

    pub fn new_with_meta(id: TypeId, lang_type: LangType, meta_data: TypeMeta) -> TypeEntry {
        TypeEntry { id, lang_type, meta_data: Some(meta_data) }
    }
}


#[derive(Debug)]
pub struct TypeTable {
    type_table: Vec<TypeEntry>, // Store resolved types for id lookups
    type_name_map: IntMap<TypeId>, // Map declare custom type IString Ids to type Ids
}


impl Default for TypeTable {
    fn default() -> Self {
        let type_table = Vec::<TypeEntry>::with_capacity(100);
        let type_name_map = IntMap::<TypeId>::with_capacity(100);
        let mut type_table = TypeTable { type_table, type_name_map };

        type_table.insert(SCACHE.NIL, TypeTable::NIL);
        type_table.insert(SCACHE.BOOL, TypeTable::BOOL);
        type_table.insert(SCACHE.STRING, TypeTable::STRING);
        type_table.insert(SCACHE.U8, TypeTable::U8);
        type_table.insert(SCACHE.U16, TypeTable::U16);
        type_table.insert(SCACHE.U32, TypeTable::U32);
        type_table.insert(SCACHE.U64, TypeTable::U64);
        type_table.insert(SCACHE.I32, TypeTable::I32);
        type_table.insert(SCACHE.I64, TypeTable::I64);
        type_table.insert(SCACHE.F32, TypeTable::F32);
        type_table.insert(SCACHE.F64, TypeTable::F64);

        type_table
    }
}


impl TypeTable {
    pub const NIL: TypeEntry = TypeEntry { id: TypeId(0), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const BOOL: TypeEntry = TypeEntry { id: TypeId(1), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const STRING: TypeEntry = TypeEntry { id: TypeId(2), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const U8: TypeEntry = TypeEntry { id: TypeId(3), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const U16: TypeEntry = TypeEntry { id: TypeId(4), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const U32: TypeEntry = TypeEntry { id: TypeId(5), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const U64: TypeEntry = TypeEntry { id: TypeId(6), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const I32: TypeEntry = TypeEntry { id: TypeId(7), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const I64: TypeEntry = TypeEntry { id: TypeId(8), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const F32: TypeEntry = TypeEntry { id: TypeId(9), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };
    pub const F64: TypeEntry = TypeEntry { id: TypeId(10), lang_type: LangType::Primitive(PrimitiveType::Nil), meta_data: None };

    pub fn get_entry(&self, type_id: TypeId) -> TypeEntry { self.type_table[type_id.as_usize()].clone() }

    pub fn lookup_by_type(&self, typ: &LangType) -> Option<TypeEntry> {
        self.type_table.iter().find(|t| t.lang_type == *typ).map(TypeEntry::clone)
    }

    pub fn lookup_by_name(&self, name: IString) -> Option<TypeId> {
        self.type_name_map.get(name.into()).map_or_else(|| None, |id| Some(*id))
    }

    fn insert(&mut self, name: IString, type_entry: TypeEntry) {
        // Ensure the existing id is the same as where it will be pushed
        // This function is only used internally but this avoids any needless bugs
        assert_eq!(type_entry.id.as_usize(), self.type_table.len());
        self.type_name_map.insert(name.into(), type_entry.id);
        self.type_table.push(type_entry)
    }

    // Only call on definition statements, and on variant of internal types (Arrays/Lambdas atm) 

    fn define_new_composite_type(&mut self, typ: LangType) -> Result<TypeEntry, TypeError> {
        if self.type_table.iter().any(|t| t.lang_type == typ) {
            return Err(TypeError::InvalidOperation("Fatal<Internal>: Attempted to redefine existing type".to_string()));
        }

        let idx = {
            let len = self.type_table.len();
            if len > u16::MAX as usize {
                return Err(TypeError::TypeOverflow);
            } else { TypeId::from(len) } // len instead of len -1, type hasn't been pushed yet
        };

        let entry = TypeEntry::new(idx, typ.clone());

        match typ {
            LangType::Composite(comp_type) => {
                self.type_table.push(entry.clone());
            }
            LangType::Custom { .. } => todo!("We are going to need to define custom types elseways"),
            _ => panic!("Fatal<Internal>: Only composite and custom types can be defined as new types, found: {:?}", typ)
        }

        Ok(entry)
    }

    pub fn resolve_type(&mut self, typ: &mut LangType) -> Result<Option<TypeEntry>, TypeError> {
        if typ.is_resolved() { return Ok(self.lookup_by_type(typ)); }

        match typ {
            LangType::Primitive(_) => panic!("Fatal<Internal>: Primitives are already resolved and this path never taken"),

            LangType::Undefined => panic!("Fatal<Internal>: Undefined types should not occur in a position that would
                result in them needing resolved, this should be enforced or pre-inferred in the compiler"),

            LangType::Composite(CompositeType::Array(inner)) => {
                // Ensure inner type is resolved
                if let Some(type_entry) = self.resolve_type(inner)? {
                    let array_type = LangType::Composite(CompositeType::Array(Box::new(type_entry.lang_type.clone())));

                    if let Some(existing) = self.lookup_by_type(&array_type) {
                         Ok(Some(existing))
                    } else {
                        let entry = self.define_new_composite_type(array_type.clone())?;
                        Ok(Some(entry))
                    }
                } else { Ok(None) }
            }

            LangType::Composite(CompositeType::Function(func_type)) => {
                if !func_type.are_params_resolved() {
                    let params = &mut func_type.param_types;
                    for p in params {
                        if !p.is_resolved() { self.resolve_type(p)?; }
                    }
                }

                if !func_type.is_return_resolved() {
                    let rtn = &mut func_type.rtn_type;
                    self.resolve_type(rtn)?;
                }

                // Now that resolution has been attempted on any unresolved function member, 
                // check to see if they are all fully resolved
                if func_type.is_resolved() {
                    // Define a new type if it does not exist (only done for built-in types)
                    let resolved_type = self.define_new_composite_type(typ.clone())?;
                    Ok(Some(resolved_type))
                } else { Ok(None) }
            }

            LangType::Composite(_) => todo!("Unimplemented Composite type"),

            LangType::Custom(custom_type) => {
                let is_resolved = self.lookup_by_type(&typ).map_or_else(|| false, |_| true);
                
                if let Some(type_entry) = self.lookup_by_type(&typ) {
                    Ok(Some(type_entry))
                } else { Ok(None) }
            }
        }
    }

    pub fn type_id_compatible(&self, src_type: TypeId, dst_type: TypeId) -> bool {
        todo!("Need to implement")
        // let src = &self.type_table[src_type.as_usize()];
        // let dst = &self.type_table[dst_type.as_usize()];
        // dst.lang_type().compatible_with(&src.lang_type)
    }
}
