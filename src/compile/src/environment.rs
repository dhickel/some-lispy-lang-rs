use std::any::Any;
use std::cmp::Ordering;
use std::string::{ToString};
use std::sync::{Arc, RwLock};
use std::sync::atomic::AtomicUsize;
use intmap::IntMap;
use lang::{ModifierFlags, ValueType};
use lang::ast::{FuncMeta, MetaData, ResolveData, Symbol};
use lang::types::{LangType, PrimitiveType, TypeEntry, TypeId, TypeTable};
use lang::util::{IString, SCACHE};


#[derive(Debug)]
pub enum EnvError {
    DupeSymbol(String),
    InvalidAccess(String),
}


#[derive(Debug, Clone)]
pub struct SymbolContext {
    pub name: IString,
    pub value_type: ValueType,
    pub mod_flags: ModifierFlags,
    pub ns_id: u16,
    pub scope_id: u32,
    pub depth: u32,
    pub type_id: TypeId,
    pub typ: LangType,
    pub meta_data: Option<MetaData>,
}


impl PartialEq for SymbolContext { fn eq(&self, other: &Self) -> bool { self.name == other.name } }


impl PartialOrd for SymbolContext {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.name.partial_cmp(&other.name) }
}


impl From<SymbolContext> for ResolveData {
    fn from(sc: SymbolContext) -> Self {
        ResolveData {
            type_id: sc.type_id,
            ns_id: sc.ns_id,
            scope_id: sc.scope_id,
            typ: sc.typ.clone(),
            meta_data: sc.meta_data.clone(),
        }
    }
}


#[derive(Default, Debug)]
struct SymbolTable {
    table: IntMap<Vec<SymbolContext>>,
}

// TODO make sure namespace symbols and symboltable symbols dont clash

impl SymbolTable {
    fn gen_key(ns_id: u16, scope_id: u32) -> u64 { ((ns_id as u64) << 32) | (scope_id as u64) }

    fn sorted_insert(&self, existing: &[SymbolContext], symbol_context: SymbolContext) -> Result<usize, EnvError> {
        match existing.binary_search_by(
            |s| s.partial_cmp(&symbol_context).unwrap_or(Ordering::Less)
        ) {
            Ok(_) => {
                let symbol_name = SCACHE.resolve(symbol_context.name);
                Err(EnvError::DupeSymbol(format!("Duplicate symbol declared: {:?}", symbol_name)))
            }
            Err(index) => Ok(index)
        }
    }

    pub fn insert_symbol(&mut self, ns_id: u16, scope_id: u32, symbol_context: SymbolContext) -> Result<(), EnvError> {
        let key = Self::gen_key(ns_id, scope_id);

        if let Some(existing) = self.table.get_mut(key) {
            match existing.binary_search_by(|s| s.partial_cmp(&symbol_context).unwrap_or(Ordering::Less)) {
                Ok(_) => {
                    let symbol_name = SCACHE.resolve(symbol_context.name);
                    return Err(EnvError::DupeSymbol(format!("Duplicate symbol declared: {:?}", symbol_name)));
                }
                Err(index) => existing.insert(index, symbol_context)
            }
        } else { self.table.insert(key, vec![symbol_context]); }
        Ok(())
    }


    pub fn get_symbol(&self, ns_id: u16, scope_id: u32, name: IString) -> Option<SymbolContext> {
        let key = Self::gen_key(ns_id, scope_id);

        if let Some(existing) = self.table.get(key) {
            if let Ok(found) = existing.binary_search_by(
                |s| s.name.partial_cmp(&name).unwrap_or(Ordering::Less)
            ) {
                existing.get(found).cloned()
            } else { None }
        } else { None }
    }

    pub fn find_symbol(&self, ns_id: u16, active_scopes: &[u32], name: IString) -> Option<SymbolContext> {
        active_scopes.iter().find_map(|s_id| { self.get_symbol(ns_id, *s_id, name) })
    }
}


#[derive(Default)]
pub struct Environment {
    namespace_map: RwLock<Vec<IString>>,
    symbol_table: Arc<RwLock<SymbolTable>>,
    ns_export_table: Arc<RwLock<IntMap<SymbolContext>>>, // ns_id key
    type_table: Arc<RwLock<TypeTable>>,
}


impl Environment {
    pub fn register_namespace(&mut self, name: IString) -> u16 {
        if let Ok(mut vec) = self.namespace_map.write() {
            let vec_len = vec.len();
            if vec_len > u16::MAX as usize { panic!("Fatal<internal>: Namespace indices exceeded 2 bytes") }
            vec.push(name);
            vec_len as u16
        } else { panic!("Fatal<internal>: Lock Poisoned") }
    }

    pub fn new_sub_env(&self, ns_id: u16) -> SubEnvironment {
        SubEnvironment {
            curr_ns: ns_id,
            curr_scope: 0,
            curr_depth: 0,
            active_scopes: vec![0],
            curr_imports: Default::default(),
            symbol_table_ref: self.symbol_table.clone(),
            ns_export_table_ref: self.ns_export_table.clone(),
            type_table_ref: self.type_table.clone(),
            curr_context: vec![EnvContext::Namespace(NameSpaceCtx::default())],
        }
    }
}


#[derive(Debug)]
pub struct ImportCtx {
    ns_chain: Vec<u16>,
    identifier: IString,
    context: SymbolContext,
}


#[derive(Default, Debug)]
pub struct NameSpaceCtx {
    pub functions: Vec<SymbolContext>,
    pub constants: Vec<SymbolContext>,
    pub declarations: Vec<SymbolContext>,
    pub imports: Vec<ImportCtx>,
    pub constant_pool: ConstantPool,
}


#[derive(Default, Debug)]
pub struct ObjCtx {
    pub functions: Vec<FunctionCtx>,
    pub variables: Vec<TypeId>,
    pub constant_pool: ConstantPool,
}


#[derive(Default, Debug)]
pub struct FunctionCtx {
    pub functions: Vec<FuncMeta>,
    pub variables: Vec<TypeId>,
    pub locals: u32,
}


#[derive(Debug)]
pub enum EnvContext {
    Namespace(NameSpaceCtx),
    Object(ObjCtx),
    Function(FunctionCtx),
}


#[derive(Default, Debug)]
pub struct ConstantPool {
    pool: Vec<u8>,
    curr_index: usize,
}


impl ConstantPool {
    pub fn push_constant(&mut self, data: [u8; 8]) -> u16 {
        if self.curr_index + 1 > u16::MAX as usize {
            panic!("Fatal<internal>: Exceeded max constant pool size: {:?}", u16::MAX)
        }
        let idx = self.curr_index as u16;
        self.pool.extend(data);
        self.curr_index += 1;
        idx
    }
}

#[derive(Debug)]
pub struct SubEnvironment {
    curr_ns: u16,
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    pub curr_imports: IntMap<SymbolContext>, // IString key
    symbol_table_ref: Arc<RwLock<SymbolTable>>,
    ns_export_table_ref: Arc<RwLock<IntMap<SymbolContext>>>, // ns_id key
    type_table_ref: Arc<RwLock<TypeTable>>,
    curr_context: Vec<EnvContext>,
}


impl SubEnvironment {
    pub fn reset_scope_for_next_iter(&mut self) {
        println!("Reset Scopes with active scopes: {:?}", self.active_scopes);
        if self.curr_depth != 0 { panic!("Fatal<Internal>: Reset scope at non-zero depth") }
        match self.active_scopes.first() {
            Some(id) if *id == 0 && self.active_scopes.len() == 1 => self.curr_scope = 0,
            _ => panic!("Fatal<Internal>: Reset scope with non-global active scope(s)"),
        }
    }
    pub fn get_parent_scope(&self) -> Result<u32, EnvError> {
        self.active_scopes.get(self.active_scopes.len() - 2).copied().ok_or(EnvError::InvalidAccess("Scope index out of bounds".to_string()))
    }

    pub fn get_curr_scope(&self) -> u32 {
        *self.active_scopes.last().expect("Fatal<Internal>: No scope in focus")
    }

    pub fn get_curr_depth(&self) -> u32 {
        self.curr_depth
    }

    pub fn push_scope(&mut self) {
        println!("Pushed Scope #{}", self.curr_scope);
        self.curr_scope += 1;
        self.curr_depth += 1;
        self.active_scopes.push(self.curr_scope)
    }

    pub fn pop_scope(&mut self) {
        println!("Popped Scope #{}", self.curr_scope);
        self.curr_depth -= 1;
        self.active_scopes.pop().expect("Fatal<Internal>: Popped global scope");
    }


    pub fn push_func_context(&mut self) {
        self.curr_context.push(EnvContext::Function(FunctionCtx::default()))
    }

    pub fn push_obj_context(&mut self) {
        self.curr_context.push(EnvContext::Object(ObjCtx::default()))
    }

    pub fn pop_context(&mut self) {
        self.curr_context.pop().expect("Fatal<Internal>: Popped non-existent context");
    }


    // FIXME clone here
    pub fn get_type_by_id(&self, type_id: TypeId) -> LangType {
        self.type_table_ref.read().unwrap().get_type_by_id(type_id).clone()
    }

    pub fn get_type_by_name(&self, name: IString) -> Option<LangType> {
        self.type_table_ref.read().unwrap().get_type_by_name(name).map_or_else(|| None, |t| Some(t.clone()))
    }
    pub fn get_type_and_id_by_name(&self, name: IString) -> Option<(LangType, TypeId)> {
        self.type_table_ref.read().unwrap().get_type_and_id_by_name(name)
            .map_or_else(|| None, |t| Some((t.0.clone(), t.1)))
    }

    pub fn get_id_for_type(&self, typ: &LangType) -> Option<TypeId> {
        self.type_table_ref.read().unwrap().get_type_id(typ)
    }

    pub fn are_types_compatible(&self, src_type: TypeId, dst_type: TypeId) -> bool {
        self.type_table_ref.read().unwrap().type_id_compatible(src_type, dst_type)
    }

    pub fn add_symbol(
        &mut self,
        symbol: Symbol,
        type_id: TypeId,
        mod_flags: ModifierFlags,
        meta_data: Option<MetaData>,
    ) -> Result<(), EnvError> {
        if let Symbol::Definition { mut is_defined, .. } = symbol {
            if is_defined { return Ok(()); }

            println!("Adding Symbol({:?}) In Scope: ns={:?}, curr_scope={:?}, active scopes:={:?}",
                     SCACHE.resolve(symbol.name()), self.curr_ns, self.curr_scope, self.active_scopes);
            let typ = self.type_table_ref.read().unwrap().get_type_by_id(type_id).clone();
            let value_type: ValueType = ValueType::from(&typ);
            let symbol = SymbolContext {
                name: symbol.name(),
                value_type,
                mod_flags,
                meta_data,
                type_id,
                typ,
                ns_id: self.curr_ns,
                scope_id: self.curr_scope,
                depth: self.curr_depth,
            };

            println!("Resolved Symbol: {:?}", symbol);
            match self.symbol_table_ref.write().unwrap()
                .insert_symbol(self.curr_ns, self.get_curr_scope(), symbol) {
                Ok(_) => {
                    is_defined = true;
                    Ok(())
                }
                Err(err) => Err(err)
            }
        } else { panic!("Fatal<Internal>: Attempted to add symbol reference(non-definition) to symbol table") }
    }

    pub fn get_resolve_data_by_type(&mut self, typ: &LangType) -> Option<ResolveData> {
        let existing = self.type_table_ref.read().unwrap().get_type_id(&typ);

        if let Some(existing) = existing {
            Some(ResolveData::new(self.curr_ns, self.curr_scope, TypeEntry::new(existing, typ.clone())))
        } else {
            // SAFETY: We know typ won't be accessed through its original reference while
            // resolve_type() is running, since we're the only ones holding the write lock
            // on type_table_ref. The mutable reference is dropped before we return.
            // TODO: this should be updated to better logic and avoid unsafe
            let type_ptr = typ as *const LangType as *mut LangType;
            let type_mut = unsafe { &mut *type_ptr };

            let resolve_result = self.type_table_ref.write().unwrap()
                .resolve_type(type_mut).expect("Need to propagate errors "); // FIXME

            if let Some(type_entry) = resolve_result {
                Some(self.new_resolve(type_entry))
            } else { None }
        }
    }

    pub fn get_resolve_data_by_type_id(&self, id: TypeId) -> ResolveData {
        let typ = self.type_table_ref.read().unwrap().get_type_by_id(id).clone();
        ResolveData::new(self.curr_ns, self.curr_scope, TypeEntry::new())
    }
    
    fn new_resolve(&self, type_entry: TypeEntry ) -> ResolveData {
        ResolveData {
            ns_id: self.curr_ns,
            scope_id: self.curr_scope,
            type_entry,
            meta_data: None,
        }
    }

    pub fn get_nil_resolve(&self) -> ResolveData {
        ResolveData::new(self.curr_ns, self.curr_scope, TypeTable::NIL, LangType::Primitive(PrimitiveType::Nil))
    }

    // TODO add ability to look up other namespaces via name?
    pub fn get_symbol(&self, ns: u16, scope_id: u32, symbol: Symbol) -> Option<SymbolContext> {
        if let Symbol::Reference(name) = symbol {
            self.symbol_table_ref.read().unwrap().get_symbol(ns, scope_id, name)
        } else { panic!("Fatal<Internal>: Attempted to look up  symbol definition as reference") }
    }

    // FIXME  Need to traverse global and import spaces as well
    pub fn find_symbol_in_scope(&self, symbol: Symbol) -> Option<SymbolContext> {
        println!("Finding Symbol({:?}) In Scope: ns={:?}, curr_scope={:?}, active scopes:={:?}",
                 SCACHE.resolve(symbol.name()), self.curr_ns, self.curr_scope, self.active_scopes);

        if let Symbol::Reference(name) = symbol {
            self.symbol_table_ref.read().unwrap().find_symbol(self.curr_ns, &self.active_scopes, name)
        } else { panic!("Fatal<Internal>: Attempted to find symbol definition as reference") }
    }
}