use std::cmp::Ordering;
use std::string::{ParseError, ToString};
use std::sync::{Arc, Mutex, RwLock};
use intmap::IntMap;
use lang::{ModifierFlags, ValueType};
use lang::types::{Type, TypeTable};
use lang::util::{IString, SCACHE};
use crate::environment::{NameSpace, TypeData};


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
    pub ns_idx: u16,
    pub scope: u32,
    pub depth: u32,
    pub typ: Type,
}


impl PartialEq for SymbolContext { fn eq(&self, other: &Self) -> bool { self.name == other.name } }


impl PartialOrd for SymbolContext {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.name.partial_cmp(other) }
}


#[derive(Default)]
struct SymbolTable {
    table: IntMap<Vec<SymbolContext>>,
}


impl SymbolTable {
    fn gen_key(ns_id: u16, scope_id: u32) -> u64 { ((ns_id as u64) << 32) | (scope_id as u64) }

    fn sorted_insert(&self, existing: &mut Vec<SymbolContext>, symbol_context: SymbolContext) -> Result<(), EnvError> {
        match existing.binary_search_by(
            |s| s.partial_cmp(&symbol_context).unwrap_or(Ordering::Less)
        ) {
            Ok(existing) => {
                let symbol_name = SCACHE.resolve(symbol_context.name);
                Err(EnvError::DupeSymbol(format!("Duplicate symbol declared: {:?}", symbol_name)))
            }
            Err(index) => {
                existing.insert(index, symbol_context);
                Ok(())
            }
        }
    }

    pub fn insert_symbol(&mut self, ns_id: u16, scope_id: u32, symbol_context: SymbolContext) -> Result<(), EnvError> {
        let key = Self::gen_key(ns_id, scope_id);

        if let Some(existing) = self.table.get_mut(key) {
            self.sorted_insert(existing, symbol_context)?;
            Ok(())
        } else {
            self.table.insert(key, vec![symbol_context]);
            Ok(())
        }
    }

    pub fn get_symbol(&mut self, ns_id: u16, scope_id: u32, name: IString) -> Option<&SymbolContext> {
        let key = Self::gen_key(ns_id, scope_id);

        if let Some(existing) = self.table.get_mut(key) {
            if let Ok(found) = existing.binary_search_by(
                |s| s.name.partial_cmp(&name).unwrap_or(Ordering::Less)
            ) {
                existing.get(found)
            } else { None }
        } else { None }
    }

    pub fn find_symbol(&mut self, ns_id: u16, active_scopes: &[u32], name: IString) -> Option<&SymbolContext> {
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
        }
    }
}


pub struct SubEnvironment {
    curr_ns: u16,
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    pub curr_imports: IntMap<SymbolContext>, // IString key
    symbol_table_ref: Arc<RwLock<SymbolTable>>,
    ns_export_table_ref: Arc<RwLock<IntMap<SymbolContext>>>, // ns_id key
    type_table_ref: Arc<RwLock<TypeTable>>,
}


impl SubEnvironment {
    pub fn get_parent_scope(&self) -> Result<u32, EnvError> {
        *self.active_scopes.get(self.active_scopes.len() - 2)
            .ok_or(EnvError::InvalidAccess("Scope index out of bounds".to_string()))
    }

    pub fn get_curr_scope(&self) -> u32 {
        *self.active_scopes.last().expect("Fatal<internal>: No scope in focus")
    }

    pub fn get_curr_depth(&self) -> u32 {
        self.curr_depth
    }

    pub fn push_scope(&mut self) {
        self.curr_scope += 1;
        self.curr_depth += 1;
        self.active_scopes.push(self.curr_scope)
    }

    pub fn pop_scope(&mut self) {
        self.curr_depth -= 1;
        self.active_scopes.pop().expect("Fatal<internal>: Popped global scope");
    }

    pub fn add_symbol(&mut self, name: IString, typ: Type, mod_flags: ModifierFlags) -> Result<(), EnvError> {
        let value_type: ValueType = ValueType::from(&typ);
        let symbol = SymbolContext {
            name,
            value_type,
            mod_flags,
            typ,
            ns_idx: self.curr_ns,
            scope: self.curr_scope,
            depth: self.curr_depth,
        };
        self.symbol_table_ref.write().unwrap().insert_symbol(self.curr_ns, self.get_curr_scope(), symbol)
    }

    // TODO add ability to look up other namespaces via name?
    pub fn get_symbol(&mut self, ns: u16, scope_id: u32, name: IString) -> Option<&SymbolContext> {
        self.symbol_table_ref.read().unwrap().get_symbol(ns, scope_id, name)
    }

    // FIXME  Need to traverse global and import spaces as well
    pub fn find_symbol_in_scope(&mut self, name: IString) -> Option<&SymbolContext> {
        self.symbol_table_ref.read().unwrap().find_symbol(self.curr_ns, &self.active_scopes, name)
    }

    fn new_symbol_context(&self, name: IString, typ: Type, value_type: ValueType, mod_flags: ModifierFlags) -> SymbolContext {}
}