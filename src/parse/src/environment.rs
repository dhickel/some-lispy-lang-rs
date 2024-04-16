use ahash::{AHashMap, HashMap};
use intmap::IntMap;
use lang::types::{ObjType, Type};
use crate::util::SCACHE;


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub name: String,
    pub value: LiteralValue,
    pub mutable: bool,
    pub private: bool,
}


#[derive(Debug, Clone, PartialEq)]
enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Nil,
    Quote,
    Object,
    Vector,
    Pair,
    Lambda,
}


#[derive(Debug, Clone, PartialEq)]
pub struct SymbolCtx {
    pub scope: u32,
    pub depth: u32,
    pub typ: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ScopeCtx {
    pub scope: u32,
    pub depth: u32,
}


pub struct Context {
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    symbols: AHashMap<u32, IntMap<SymbolCtx>>,
    globals: IntMap<SymbolCtx>,
    types: IntMap<Type>,
    unresolved: Type,
}


impl Default for Context {
    fn default() -> Self {
        let mut types = IntMap::<Type>::with_capacity(50);
        types.insert(SCACHE.const_int, Type::Integer);
        types.insert(SCACHE.const_float, Type::Float);
        types.insert(SCACHE.const_bool, Type::Boolean);
        types.insert(SCACHE.const_string, Type::String);
        types.insert(SCACHE.const_nil, Type::Nil);

        Context {
            curr_scope: 0,
            curr_depth: 0,
            active_scopes: Vec::<u32>::new(),
            symbols: AHashMap::<u32, IntMap<SymbolCtx>>::with_capacity(50),
            globals: IntMap::<SymbolCtx>::with_capacity(50),
            types,
            unresolved: Type::Unresolved,
        }
    }
}


impl Context {
    pub fn add_symbol(&mut self, symbol: u64, typ: Type) -> Result<SymbolCtx, String> {
        let s_int = symbol;
        let data = SymbolCtx { scope: self.curr_scope, depth: self.curr_depth, typ };

        if self.curr_depth == 0 {
            self.globals.insert(s_int, data.clone());
            return Ok(data.clone());
        }

        if let Some(existing) = self.symbols.get_mut(&self.curr_scope) {
            return match existing.insert_checked(s_int, data.clone()) {
                true => Ok(data.clone()),
                false => Err(format!("Redefinition of existing binding: {}", SCACHE.resolve(symbol)))
            };
        }

        let mut scope_table = IntMap::<SymbolCtx>::new();
        scope_table.insert(s_int, data.clone());
        self.symbols.insert(self.curr_scope, scope_table);
        Ok(data.clone())
    }

    pub fn get_symbol_type(&self, symbol: u64) -> &Type {
        let s_int = symbol;

        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(scope_symbols) = self.symbols.get(&scope_id) {
                if let Some(symbol_ctx) = scope_symbols.get(s_int) {
                    return &symbol_ctx.typ;
                }
            }
        }

        if let Some(global_symbol) = self.globals.get(s_int) {
            return &global_symbol.typ;
        }

        &self.unresolved
    }


    pub fn get_symbol_ctx(&self, symbol: u64) -> Option<SymbolCtx> {
        let s_int = symbol;

        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(scope_symbols) = self.symbols.get(&scope_id) {
                if let Some(symbol_ctx) = scope_symbols.get(s_int) {
                    return Some(symbol_ctx.clone());
                }
            }
        }

        if let Some(global_symbol) = self.globals.get(s_int) {
            return Some(global_symbol.clone());
        }

        None
    }

    pub fn validate_type(&self, u64: u64) -> Type {
        let found = self.types.get(u64);
        if found.is_none() { return Type::Unresolved; }

        match found.unwrap() {
            Type::Unresolved | Type::Integer | Type::Float | Type::String | Type::Boolean |
            Type::Vector(_) | Type::Nil | Type::Pair => found.unwrap().clone(),
            Type::Quote => todo!(),
            Type::Object(obj) => {
                for typ in &obj.super_types {
                    if let Type::Object(obj_type) = typ {
                        if obj_type.name == u64 { return typ.clone(); }
                    }
                } // Object will always have a u64 value
                Type::Unresolved
            }
            Type::Lambda(_) => todo!(),
        }
    }

    pub fn add_type(&mut self, u64: u64, typ: Type) {
        self.types.insert_checked(u64, typ);
    }

    pub fn push_scope(&mut self) {
        self.curr_scope += 1;
        self.curr_depth += 1;
        self.active_scopes.push(self.curr_scope)
    }

    pub fn pop_scope(&mut self) {
        self.curr_depth -= 1;
        self.active_scopes.pop().expect("Fatal: Popped global scope");
    }

    pub fn get_scope_ctx(&self) -> ScopeCtx {
        ScopeCtx {
            scope: self.curr_scope,
            depth: self.curr_depth,
        }
    }

    pub fn get_type_id(&self, typ : Type) -> u64 {


        match typ {
            Type::Unresolved => panic!("Attempted to lookup unresolved type"),
            Type::Integer => SCACHE.const_int,
            Type::Float => SCACHE.const_float,
            Type::Boolean => SCACHE.const_bool,
            Type::Vector(_) => todo!(),
            Type::String => SCACHE.const_string,
            Type::Pair => todo!(),
            Type::Nil => SCACHE.const_nil,
            Type::Quote => todo!(),
            Type::Object(obj) => obj.name,
            Type::Lambda(_) => todo!()
        }
        
    }
}

