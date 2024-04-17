use ahash::{AHashMap, HashMap};
use intmap::IntMap;
use lang::types::{ObjType, Type};
use lang::types::Type::Unresolved;
use crate::util::{SCACHE, SCache};


#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub name: String,
    pub value: LiteralValue,
    pub mutable: bool,
    pub private: bool,
}


#[derive(Debug)]
pub struct TypeTable {
    type_defs: AHashMap<Type, u16>,
    type_ids: Vec<Type>,
    type_names: AHashMap<u64, u16>,
    pub nil: u16,
    pub bool: u16,
    pub int: u16,
    pub float: u16,
    pub string: u16,
    pub pair: u16,

}


impl TypeTable {
    pub fn new() -> Self {
        let mut type_defs = AHashMap::<Type, u16>::with_capacity(50);
        let mut type_ids = Vec::<Type>::with_capacity(50);
        let mut type_names = AHashMap::<u64, u16>::with_capacity(50);


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

    pub fn get_or_define_type(&mut self, typ: Type) -> u16 {
        if typ == Unresolved {
            panic!("Passed unresolved type to define_type");
        }
        if self.type_defs.contains_key(&typ) {
            return *self.type_defs.get(&typ).unwrap();
        }

        let id = self.type_ids.len();

        match &typ {
            Type::Vector(data) => todo!(),
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

    pub fn get_type_by_name(&self, itern_string: u64) -> Option<&Type> {
        return if let Some(id) = self.type_names.get(&itern_string) {
            Some(self.get_type_by_id(*id))
        } else { None };
    }
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
    pub typ: u16,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ScopeCtx {
    pub scope: u32,
    pub depth: u32,
}


#[derive(Debug)]
pub struct Context {
    pub symbols: AHashMap<u32, IntMap<SymbolCtx>>,
    pub globals: IntMap<SymbolCtx>,
    pub types: TypeTable,
}


impl Default for Context {
    fn default() -> Self {
        let globals = IntMap::<SymbolCtx>::with_capacity(50);
        let symbols = AHashMap::<u32, IntMap<SymbolCtx>>::with_capacity(50);
        Context {
            symbols,
            globals,
            types: TypeTable::new(),
        }
    }
}


pub struct Environment<'a> {
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    ctx: &'a mut Context,
    unresolved: Type,
}


impl<'a> Environment<'a> {
    pub fn new(ctx: &'a mut Context) -> Self {
        Environment {
            curr_scope: 0,
            curr_depth: 0,
            active_scopes: Vec::<u32>::with_capacity(10),
            ctx,
            unresolved: Unresolved,
        }
    }

    pub fn add_symbol(&mut self, symbol: u64, typ: Type) -> Result<SymbolCtx, String> {
        let type_id = self.ctx.types.get_or_define_type(typ);
        let s_int = symbol;
        let data = SymbolCtx { scope: self.curr_scope, depth: self.curr_depth, typ: type_id };

        if self.curr_depth == 0 {
            self.ctx.globals.insert(s_int, data.clone());
            return Ok(data.clone());
        }

        if let Some(existing) = self.ctx.symbols.get_mut(&self.curr_scope) {
            return match existing.insert_checked(s_int, data.clone()) {
                true => Ok(data.clone()),
                false => Err(format!("Redefinition of existing binding: {}", SCACHE.resolve(symbol)))
            };
        }

        let mut scope_table = IntMap::<SymbolCtx>::new();
        scope_table.insert(s_int, data.clone());
        self.ctx.symbols.insert(self.curr_scope, scope_table);
        Ok(data.clone())
    }

    pub fn get_symbol_type(&self, symbol: u64) -> &Type {
        let s_int = symbol;

        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(scope_symbols) = self.ctx.symbols.get(&scope_id) {
                if let Some(symbol_ctx) = scope_symbols.get(s_int) {
                    return self.ctx.types.get_type_by_id(symbol_ctx.typ);
                }
            }
        }

        if let Some(global_symbol) = self.ctx.globals.get(s_int) {
            return self.ctx.types.get_type_by_id(global_symbol.typ);
        }

        &self.unresolved
    }


    pub fn get_symbol_ctx(&self, symbol: u64) -> Option<SymbolCtx> {
        let s_int = symbol;

        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(scope_symbols) = self.ctx.symbols.get(&scope_id) {
                if let Some(symbol_ctx) = scope_symbols.get(s_int) {
                    return Some(symbol_ctx.clone());
                }
            }
        }

        if let Some(global_symbol) = self.ctx.globals.get(s_int) {
            return Some(global_symbol.clone());
        }

        None
    }

    pub fn validate_type(&self, intern_str: u64) -> &Type {
        let found = self.ctx.types.get_type_by_name(intern_str);
        if found.is_none() { return &self.unresolved; }

        match found.unwrap() {
            Type::Unresolved | Type::Integer | Type::Float | Type::String | Type::Boolean |
            Type::Vector(_) | Type::Nil | Type::Pair => found.unwrap(),
            Type::Quote => todo!(),
            Type::Object(obj) => {
                for typ in &obj.super_types {
                    if let Type::Object(obj_type) = typ {
                        if obj_type.name == intern_str { return typ; }
                    }
                } // Object will always have a u64 value
                &self.unresolved
            }
            Type::Lambda(_) => todo!(),
        }
    }

    pub fn define_type(&mut self, typ: Type) -> u16 {
        self.ctx.types.get_or_define_type(typ)
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

    pub fn get_type_id(&self, typ: Type) -> u64 {
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

