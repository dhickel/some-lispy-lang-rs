use intmap::IntMap;
use lasso::{Key, Spur};
use lang::types::{ObjType, Type};
use crate::util::SCACHE;


pub struct Binding {
    pub name: String,
    pub value: LiteralValue,
    pub mutable: bool,
    pub private: bool,
}


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


pub struct SymbolCtx {
    pub scope: u32,
    pub depth: u32,
    pub typ: Type,
}


pub struct Context {
    curr_scope: u32,
    curr_depth: u32,
    symbols: IntMap<Vec<SymbolCtx>>,
    globals: IntMap<Vec<SymbolCtx>>
    types: IntMap<Type>,
}


impl Default for Context {
    fn default() -> Self {
        let mut types = IntMap::<Type>::with_capacity(50);
        types.insert(SCACHE.const_int.into_usize() as u64, Type::Integer);
        types.insert(SCACHE.const_float.into_usize() as u64, Type::Float);
        types.insert(SCACHE.const_bool.into_usize() as u64, Type::Boolean);
        types.insert(SCACHE.const_string.into_usize() as u64, Type::String);
        types.insert(SCACHE.const_nil.into_usize() as u64, Type::Nil);

        Context {
            curr_scope: 0,
            curr_depth: 0,
            symbols: IntMap::<Vec<SymbolCtx>>::with_capacity(50),
            globals: IntMap::<Vec<SymbolCtx>>::with_capacity(50),
            types,
        }
        
    }
}


impl Context {
    pub fn add_symbol(&mut self, symbol: Spur, typ: Type) -> Result<(), String> {
        let s_int = symbol.into_usize() as u64;
        let data = SymbolCtx { scope: self.curr_scope, depth: self.curr_depth, typ };

        match self.symbols.get_mut(s_int) {
            None => {
                let mut symbol_vec = Vec::with_capacity(4);
                symbol_vec.push(data);
                self.symbols.insert(s_int, symbol_vec);
                Ok(())
            }
            Some(mut existing) => {
                let collision = existing.iter()
                    .find(|ctx| ctx.scope == self.curr_scope && ctx.depth == self.curr_depth);

                match collision {
                    None => Ok(existing.push(data)),
                    Some(_) => Err(format!("Existing binding found for: {}", SCACHE.resolve(&symbol)))
                }
            }
        }
    }

    pub fn get_symbol_type(&self, symbol: Spur) -> Type {
        let found = self.symbols.get(symbol.into_usize() as u64);

        if let Some(entries) = found {
            let mut filtered: Vec<&SymbolCtx> = entries.iter()
                .filter(|ctx| ctx.scope == self.curr_scope && ctx.depth <= self.curr_depth)
                .collect();

            filtered.sort_by_key(|ctx| std::cmp::Reverse(ctx.depth));
            
            if !filtered.is_empty() {
                filtered[0].typ.clone()
            } else { Type::Unresolved }
        } else { Type::Unresolved }
    }

    pub fn validate_type(&self, spur: Spur) -> Type {
        let found = self.types.get(spur.into_usize() as u64);
        if found.is_none() { return Type::Unresolved; }

        match found.unwrap() {
            Type::Unresolved | Type::Integer | Type::Float | Type::String | Type::Boolean |
            Type::Vector(_) | Type::Nil | Type::Pair => found.unwrap().clone(),
            Type::Quote => todo!(),
            Type::Object(obj) => {
                for typ in &obj.super_types {
                    if let Type::Object(obj_type) = typ {
                        if obj_type.name == spur { return typ.clone(); }
                    }
                } // Object will always have a spur value
                Type::Unresolved
            }
            Type::Lambda(_) => todo!(),
        }
    }

    pub fn add_type(&mut self, spur: Spur, typ: Type) {
        self.types.insert_checked(spur.into_usize() as u64, typ);
    }

    pub fn pushScope(&mut self) {
        self.curr_depth += 1
    }


    pub fn popScope(&mut self) {
        self.curr_depth += 1;
        if self.curr_depth == 0 {
            self.curr_scope += 1;
        }
    }

    pub fn get_scope_tup(&self) -> (u32, u32) {
        (self.curr_scope, self.curr_depth)
    }
}

