use ahash::AHashMap;
use intmap::IntMap;
use lasso::{Key, Spur};
use lang::types::Type;
use crate::util::SCACHE;


pub struct SymbolCtx {
    pub scope: i32,
    pub depth: i32,
    pub typ: Type,
}


pub struct Context {
    curr_scope: i32,
    curr_depth: i32,
    pub symbols: IntMap<Vec<SymbolCtx>>,
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

    pub fn get_symbol_type(&self, symbol: Spur) -> Option<&Type> {
        let found = self.symbols.get(symbol.into_usize() as u64);

        if let Some(entries) = found {
            let mut filtered: Vec<&SymbolCtx> = entries.iter()
                .filter(|ctx| ctx.scope == self.curr_scope)
                .collect();

            filtered.sort_by_key(|ctx| std::cmp::Reverse(ctx.depth));
            if !filtered.is_empty() {
                return Some(&filtered[0].typ);
            } else { None }
        } else { None }
    }
}