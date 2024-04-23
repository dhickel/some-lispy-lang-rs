use ahash::{AHashMap};
use intmap::IntMap;
use crate::types::{Type, TypeTable};
use crate::util::{IString, SCACHE};


#[derive(Debug)]
pub struct MetaSpace {
    pub namespaces: Vec<NameSpace>,
    ns_map: IntMap<u16>,
    pub symbols: AHashMap<u32, IntMap<SymbolCtx>>,
    pub types: TypeTable,
}


impl Default for MetaSpace {
    fn default() -> Self {
        let mut namespaces = Vec::<NameSpace>::with_capacity(10);
        let mut ns_map = IntMap::<u16>::with_capacity(10);
        let symbols = AHashMap::<u32, IntMap<SymbolCtx>>::with_capacity(50);
        let i_main = SCACHE.intern("main".to_string());
        let main_ns = NameSpace::new(i_main);
        ns_map.insert(i_main.value, 0);
        namespaces.push(main_ns);

        MetaSpace {
            namespaces,
            ns_map,
            symbols,
            types: TypeTable::default(),
        }
    }
}


impl MetaSpace {
    pub fn get_ns(&mut self, ns_name: IString) -> &NameSpace {
        if let Some(ns) = self.ns_map.get(ns_name.value) {
            self.namespaces.get(*ns as usize).expect("Fatal: Namespace failed to resolve")
        } else {
            let id = self.namespaces.len();
            if id > u16::MAX as usize { panic!("Exceeded 65,535 namespace definitions") }
            let ns = NameSpace::new(ns_name);
            self.ns_map.insert(ns_name.value, id as u16);
            self.namespaces.push(ns);
            &self.namespaces[id]
        }
    }

    pub fn get_ns_by_id(&mut self, id: u16) -> &mut NameSpace {
        self.namespaces.get_mut(id as usize).expect("Fatal: Namespace failed to resolve")
    }

    pub fn add_local_symbol(&mut self, scope_id: u32, name: IString, s_ctx: SymbolCtx) {
        if let Some(existing) = self.symbols.get_mut(&scope_id) {
            let valid = existing.insert_checked(name.value, s_ctx);
            if !valid { panic!("Attempted to redefine existing symbol") }
        } else {
            let mut scope_map = IntMap::<SymbolCtx>::with_capacity(5);
            scope_map.insert(name.value, s_ctx);
            self.symbols.insert(scope_id, scope_map);
        }
    }

    pub fn get_local_symbol(&self, scope_id: u32, name: IString) -> Option<&SymbolCtx> {
        self.symbols.get(&scope_id).expect("Fatal: Failed to resolve scope").get(name.value)
    }

    pub fn get_local_symbol_by_id(&self, scope_id: u32, id: u64) -> Option<&SymbolCtx> {
        self.symbols.get(&scope_id).expect("Fatal: Failed to resolve scope").get(id)
    }

    pub fn get_ns_global_symbol(&self, ns_id: u16, symbol_name: IString) -> Option<&SymbolCtx> {
        self.namespaces[ns_id as usize].global_symbols.get(symbol_name.value)
    }
}


#[derive(Debug)]
pub struct NameSpace {
    pub name: IString,
    pub global_symbols: IntMap<SymbolCtx>,
    pub definitions: Vec<Definition>,
    pub obj_metadata: Vec<ObjectMeta>,
    pub constants: Vec<[u8; 8]>,
}


impl NameSpace {
    pub fn new(name: IString) -> Self {
        NameSpace {
            name,
            global_symbols: IntMap::<SymbolCtx>::with_capacity(20),
            definitions: Vec::<Definition>::with_capacity(20),
            obj_metadata: Vec::<ObjectMeta>::new(),
            constants: Vec::<[u8; 8]>::with_capacity(50),
        }
    }

    pub fn add_definition(&mut self, def: Definition) -> u16 {
        let idx = self.definitions.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded namespace definition limit (65,535)")
        } else {
            self.definitions.push(def);
            idx as u16
        }
    }

    pub fn add_global_symbol(&mut self, name: IString, s_ctx: SymbolCtx) {
        self.global_symbols.insert(name.value, s_ctx);
    }

    pub fn get_global_symbol(&self, name: IString) -> Option<&SymbolCtx> {
        self.global_symbols.get(name.value)
    }

    pub fn get_global_symbol_by_id(&self, id: u64) -> Option<&SymbolCtx> {
        self.global_symbols.get(id)
    }

    pub fn add_object_meta(&mut self, obj_meta: ObjectMeta) -> u16 {
        let idx = self.obj_metadata.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded max object definitions (65,535")
        }
        self.obj_metadata.push(obj_meta);
        idx as u16
    }

    fn add_constant(&mut self, bytes: [u8; 8]) -> u16 {
        unsafe {
            let curr_index = self.constants.len();
            if curr_index == u16::MAX as usize {
                panic!("Exceeded size of constant pool")
            }
            self.constants.push(bytes);
            curr_index as u16
        }
    }

    pub fn push_constant<T>(&mut self, value: &T) -> u16 {
        unsafe {
            let size = std::mem::size_of::<T>();
            if size > 8 {
                panic!("Attempted to add constant of more than 8 bytes");
            }

            let value_ptr = value as *const T as *const u8;
            let bytes_slice = std::slice::from_raw_parts(value_ptr, size);

            let mut padded_bytes = [0u8; 8];
            if size < 8 {
                padded_bytes[8 - size..].copy_from_slice(bytes_slice);
            } else {
                padded_bytes.copy_from_slice(bytes_slice);
            }

            self.add_constant(padded_bytes)
        }
    }
}


#[derive(Debug)]
pub enum Definition {
    Variable(VarMeta),
    Function(FuncMeta),
}


#[derive(Debug)]
pub struct VarMeta {
    pub name: u64,

}


#[derive(Debug)]
pub struct FuncMeta {
    pub name: u64,
    pub local_count: u16,
    pub arg_types: Vec<u16>,
    pub rtn_type: u16,
}


impl FuncMeta {
    pub fn next_local(&mut self) -> u16 {
        let idx = self.local_count;
        self.local_count += 1;
        return idx;
    }

    pub fn new(name: u64, arg_types: Vec<u16>, rtn_type: u16) -> Self {
        FuncMeta {
            name,
            local_count: 0,
            arg_types,
            rtn_type,
        }
    }
}


#[derive(Debug)]
pub enum ObjectMeta {
    Class(ClassMeta),
    Record(RecordMeta),
    Interface(InterfaceMeta),
}


#[derive(Debug)]
pub struct ClassMeta {}


#[derive(Debug)]
pub struct RecordMeta {}


#[derive(Debug)]
pub struct InterfaceMeta {}


#[derive(Debug, Clone, PartialEq)]
pub struct SymbolCtx {
    pub scope: u32,
    pub depth: u32,
    pub typ: u16,
    pub namespace: Option<u16>,
    pub index: u16,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ScopeCtx {
    pub namespace: Option<u64>,
    pub index: u16,
    pub is_field: bool,
    pub scope: u32,
    pub depth: u32,

}


pub struct Environment<'a> {
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    meta_space: &'a mut MetaSpace,
    pub curr_ns: u16,
    pub curr_func: Option<FuncMeta>,
    pub curr_obj: Option<ObjectMeta>,
    unresolved: Type,
}


impl<'a> Environment<'a> {
    pub fn new(meta_space: &'a mut MetaSpace) -> Self {
        Environment {
            curr_scope: 0,
            curr_depth: 0,
            active_scopes: Vec::<u32>::with_capacity(10),
            meta_space,
            unresolved: Type::Unresolved,
            curr_func: None,
            curr_obj: None,
            curr_ns: 0,
        }
    }

    pub fn push_func(&mut self, name: u64, arg_types: Vec<u16>, rtn_type: u16) {
        self.curr_func = Some(FuncMeta::new(name, arg_types, rtn_type))
    }

    pub fn add_symbol(&mut self, symbol: IString, typ: Type) -> Result<SymbolCtx, String> {
        let type_id = self.meta_space.types.get_or_define_type(typ);
        let data = SymbolCtx { scope: self.curr_scope, depth: self.curr_depth, typ: type_id };

        if self.curr_depth == 0 {
            self.meta_space.get_ns_by_id(self.curr_ns).add_global_symbol(symbol, data.clone());
        } else {
            self.meta_space.add_local_symbol(self.curr_scope, symbol, data.clone());
        }
        Ok(data)
    }

    pub fn get_symbol_type(&mut self, name: IString) -> &Type {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(symbol) = self.meta_space.get_local_symbol(scope_id, name) {
                return self.meta_space.types.get_type_by_id(symbol.typ);
            }
        }

        if let Some(symbol) = self.meta_space.get_ns_global_symbol(self.curr_ns, name) {
            return self.meta_space.types.get_type_by_id(symbol.typ);
        }
        &self.unresolved
    }


    pub fn get_symbol_ctx(&mut self, name: IString) -> Option<&SymbolCtx> {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(symbol) = self.meta_space.get_local_symbol(scope_id, name) {
                return Some(symbol);
            }
        }

        if let Some(symbol) = self.meta_space.get_ns_global_symbol(self.curr_ns, name) {
            return Some(symbol);
        }
        None
    }

    pub fn validate_type(&self, intern_str: IString) -> &Type {
        let found = self.meta_space.types.get_type_by_name(intern_str);
        if found.is_none() { return &self.unresolved; }

        match found.unwrap() {
            Type::Unresolved | Type::Integer | Type::Float | Type::String | Type::Boolean |
            Type::Array(_) | Type::Nil | Type::Pair => found.unwrap(),
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
        self.meta_space.types.get_or_define_type(typ)
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

    pub fn get_scope_ctx(&self) -> (u32, u32) {
        (self.curr_scope, self.curr_depth)
    }

    pub fn get_type_id(&self, typ: Type) -> IString {
        match typ {
            Type::Unresolved => panic!("Attempted to lookup unresolved type"),
            Type::Integer => SCACHE.const_int,
            Type::Float => SCACHE.const_float,
            Type::Boolean => SCACHE.const_bool,
            Type::Array(_) => todo!(),
            Type::String => SCACHE.const_string,
            Type::Pair => todo!(),
            Type::Nil => SCACHE.const_nil,
            Type::Quote => todo!(),
            Type::Object(obj) => obj.name,
            Type::Lambda(_) => todo!()
        }
    }
}

