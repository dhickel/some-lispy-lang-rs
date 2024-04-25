use intmap::IntMap;
use lang::util::{IString, SCACHE};
use crate::ast::DefLambdaData;
use crate::token::Mod;
use crate::types::{Type, TypeTable};


#[derive(Debug)]
pub struct MetaSpace {
    pub namespaces: Vec<NameSpace>,
    ns_map: IntMap<u16>,
    pub types: TypeTable,
}


impl Default for MetaSpace {
    fn default() -> Self {
        let mut namespaces = Vec::<NameSpace>::with_capacity(10);
        let mut ns_map = IntMap::<u16>::with_capacity(10);
        let i_main = SCACHE.intern("main".to_string());
        let main_ns = NameSpace::new(i_main);

        ns_map.insert(i_main.value, 0);
        namespaces.push(main_ns);

        MetaSpace {
            namespaces,
            ns_map,
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

    pub fn get_ns_id(&mut self, name: IString) -> u16 {
        *self.ns_map.get(name.value).expect("Fatal: Namespace failed to resolve")
    }

    pub fn get_func(&mut self, env_ctx: &ExprCtx) -> &mut FuncMeta {
        let ns = self.get_ns_by_id(env_ctx.ns);
        if let Some(class_id) = env_ctx.class {
            let mut obj = &ns.obj_metadata[class_id as usize];
            if let ObjectMeta::Class(mut class) = obj {
                class.functions
                    .get_mut(env_ctx.func.expect("Expected class id") as usize)
                    .expect("Fatal: Failed to resolve function definition")
            } else { panic!("Fatal: Definition is not class") }
        } else {
            ns.functions.get_mut(env_ctx.func.expect("Expected class id") as usize)
                .expect("Fatal: Failed to resolve function definition")
        }
    }


    pub fn add_local_symbol(&mut self, ns_id: u16, scope_id: u32, name: IString, s_ctx: SymbolCtx) -> &SymbolCtx {
        let mut ns = &self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        if let Some(existing) = ns.local_symbols.get_mut(scope_id as u64) {
            let valid = existing.insert_checked(name.value, s_ctx);
            if !valid { panic!("Attempted to redefine existing symbol") }
            existing.get(name.value).unwrap()
        } else {
            let mut scope_map = IntMap::<SymbolCtx>::with_capacity(5);
            scope_map.insert(name.value, s_ctx);
            ns.local_symbols.insert(scope_id as u64, scope_map);
            ns.local_symbols.get(scope_id as u64).unwrap().get(name.value).unwrap()
        }
    }


    pub fn get_local_symbol(&self, ns_id: u16, scope_id: u32, name_val: u64) -> Option<&SymbolCtx> {
        let mut ns = &self.namespaces.get(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        ns.local_symbols.get(scope_id as u64)
            .expect("Fatal: Failed to resolve scope").get(name_val)
    }


    pub fn add_global_symbol(&mut self, ns_id: u16, name: IString, s_ctx: SymbolCtx) -> &SymbolCtx {
        let mut ns = &self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        ns.global_symbols.insert(name.value, s_ctx);
        ns.global_symbols.get(name.value).unwrap()
    }


    pub fn get_global_symbol(&self, ns_id: u16, name_val: u64) -> Option<&SymbolCtx> {
        self.namespaces[ns_id as usize].global_symbols.get(name_val)
    }


    pub fn add_var_def(&mut self, ns_id: u16, var_def: VarMeta) -> u16 {
        let mut ns = &self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let idx = ns.variables.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded namespace definition limit (65,535)")
        } else {
            ns.variables.push(var_def);
            idx as u16
        }
    }


    pub fn add_func_def(&mut self, ns_id: u16, func_def: FuncMeta) -> u16 {
        let mut ns = &self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let idx = ns.functions.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded namespace definition limit (65,535)")
        } else {
            ns.functions.push(func_def);
            idx as u16
        }
    }


    pub fn add_constant<T>(&mut self, value: &T, env_ctx: &ExprCtx) -> u16 {
        unsafe {
            let mut ns = &self.namespaces.get_mut(env_ctx.ns as usize)
                .expect("Fatal: Failed to resolve namespace");


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

            let curr_index = ns.constants.len();
            if curr_index == u16::MAX as usize {
                panic!("Exceeded size of constant pool")
            }

            let hash = u64::from_ne_bytes(padded_bytes);
            ns.existing_consts.insert(hash, curr_index as u16);
            ns.constants.push(padded_bytes);
            curr_index as u16
        }
    }


    pub fn add_object_meta(&mut self, ns_id: u16, obj_meta: ObjectMeta) -> u16 {
        let mut ns = &self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let idx = ns.obj_metadata.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded max object definitions (65,535")
        }
        ns.obj_metadata.push(obj_meta);
        idx as u16
    }


    pub fn push_code(&mut self, env_ctx: ExprCtx, code: &mut Vec<u8>) {
        let ns = self.namespaces.get_mut(env_ctx.ns as usize)
            .expect("Failed ro resolve namespace");

        if let Some(class_id) = env_ctx.class {
            let class = if let Some(ObjectMeta::Class(class)
            ) = ns.obj_metadata.get_mut(class_id as usize) {
                class
            } else { panic!("Fatal: Failed to resolve class") };

            if let Some(func_id) = env_ctx.func {
                let func = ns.functions.get_mut(func_id as usize)
                    .expect("Fatal: Failed to resolve function");
                func.code.append(code);
            } else { class.code.append(code) }
        } else { ns.code.append(code) }
    }
}


#[derive(Debug)]
pub struct NameSpace {
    pub name: IString,
    pub global_symbols: IntMap<SymbolCtx>,
    pub local_symbols: IntMap<IntMap<SymbolCtx>>,
    pub variables: Vec<VarMeta>,
    pub functions: Vec<FuncMeta>,
    pub obj_metadata: Vec<ObjectMeta>,
    pub constants: Vec<[u8; 8]>,
    pub existing_consts: IntMap<u16>,
    pub code: Vec<u8>,
}


impl NameSpace {
    pub fn new(name: IString) -> Self {
        NameSpace {
            name,
            global_symbols: IntMap::<SymbolCtx>::with_capacity(20),
            local_symbols: IntMap::<IntMap::<SymbolCtx>>::with_capacity(35),
            variables: Vec::<VarMeta>::with_capacity(20),
            functions: Vec::<FuncMeta>::with_capacity(20),
            obj_metadata: Vec::<ObjectMeta>::new(),
            constants: Vec::<[u8; 8]>::with_capacity(50),
            existing_consts: IntMap::<u16>::with_capacity(50),
            code: Vec::<u8>::with_capacity(100),
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
    pub name: IString,
}


#[derive(Debug)]
pub struct FuncMeta {
    pub name: IString,
    pub local_indices: u16,
    pub arity: u16,
    pub arg_types: Vec<u16>,
    pub rtn_type: u16,
    pub code: Vec<u8>,
}


impl FuncMeta {
    pub fn next_local(&mut self) -> u16 {
        let idx = self.local_indices - 1;
        self.local_indices += 1;
        return idx;
    }

    pub fn new(name: IString, arg_types: Vec<u16>, rtn_type: u16) -> Self {
        if arg_types.len() >= u16::MAX as usize {
            panic!("Function can not have more than 255 parameters")
        }
        FuncMeta {
            name,
            local_indices: 1,
            arity: arg_types.len() as u16,
            arg_types,
            rtn_type,
            code: Vec::<u8>::new(),
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
pub struct ClassMeta {
    pub name: IString,
    pub global_symbols: IntMap<SymbolCtx>,
    pub local_symbols: IntMap<IntMap<SymbolCtx>>,
    pub variables: Vec<VarMeta>,
    pub functions: Vec<FuncMeta>,
    pub obj_metadata: Vec<ObjectMeta>,
    pub code: Vec<u8>,
}


impl ClassMeta {
    pub fn new(name: IString) -> Self {
        ClassMeta {
            name,
            global_symbols: IntMap::<SymbolCtx>::with_capacity(20),
            local_symbols: IntMap::<IntMap::<SymbolCtx>>::with_capacity(35),
            variables: Vec::<VarMeta>::with_capacity(20),
            functions: Vec::<FuncMeta>::with_capacity(20),
            obj_metadata: Vec::<ObjectMeta>::new(),
            code: Vec::<u8>::with_capacity(50),
        }
    }
}


#[derive(Debug)]
pub struct RecordMeta {}


#[derive(Debug)]
pub struct InterfaceMeta {}


pub struct ResData {
    ctx: Context,
    typ: Type,
}

pub enum Context{
    Symbol(SymbolCtx),
    Expression(ExprCtx)
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolCtx {
    pub scope: u32,
    pub depth: u32,
    pub typ: u16,
    pub ns: u16,
    pub index: u16,
    pub nullable: bool,
    pub mutable: bool,
    pub public: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCtx {
    pub scope: u32,
    pub depth: u32,
    pub ns: u16,
    pub class: Option<u16>,
    pub func: Option<u16>,
}




impl SymbolCtx {
    pub fn new(scope: u32, depth: u32, typ: u16, namespace: u16, index: u16, modifiers: &[Mod],
    ) -> SymbolCtx {
        let nullable = modifiers.contains(&Mod::Nullable);
        let mutable = modifiers.contains(&Mod::Mutable);
        let public = modifiers.contains(&Mod::Public);

        SymbolCtx {
            scope,
            depth,
            typ,
            ns: namespace,
            index,
            nullable,
            mutable,
            public,
        }
    }
}



pub struct Environment<'a> {
    curr_scope: u32,
    curr_depth: u32,
    active_scopes: Vec<u32>,
    meta_space: &'a mut MetaSpace,
    pub curr_ns: u16,
    pub curr_func: Option<u16>,
    pub curr_class: Option<u16>,
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
            curr_class: None,
            curr_ns: 0,
        }
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


    pub fn pop_func(&mut self) {
        self.curr_func = None
    }


    // For symbol defs, depth 0 will always be top level of the current namespace, TODO depth check redundant?
    // If function and class == None, then symbol def is inside  a lexical scope, but still top level to ns
    // If not top level, check for existing functions first as it may be nested in a class.
    // If not currently resolve a function body, and not a top level ns def, symbol must be
    // a top level class definition
    pub fn add_var_symbol(&mut self, name: IString, typ: Type, mods: &[Mod]) -> Result<&SymbolCtx, String> {
        let type_id = self.meta_space.types.get_or_define_type(typ);
        let def = VarMeta { name };


        if self.curr_depth == 0 || (self.curr_func.is_none() && self.curr_class.is_none()) {
            let index = self.meta_space.add_var_def(self.curr_ns, def);
            let data = SymbolCtx::new(self.curr_scope, self.curr_depth, type_id, self.curr_ns, index, mods);
            let data = self.meta_space.add_global_symbol(self.curr_ns, name, data);
            Ok(data)
        } else if let Some(func_idx) = &mut self.curr_func {
            let func = self.meta_space.get_func(&self.get_env_ctx());
            let index = func.next_local();
            let data = SymbolCtx::new(self.curr_scope, self.curr_depth, type_id, self.curr_ns, index, mods);
            let data = self.meta_space.add_local_symbol(self.curr_ns, self.curr_scope, name, data);
            Ok(data)
        } else if let Some(class) = &self.curr_class {
            todo!("Classes no implemented")
        } else {
            panic!("Fatal: Condition for resolution not met")
        }
    }

    // Implicitly pushes a new curr_func id which need to always be popped with pop_func after
    // evaluating the body of the function
    pub fn add_func_symbol(&mut self, name: IString, lambda: DefLambdaData) -> Result<&SymbolCtx, String> {
        let arg_type_ids = if let Some(params) = lambda.parameters {
            params.iter()
                .map(|p| self.meta_space.types.get_or_define_type(p.c_type.clone()))
                .collect()
        } else { vec![] };

        let rtn_type_id = self.meta_space.types.get_or_define_type(lambda.typ);
        let def = FuncMeta::new(name, arg_type_ids, rtn_type_id);

        if self.curr_depth == 0 || (self.curr_func.is_none() & &self.curr_class.is_none()) {
            let index = self.meta_space.add_func_def(self.curr_ns, def);
            self.curr_func = Some(index); // Set curr func index for body resolutions

            let data = SymbolCtx::new(
                self.curr_scope, self.curr_depth,
                rtn_type_id, // FIXME, lambda defs need to have a "full" type_id of args + return for resolution checks
                self.curr_ns,
                index,
                &lambda.modifiers.unwrap_or_else(|| vec![]),
            );

            let data = self.meta_space.add_global_symbol(self.curr_ns, name, data);
            Ok(data)
        } else if let Some(func_id) = self.curr_func {
            let mut func = self.meta_space.get_func(&self.get_env_ctx());
            let index = func.next_local();
            self.curr_func = Some(index); // Set curr func index for body resolutions

            let data = SymbolCtx::new(
                self.curr_scope,
                self.curr_depth,
                rtn_type_id,
                self.curr_ns,
                index,
                &lambda.modifiers.unwrap_or_else(|| vec![]),
            );

            let data = self.meta_space.add_local_symbol(self.curr_ns, self.curr_scope, name, data);
            Ok(data)
        } else if let Some(class) = &self.curr_class {
            todo!("Classes no implemented")
        } else {
            panic!("Fatal: Condition for resolution not met")
        }
    }


    pub fn get_symbol_type(&mut self, name: IString) -> &Type {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(symbol) = self.meta_space.get_local_symbol(self.curr_ns, scope_id, name.value) {
                return self.meta_space.types.get_type_by_id(symbol.typ);
            }
        }

        if let Some(symbol) = self.meta_space.get_global_symbol(self.curr_ns, name.value) {
            return self.meta_space.types.get_type_by_id(symbol.typ);
        }
        &self.unresolved
    }


    pub fn get_symbol_ctx(&mut self, name: IString) -> Option<&SymbolCtx> {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(symbol) = self.meta_space.get_local_symbol(self.curr_ns, scope_id, name.value) {
                return Some(symbol);
            }
        }

        if let Some(symbol) = self.meta_space.get_global_symbol(self.curr_ns, name.value) {
            return Some(symbol);
        }
        None
    }

    // Used to validate user type annotations with resolved types
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

    pub fn get_env_ctx(&self) -> ExprCtx {
        ExprCtx {
            scope: self.curr_scope,
            depth: self.curr_depth,
            ns: self.curr_ns,
            class: self.curr_class,
            func: self.curr_func,
        }
    }
}

