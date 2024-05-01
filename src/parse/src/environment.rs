use intmap::IntMap;
use lang::util::{IString, SCACHE};
use crate::ast::DefLambdaData;
use crate::op_codes::OpCode;
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

    pub fn get_func(&mut self, env_ctx: &SymbolCtx) -> &mut FuncMeta {
        let ns = self.get_ns_by_id(env_ctx.ns);

        if let Some(class_id) = env_ctx.class {
            if let ObjectMeta::Class(ref mut class) = ns.obj_metadata[class_id as usize] {
                return class.functions
                    .get_mut(env_ctx.func.expect("Expected function index") as usize)
                    .expect("Fatal: Failed to resolve function definition");
            } else {
                panic!("Fatal: Definition is not class");
            }
        } else {
            return ns.functions
                .get_mut(env_ctx.func.expect("Expected function index") as usize)
                .expect("Fatal: Failed to resolve function definition");
        }
    }


    fn add_symbol(&mut self, ns_id: u16, scope_id: u32, name: IString, data: ResData) -> &ResData {
        let mut ns = self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        if ns.symbol_table.contains_key(scope_id as u64) {
            ns.symbol_table.get_mut(scope_id as u64).unwrap().insert_checked(name.value, data);
            ns.symbol_table.get(scope_id as u64).unwrap().get(name.value).unwrap()
        } else {
            let mut scope_map = IntMap::<ResData>::with_capacity(5);
            scope_map.insert(name.value, data);
            ns.symbol_table.insert(scope_id as u64, scope_map);
            ns.symbol_table.get(scope_id as u64).unwrap().get(name.value).unwrap()
        }
    }


    pub fn get_symbol(&self, ns_id: u16, scope_id: u32, name_val: u64) -> Option<&ResData> {
        let mut ns = self.namespaces.get(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        if let Some(locals) = ns.symbol_table.get(scope_id as u64) {
            locals.get(name_val)
        } else { None }
    }


    fn add_var_def(&mut self, ns_id: u16, var_def: VarMeta) -> u16 {
        let mut ns = self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let idx = ns.variables.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded namespace definition limit (65,535)")
        } else {
            ns.variables.push(var_def);
            idx as u16
        }
    }

    fn add_func_def(&mut self, ns_id: u16, func_def: FuncMeta) -> u16 {
        let mut ns = self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let index = ns.functions.len();
        if index > u16::MAX as usize {
            panic!("Fatal: Invalid function index in environment construction")
        }

        ns.functions.push(func_def);
        index as u16
    }

    pub fn add_constant<T>(&mut self, value: &T, env_ctx: &ExprContext) -> u16 {
        unsafe {
            let mut ns = self.namespaces.get_mut(env_ctx.ns as usize)
                .expect("Fatal: Failed to resolve namespace");


            let size = std::mem::size_of::<T>();
            if size != 8 {
                panic!("Attempted to add constant of more than 8 bytes");
            }

            let value_ptr = value as *const T as *const u8;
            let bytes = std::ptr::read(value_ptr as *const [u8; 8]);

            let hash = u64::from_ne_bytes(bytes);
            if let Some(&index) = ns.existing_consts.get(hash) {
                return index;
            }

            let curr_index = ns.constants.len();
            if curr_index >= u16::MAX as usize {
                panic!("Exceeded size of constant pool");
            }

            ns.existing_consts.insert(hash, (curr_index / 8) as u16); // index by word
            ns.constants.extend_from_slice(&bytes);
            (curr_index / 8) as u16 // index by word
        }
    }

    pub fn add_object_meta(&mut self, ns_id: u16, obj_meta: ObjectMeta) -> u16 {
        let mut ns = self.namespaces.get_mut(ns_id as usize)
            .expect("Fatal: Failed to resolve namespace");

        let idx = ns.obj_metadata.len();
        if idx == u16::MAX as usize {
            panic!("Exceeded max object definitions (65,535")
        }
        ns.obj_metadata.push(obj_meta);
        idx as u16
    }

    pub fn push_func_code(&mut self, env_ctx: &SymbolCtx, code: &mut Vec<u8>) {
        let ns = self.namespaces.get_mut(env_ctx.ns as usize)
            .expect("Failed ro resolve namespace");

        if let Some(class_id) = env_ctx.class {
            let class = if let Some(ObjectMeta::Class(class)
            ) = ns.obj_metadata.get_mut(class_id as usize) {
                class
            } else { panic!("Fatal: Failed to resolve class") };
        } else {
            ns.functions.get_mut(env_ctx.index as usize)
                .expect("Fatal: Failed to resolve func for code push")
                .code.append(code);
        }
    }
}

// TODO, REPL and dynamicism is WIP, namespace should store index/count for var/func/constants
//  then data can get added to the RunTimeSpace, and the actual namespace data reset, then
//  when adding new data via the repl dynamically the indexs persists for code gen to match
//  with the run time structure updates. This way data is copied but only in one structure at a time.
//  Pointers are used for efficient copy operations, which is fine as long as code is single thread
//  since the dynamic structures can re allocate, but this wont happen mid copy in a ST environment
// The more I think about it only the const vector needs to be drained, and the bytecode for the
// functions zeroed after being copied, this way metadata can persist at runtime.

#[derive(Debug)]
pub struct NameSpace {
    pub name: IString,
    pub symbol_table: IntMap<IntMap<ResData>>,
    pub variables: Vec<VarMeta>,
    pub functions: Vec<FuncMeta>,
    pub obj_metadata: Vec<ObjectMeta>,
    pub constants: Vec<u8>,
    pub existing_consts: IntMap<u16>,
    pub code: Vec<u8>,
}


#[derive(Debug)]
pub struct StackFrame {
    pub local_count: u16,
    pub code_ptr: *const u8,
    pub code_size: u16,
    pub constant_ptr: *const u8,
}


#[derive(Debug)]
pub struct PermNameSpace {
    pub func_data: Vec<u8>,
    pub func_table: Vec<(u32, u16, u16)>,
    // start, size, locals count
    pub var_data: Vec<u8>,
    pub constants: Vec<u8>,
}


#[derive(Debug)]
pub struct PermaSpace {
    pub namespaces: Vec<PermNameSpace>,
    pub init_code: Vec<u8>,
}


impl PermaSpace {
    pub fn new(meta_space: &mut MetaSpace) -> PermaSpace {
        let mut init_code = Vec::<u8>::with_capacity(meta_space.namespaces.len() * 30);
        let mut namespaces = Vec::<PermNameSpace>::with_capacity(meta_space.namespaces.len());
        for ns in meta_space.namespaces.iter_mut() {
            namespaces.push(PermNameSpace::new(ns));
            init_code.append(&mut ns.code);
        }
        init_code.push(OpCode::Exit as u8);
        PermaSpace { namespaces, init_code }
    }
}


impl PermNameSpace {
    pub fn get_func(&self, index: u16) -> StackFrame {
        unsafe {
            let meta = *self.func_table.get_unchecked(index as usize);
            StackFrame {
                local_count: meta.2,
                code_ptr: self.func_data.as_ptr().add(meta.0 as usize),
                code_size: meta.1,
                constant_ptr: self.constants.as_ptr(),
            }
        }
    }

    pub unsafe fn set_var_data(&mut self, index: u16, copy_from: *const u8) {
        unsafe {
            let copy_to = self.var_data.as_mut_ptr().add(index as usize * 8);
            std::ptr::copy_nonoverlapping(copy_from, copy_to, 8)
        }
    }

    pub fn get_var_data(&mut self, index: u16) -> *const u8 {
        unsafe {
            self.var_data.as_ptr().add(index as usize * 8)
        }
    }

    pub fn get_constants(&self, index: u16) -> *const u8 {
        unsafe {
            self.constants.as_ptr().add(index as usize * 8) // expand to word length
        }
    }

    pub fn new(ns: &mut NameSpace) -> PermNameSpace {
        let mut func_data = Vec::<u8>::with_capacity(ns.functions.len() * 32);
        let mut func_table = Vec::<(u32, u16, u16)>::with_capacity(ns.functions.len());
        for func in ns.functions.iter_mut() {
            func_table.push((func_data.len() as u32, func.code.len() as u16, func.local_indices));
            func_data.append(&mut func.code);
        }

        let var_data = vec![0u8; ns.variables.len() * 8];
        let mut constants = Vec::<u8>::with_capacity(ns.constants.len());
        constants.append(&mut ns.constants);

        PermNameSpace {
            func_data,
            func_table,
            var_data,
            constants,
        }
    }
}


impl NameSpace {
    pub fn new(name: IString) -> Self {
        NameSpace {
            name,
            symbol_table: IntMap::<IntMap::<ResData>>::with_capacity(35),
            variables: Vec::<VarMeta>::with_capacity(20),
            functions: Vec::<FuncMeta>::with_capacity(20),
            obj_metadata: Vec::<ObjectMeta>::new(),
            constants: Vec::<u8>::with_capacity(50 * 8),
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
    pub param_types: Vec<u16>,
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
            param_types: arg_types,
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
    pub global_symbols: IntMap<ResData>,
    pub local_symbols: IntMap<IntMap<ResData>>,
    pub variables: Vec<VarMeta>,
    pub functions: Vec<FuncMeta>,
    pub obj_metadata: Vec<ObjectMeta>,
    pub code: Vec<u8>,
}


impl ClassMeta {
    pub fn new(name: IString) -> Self {
        ClassMeta {
            name,
            global_symbols: IntMap::<ResData>::with_capacity(20),
            local_symbols: IntMap::<IntMap::<ResData>>::with_capacity(35),
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


#[derive(Debug, Clone, PartialEq)]
pub struct ResData {
    pub self_ctx: Context,
    pub target_ctx: Option<Context>,
    pub type_data: TypeData,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Context {
    Symbol(SymbolCtx),
    Expr(ExprContext),
}


#[derive(Debug, Clone, PartialEq)]
pub struct SymbolCtx {
    pub scope: u32,
    pub depth: u32,
    pub ns: u16,
    pub class: Option<u16>,
    pub func: Option<u16>,
    pub index: u16,
    pub nullable: bool,
    pub mutable: bool,
    pub public: bool,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprContext {
    pub scope: u32,
    pub depth: u32,
    pub ns: u16,
    pub class: Option<u16>,
    pub func: Option<u16>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct TypeData {
    pub type_id: u16,
    pub typ: Type,
}


impl SymbolCtx {
    pub fn new(ctx: ExprContext, index: u16, modifiers: Option<Vec<Mod>>) -> SymbolCtx {
        let mut nullable = false;
        let mut mutable = false;
        let mut public = false;
        if let Some(mods) = modifiers {
            nullable = mods.contains(&Mod::Nullable);
            mutable = mods.contains(&Mod::Mutable);
            public = mods.contains(&Mod::Public);
        }

        SymbolCtx {
            scope: ctx.scope,
            depth: ctx.depth,
            ns: ctx.ns,
            class: ctx.class,
            func: ctx.func,
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
    pub meta_space: &'a mut MetaSpace,
    pub curr_ns: u16,
    pub curr_func: Vec<u16>,
    pub curr_class: Vec<u16>,
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
            curr_func: Vec::<u16>::with_capacity(4),
            curr_class: Vec::<u16>::with_capacity(2),
            curr_ns: 0,
        }
    }

    pub fn in_class_scope(&self) -> bool {
        !self.curr_class.is_empty()
    }

    pub fn in_func_scope(&self) -> bool {
        !self.curr_func.is_empty()
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
        self.curr_func.pop();
        self.pop_scope();
    }

    pub fn add_func_symbol(&mut self, name: IString, lambda: &DefLambdaData, rtn_type: Type) -> Result<&ResData, String> {
        self.push_scope();

        let param_types = if let Some(params) = &lambda.parameters {
            let mut param_types = Vec::<Type>::with_capacity(params.len());
            for param in params {
                let typ = if let Some(typ) = self.meta_space.types.get_type_by_name(param.d_type) {
                    typ
                } else {
                    self.pop_scope();
                    panic!("Non defined type encountered in parameter")
                }; // FIXME multiple type resolution, maybe make a method that returns (id,type)
                param_types.push(typ.clone());
            }
            for (param, typ) in params.into_iter().zip(param_types.iter()) {
                let _ = self.add_var_symbol(param.name, typ.clone(), param.modifiers.clone());
            }
            param_types.iter().map(|t| self.meta_space.types.get_type_id(t)).collect()
        } else { vec![] };


        let rtn_type_id = self.meta_space.types.get_type_id(&rtn_type);
        let def = FuncMeta::new(name, param_types, rtn_type_id);

        if self.in_class_scope() {
            // add method to classes methods and get index into it
            todo!("Classes not implemented")
        }

        // If not inside a class, function will be added to the namespaces methods
        // adding to Local or global symbols for scoping constraints

        let index = self.meta_space.add_func_def(self.curr_ns, def);
        self.curr_func.push(index);

        let mut self_ctx = self.get_env_ctx();
        self_ctx.scope -= 1; // Adjust to outer scope where symbol is defined

        let symbol_data = SymbolCtx::new(
            self_ctx,
            index,
            lambda.modifiers.clone(),
        );

        let res_data = ResData {
            self_ctx: Context::Symbol(symbol_data),
            target_ctx: None,
            type_data: TypeData { type_id: rtn_type_id, typ: rtn_type },
        };

        // curr_scope - 1 is used to get the outer scope where the symbol is declared, as the curr
        // scope is the inner scope of the function
        let data = self.meta_space.add_symbol(self.curr_ns, self.curr_scope - 1, name, res_data);
        Ok(data)
    }


    // For symbol defs, depth 0 will always be top level of the current namespace, TODO depth check redundant?
    // If function and class == None, then symbol def is inside  a lexical scope, but still top level to ns
    // If not top level, check for existing functions first as it may be nested in a class.
    // If not currently resolve a function body, and not a top level ns def, symbol must be
    // a top level class definition
    pub fn add_var_symbol(&mut self, name: IString, typ: Type, mods: Option<Vec<Mod>>) -> Result<&ResData, String> {
        let type_id = self.meta_space.types.get_or_define_type(&typ);
        let def = VarMeta { name };


        if self.in_class_scope() {
            todo!("Classes not implemented")
        } else {
            let index = self.meta_space.add_var_def(self.curr_ns, def);
            let symbol_data = SymbolCtx::new(self.get_env_ctx(), index, mods);
            let res_data = ResData {
                self_ctx: Context::Symbol(symbol_data),
                target_ctx: None,
                type_data: TypeData { type_id, typ },
            };
            Ok(self.meta_space.add_symbol(self.curr_ns, self.curr_scope, name, res_data))
        }
    }


    pub fn get_symbol_type(&mut self, name: IString) -> &Type {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(symbol) = self.meta_space.get_symbol(self.curr_ns, scope_id, name.value) {
                return &symbol.type_data.typ;
            }
        }
        &self.unresolved
    }


    pub fn get_symbol_ctx(&self, name: IString) -> Option<&ResData> {
        for &scope_id in self.active_scopes.iter().rev() {
            if let Some(data) = self.meta_space.get_symbol(self.curr_ns, scope_id, name.value) {
                return Some(data);
            }
        }
        None
    }


    // Used to validate user type annotations with resolved types
    pub fn validate_type(&self, intern_str: IString) -> &Type {
        let found = self.meta_space.types.get_type_by_name(intern_str);
        if found.is_none() { return &self.unresolved; }

        match found.unwrap() {
            Type::Unresolved | Type::Integer | Type::Float | Type::String | Type::Boolean |
            Type::Array(_) | Type::Nil | Type::Pair | Type::Void => found.unwrap(),
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


    pub fn get_env_ctx(&self) -> ExprContext {
        ExprContext {
            scope: self.curr_scope,
            depth: self.curr_depth,
            ns: self.curr_ns,
            class: self.curr_class.last().copied(),
            func: self.curr_func.last().copied(),
        }
    }
}

