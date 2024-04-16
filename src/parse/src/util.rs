use std::alloc::{alloc, Layout};
use std::cell::{RefCell, UnsafeCell};
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::{Mutex, RwLock, RwLockReadGuard};
use ahash::AHashMap;
use intmap::IntMap;
use lazy_static::lazy_static;
use crate::code_gen::GenData;
use crate::op_codes::OpCode;


pub struct Interner {
    map: AHashMap<String, u64>,  
    list: Vec<*const str>,      
}
unsafe impl Send for Interner {}
unsafe impl Sync for Interner {}
impl Default for Interner {
    fn default() -> Self {
        Interner {
            map: AHashMap::with_capacity(100),
            list: Vec::with_capacity(100),
        }
    }
}

impl Interner {
    pub fn intern(&mut self, string: String) -> u64 {
        if let Some(&id) = self.map.get(&string) {
            return id;
        }
  
        let new_id = self.list.len() as u64;
        let raw_ptr = string.as_str() as *const str;
        self.list.push(raw_ptr);
        self.map.insert(string, new_id);
        new_id
    }

    pub fn resolve(&self, id: u64) -> &str {
        self.list.get(id as usize).map(|&ptr| unsafe { &*ptr }).expect("Invalid interned id")
    }
    
    pub fn print_cache(&self) {
        println!("Intern Cache: {:?}", self.map)
    }
}


impl Drop for Interner {
    fn drop(&mut self) {
        for &ptr in &self.list {
            unsafe {
                let _ = Box::from_raw(ptr as *mut str);  
            }
        }
    }
}


pub struct SCache {
    pub cache: Mutex<Interner>,
    pub const_init: u64,
    pub const_param: u64,
    pub const_var: u64,
    pub const_func: u64,
    pub const_pre: u64,
    pub const_post: u64,
    pub const_final: u64,
    pub const_validate: u64,
    pub const_int: u64,
    pub const_float: u64,
    pub const_bool: u64,
    pub const_string: u64,
    pub const_nil: u64,
}


impl Default for SCache {
    fn default() -> Self {
        let mut cache = Interner::default();
        let const_init = cache.intern("init".to_string());
        let const_param = cache.intern("param".to_string());
        let const_var = cache.intern("var".to_string());
        let const_func = cache.intern("func".to_string());
        let const_pre = cache.intern("pre".to_string());
        let const_post = cache.intern("post".to_string());
        let const_final = cache.intern("final".to_string());
        let const_validate = cache.intern("validate".to_string());
        let const_int = cache.intern("int".to_string());
        let const_float = cache.intern("float".to_string());
        let const_bool = cache.intern("bool".to_string());
        let const_string = cache.intern("string".to_string());
        let const_nil = cache.intern("nil".to_string());

        SCache {
            cache: Mutex::new(cache),
            const_init,
            const_param,
            const_var,
            const_func,
            const_pre,
            const_post,
            const_final,
            const_validate,
            const_int,
            const_float,
            const_bool,
            const_string,
            const_nil,
        }
    }
}


impl SCache {
    pub fn intern(&self, s: String) -> u64 {
        self.cache.lock().unwrap().intern(s)
    }

    // Unsafe: values are never dropped from the cache, due to the need for a
    // rw lock the value is owned by the lock. As such this function transmutes
    // the pointer to static as to be able to return it. These reference are never
    // stored externally and are just used for temporary access.
    pub fn resolve(&self, id: u64) -> &str {
        let interner = self.cache.lock().unwrap();
        let string_ref: &str = interner.resolve(id);
        unsafe {
            std::mem::transmute::<&str, &'static str>(string_ref)
        }
    }
}

lazy_static! {pub static ref SCACHE : SCache = SCache::default();}



pub fn get_wide_bytes(val: u16) -> (u8, u8) {
    ((val & 0xFF) as u8, ((val >> 8) & 0xFF) as u8)
}


pub fn read_wide_bytes(val1: u8, val2: u8) -> u16 {
    let low_byte = val1 as u16;
    let high_byte = val2 as u16;
    (high_byte << 8) | low_byte
}


pub fn get_byte_array(size: usize) -> *mut u8 {
    unsafe {
        let layout = Layout::array::<u8>(size).unwrap();
        let ptr = alloc(layout);
        if ptr.is_null() { panic!("Failed to allocate memory"); }
        ptr
    }
}


pub enum ConstValue {
    Integer,
    Float,
    String,
}


#[derive(Debug, Clone)]
pub struct CompUnit {
    pub code: Vec<u8>,
    pub constants: Vec<[u8; 8]>,
    pub existing_str: AHashMap<u64, u16>,
}


impl CompUnit {
    pub fn write_op_code(&mut self, op: OpCode) {
        self.code.push(op as u8)
    }

    pub fn write_operand(&mut self, val: u8) {
        self.code.push(val);
    }

    pub fn write_wide_inst(&mut self, val: u16) {
        self.code.push((val & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
    }

    fn add_constant(&mut self, bytes: [u8; 8]) -> u16 {
        unsafe {
            let curr_index = self.constants.len();
            if curr_index >= u16::MAX as usize {
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
    pub fn push_code_gen(&mut self, mut other: GenData) {
        self.code.append(&mut other.code);
    }
}

