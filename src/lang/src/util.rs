use std::alloc::{alloc, Layout};
use std::sync::{Mutex};

use ahash::AHashMap;
use lazy_static::lazy_static;


pub struct Interner {
    map: AHashMap<String, u64>,
    list: Vec<*const str>,
}


#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct IString {
    pub value: u64,
}


impl PartialEq<u64> for IString {
    fn eq(&self, other: &u64) -> bool {
        self.value == *other
    }
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
    pub fn intern(&mut self, string: String) -> IString {
        if let Some(&id) = self.map.get(&string) {
            return IString { value: id };
        }

        let new_id = self.list.len() as u64;
        let raw_ptr = string.as_str() as *const str;
        self.list.push(raw_ptr);
        self.map.insert(string, new_id);
        IString { value: new_id }
    }

    pub fn resolve(&self, i_string: IString) -> &str {
        unsafe {
            self.list.get(i_string.value as usize).map(|&ptr| &*ptr).expect("Invalid interned id")
        }
    }

    pub fn resolve_value(&self, value: u64) -> &str {
        unsafe {
            self.list.get(value as usize).map(|&ptr| &*ptr).expect("Invalid interned id")
        }
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
    pub const_init: IString,
    pub const_param: IString,
    pub const_var: IString,
    pub const_func: IString,
    pub const_pre: IString,
    pub const_post: IString,
    pub const_final: IString,
    pub const_validate: IString,
    pub const_int: IString,
    pub const_float: IString,
    pub const_bool: IString,
    pub const_string: IString,
    pub const_nil: IString,
    pub const_pair: IString,
    pub const_void: IString
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
        let const_nil = cache.intern("#nil".to_string());
        let const_pair = cache.intern("pair".to_string());
        let const_void = cache.intern("#void".to_string());

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
            const_pair,
            const_void,
        }
    }
}


impl SCache {
    pub fn intern(&self, s: String) -> IString {
        self.cache.lock().unwrap().intern(s)
    }

    // Unsafe: values are never dropped from the cache, due to the need for a
    // rw lock the value is owned by the lock. As such this function transmutes
    // the pointer to static as to be able to return it. These reference are never
    // stored externally and are just used for temporary access.
    pub fn resolve(&self, id: IString) -> &str {
        let interner = self.cache.lock().unwrap();
        let string_ref: &str = interner.resolve(id);
        unsafe {
            std::mem::transmute::<&str, &'static str>(string_ref)
        }
    }

    pub fn resolve_value(&self, id: u64) -> &str {
        let interner = self.cache.lock().unwrap();
        let string_ref: &str = interner.resolve_value(id);
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

#[macro_export] macro_rules! format_error {
    ($loc:expr, $err:expr) => {
        format!("ERROR | Line: {}, Char: {} | {}", $loc.0, $loc.1, $err)
    };
}
