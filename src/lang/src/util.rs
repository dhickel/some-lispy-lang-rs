use std::alloc::{alloc, Layout};
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::{BufReader, Error, Read};
use std::path::PathBuf;
use std::sync::{Mutex};

use ahash::AHashMap;
use lazy_static::lazy_static;
use sha2::{Digest, Sha256};
use sha2::digest::Update;
use crate::Sha256Hash;

lazy_static! {pub static ref SCACHE : SCache = SCache::default();}

pub struct Interner {
    map: AHashMap<&'static str, u32>,
    list: Vec<Box<String>>,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, PartialOrd)]
pub struct IString {
    pub value: u32,
}

impl Debug for IString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(SCACHE.resolve(*self))
    }
}


impl Into<u64> for IString { fn into(self) -> u64 { self.value as u64 } }
impl Into<u32> for IString { fn into(self) -> u32 { self.value } }

impl PartialEq<u32> for IString { fn eq(&self, other: &u32) -> bool { self.value == *other } }

impl PartialOrd<u32> for IString {
    fn partial_cmp(&self, other: &u32) -> Option<Ordering> { self.value.partial_cmp(other) }
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
        if let Some(&id) = self.map.get(string.as_str()) {
            return IString { value: id };
        }

        let new_id = if self.list.len() > u32::MAX as usize {
            panic!("Interner Overflow")
        } else {
            self.list.len() as u32
        };

        let boxed = Box::new(string);
        let str_ref: &str = boxed.as_str();
        let static_ref = unsafe { std::mem::transmute::<&str, &'static str>(str_ref) };
        self.map.insert(static_ref, new_id);
        self.list.push(boxed);
        IString { value: new_id }
    }

    pub fn resolve(&self, i_string: IString) -> &str {
        self.list.get(i_string.value as usize)
            .expect("Invalid interned id")
            .as_str()
    }


    pub fn print_cache(&self) {
        println!("Intern Cache: {:#?}", self.map)
    }
}



pub struct SCache {
    cache: Mutex<Interner>,
    pub NIL: IString,
    pub BOOL: IString,
    pub STRING: IString,
    pub U8: IString,
    pub U16: IString,
    pub U32: IString,
    pub U64: IString,
    pub I32: IString,
    pub I64: IString,
    pub F32: IString,
    pub F64: IString,

}


impl Default for SCache {
    fn default() -> Self {
        let mut cache = Interner::default();
        SCache {
            NIL: cache.intern("Nil".to_string()),
            BOOL: cache.intern("Bool".to_string()),
            STRING: cache.intern("String".to_string()),
            U8: cache.intern("U8".to_string()),
            U16: cache.intern("U16".to_string()),
            U32: cache.intern("U32".to_string()),
            U64: cache.intern("U64".to_string()),
            I32: cache.intern("I32".to_string()),
            I64: cache.intern("I64".to_string()),
            F32: cache.intern("F32".to_string()),
            F64: cache.intern("F64".to_string()),
            cache: Mutex::new(cache),
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

   
}


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


pub fn get_sha256(path: &PathBuf) -> std::io::Result<Sha256Hash> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let hash = Sha256::digest(&buffer);
    Ok(Sha256Hash::new(hash.into()))
}


pub fn read_file(path: &PathBuf) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut file_str = String::with_capacity(512);
    file.read_to_string(&mut file_str)?;
    Ok(file_str.into())
}
