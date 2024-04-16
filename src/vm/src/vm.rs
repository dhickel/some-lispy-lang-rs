use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};
use std::os::unix::fs::OpenOptionsExt;
use parser::op_codes::{decode, OpCode};
use std::time::{SystemTime, UNIX_EPOCH};
use intmap::IntMap;
use lang::types::{ObjType, Type};
use parser::util;
use parser::util::{CompUnit, SCACHE};

macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("")
            .as_nanos()
    };
}

pub enum InterpResult {
    Ok,
    CompileErr(String),
    InterpErr(String),
}


#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub enum Value {
    I64(i64),
    F64(f64),
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HeapTag {
    // typ: u16,
    size: usize,
    loc: *mut u8,
    active: bool,
}


#[derive(Debug)]
pub struct Heap {
    items: Vec<HeapTag>,
    data_start: *mut u8,
    data_end: *mut u8,
    data_capacity: usize,
    free_blocks: Vec<(usize, *mut u8)>,
    free_indexes: Vec<usize>,
}


impl Heap {
    pub fn new(size: usize) -> Heap {
        let heap = util::get_byte_array(size);
        Heap {
            items: Vec::<HeapTag>::with_capacity(100),
            data_start: heap,
            data_end: heap,
            data_capacity: size,
            free_blocks: Vec::<(usize, *mut u8)>::with_capacity(100),
            free_indexes: Vec::<usize>::with_capacity(100),
        }
    }

    pub fn insert_item(&mut self, item: &[u8] /*typ: u16*/) -> u64 {
        unsafe {
            let loc = self.get_insert_loc(item.len());
            std::ptr::copy_nonoverlapping(item.as_ptr(), loc, item.len());

            let tag = HeapTag {
                //typ,
                size: item.len(),
                loc,
                active: true,
            };

            let index = self.get_index();
            self.items.insert(index, tag);
            index as u64
        }
    }

    pub unsafe fn insert_bytes(&mut self, ptr: *const u8, size: usize) -> u64 {
        unsafe {
            let loc = self.get_insert_loc(size);
            std::ptr::copy_nonoverlapping(ptr, loc, size);

            let tag = HeapTag {
                //typ,
                size: size,
                loc,
                active: true,
            };

            let index = self.get_index();
            self.items.insert(index, tag);
            index as u64
        }
    }

    pub fn get_item(&self, index: u64) -> (*const u8, usize) {
        let meta = self.items[index as usize];
        unsafe {
            (meta.loc, meta.size)
        }
    }

    fn get_index(&mut self) -> usize {
        if !self.free_indexes.is_empty() {
            self.free_indexes.pop().unwrap()
        } else { self.items.len() }
    }

    fn get_insert_loc(&mut self, size: usize) -> *mut u8 {
        let mut loc = None;
        let mut loc_size = usize::MAX;
        let mut idx: usize = 0;

        for i in 0..self.free_blocks.len() {
            let block = &self.free_blocks[i];
            if block.0 >= size && block.0 - size < loc_size {
                loc = Some(block.1);
                loc_size = block.0;
                idx = i;
            }
        }
        if let Some(pointer) = loc {
            self.free_blocks.swap_remove(idx);
            pointer
        } else {
            if self.data_capacity < size { panic!("Heap overflow") }
            let loc = self.data_end;
            self.data_capacity -= size;
            unsafe { self.data_end = loc.add(size); }
            loc
        }
    }
}


#[derive(Debug)]
pub struct Vm<'a> {
    comp_unit: &'a mut CompUnit,
    ip: *mut u8,
    stack: [u8; 1048576],
    stack_top: *mut u8,
    int_cache: [i64; 256],
    float_cache: [f64; 256],
    byte_cache: [u8; 2048],
    heap: Heap,
    global_defs: IntMap<u64>,
}


impl<'a> Vm<'a> {
    pub fn new(chunk: &'a mut CompUnit) -> Self {
        Vm {
            comp_unit: chunk,
            ip: std::ptr::null_mut(),
            stack: [0; 1048576],
            stack_top: std::ptr::null_mut(),
            int_cache: [0; 256],
            float_cache: [0.0; 256],
            byte_cache: [0; 2048],
            heap: Heap::new(1.049e+8 as usize),
            global_defs: IntMap::<u64>::with_capacity(50),
        }
    }

    fn read_op_code(&mut self) -> OpCode {
        unsafe {
            let code = *self.ip;
            self.ip = self.ip.add(1);
            let code: OpCode = std::mem::transmute(code);
           // println!("At Inst: {:?}", code);
            code
        }
    }

    pub fn read_wide_inst(&mut self) -> u16 {
        unsafe {
            let low_byte = *self.ip as u16;
            self.ip = self.ip.add(1);

            let high_byte = *self.ip as u16;
            self.ip = self.ip.add(1);

            (high_byte << 8) | low_byte
        }
    }

    pub fn read_inst(&mut self) -> u8 {
        unsafe {
            let byte = *self.ip;
            self.ip = self.ip.add(1);
            byte
        }
    }

    fn load_constant(&mut self, index: usize, size: usize) {
        unsafe {
            let data_ptr = self.comp_unit.constants.get_unchecked(index).as_ptr();
            let new_stack_top = self.stack_top.add(8);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, 8);
            self.stack_top = new_stack_top;
        }
    }

    fn push_8_bytes(&mut self, bytes: [u8; 8]) {
        unsafe {
            let new_stack_top = self.stack_top.add(8);
            let byte_ptr = bytes.as_ptr();
            std::ptr::copy_nonoverlapping(byte_ptr, self.stack_top, 8);
            self.stack_top = new_stack_top;
        }
    }

    fn push_arbitrary_bytes(&mut self, pointer: (*const u8, usize)) {
        unsafe {
            let new_stack_top = self.stack_top.add(pointer.1);
            std::ptr::copy_nonoverlapping(pointer.0, self.stack_top, pointer.1);
            self.stack_top = new_stack_top;
        }
    }

    fn push_i64(&mut self, value: i64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_8_bytes(bytes);
        }
    }

    fn push_u64(&mut self, value: u64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_8_bytes(bytes);
        }
    }

    fn push_f64(&mut self, value: f64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_8_bytes(bytes);
        }
    }

    fn push_bool(&mut self, value: bool) {
        self.push_i64(if value { 1 } else { 0 })
    }

    fn replace_top_bytes(&mut self, bytes: [u8; 8]) {
        unsafe {
            let byte_ptr = bytes.as_ptr();
            std::ptr::copy_nonoverlapping(byte_ptr, self.stack_top.sub(8), 8);
        }
    }

    fn replace_top_i64(&mut self, value: i64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.replace_top_bytes(bytes);
        }
    }

    fn replace_top_f64(&mut self, value: f64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.replace_top_bytes(bytes);
        }
    }

    fn pop_bytes(&mut self, n: usize) -> *const u8 {
        unsafe {
            if self.stack_top.sub(n) < self.stack.as_mut_ptr() {
                panic!("Attempted to pop stack less than start index")
            }
            self.stack_top = self.stack_top.offset(-(n as isize));
            self.stack_top as *const u8
            // std::slice::from_raw_parts(self.stack_top, n)
        }
    }

    // fn print_stack_location(&self) {
    //     let start_ptr = self.stack.as_ptr();
    //     let end_ptr = unsafe { start_ptr.add(self.stack.len()) };
    //     println!("Array starts at: {:p}", start_ptr);
    //     println!("Array ends at: {:p}", end_ptr);
    // }

    pub fn pop_f64(&mut self) -> f64 {
        unsafe {
            let ptr = self.pop_bytes(std::mem::size_of::<f64>());
            let value = std::ptr::read(ptr as *const f64);
            value
        }
    }

    pub fn pop_i64(&mut self) -> i64 {
        unsafe {
            let ptr = self.pop_bytes(std::mem::size_of::<i64>());
            let value = std::ptr::read(ptr as *const i64);
            value
        }
    }

    pub fn pop_u64(&mut self) -> u64 {
        unsafe {
            let ptr = self.pop_bytes(std::mem::size_of::<u64>());
            let value = std::ptr::read(ptr as *const u64);
            value
        }
    }


    fn pop_bool(&mut self) -> bool {
        let val = self.pop_i64();
        val != 0
    }

    fn discard_n_words(&mut self, n: u8) {
        if n > self.stack_top_index() as u8 {
            panic!("Popped more than stack size") // FIXME debug, remove on release
        }
        unsafe {
            self.stack_top = self.stack_top.offset(-((n * 8) as isize));
        }
    }

    pub fn peek_bytes(&self, index: usize) -> &[u8] {
        unsafe {
            let ptr = self.stack_top.offset(-((index as isize + 1) * 8));
            std::slice::from_raw_parts(ptr, 8)
        }
    }

    pub fn peek_f64(&self, index: usize) -> f64 {
        unsafe {
            let ptr = self.stack_top.offset(-((index as isize + 1) * std::mem::size_of::<f64>() as isize));
            std::ptr::read(ptr as *const f64)
        }
    }

    pub fn peek_i64(&self, index: usize) -> i64 {
        unsafe {
            let ptr = self.stack_top.offset(-((index as isize + 1) * std::mem::size_of::<i64>() as isize));
            std::ptr::read(ptr as *const i64)
        }
    }

    pub fn peek_bool(&self, index: usize) -> bool {
        let val = self.peek_i64(index);
        val != 0
    }

    pub fn print_stack(&self) {
        for i in (0..self.stack_top_index()).step_by(8) {
            println!("Stack[{}]: {:?}", i, &self.stack[i..i + 8])
        }
        println!()
    }

    pub fn stack_top_index(&self) -> usize {
        unsafe {
            self.stack_top.offset_from(self.stack.as_ptr()) as usize
        }
    }

    fn print_value<T>(&self, value: &T) where T: Debug {
        println!("Value: {:?}", value)
    }
    
    pub fn print_remaining_ops(&self) {
        unsafe {
            let offset = self.ip.offset_from(self.comp_unit.code.as_ptr()) as usize;
            println!("Remaining Ops{:?}", decode(&self.comp_unit.code[offset..]))
        }
    }

    pub fn interpret(&mut self, chunk: &'a mut CompUnit) -> InterpResult {
        self.ip = chunk.code.as_mut_ptr();
        self.comp_unit = chunk;
        InterpResult::Ok
    }

    //
    pub fn run(&mut self) -> InterpResult {
        self.stack_top = self.stack.as_mut_ptr();
        self.ip = self.comp_unit.code.as_mut_ptr();
        let t = nano_time!();
        
        let mut i = 0;

        'outer: loop {
            i += 1;
            if i > 1000 {
                break
            }
            match self.read_op_code() {
                OpCode::Exit => {
                    println!("Exec Time: {}ns", nano_time!() - t);
                    break;
                }

                OpCode::RtnI64 => {
                    let val = self.pop_i64();
                  //  self.print_value(&val);
                }

                OpCode::RtnF64 => {
                    let val = self.pop_f64();
                    self.print_value(&val);
                }

                OpCode::RtnBool => {
                    let val = self.pop_bool();
                    self.print_value(&val);
                }

                OpCode::RtnRef => {
                    let val = self.pop_bytes(8);
                }

                OpCode::LoadConst => {
                    let index = self.read_inst() as usize;
                    self.load_constant(index, 8);
                }

                OpCode::LoadConstW => {
                    let index = self.read_wide_inst() as usize;
                    self.load_constant(index, 8);
                }

                OpCode::AddI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1 + val2);
                }

                OpCode::AddF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1 + val2);
                }

                OpCode::SubI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1 - val2)
                }

                OpCode::SubF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1 - val2)
                }

                OpCode::MulI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1 * val2)
                }

                OpCode::MulF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1 * val2)
                }

                OpCode::DivI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1 / val2)
                }

                OpCode::DivF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1 / val2)
                }

                OpCode::PowI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1.pow(val2 as u32))
                }

                OpCode::PowF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1.powf(val2))
                }

                OpCode::ModI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_i64(val1 % val2)
                }

                OpCode::ModF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_f64(val1 % val2)
                }

                OpCode::NegI64 => {
                    let val = self.pop_i64();
                    self.push_i64(-val)
                }

                OpCode::NegF64 => {
                    let val = self.pop_f64();
                    self.push_f64(-val)
                }

                OpCode::NegBool => {
                    let val = self.pop_i64();
                    self.push_i64(if val == 0 { 1 } else { 0 })
                }

                OpCode::I64ToF64 => {
                    let val = self.pop_i64();
                    self.push_f64(val as f64)
                }

                OpCode::F64ToI64 => {
                    let val = self.pop_f64();
                    self.push_i64(val as i64)
                }

                OpCode::ConstT => {
                    self.push_i64(1)
                }

                OpCode::ConstF => {
                    self.push_i64(0)
                }

                OpCode::CompI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    if val1 == val2 {
                        self.push_i64(0);
                        continue;
                    }
                    if val1 > val2 {
                        self.push_i64(1)
                    } else { self.push_i64(-1) }
                }

                OpCode::CompI64N => {
                    let n = self.read_inst();
                    let mut val1 = self.pop_i64();

                    for i in 1..n as usize {
                        let val2 = self.pop_i64();
                        if val1 == val2 {
                            self.int_cache[i - 1] = 0;
                        } else {
                            self.int_cache[i - 1] = if val1 > val2 { 1 } else { -1 }
                        }
                        val1 = val2;
                    }

                    for i in 0..(n - 1) as usize {
                        self.push_i64(self.int_cache[i])
                    }
                }

                OpCode::CompF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    // if nan IEEE 754 specifies always false
                    if val1.is_nan() || val2.is_nan() {
                        self.push_i64(0);
                        continue;
                    }
                    if val1 == val2 {
                        self.push_i64(0);
                        continue;
                    }
                    if val1 > val2 {
                        self.push_i64(1)
                    } else { self.push_i64(-1) }
                }

                OpCode::CompF64N => {
                    let n = self.read_inst();
                    let mut val1 = self.pop_f64();

                    for i in 1..n as usize {
                        let val2 = self.pop_f64();

                        if val1.is_nan() || val2.is_nan() {
                            self.push_i64(0);
                            continue;
                        }
                        if val1 == val2 {
                            self.int_cache[i - 1] = 0;
                        } else {
                            self.int_cache[i - 1] = if val1 > val2 { 1 } else { -1 }
                        }
                        val1 = val2;
                    }

                    for i in 0..(n - 1) as usize {
                        self.push_i64(self.int_cache[i])
                    }
                }

                OpCode::LogicOr => {
                    let n = self.read_inst();
                    for i in 0..n {
                        if self.pop_bool() {
                            self.discard_n_words(n - 1 - i);
                            self.push_bool(true);
                            continue 'outer;
                        }
                    }
                    self.push_bool(false)
                }

                OpCode::LogicAnd => {
                    let n = self.read_inst();
                    for i in 0..n {
                        if !self.pop_bool() {
                            self.discard_n_words(n - 1 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }
                OpCode::LogicXor => {
                    let mut truths = 0;
                    let n = self.read_inst();
                    for i in 0..n {
                        if self.pop_bool() {
                            truths += 1;
                        }
                    }
                    self.push_bool(truths % 2 == 1)
                }

                OpCode::LogicNegate => {
                    let val = !self.pop_bool();
                    self.push_bool(val)
                }

                OpCode::CompGt => {
                    let val = self.pop_i64() == 1;
                    self.push_bool(val);
                }

                OpCode::CompGtN => {
                    let n = self.read_inst();
                    for i in 0..n - 1 {
                        if self.pop_i64() != 1 {
                            self.discard_n_words(n - 2 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }

                OpCode::CompGtEq => {
                    let val = self.pop_i64() >= 0;
                    self.push_bool(val);
                }

                OpCode::CompGtEqN => {
                    let n = self.read_inst();
                    for i in 0..n - 1 {
                        if self.pop_i64() < 0 {
                            self.discard_n_words(n - 2 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }

                OpCode::CompEq => {
                    let val = self.pop_i64() == 0;
                    self.push_bool(val);
                }

                OpCode::CompEqN => {
                    let n = self.read_inst();
                    for i in 0..n - 1 {
                        if self.pop_i64() != 0 {
                            self.discard_n_words(n - 2 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }

                OpCode::CompLt => {
                    let val = self.pop_i64() == -1;
                    self.push_bool(val);
                }

                OpCode::CompLtN => {
                    let n = self.read_inst();
                    for i in 0..n - 1 {
                        if self.pop_i64() != -1 {
                            self.discard_n_words(n - 2 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }

                OpCode::CompLtEq => {
                    let val = self.pop_i64() <= 0;
                    self.push_bool(val);
                }

                OpCode::CompLtEqN => {
                    let n = self.read_inst();
                    for i in 0..n - 1 {
                        if self.pop_i64() > 0 {
                            self.discard_n_words(n - 2 - i);
                            self.push_bool(false);
                            continue 'outer;
                        }
                    }
                    self.push_bool(true)
                }

                OpCode::JumpTrue => {}

                OpCode::JumpFalse => {
                    let offset = self.read_wide_inst();
                    if self.pop_bool() == false {
                        unsafe {
                            self.ip = self.ip.add(offset as usize);
                        }
                    }
                }

                OpCode::JumpFWd => {
                    self.print_stack();
                    let offset = self.read_wide_inst();
                    unsafe {
                        self.ip = self.ip.add(offset as usize);
                    }
                }

                OpCode::JumpBack => {
                    let offset = self.read_wide_inst();
                    unsafe {
                        self.ip = self.ip.sub(offset as usize);
                    }
                }

                OpCode::IConstM1 => self.push_i64(-1),
                OpCode::IConst0 => self.push_i64(0),
                OpCode::IConst1 => self.push_i64(1),
                OpCode::IConst2 => self.push_i64(2),
                OpCode::IConst3 => self.push_i64(3),
                OpCode::IConst4 => self.push_i64(4),
                OpCode::IConst5 => self.push_i64(5),
                OpCode::Pop => { self.pop_bytes(8); }

                OpCode::DefGlobal => {
                    let name_id = self.pop_u64();
                    let value = self.pop_u64();
                    self.global_defs.insert(name_id, value);
                }
                OpCode::AssignGlobal => {
                    let name_id = self.pop_u64();
                    let value = self.pop_u64();
                    self.global_defs.insert(name_id, value);
                 
                }

                OpCode::LoadGlobal => {
                    let name_id = self.pop_u64();
                    let item_ref = if let Some(val) = self.global_defs.get(name_id) {
                        *val
                    } else {
                        println!("{}", SCACHE.resolve(14));
                        println!("Global defs: {:?}", self.global_defs);
                        println!("SCache: {:?}",SCACHE.cache.lock().unwrap().print_cache());
                        println!("name_id: {}", SCACHE.resolve(name_id));
                        panic!("Failed to resolve variable binding {}", name_id)
                    };
                    unsafe {
                        let item_bytes = self.heap.get_item(item_ref);
                        self.push_arbitrary_bytes(item_bytes);
                    }
                }

                OpCode::HeapStore => {
                    let size = self.read_wide_inst() as usize;
                    let ptr = self.pop_bytes(size);
                    unsafe {
                        let tag_ref = self.heap.insert_bytes(ptr, size);
                        self.push_u64(tag_ref);
                    }
                }
            }
        }

        InterpResult::Ok
    }
}

//
// #[cfg(test)]
// mod tests {
//     use parser::util::CompUnit;
//     use crate::vm::Vm;
//     use super::*;
//
//
//     #[test]
//     fn test_wide_rw() {
//         let value = 24_031_u16;
//
//         let mut chunk = CompUnit {
//             code: Vec::<u8>::new(),
//             constants: Vec::<u8>::new(),
//             existing_u64s: Default::default(),
//         };
//
//
//         chunk.write_wide_inst(value);
//         let mut vm = Vm::new(&mut chunk);
//         let rtn_value = vm.read_wide_inst();
//         assert_eq!(value, rtn_value)
//     }
//
//
//     #[test]
//     fn test_stack_values() {
//         let mut chunk = CompUnit {
//             code: Vec::<u8>::new(),
//             constants: Vec::<u8>::new(),
//         };
//
//         let value1 = 23423423423_i64;
//         let value2 = 3423.34234234_f64;
//
//         let idx1 = chunk.push_constant(&value1);
//         let idx2 = chunk.push_constant(&value2);
//
//         let mut vm = Vm::new(&mut chunk);
//
//         vm.push_constant(idx1, 8);
//         vm.push_constant(idx2, 8);
//
//         let value2_rtn = vm.pop_f64();
//         let value1_rtn = vm.pop_i64();
//
//         assert_eq!(value1, value1_rtn);
//         assert_eq!(value2, value2_rtn);
//     }
// }

