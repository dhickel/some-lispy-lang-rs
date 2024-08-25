use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};
use compile::op_codes::{decode, OpCode};
use std::time::{SystemTime, UNIX_EPOCH};
use compile::environment::{MetaSpace, PermSpace, PermNameSpace, StackFrame};

use crate::heap::Heap;

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


#[derive(Debug)]
pub struct Vm {
    ip: *const u8,
    stack: [u8; 1048576],
    stack_top: *mut u8,
    int_cache: [i64; 256],
    float_cache: [f64; 256],
    byte_cache: [u8; 2048],
    heap: Heap,
    meta: MetaSpace,
    perm: PermSpace,
    curr_frame: *const u8,
    curr_meta: *const u8,
    curr_const: *const u8,
    frame_cnt: u64,
}


impl Vm {
    pub fn new(mut meta_space: MetaSpace) -> Self {
        let perm = PermSpace::new(&mut meta_space);
        Vm {
            ip: std::ptr::null_mut(),
            stack: [0; 1048576],
            stack_top: std::ptr::null_mut(),
            int_cache: [0; 256],
            float_cache: [0.0; 256],
            byte_cache: [0; 2048],
            heap: Heap::new(1.049e+8 as usize),
            meta: meta_space,
            perm: perm,
            curr_frame: std::ptr::null(),
            curr_meta: std::ptr::null(),
            curr_const: std::ptr::null(),
            frame_cnt: 0,
        }
    }

    unsafe fn transmute_op(ptr: *const u8) -> OpCode {
        std::mem::transmute(std::ptr::read(ptr))
    }

    fn read_op_code(&mut self) -> OpCode {
        unsafe {

            // println!("ip is null? {:?}", self.ip.is_null());
            let code = *self.ip;
            self.ip = self.ip.add(1);
            let code: OpCode = std::mem::transmute(code);

            println!("Executing: {:?}", code);
            // println!("before Op Code");
            // self.print_stack();

            code
        }
    }

    #[inline(always)]
    pub fn read_wide_inst(&mut self) -> u16 {
        unsafe {
            let low_byte = *self.ip as u16;
            self.ip = self.ip.add(1);

            let high_byte = *self.ip as u16;
            self.ip = self.ip.add(1);

            (high_byte << 8) | low_byte
        }
    }

    #[inline(always)]
    pub fn read_inst(&mut self) -> u8 {
        unsafe {
            let byte = *self.ip;
            self.ip = self.ip.add(1);
            byte
        }
    }

    #[inline(always)]
    fn load_ns_constant(&mut self, ns: u16, index: u16) {
        unsafe {
            let data_ptr = self.perm.namespaces.get_unchecked(ns as usize * 8).get_constants(index);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, 8);
            self.stack_top = self.stack_top.add(8);
        }
    }

    #[inline(always)]
    fn load_local_constant(&mut self, index: u16) {
        unsafe {
            // println!("load1");
            // println!("IsNull: {}", self.curr_const.is_null());
            let data_ptr = self.curr_const.add(index as usize * 8);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, 8);
            self.stack_top = self.stack_top.add(8);
        }
    }

    #[inline(always)]
    fn load_ns_var(&mut self, ns: u16, index: u16) {
        unsafe {
            let data_ptr = self.perm.namespaces[ns as usize].get_var_data(index);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, 8);
            self.stack_top = self.stack_top.add(8);
        }
    }

    #[inline(always)]
    fn load_local_var(&mut self, index: u16) {
        unsafe {
            println!("Loading Idx: {}", index);
            let data_ptr = self.curr_frame.add((index * 8) as usize);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, 8);
            self.stack_top = self.stack_top.add(8);
        }
    }

    #[inline(always)]
    fn store_ns_var(&mut self, ns: u16, index: u16) {
        unsafe {
            self.perm.namespaces[ns as usize].set_var_data(index, self.stack_top.sub(8));
            self.stack_top = self.stack_top.sub(8);
        }
    }

    #[inline(always)]
    fn store_local_var(&mut self, index: u16) {
        unsafe {
            let index_ptr = self.curr_frame.add((index * 8) as usize);
            std::ptr::copy_nonoverlapping(index_ptr, self.stack_top.sub(8), 8);
            self.stack_top = self.stack_top.sub(8);
        }
    }

    #[inline(always)]
    fn push_word(&mut self, bytes: [u8; 8]) {
        unsafe {
            let new_stack_top = self.stack_top.add(8);
            let byte_ptr = bytes.as_ptr();
            std::ptr::copy_nonoverlapping(byte_ptr, self.stack_top, 8);
            self.stack_top = new_stack_top;
        }
    }

    #[inline(always)]
    fn push_arbitrary_bytes(&mut self, pointer: (*const u8, usize)) {
        unsafe {
            let new_stack_top = self.stack_top.add(pointer.1);
            std::ptr::copy_nonoverlapping(pointer.0, self.stack_top, pointer.1);
            self.stack_top = new_stack_top;
        }
    }

    #[inline(always)]
    fn push_i64(&mut self, value: i64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_word(bytes);
        }
    }

    #[inline(always)]
    fn push_ref(&mut self, value: u64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_word(bytes);
        }
    }

    #[inline(always)]
    fn push_f64(&mut self, value: f64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_word(bytes);
        }
    }

    #[inline(always)]
    fn push_bool(&mut self, value: bool) {
        self.push_i64(if value { 1 } else { 0 })
    }

    fn pop_word(&mut self) -> *const u8 {
        unsafe {
            if self.stack_top.sub(8) < self.stack.as_mut_ptr() {
                panic!("Attempted to pop stack less than start index")
            }
            self.stack_top = self.stack_top.sub(8);
            self.stack_top as *const u8
        }
    }

    #[inline(always)]
    fn pop_word_to_bytes(&mut self) -> [u8; 8] {
        unsafe {
            let ptr = self.pop_word();
            std::mem::transmute(ptr)
        }
    }

    #[inline(always)]
    pub fn pop_f64(&mut self) -> f64 {
        unsafe {
            let ptr = self.pop_word();
            let value = std::ptr::read(ptr as *const f64);
            value
        }
    }

    #[inline(always)]
    pub fn pop_i64(&mut self) -> i64 {
        unsafe {
            let ptr = self.pop_word();
            let value = std::ptr::read(ptr as *const i64);
            value
        }
    }

    #[inline(always)]
    pub fn pop_ref(&mut self) -> u64 {
        unsafe {
            let ptr = self.pop_word();
            let value = std::ptr::read(ptr as *const u64);
            value
        }
    }

    #[inline(always)]
    fn pop_bool(&mut self) -> bool {
        let val = self.pop_i64();
        val != 0
    }

    #[inline(always)]
    fn discard_n_words(&mut self, n: u8) {
        if n > self.stack_top_index() as u8 {
            panic!("Popped more than stack size")
        }
        unsafe {
            self.stack_top = self.stack_top.offset(-((n * 8) as isize));
        }
    }

    pub fn print_stack(&self) {
        println!();
        for i in (0..self.stack_top_index()).step_by(8) {
            let bytes = &self.stack[i..i + 8];
            let u64_value = u64::from_ne_bytes(bytes.try_into().unwrap());
            let as_hex = format!("0x{:x}", u64_value);
            let as_int = i64::from_ne_bytes(bytes.try_into().unwrap());
            let as_float = f64::from_ne_bytes(bytes.try_into().unwrap());

            println!(
                "Stack[{:<5}]: {:<40} | {:<20} | {:<20} | {:<20.5}",
                i, format!("{:?}", bytes), as_hex, as_int, as_float
            );
        }
        println!()
    }

    pub fn stack_top_index(&self) -> usize {
        unsafe {
            self.stack_top.offset_from(self.stack.as_ptr()) as usize
        }
    }


    /* 
    Construct frame on stack, stack frame is locals count + 5 words for metadata + operands
       [--- Stack Frame ---]
           locals....
           constant pool ptr
           previous frame start
           previous frame meta start
           previous frame ip return address
           operands.....
       [--------------------]
    */

    #[inline(always)]
    fn push_stack_frame(&mut self, frame: StackFrame) {
        println!("Frame: {:?}", frame);
        unsafe {
            // Shift frame down by param count to being in passed parameters on top of stack


            let prev_frame = self.curr_frame;
            let prev_meta = self.curr_meta;

            let ip_as_usize = self.ip as usize;
            std::ptr::copy_nonoverlapping(&ip_as_usize as *const usize, self.curr_meta.add(24) as *mut usize, 1);

            self.curr_frame = self.stack_top.sub(frame.arity as usize * 8);
            self.stack_top = self.stack_top.add((frame.local_count - frame.arity) as usize * 8);
            self.curr_const = frame.constant_ptr;
            self.curr_meta = self.stack_top;


            //self.frame_cnt += 1;

            //  println!("vm: {:?}", self.curr_frame);

            //  ptr to constant pool for frame
            let constant_ptr = frame.constant_ptr as usize;
            std::ptr::copy_nonoverlapping(&constant_ptr as *const usize, self.stack_top as *mut usize, 1);

            // Store current (previous) frame start
            let prev_frame_ptr = prev_frame as usize;
            std::ptr::copy_nonoverlapping(&prev_frame_ptr as *const usize, self.stack_top.add(8) as *mut usize, 1);

            // Store current (previous) frame meta start
            let prev_meta_ptr = prev_meta as usize;
            std::ptr::copy_nonoverlapping(&prev_meta_ptr as *const usize, self.stack_top.add(16) as *mut usize, 1);

            // Return address
            let ip_ptr = self.ip as usize;
            std::ptr::copy_nonoverlapping(&ip_ptr as *const usize, self.stack_top.add(24) as *mut usize, 1);

            // // Frame count
            // std::ptr::copy_nonoverlapping(self.frame_cnt.to_ne_bytes().as_ptr(), self.stack_top.add(32), 8);
            //
            // println!("Pushed to  Frame");
            // println!("curr frame: {:?}", self.curr_frame);
            // println!("curr const: {:?}", self.curr_const);
            // println!("curr meta: {:?}", self.curr_meta);
            // println!("curr ip: {:?}", self.ip);


            // set ip to current bytecode address
            self.ip = frame.code_ptr;

            self.stack_top = self.stack_top.add(32);
        }
    }

    #[inline(always)]
    fn pop_stack_frame(&mut self, rtn_val: bool) {
        //  println!("\nPopping Frame\n");

        unsafe {
            // let frame_count_bytes = std::ptr::read(self.curr_meta.add(32) as *const [u8; 8]);
            // let frame_count = u64::from_ne_bytes(frame_count_bytes);
            // println!("Popping Frame #: {:?}", frame_count);

            let new_stack_top = self.curr_frame as *mut u8;
            self.curr_frame = *(self.curr_meta.add(8) as *const *const u8);
            self.curr_meta = *(self.curr_meta.add(16) as *const *const u8);
            self.curr_const = *(self.curr_meta as *const *const u8);
            self.ip = *(self.curr_meta.add(24) as *const *const u8);


            //  println!("Cure Frame Meta", last_frame_start)

            // println!("Restored to  Frame");
            // println!("curr frame: {:?}", self.curr_frame);
            // println!("curr const: {:?}", self.curr_const);
            // println!("curr meta: {:?}", self.curr_meta);
            // print!("curr ip: {:?}", self.ip);

            // return value if needed

            if rtn_val {
                std::ptr::copy_nonoverlapping(self.stack_top.sub(8), new_stack_top, 8);
                self.stack_top = new_stack_top.add(8);
            } else {
                self.stack_top = new_stack_top;
            }


            // println!("Returnign to Ip Op {:?}", Self::transmute_op(self.ip));
            // println!("Returnign to Ip Op {:?}", self.ip);
            // self.print_stack()
        }
    }

    #[inline(always)]
    fn push_init_frame(&mut self, frame: StackFrame) {
        unsafe {
            // These are pretty much just ignored values, this just sets up the initial code
            // for the ns that is set as the entry point as a faux method. None of the data in
            // really used, it's restored to the locals on the last frame pop, which will then
            // exec exit and not refer to any of these values. This mainly sets the ip to the ns code

            self.curr_frame = self.stack_top.sub(frame.arity as usize * 8);
            self.stack_top = self.stack_top.add((frame.local_count - frame.arity) as usize * 8);
            self.curr_const = frame.constant_ptr;
            self.curr_meta = self.stack_top;
            self.ip = frame.code_ptr;

            let constant_ptr = frame.constant_ptr as usize;
            std::ptr::copy_nonoverlapping(&constant_ptr as *const usize, self.stack_top as *mut usize, 1);

            let ip_ptr = self.ip as usize;
            std::ptr::copy_nonoverlapping(&ip_ptr as *const usize, self.stack_top.add(24) as *mut usize, 1);

            self.stack_top = self.stack_top.add(32);
        }
    }


    pub fn run(&mut self) -> InterpResult {
        let t = nano_time!();
        self.stack_top = self.stack.as_mut_ptr();
        let init_frame = self.perm.init_frame();
        self.push_init_frame(init_frame);

        //let mut i = 0;

        'outer: loop {
            // i += 1;
            // if i > 1000 {
            //     break;
            // }

            match self.read_op_code() {
                OpCode::Exit => {

                    // self.print_stack();
                    // let val = self.pop_i64();
                    // println!("val: {}", val);
                    println!("Exec time: {} ns", nano_time!() - t);
                    break;
                }

                OpCode::ReturnVal => {
                    self.pop_stack_frame(true);
                }

                OpCode::Return => {
                    self.pop_stack_frame(false)
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
                    self.print_stack();
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    println!("Val1: {}, Val2: {}", val1, val2);
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

                    self.push_i64(val1.cmp(&val2) as i64)
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

                OpCode::JumpFwd => {
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
                OpCode::Pop => { self.pop_word(); }

                OpCode::Cons => {
                    unsafe {
                        let cell = self.heap.insert_pair(
                            self.stack_top.sub(8), // car
                            self.stack_top.sub(16), // cdr
                            8,
                            self.meta.types.pair,
                        );
                        self.stack_top = self.stack_top.sub(16); // "pop" items
                        self.push_ref(cell);
                    }
                }

                OpCode::Car => {
                    let pair_ref = self.pop_ref();
                    let meta = self.heap.get_item_meta(pair_ref);
                    if meta.typ != self.meta.types.pair {
                        panic!("car called on non-pair item")
                    }
                    // Only push first 8 bytes for car
                    self.push_arbitrary_bytes((meta.loc, 8))
                }

                OpCode::Cdr => {
                    unsafe {
                        let pair_ref = self.pop_ref();
                        let meta = self.heap.get_item_meta(pair_ref);
                        if meta.typ != self.meta.types.pair {
                            panic!("car called on non-pair item")
                        }
                        // Only push last 8 bytes for cdr
                        self.push_arbitrary_bytes((meta.loc.add(8), 8));
                    }
                }

                OpCode::NewArray => {
                    let typ = self.read_wide_inst();
                    let size = self.read_wide_inst() as usize * 8;
                    unsafe {
                        let ptr = self.stack_top.sub(size);
                        // Can copy directly off the stack as all value/refs will be on it ordered
                        let heap_ref = self.heap.insert_bytes(ptr, size, typ);
                        self.stack_top = ptr;
                        self.push_ref(heap_ref);
                    }
                }

                OpCode::Aacc => {
                    let arr_ref = self.pop_ref();
                    let index = self.pop_i64();
                    let arr_meta = self.heap.get_item_meta(arr_ref);
                    if index < 0 || (index * 8) as usize > arr_meta.size {
                        panic!("Invalid index for  access, index: {}, array length: {}", index, arr_meta.size / 8)
                    }
                    unsafe {
                        self.push_arbitrary_bytes((arr_meta.loc.add(index as usize * 8), 8))
                    }
                }

                OpCode::InvokeN => {
                    let ns = self.read_wide_inst() as usize;
                    let idx = self.read_wide_inst();
                    unsafe {
                        let frame = self.perm.namespaces.get_unchecked(ns).get_func(idx);
                        self.push_stack_frame(frame);
                    }
                }
                OpCode::InvokeC => {
                    todo!()
                }
                OpCode::LoadConstN => {
                    let ns = self.read_wide_inst();
                    let idx = self.read_inst() as u16;
                    self.load_ns_constant(ns, idx);
                }
                OpCode::LoadConstNWide => {
                    let ns = self.read_wide_inst();
                    let idx = self.read_wide_inst();
                    self.load_ns_constant(ns, idx);
                }
                OpCode::LoadConstL => {
                    let idx = self.read_inst() as u16;
                    self.load_local_constant(idx);
                }
                OpCode::LoadConstLWide => {
                    let idx = self.read_wide_inst();
                    self.load_local_constant(idx);
                }
                OpCode::LoadVarN => {
                    let ns = self.read_wide_inst();
                    let idx = self.read_wide_inst();
                    self.load_ns_var(ns, idx);
                }
                OpCode::LoadVarL => {
                    let idx = self.read_wide_inst();
                    self.load_local_var(idx);
                }
                OpCode::LoadVarC => {
                    todo!()
                }
                OpCode::StoreVarN => {
                    let ns = self.read_wide_inst();
                    let idx = self.read_wide_inst();
                    self.store_ns_var(ns, idx);
                }
                OpCode::StoreVarL => {
                    let idx = self.read_wide_inst();
                    self.store_local_var(idx);
                }
                OpCode::StoreVarC => {}
            }

            // println!("After Op Stack");
            // self.print_stack();
            // println!("Index: {}", self.stack_top_index())
        }

        InterpResult::Ok
    }
}

