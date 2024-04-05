use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};
use parser::op_codes::{OpCode};
use std::time::{SystemTime, UNIX_EPOCH};
use parser::parser::CompUnit;

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






#[derive(Debug)]
pub struct Vm<'a> {
    comp_unit: &'a CompUnit,
    ip: *mut u8,
    stack: [u8; 1048576],
    stack_top: *mut u8,
}


impl<'a> Vm<'a> {
    pub fn new(chunk: &'a mut CompUnit) -> Self {
        let ip = chunk.code.as_mut_ptr();
        let mut vm = Vm {
            comp_unit: chunk,
            ip,
            stack: [0; 1048576],
            stack_top: std::ptr::null_mut(),
        };
        vm.stack_top = vm.stack.as_mut_ptr();
        vm
    }

    fn read_op_code(&mut self) -> OpCode {
        unsafe {
            let code = *self.ip;
            self.ip = self.ip.add(1);
            std::mem::transmute(code)
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

    fn push_constant(&mut self, index: usize, size: usize) {
        unsafe {
            let data_ptr = self.comp_unit.constants.as_ptr().add(index);
            let new_stack_top = self.stack_top.add(size);
            std::ptr::copy_nonoverlapping(data_ptr, self.stack_top, size);
            self.stack_top = new_stack_top;
        }
    }

    fn push_bytes(&mut self, bytes: [u8; 8]) {
        unsafe {
            let new_stack_top = self.stack_top.add(8);
            let byte_ptr = bytes.as_ptr();
            std::ptr::copy_nonoverlapping(byte_ptr, self.stack_top, 8);
            self.stack_top = new_stack_top;
        }
    }

    fn push_i64(&mut self, value: i64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_bytes(bytes);
        }
    }

    fn push_f64(&mut self, value: f64) {
        unsafe {
            let bytes: [u8; 8] = std::mem::transmute(value);
            self.push_bytes(bytes);
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

    fn pop_bytes(&mut self, n: usize) -> &[u8] {
        unsafe {
            self.stack_top = self.stack_top.offset(-(n as isize));
            std::slice::from_raw_parts(self.stack_top, n)
        }
    }

    pub fn pop_f64(&mut self) -> f64 {
        unsafe {
            let bytes = self.pop_bytes(std::mem::size_of::<f64>());
            let value = std::ptr::read(bytes.as_ptr() as *const f64);
            value
        }
    }

    pub fn pop_i64(&mut self) -> i64 {
        unsafe {
            let bytes = self.pop_bytes(std::mem::size_of::<i64>());
            let value = std::ptr::read(bytes.as_ptr() as *const i64);
            value
        }
    }

    fn pop_bool(&mut self) -> bool {
        let val = self.pop_i64();
        val != 0
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

    fn print_value<T>(&self, value: &T) where T: Debug {
        println!("Value: {:?}", value)
    }

    pub fn interpret(&mut self, chunk: &'a mut CompUnit) -> InterpResult {
        self.ip = chunk.code.as_mut_ptr();
        self.comp_unit = chunk;
        InterpResult::Ok
    }

    //
    pub fn run(&mut self) -> InterpResult {
        let t = nano_time!();
        loop {
            match self.read_op_code() {
                OpCode::Exit => {
                    break;
                }

                OpCode::RtnI64 => {
                    let val = self.pop_i64();
                    self.print_value(&val);
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
                    println!("{:?}", val)
                }

                OpCode::Ldc => {
                    let index = self.read_inst() as usize;
                    self.push_constant(index, 8);
                }

                OpCode::LdcW => {
                    let index = self.read_wide_inst() as usize;
                    self.push_constant(index, 8);
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

                OpCode::CompOr => {
                    let val1 = self.pop_bool();
                    let val2 = self.pop_bool();
                    self.push_bool(val1 || val2)
                }

                OpCode::CompAnd => {
                    let val1 = self.pop_bool();
                    let val2 = self.pop_bool();
                    self.push_bool(val1 && val2);
                }

                OpCode::CompNot => {
                    let val = self.pop_bool() == false;
                    self.push_bool(val)
                }
            }
        }

        InterpResult::Ok
    }
}


#[cfg(test)]
mod tests {
    use crate::vm::{CompUnit, Vm};
    use super::*;


    #[test]
    fn test_wide_rw() {
        let value = 24_031_u16;

        let mut chunk = CompUnit {
            code: Vec::<u8>::new(),
            constants: Vec::<u8>::new(),
        };


        chunk.write_wide_inst(value);
        let mut vm = Vm::new(&mut chunk);
        let rtn_value = vm.read_wide_inst();
        assert_eq!(value, rtn_value)
    }


    #[test]
    fn test_stack_values() {
        let mut chunk = CompUnit {
            code: Vec::<u8>::new(),
            constants: Vec::<u8>::new(),
        };

        let value1 = 23423423423_i64;
        let value2 = 3423.34234234_f64;

        let idx1 = chunk.push_constant(&value1);
        let idx2 = chunk.push_constant(&value2);

        let mut vm = Vm::new(&mut chunk);

        vm.push_constant(idx1, 8);
        vm.push_constant(idx2, 8);

        let value2_rtn = vm.pop_f64();
        let value1_rtn = vm.pop_i64();

        assert_eq!(value1, value1_rtn);
        assert_eq!(value2, value2_rtn);
    }
}

