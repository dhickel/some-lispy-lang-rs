use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};
use crate::op_codes::{OpCode, TestStruct};
use crate::operations;


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
pub struct CompUnit {
    pub code: Vec<u8>,
    pub constants: Vec<u8>,
}


impl CompUnit {
    pub fn write_inst(&mut self, val: u8) {
        self.code.push(val);
    }
    pub fn write_wide_inst(&mut self, val: u16) {
        self.code.push((val & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
    }

    pub fn add_constant(&mut self, bytes: &[u8]) -> usize {
        unsafe {
            let start_index = self.constants.len();
            let additional_len = bytes.len();
            self.constants.reserve(additional_len);

            let new_len = start_index + additional_len;
            let dst_ptr = self.constants.as_mut_ptr().add(start_index);
            let src_ptr = bytes.as_ptr();

            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, additional_len);
            self.constants.set_len(new_len);

            start_index
        }
    }

    pub fn add_fixed_size_const<T>(&mut self, value: &T) -> usize {
        unsafe {
            let size = std::mem::size_of::<T>();

            if size > 8 { panic!("Attempted to add constant greater than 8 bytes"); }

            let value_ptr = value as *const T as *const u8;
            let bytes_slice = std::slice::from_raw_parts(value_ptr, size);

            println!("bytes in: {:?}", bytes_slice);

            if size < 8 {
                let mut padded_bytes = vec![0u8; 8];
                padded_bytes[..size].copy_from_slice(bytes_slice);
                self.add_constant(&padded_bytes)
            } else {
                self.add_constant(bytes_slice)
            }
        }
    }
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

    fn push_bytes(&mut self, bytes: [u8; 8], size: usize) {
        unsafe {
            let new_stack_top = self.stack_top.add(size);
            let byte_ptr = bytes.as_ptr();
            std::ptr::copy_nonoverlapping(byte_ptr, self.stack_top, size);
            self.stack_top = new_stack_top;
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
            println!("bytes out f64: {:?}", bytes);
            let value = std::ptr::read(bytes.as_ptr() as *const f64);
            value
        }
    }

    // Pop bytes and transmute to i64
    pub fn pop_i64(&mut self) -> i64 {
        unsafe {
            let bytes = self.pop_bytes(std::mem::size_of::<i64>());
            println!("bytes out i64: {:?}", bytes);
            let value = std::ptr::read(bytes.as_ptr() as *const i64);
            value
        }
    }

    // fn print_stack_trace(&self) {
    //     unsafe {
    //         print!("          ");
    //         let mut slot = self.stack.as_ptr() as *const Value;
    //         while (slot as *const u8) < self.stack_top {
    //             let value = &*slot;
    //             println!("[ {:?} ]", value);
    //             slot = slot.add(1);
    //         }
    //     }
    // }

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
        loop {
            match self.read_op_code() {
                OpCode::Exit => {
                    break;
                }
                
                OpCode::Return => {
                    let val = self.pop_i64();
                    self.print_value(&val);
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
                    self.push_bytes((val1 + val2).to_ne_bytes(), 8)
                }
                
                OpCode::AddF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes((val1 + val2).to_ne_bytes(), 8)
                }
                
                OpCode::SubI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_bytes((val1 - val2).to_ne_bytes(), 8)
                }

                OpCode::SubF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes((val1 -val2).to_ne_bytes(), 8)
                }

                OpCode::MulI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_bytes((val1 *val2).to_ne_bytes(), 8)
                }
                
                OpCode::MulF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes((val1 * val2).to_ne_bytes(), 8)
                }
                
                OpCode::DivI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_bytes((val1 / val2).to_ne_bytes(), 8)
                }
                
                OpCode::DivF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes((val1 / val2).to_ne_bytes(), 8)
                }
                
                OpCode::PowI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_bytes(val1.pow(val2 as u32).to_ne_bytes(), 8)
                }
                
                OpCode::PowF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes(val1.powf(val2).to_ne_bytes(), 8)
                }
                
                OpCode::ModI64 => {
                    let val1 = self.pop_i64();
                    let val2 = self.pop_i64();
                    self.push_bytes((val1 % val2).to_ne_bytes(), 8)
                }
                
                OpCode::ModF64 => {
                    let val1 = self.pop_f64();
                    let val2 = self.pop_f64();
                    self.push_bytes((val1 % val2).to_ne_bytes(), 8)
                }

                OpCode::NegI64 => {
                    let val = self.pop_f64();
                    self.push_bytes(val.to_ne_bytes(), 8)
                }
                
                OpCode::NegF64 => {
                    let val = self.pop_i64();
                    self.push_bytes(val.to_ne_bytes(), 8)
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

        let idx1 = chunk.add_fixed_size_const(&value1);
        let idx2 = chunk.add_fixed_size_const(&value2);

        let mut vm = Vm::new(&mut chunk);

        vm.push_constant(idx1, 8);
        vm.push_constant(idx2, 8);

        let value2_rtn = vm.pop_f64();
        let value1_rtn = vm.pop_i64();

        assert_eq!(value1, value1_rtn);
        assert_eq!(value2, value2_rtn);
    }
}

