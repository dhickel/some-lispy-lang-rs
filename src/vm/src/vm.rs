use crate::op_codes::{OpCode, TestStruct};


pub enum InterpResult {
    Ok,
    CompileErr(String),
    InterpErr(String),
}


#[derive(Debug)]
pub struct CompUnit {
    pub code: Vec<u8>,
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
        let mut ip = chunk.code.as_mut_ptr();
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


    pub fn push_to_stack<T: Copy>(&mut self, obj: T) {
        let obj_bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                &obj as *const T as *const u8,
                std::mem::size_of::<T>(),
            )
        };

        let new_stack_top = unsafe { self.stack_top.add(std::mem::size_of::<T>()) };
        if new_stack_top as usize > (self.stack.as_ptr() as usize + self.stack.len()) {
            panic!("Stack overflow");
        }

        unsafe {
            std::ptr::copy_nonoverlapping(obj_bytes.as_ptr(), self.stack_top, std::mem::size_of::<T>());
            self.stack_top = new_stack_top;
        }
    }

    pub fn test(&mut self) {
        let s = TestStruct {
            op_code: OpCode::OpReturn,
            int: 64,
        };


        match self.read_op_code() {
            OpCode::OpReturn => { println!("Found op return") },
            _ => { println!("Error reading op code") }
        }
    }


    pub fn interpret(&mut self, chunk: &'a mut CompUnit) -> InterpResult {
        self.ip = chunk.code.as_mut_ptr();
        self.comp_unit = chunk;
        InterpResult::Ok
    }

    pub fn run(&mut self) -> InterpResult {
        match self.read_op_code() {
            OpCode::OpReturn => { todo!()}
        }
    }
}