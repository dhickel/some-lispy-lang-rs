#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    OpReturn
}


#[repr(C)]
#[derive(Copy, Clone)]
pub struct TestStruct {
    pub op_code: OpCode,
    pub int: i64,
}


#[repr(C)]
pub struct SmallInst {
    op_code: OpCode,
}