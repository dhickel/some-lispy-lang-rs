#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    Exit,
    Return,
    Ldc,
    LdcW,
    AddI64,
    AddF64,
    SubI64,
    SubF64,
    MulI64,
    MulF64,
    DivI64,
    DivF64,
    PowI64,
    PowF64,
    ModI64,
    ModF64,
    NegI64,
    NegF64


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