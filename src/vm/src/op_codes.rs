#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    Exit,
    RtnI64,
    RtnF64,
    RtnBool,
    RtnRef,
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
    NegF64,
    NegBool,
    I64ToF64,
    F64ToI64,
    ConstT,
    ConstF,
    CompI64,
    CompF64,
    CompOr,
    CompAnd,
    CompNot,
}

