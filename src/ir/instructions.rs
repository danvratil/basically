#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(i16),
    Print,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}