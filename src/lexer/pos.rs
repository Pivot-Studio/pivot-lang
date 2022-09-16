#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Pos {
    pub line: usize,   // 1based
    pub column: usize, // 1based
    pub offset: usize, // 0based
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}


impl Pos {
    pub fn to(&self, end: Pos) -> Range {
        Range { start: *self, end: end }
    }
}
