type Span<'a> = nom_locate::LocatedSpan<&'a str>;
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
pub trait RangeTrait {
    fn range(&self) -> Range;
}
impl Pos {
    pub fn to(&self, end: Pos) -> Range {
        Range { start: *self, end }
    }
}

impl Range {
    pub fn new(start: Span, end: Span) -> Range {
        Range {
            start: Pos {
                line: start.location_line() as usize,
                column: start.get_column(),
                offset: start.location_offset(),
            },
            end: Pos {
                line: end.location_line() as usize,
                column: end.get_column(),
                offset: end.location_offset(),
            },
        }
    }
}
