use lsp_types::Position;

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

/// # Pos
/// source code position in file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Hash)]
pub struct Pos {
    pub line: usize,   // 1based
    pub column: usize, // 1based
    pub offset: usize, // 0based
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}
impl Pos {
    pub fn to(&self, end: Pos) -> Range {
        Range { start: *self, end }
    }
    pub fn from(&self, start: Pos) -> Range {
        Range { start, end: *self }
    }
    pub fn is_in(&self, range: Range) -> bool {
        ((self.line == range.start.line && self.column >= range.start.column)
            || self.line > range.start.line)
            && ((self.line == range.end.line && self.column <= range.end.column)
                || self.line < range.end.line)
    }

    pub fn from_diag_pos(pos: &Position) -> Self {
        Self {
            line: pos.line as usize + 1,
            column: pos.character as usize + 1,
            offset: 0,
        }
    }
}

#[cfg(test)]
#[test]
fn test_pos_in() {
    let mut pos = Pos {
        line: 1,
        column: 1,
        offset: 0,
    };
    let range = Range {
        start: Pos {
            line: 1,
            column: 1,
            offset: 0,
        },
        end: Pos {
            line: 2,
            column: 2,
            offset: 1,
        },
    };
    assert!(pos.is_in(range));
    pos.column = 0;
    assert!(!pos.is_in(range));
    pos.column = 2;
    assert!(pos.is_in(range));
    pos.line = 2;
    assert!(pos.is_in(range));
    pos.column = 3;
    assert!(!pos.is_in(range));
}

/// # Range
/// source code range in file
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

    pub fn to_diag_range(&self) -> lsp_types::Range {
        if self.start.line < 1 {
            return Default::default();
        }
        lsp_types::Range {
            start: lsp_types::Position {
                line: self.start.line as u32 - 1,
                character: self.start.column as u32 - 1,
            },
            end: lsp_types::Position {
                line: self.end.line as u32 - 1,
                character: self.end.column as u32 - 1,
            },
        }
    }
    pub fn from_diag_range(range: lsp_types::Range) -> Range {
        Range {
            start: Pos {
                line: range.start.line as usize + 1,
                column: range.start.character as usize + 1,
                offset: 0,
            },
            end: Pos {
                line: range.end.line as usize + 1,
                column: range.end.character as usize + 1,
                offset: 0,
            },
        }
    }
}
