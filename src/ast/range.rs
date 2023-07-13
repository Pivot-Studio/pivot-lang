use lsp_types::Position;

use super::diag::{ErrorCode, PLDiag, WarnCode};
use crate::nomparser::Span;

/// # Pos
/// source code position in file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Hash, PartialOrd)]
pub struct Pos {
    pub line: usize,   // 1based
    pub column: usize, // 1based
    pub offset: usize, // 0based
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, PartialOrd, Hash)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}

impl Ord for Range {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.line == other.line {
            self.column.cmp(&other.column)
        } else {
            self.line.cmp(&other.line)
        }
    }
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
    pub fn from_span(span: &Span) -> Self {
        Self {
            line: span.location_line() as usize,
            column: span.get_utf8_column(),
            offset: span.location_offset(),
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
    pub fn start_point(&self) -> Range {
        Range {
            start: self.start,
            end: self.start,
        }
    }
    pub fn end_point(&self) -> Range {
        Range {
            start: self.end,
            end: self.end,
        }
    }
    pub fn new(start: Span, end: Span) -> Range {
        Range {
            start: Pos {
                line: start.location_line() as usize,
                column: start.get_utf8_column(),
                offset: start.location_offset(),
            },
            end: Pos {
                line: end.location_line() as usize,
                column: end.get_utf8_column(),
                offset: end.location_offset(),
            },
        }
    }
    pub fn new_err(&self, err: ErrorCode) -> PLDiag {
        PLDiag::new_error(*self, err)
    }
    pub fn new_warn(&self, warn: WarnCode) -> PLDiag {
        PLDiag::new_warn(*self, warn)
    }

    pub fn to_diag_range(self) -> lsp_types::Range {
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
