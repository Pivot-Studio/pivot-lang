use dissimilar::Chunk;
use lsp_types::{Position, Range, TextEdit};
use rowan::{TextRange, TextSize};
pub fn diff(left: &str, right: &str) -> TextDiff {
    let chunks = dissimilar::diff(left, right);
    let mut builder = TextDiff::new();
    let mut pos = TextSize::default();
    let mut chunks = chunks.into_iter().peekable();
    while let Some(chunk) = chunks.next() {
        if let (Chunk::Delete(deleted), Some(&Chunk::Insert(inserted))) = (chunk, chunks.peek()) {
            chunks.next().unwrap();
            let deleted_len = TextSize::of(deleted);
            builder.replace(TextRange::at(pos, deleted_len), inserted.into());
            pos += deleted_len;
            continue;
        }

        match chunk {
            Chunk::Equal(text) => {
                pos += TextSize::of(text);
            }
            Chunk::Delete(deleted) => {
                let deleted_len = TextSize::of(deleted);
                builder.delete(TextRange::at(pos, deleted_len));
                pos += deleted_len;
            }
            Chunk::Insert(inserted) => {
                builder.insert(pos, inserted.into());
            }
        }
    }
    builder
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Indel {
    pub insert: String,
    pub delete: TextRange,
}
impl Indel {
    pub fn new_insert(offset: TextSize, text: String) -> Indel {
        Indel::new_replace(TextRange::empty(offset), text)
    }
    pub fn new_delete(range: TextRange) -> Indel {
        Indel::new_replace(range, String::new())
    }
    pub fn new_replace(range: TextRange, replace_with: String) -> Indel {
        Indel {
            delete: range,
            insert: replace_with,
        }
    }
}
#[derive(Default, Debug, Clone)]
pub struct TextDiff {
    indels: Vec<Indel>,
}
impl TextDiff {
    pub fn new() -> Self {
        Self { indels: vec![] }
    }
    pub fn replace(&mut self, range: TextRange, replace_with: String) {
        self.indels.push(Indel::new_replace(range, replace_with));
    }
    pub fn delete(&mut self, range: TextRange) {
        self.indels.push(Indel::new_delete(range));
    }
    pub fn insert(&mut self, offset: TextSize, text: String) {
        self.indels.push(Indel::new_insert(offset, text));
    }
    pub fn into_text_edit(self, line_index: &LineIndex) -> Vec<TextEdit> {
        self.indels
            .into_iter()
            .map(|indel| {
                let range = indel.delete;
                let (line_col_start, line_col_end) = (
                    line_index.line_col(range.start()),
                    line_index.line_col(range.end()),
                );
                let (start, end) = (
                    Position {
                        line: line_col_start.line,
                        character: line_col_start.col,
                    },
                    Position {
                        line: line_col_end.line,
                        character: line_col_end.col,
                    },
                );
                TextEdit::new(Range::new(start, end), indel.insert)
            })
            .collect()
    }
}
pub struct LineIndex {
    /// Offset the the beginning of each line, zero-based
    pub newlines: Vec<TextSize>,
}
impl LineIndex {
    pub fn new(text: &str) -> LineIndex {
        let mut newlines = Vec::with_capacity(16);
        newlines.push(TextSize::from(0));
        let mut curr_row = 0.into();
        let mut curr_col: TextSize = 0.into();
        for c in text.chars() {
            let c_len = TextSize::of(c);
            curr_row += c_len;
            if c == '\n' {
                newlines.push(curr_row);
                // Prepare for processing the next line
                curr_col = 0.into();
                continue;
            }
            curr_col += c_len;
        }
        LineIndex { newlines }
    }
    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let line = self.newlines.partition_point(|&it| it <= offset) - 1;
        let line_start_offset = self.newlines[line];
        let col = offset - line_start_offset;
        LineCol {
            line: line as u32,
            col: col.into(),
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LineCol {
    /// Zero-based
    pub line: u32,
    /// Zero-based
    pub col: u32,
}
