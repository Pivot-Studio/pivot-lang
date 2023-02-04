use crate::consts::ALIGN;

// big obj struct
#[derive(Debug)]
pub struct BigObj {
    /// |                       BIG OBJRCT HEADER(1 byte)                               |
    /// |    7    |    6    |    5    |    4    |    3    |    2    |    1    |    0    |
    /// |               not used                |    object type    |  marked | not used|
    pub header: BigObjHeader,
    pub size: usize,
}

type BigObjHeader = u8;

impl BigObj {
    /// Create a new block.
    ///
    /// at must be a `ALIGN` aligned pointer.
    pub fn new(at: *mut u8, size: usize) -> &'static mut Self {
        unsafe {
            let ptr = at as *mut Self;
            debug_assert!(ptr as usize % ALIGN == 0);
            debug_assert!(size % ALIGN == 0);
            ptr.write(Self {
                header: 0b10000000,
                size, // size of [[ obj_st |     data     ]]
            });
            &mut *ptr
        }
    }

    pub fn get_size(&self) -> usize {
        self.size
    }
    pub fn reset_header(&mut self) {
        self.header = 0b10000000;
        self.size = 0;
    }
}
