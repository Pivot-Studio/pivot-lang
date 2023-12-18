pub const BLOCK_SIZE: usize = 32 * 1024;

pub const LINE_SIZE: usize = 128;

pub const NUM_LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

pub const MAX_SMALL_OBJECT_SIZE: usize = LINE_SIZE;

pub const MAX_MEDIUM_OBJECT_SIZE: usize = BLOCK_SIZE;

pub const EVA_BLOCK_PROPORTION: f64 = 0.025;

pub const ALIGN: usize = 4096;

pub const THRESHOLD_PROPORTION: f64 = 1000.75;

pub const LLVM_GC_STRATEGY_NAME: &str = "plimmix";

pub const BIG_OBJ_ALIGN: usize = 128;
