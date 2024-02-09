pub const BLOCK_SIZE: usize = 32 * 1024;

pub const LINE_SIZE: usize = 128;

pub const NUM_LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

pub const MAX_SMALL_OBJECT_SIZE: usize = LINE_SIZE;

pub const MAX_MEDIUM_OBJECT_SIZE: usize = BLOCK_SIZE;

pub const EVA_BLOCK_PROPORTION: f64 = 0.025;

pub const ALIGN: usize = 4096;

/// When gc heap expand condition was met,
/// the new heap size would be current_threshold*THRESHOLD_PROPORTION
pub const THRESHOLD_PROPORTION: f64 = 2.0;

/// Controls the frequency of gc triggerance & heap expantion
///
/// ## Collect condition
///
/// our gc collects in two cases:
/// - whenever OOM
/// - when heap used memory size reached threshold
/// You can think `threshold` as sort of `current_heap_size`,
/// as heap memory will never overflow it.
///
/// To keep gc from collecting more and more frequent,
/// the threshold will expand on need --
/// if in a gc cycle, collected memory is less than
/// bytes_allocated_since_last_gc / [FREE_SPACE_DIVISOR],
/// we expand the heapsize according to [THRESHOLD_PROPORTION].
///
/// The idea is inspired by [BdwGC](https://github.com/ivmai/bdwgc/blob/master/docs/gcdescr.md#allocation)
///
/// ## Further works
///
/// TODO:
///
/// implement sort of heap shrink logic similar to expantion logic
pub const FREE_SPACE_DIVISOR: usize = 4;

pub const LLVM_GC_STRATEGY_NAME: &str = "plimmix";

pub const BIG_OBJ_ALIGN: usize = 128;
