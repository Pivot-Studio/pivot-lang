

declare ptr @gc_get_handle() readnone allockind("alloc") "gc-leaf-function"

; declare DioGC__malloc_slowpath
declare noalias ptr addrspace(1) @DioGC__malloc_slowpath_jit(
    i64 %size,
    i8 %obj_type,
    i64 %rsp
) allockind("alloc")


declare noalias void @DioGC__safepoint_ex(
    i64 %rsp,
    ptr %space
)


define noalias void @DioGC__safepoint(
    i64 %rsp) {
    
    %collector_ptr = call ptr @gc_get_handle()
    call void @DioGC__safepoint_ex(i64 %rsp, ptr %collector_ptr)
    ret void
}


declare noalias void @gc_set_handle(ptr %handle)

define void @gc_thread_init() {
    ret void
}

declare void @llvm.memset.p0.i64(ptr nocapture, i8, i64, i1) nounwind



declare double @llvm.sqrt.f64(double %Val)

define double @sqrt_64(double %Val) {
    %1 = call double @llvm.sqrt.f64(double %Val)
    ret double %1
}

; define new DioGC__malloc
define ptr addrspace(1) @DioGC__malloc(i64 %size, i8 %obj_type, i64 %rsp) noinline optnone allockind("alloc") {
entry:
    ; if size > 128, call slowpath
    ; call void @printi64ln(i64 2222)
    %size_gt_128 = icmp ugt i64 %size, 128
    br i1 %size_gt_128, label %call_slowpath, label %check_collector
check_collector:
    ; Load collector from gc_handle
    %collector_ptr = call ptr @gc_get_handle()
    
    ; Check if collector_ptr is null
    %is_null = icmp eq ptr %collector_ptr, null
    br i1 %is_null, label %call_slowpath, label %fastpath_start


call_slowpath:
    %innerrsp = tail call ptr asm alignstack "mov %rsp, $0", "=r"() #0
    %rspi = ptrtoint ptr %innerrsp to i64
    %slowpath_result = call ptr addrspace(1) @DioGC__malloc_slowpath_jit(i64 %size, i8 %obj_type, i64 %rspi)
    ; call void @printi64ln(i64 999)
    ; %slowpath_result_i = ptrtoint ptr addrspace(1) %slowpath_result to i64
    ; call void @printi64ln(i64 %slowpath_result_i)
    ret ptr addrspace(1) %slowpath_result
fastpath_start:    
    ; call void @printi64ln(i64 1)
    %thread_local_allocator_ptr = load ptr, ptr %collector_ptr, align 8

    ; ; get second field of collector, which is bytes_allocated_since_last_gc
    ; %bytes_allocated_since_last_gc_ptr = getelementptr i64, ptr %collector_ptr, i32 1

    ; Get thread_local_allocator (first field)
    %block = load ptr addrspace(1), ptr %thread_local_allocator_ptr, align 8

    ; check block is null
    %block_is_null = icmp eq ptr addrspace(1) %block, null
    br i1 %block_is_null, label %call_slowpath, label %load_block_fields
    
load_block_fields:
    ; Load block fields
    %cursor_ptr = getelementptr i64, ptr addrspace(1) %block, i32 0
    %cursor = load ptr addrspace(1), ptr addrspace(1) %cursor_ptr, align 8
    ; call void @printi64ln(i64 2)
;     ; if cursor > 256, call slowpath
;     %cursor_gt_256 = icmp ugt i64 %cursor, 255
;     br i1 %cursor_gt_256, label %call_slowpath, label %load_block_fields_2

; load_block_fields_2:
    
    %hole_end_ptr = getelementptr i64, ptr addrspace(1) %block, i32 1
    %hole_end = load ptr addrspace(1), ptr addrspace(1) %hole_end_ptr, align 8
    ; call void @printi64ln(i64 3)

    

    
    ; Calculate alloc size = (size + 7) / 8 * 8
    ; LINE_SIZE is 128
    %size_plus_7 = add i64 %size, 7
    %size_div_8 = lshr i64 %size_plus_7, 3
    %alloc_size = shl i64 %size_div_8, 3



    %cursor_i64 = ptrtoint ptr addrspace(1) %cursor to i64
    %hole_end_i64 = ptrtoint ptr addrspace(1) %hole_end to i64
    %hole_end_minus_cursor = sub i64 %hole_end_i64, %cursor_i64
    ; check if hole_end - cursor >= alloc_size
    %hole_end_minus_cursor_ge_alloc_size = icmp sge i64 %hole_end_minus_cursor, %alloc_size
    br i1 %hole_end_minus_cursor_ge_alloc_size, label %check_current_line, label %call_slowpath
    
check_current_line:
    ; Check if alloc in current line is possible
    ; let current_line_remains = self.cursor.align_offset(LINE_SIZE);
    %current_line_occupied = and i64 %cursor_i64, 127
    %current_line_remains = sub i64 128, %current_line_occupied

    
    ; call void @printi64ln(i64 %current_line_remains)
    ; call void @printi64ln(i64 %alloc_size)
    ; check if alloc_size <= current_line_remains && current_line_remains != 0
    %alloc_size_le_remains = icmp ule i64 %alloc_size, %current_line_remains
    %current_line_remains_ne_0 = icmp ne i64 %current_line_remains, 0
    %alloc_size_le_remains_and_ne_0 = and i1 %alloc_size_le_remains, %current_line_remains_ne_0
    br i1 %alloc_size_le_remains_and_ne_0, label %fast_path, label %check_remaining

check_remaining:
    ; Check if 128 <= hole_end - cursor
    %hole_end_minus_cursor_ge_128 = icmp uge i64 %hole_end_minus_cursor, 128

    ; self.cursor = self.cursor.add(current_line_remains);
    %new_cursor_i = add i64 %cursor_i64, %current_line_remains
    %new_cursor = inttoptr i64 %new_cursor_i to ptr addrspace(1)
    br i1 %hole_end_minus_cursor_ge_128, label %fast_path, label %call_slowpath


fast_path:
    ; phi get cursor
    %cursor_phi = phi ptr addrspace(1) [ %cursor, %check_current_line ], [ %new_cursor, %check_remaining ]

    %cursor_phi_i = ptrtoint ptr addrspace(1) %cursor_phi to i64
    ; Update cursor
    %new_cursor_after_alloc_i = add i64 %cursor_phi_i, %alloc_size
    %new_cursor_after_alloc = inttoptr i64 %new_cursor_after_alloc_i to ptr addrspace(1)
    store ptr addrspace(1) %new_cursor_after_alloc, ptr addrspace(1) %cursor_ptr, align 8
    ; call void @printi64ln(i64 4)
    ; call void @printi64ln(i64 %cursor_phi_i)
    ret ptr addrspace(1) %cursor_phi
}


attributes #0 = { nounwind allockind("alloc") "gc-leaf-function" }
!0 = !{}