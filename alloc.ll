; create a threadlocal collecor handle

@gc_handle = thread_local(localexec) global ptr null, align 8

; declare DioGC__malloc_slowpath
declare noalias ptr addrspace(1) @DioGC__malloc_slowpath(
    i64 %size,
    i8 %obj_type,
    i64 %rsp,
    ptr %space
) allockind("alloc")

declare double @llvm.sqrt.f64(double %Val)

define double @sqrt_64(double %Val) {
    %1 = call double @llvm.sqrt.f64(double %Val)
    ret double %1
}

; define new DioGC__malloc
define ptr addrspace(1) @DioGC__malloc(i64 %size, i8 %obj_type, ptr %rsp) noinline optnone allockind("alloc") {
entry:
    ; if size > 7936, call slowpath
    %size_gt_7936 = icmp ugt i64 %size, 7936
    br i1 %size_gt_7936, label %call_slowpath, label %check_collector
check_collector:
    ; Load collector from gc_handle
    %collector_ptr = load ptr, ptr @gc_handle, align 8
    
    ; Check if collector_ptr is null
    %is_null = icmp eq ptr %collector_ptr, null
    br i1 %is_null, label %call_slowpath, label %fastpath_start


call_slowpath:
    %slowpath_result = call ptr addrspace(1) @DioGC__malloc_slowpath(i64 %size, i8 %obj_type, ptr %rsp, ptr @gc_handle)
    ret ptr addrspace(1) %slowpath_result
fastpath_start:    
    %thread_local_allocator_ptr = load ptr, ptr %collector_ptr, align 8

    ; get second field of collector, which is bytes_allocated_since_last_gc
    %bytes_allocated_since_last_gc_ptr = getelementptr i64, ptr %collector_ptr, i32 1

    ; Get thread_local_allocator (first field)
    %block = load ptr addrspace(1), ptr %thread_local_allocator_ptr, align 8
    
    ; Load block fields
    %cursor_ptr = getelementptr i64, ptr addrspace(1) %block, i32 0
    %cursor = load i64, ptr addrspace(1) %cursor_ptr, align 8

;     ; if cursor > 256, call slowpath
;     %cursor_gt_256 = icmp ugt i64 %cursor, 255
;     br i1 %cursor_gt_256, label %call_slowpath, label %load_block_fields_2

; load_block_fields_2:
    
    %next_hole_size_ptr = getelementptr i64, ptr addrspace(1) %block, i32 1
    %next_hole_size = load i64, ptr addrspace(1) %next_hole_size_ptr, align 8
    
    %available_line_num_ptr = getelementptr i64, ptr addrspace(1) %block, i32 2
    %available_line_num = load i64, ptr addrspace(1) %available_line_num_ptr, align 8
    
    
    ; Calculate line_size = (size - 1) / LINE_SIZE + 1
    ; LINE_SIZE is 128
    %size_minus_1 = sub i64 %size, 1
    %div = udiv i64 %size_minus_1, 128
    %line_size = add i64 %div, 1
    
    ; Check if fast path is possible (next_hole_size >= line_size)
    %fast_path_possible = icmp uge i64 %next_hole_size, %line_size
    br i1 %fast_path_possible, label %fast_path, label %call_slowpath

fast_path:
    ; Update available_line_num
    %new_available = sub i64 %available_line_num, %line_size
    store i64 %new_available, ptr addrspace(1) %available_line_num_ptr, align 8
    
    ; Get line map pointer (after the first three fields)
    %line_map_ptr = getelementptr i64, ptr addrspace(1) %block, i32 3
    
    ; Mark lines as used and set object type for first line
    %first_line_ptr = getelementptr i8, ptr addrspace(1) %line_map_ptr, i64 %cursor
    ; Set object type and mark as used (obj_type << 2 | 0b10000001)
    %shifted_type = shl i8 %obj_type, 2
    %header_val = or i8 %shifted_type, 129 ; 129 = 0b10000001
    store i8 %header_val, ptr addrspace(1) %first_line_ptr, align 1
    
    ; Check if line_size is 1
    %is_one_line = icmp eq i64 %line_size, 1
    br i1 %is_one_line, label %finish_fast_path, label %mark_lines_start

mark_lines_start:
    ; Mark remaining lines as used (0b00000001)
    %next_cursor = add i64 %cursor, 1
    %end_cursor = add i64 %cursor, %line_size
    br label %mark_lines

mark_lines:
    %current = phi i64 [ %next, %mark_lines ], [ %next_cursor, %mark_lines_start ]
    %line_ptr = getelementptr i8, ptr addrspace(1) %line_map_ptr, i64 %current
    store i8 1, ptr addrspace(1) %line_ptr, align 1
    %next = add i64 %current, 1
    %continue = icmp ult i64 %next, %end_cursor
    br i1 %continue, label %mark_lines, label %finish_fast_path

finish_fast_path:
    ; Calculate return address (block + cursor * LINE_SIZE)
    %base_offset = mul i64 %cursor, 128
    %result_addr = getelementptr i8, ptr addrspace(1) %block, i64 %base_offset
    
    ; Update cursor
    %new_cursor = add i64 %cursor, %line_size
    store i64 %new_cursor, ptr addrspace(1) %cursor_ptr, align 8
    
    ; Update next_hole_size
    %new_hole_size = sub i64 %next_hole_size, %line_size
    store i64 %new_hole_size, ptr addrspace(1) %next_hole_size_ptr, align 8
    
    ; Update bytes_allocated_since_last_gc (the size should be line_size * 128)
    %bytes_allocated_since_last_gc = load i64, ptr %bytes_allocated_since_last_gc_ptr, align 8
    %size_128 = mul i64 %line_size, 128
    %new_bytes_allocated = add i64 %size_128, %bytes_allocated_since_last_gc
    store i64 %new_bytes_allocated, ptr %bytes_allocated_since_last_gc_ptr, align 8
    ret ptr addrspace(1) %result_addr
}


attributes #0 = { nounwind allockind("alloc") "gc-leaf-function" }
