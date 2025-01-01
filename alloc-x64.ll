; create a threadlocal collecor handle

@gc_handle = thread_local(localexec) global ptr null, align 8

; declare DioGC__malloc_slowpath
declare noalias ptr addrspace(1) @DioGC__malloc_slowpath(
    i64 %size,
    i8 %obj_type,
    i64 %rsp,
    ptr %space
) allockind("alloc")


declare noalias void @DioGC__safepoint_ex(
    i64 %rsp,
    ptr %space
)


define noalias void @DioGC__safepoint(
    i64 %rsp) {
    %collector_ptr = load ptr, ptr @gc_handle, align 8, !invariant.load !0
    call void @DioGC__safepoint_ex(i64 %rsp, ptr %collector_ptr)
    ret void
}


declare noalias void @gc_set_handle(ptr %handle)

define void @gc_thread_init() {
    call void @gc_set_handle(ptr @gc_handle)
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
    ; if size > 7936, call slowpath
    ; call void @printi64ln(i64 2222)
    %size_gt_128 = icmp ugt i64 %size, 7936
    br i1 %size_gt_128, label %call_slowpath, label %check_collector
check_collector:
    ; Load collector from gc_handle
    %collector_ptr = load ptr, ptr @gc_handle, align 8, !invariant.load !0
    
    ; Check if collector_ptr is null
    %is_null = icmp eq ptr %collector_ptr, null
    br i1 %is_null, label %call_slowpath, label %fastpath_start


call_slowpath:
    %innerrsp = tail call ptr asm alignstack "mov %rsp, $0", "=r"() #0
    %rspi = ptrtoint ptr %innerrsp to i64
    %slowpath_result = call ptr addrspace(1) @DioGC__malloc_slowpath(i64 %size, i8 %obj_type, i64 %rspi, ptr @gc_handle)
    call void @llvm.memset.p1.i64(ptr addrspace(1) %slowpath_result, i8 0, i64 %size, i1 false)
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

    

    
    ; Calculate alloc size = (size + 7) / 8 * 8 + 8
    ; LINE_SIZE is 128
    %size_plus_7 = add i64 %size, 7
    %size_div_8 = lshr i64 %size_plus_7, 3
    %alloc_size_body = shl i64 %size_div_8, 3
    %alloc_size = add i64 %alloc_size_body, 8



    %cursor_i64 = ptrtoint ptr addrspace(1) %cursor to i64
    %hole_end_i64 = ptrtoint ptr addrspace(1) %hole_end to i64
    %hole_end_minus_cursor = sub i64 %hole_end_i64, %cursor_i64
    ; check if hole_end - cursor >= alloc_size
    %hole_end_minus_cursor_ge_alloc_size = icmp sge i64 %hole_end_minus_cursor, %alloc_size
    br i1 %hole_end_minus_cursor_ge_alloc_size, label %fast_path, label %call_slowpath
    

fast_path:

    ; set header
    ; 1. store zero to first bytt of %cursor
    store i8 0, ptr addrspace(1) %cursor
    ; 2. store obj_type to second byte of %cursor
    %cursor_obj_type_ptr = getelementptr i8, ptr addrspace(1) %cursor, i64 1
    store i8 %obj_type, ptr addrspace(1) %cursor_obj_type_ptr
    ; 3. store size (trunc to i16) to third and fourth byte of %cursor
    %cursor_size_ptr = getelementptr i16, ptr addrspace(1) %cursor, i64 1
    %size_cast = trunc i64 %alloc_size to i16
    store i16 %size_cast, ptr addrspace(1) %cursor_size_ptr
    ; 4. store %cursor's lower 32 bits to fifth to eighth byte of %cursor
    %cursor_i32 = trunc i64 %cursor_i64 to i32
    %cursor_i32_ptr = getelementptr i32, ptr addrspace(1) %cursor, i64 1
    store i32 %cursor_i32, ptr addrspace(1) %cursor_i32_ptr



    %cursor_phi_i = add i64 %cursor_i64, 8
    %cursor_phi = inttoptr i64 %cursor_phi_i to ptr addrspace(1)
    ; Update cursor
    %new_cursor_after_alloc_i = add i64 %cursor_i64, %alloc_size
    ; checi if new_cursor_after_alloc_i is zero
    %new_cursor_after_alloc_is_zero = icmp eq i64 %new_cursor_after_alloc_i, 0
    br i1 %new_cursor_after_alloc_is_zero, label %unreachable_path, label %update_cursor
unreachable_path:
    unreachable

update_cursor:
    %new_cursor_after_alloc = inttoptr i64 %new_cursor_after_alloc_i to ptr addrspace(1)
    store ptr addrspace(1) %new_cursor_after_alloc, ptr addrspace(1) %cursor_ptr, align 8
    ; call void @printi64ln(i64 4)
    ; call void @printi64ln(i64 %cursor_phi_i)
    call void @llvm.memset.p1.i64(ptr addrspace(1) %cursor_phi, i8 0, i64 %size, i1 false)
    ret ptr addrspace(1) %cursor_phi
}



attributes #0 = { nounwind allockind("alloc") "gc-leaf-function" }
!0 = !{}