! ******************************************************************
! Copyright (c) 1997-2021 OpenMP Architecture Review Board.        *
!                                                                  *
! Permission to copy without fee all or part of this material is   *
! granted, provided the OpenMP Architecture Review Board copyright *
! notice appears. Notice is given that copying is by permission of *
! the OpenMP Architecture Review Board.                            *
! ******************************************************************

module omp_lib_kinds
  integer, parameter :: omp_lock_kind = selected_int_kind( 10 )
  integer, parameter :: omp_nest_lock_kind = selected_int_kind( 10 )
  integer, parameter :: omp_sync_hint_kind = selected_int_kind( 10 )
! omp_lock_hint_kind and the related parameters have been deprecated
  integer, parameter :: omp_lock_hint_kind = selected_int_kind( 10 )

  integer (kind=omp_sync_hint_kind), parameter :: &
    omp_sync_hint_none = 0
  integer (kind=omp_lock_hint_kind), parameter :: &
    omp_lock_hint_none = omp_sync_hint_none
  integer (kind=omp_sync_hint_kind), parameter :: &
    omp_sync_hint_uncontended = 1
  integer (kind=omp_lock_hint_kind), parameter :: &
    omp_lock_hint_uncontended = omp_sync_hint_uncontended
  integer (kind=omp_sync_hint_kind), parameter :: &
    omp_sync_hint_contended = 2
  integer (kind=omp_lock_hint_kind), parameter :: &
    omp_lock_hint_contended = omp_sync_hint_contended
  integer (kind=omp_sync_hint_kind), parameter :: &
    omp_sync_hint_nonspeculative = 4
  integer (kind=omp_lock_hint_kind), parameter :: &
    omp_lock_hint_nonspeculative = omp_sync_hint_nonspeculative
  integer (kind=omp_sync_hint_kind), parameter :: &
    omp_sync_hint_speculative = 8
  integer (kind=omp_lock_hint_kind), parameter :: &
    omp_lock_hint_speculative = omp_sync_hint_speculative

  integer, parameter :: omp_sched_kind = selected_int_kind( 8 )
  integer(kind=omp_sched_kind), parameter :: omp_sched_static = 1
  integer(kind=omp_sched_kind), parameter :: omp_sched_dynamic = 2
  integer(kind=omp_sched_kind), parameter :: omp_sched_guided = 3
  integer(kind=omp_sched_kind), parameter :: omp_sched_auto = 4
  integer(kind=omp_sched_kind), parameter :: &
    omp_sched_monotonic = ishft(int(1, kind=omp_sched_kind), 31)

  integer, parameter :: omp_proc_bind_kind = selected_int_kind( 8 )
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_false = 0
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_true = 1
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_primary = 2
  integer (kind=omp_proc_bind_kind), parameter :: &
    omp_proc_bind_master = omp_proc_bind_primary ! (deprecated)
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_close = 3
  integer (kind=omp_proc_bind_kind), parameter :: omp_proc_bind_spread = 4

  integer, parameter :: omp_depend_kind = selected_int_kind( 10 )

  integer, parameter :: omp_pause_resource_kind = selected_int_kind( 8 )
  integer (kind=omp_pause_resource_kind), parameter :: omp_pause_soft = 1
  integer (kind=omp_pause_resource_kind), parameter :: omp_pause_hard = 2

  integer, parameter ( omp_initial_device = -1 )
  integer, parameter ( omp_invalid_device = -42 )

  integer, parameter :: omp_event_handle_kind = selected_int_kind( 8 )

  integer, parameter :: omp_memspace_handle_kind = selected_int_kind( 8 )
  integer (kind=omp_memspace_handle_kind), parameter :: &
    omp_default_mem_space = 0
  integer (kind=omp_memspace_handle_kind), parameter :: &
    omp_large_cap_mem_space = 1
  integer (kind=omp_memspace_handle_kind), parameter :: &
    omp_const_mem_space = 2
  integer (kind=omp_memspace_handle_kind), parameter :: &
    omp_high_bw_mem_space = 3
  integer (kind=omp_memspace_handle_kind), parameter :: &
    omp_low_lat_mem_space = 4

  integer, parameter :: omp_allocator_handle_kind = selected_int_kind( 8 )
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_null_allocator = 0
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_default_mem_alloc = -12
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_large_cap_mem_alloc = -11
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_const_mem_alloc = -10
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_high_bw_mem_alloc = -9
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_low_lat_mem_alloc = -8
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_cgroup_mem_alloc = -7
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_pteam_mem_alloc = -6
  integer (kind=omp_allocator_handle_kind), parameter :: &
    omp_thread_mem_alloc = -5

  integer, parameter :: omp_alloctrait_key_kind = selected_int_kind( 8 )
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_sync_hint = 1
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_alignment = 2
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_access = 3
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_pool_size = 4
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_fallback = 5
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_fb_data = 6
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_pinned = 7
  integer (kind=omp_alloctrait_key_kind), parameter :: &
    omp_atk_partition = 8

  integer, parameter :: omp_alloctrait_val_kind = selected_int_kind( 10 )
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_default = -1
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_false = 0
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_true = 1
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_contended = 3
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_uncontended = 4
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_serialized = 5
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_sequential = omp_atv_serialized ! (deprecated)
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_private = 6
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_all = 7
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_thread = 8
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_pteam = 9
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_cgroup = 10
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_default_mem_fb = 11
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_null_fb = 12
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_abort_fb = 13
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_allocator_fb = 14
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_environment = 15
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_nearest = 16
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_blocked = 17
  integer (kind=omp_alloctrait_val_kind), parameter :: &
    omp_atv_interleaved = 18

  type omp_alloctrait
    sequence
    integer (kind=omp_alloctrait_key_kind) :: key
    integer (kind=omp_alloctrait_val_kind) :: value
  end type omp_alloctrait

  integer, parameter :: omp_control_tool_kind = selected_int_kind( 8 )
  integer (kind=omp_control_tool_kind), parameter :: &
    omp_control_tool_start = 1
  integer (kind=omp_control_tool_kind), parameter :: &
    omp_control_tool_pause = 2
  integer (kind=omp_control_tool_kind), parameter :: &
    omp_control_tool_flush = 3
  integer (kind=omp_control_tool_kind), parameter :: &
    omp_control_tool_end = 4

  integer, parameter :: &
    omp_control_tool_result_kind = selected_int_kind( 8 )
  integer (kind=omp_control_tool_result_kind), parameter :: &
    omp_control_tool_notool = -2
  integer (kind=omp_control_tool_result_kind), parameter :: &
    omp_control_tool_nocallback = -1
  integer (kind=omp_control_tool_result_kind), parameter :: &
    omp_control_tool_success = 0
  integer (kind=omp_control_tool_result_kind), parameter :: &
    omp_control_tool_ignored = 1

end module omp_lib_kinds

module omp_lib

  use omp_lib_kinds

!                               OpenMP API v5.2
  integer, parameter :: openmp_version = 202111

  interface

    subroutine omp_set_num_threads (num_threads)
      integer, intent(in) :: num_threads
    end subroutine omp_set_num_threads

    function omp_get_num_threads ()
      integer :: omp_get_num_threads
    end function omp_get_num_threads

    function omp_get_max_threads ()
      integer :: omp_get_max_threads
    end function omp_get_max_threads

    function omp_get_thread_num ()
      integer :: omp_get_thread_num
    end function omp_get_thread_num

    function omp_get_num_procs ()
      integer :: omp_get_num_procs
    end function omp_get_num_procs

    function omp_in_parallel ()
      logical :: omp_in_parallel
    end function omp_in_parallel

    subroutine omp_set_dynamic (dynamic_threads)
      logical, intent(in) :: dynamic_threads
    end subroutine omp_set_dynamic

    function omp_get_dynamic ()
      logical :: omp_get_dynamic
    end function omp_get_dynamic

    function omp_get_cancellation ()
      logical :: omp_get_cancellation
    end function omp_get_cancellation

    subroutine omp_set_nested (nested) ! (deprecated)
      logical, intent(in) :: nested
    end subroutine omp_set_nested

    function omp_get_nested () ! (deprecated)
      logical :: omp_get_nested
    end function omp_get_nested

    subroutine omp_set_schedule (kind, chunk_size)
      use omp_lib_kinds
      integer(kind=omp_sched_kind), intent(in) :: kind
      integer, intent(in) :: chunk_size
    end subroutine omp_set_schedule

    subroutine omp_get_schedule (kind, chunk_size)
      use omp_lib_kinds
      integer(kind=omp_sched_kind), intent(out) :: kind
      integer, intent(out) :: chunk_size
    end subroutine omp_get_schedule

    function omp_get_thread_limit ()
      integer :: omp_get_thread_limit
    end function omp_get_thread_limit

    function omp_get_supported_active_levels ()
      integer :: omp_get_supported_active_levels
    end function omp_get_supported_active_levels

    subroutine omp_set_max_active_levels (max_levels)
      integer, intent(in) :: max_levels
    end subroutine omp_set_max_active_levels

    function omp_get_max_active_levels ()
      integer :: omp_get_max_active_levels
    end function omp_get_max_active_levels

    function omp_get_level ()
      integer :: omp_get_level
    end function omp_get_level

    function omp_get_ancestor_thread_num (level)
      integer, intent(in) :: level
      integer :: omp_get_ancestor_thread_num
    end function omp_get_ancestor_thread_num

    function omp_get_team_size (level)
      integer, intent(in) :: level
      integer :: omp_get_team_size
    end function omp_get_team_size

    function omp_get_active_level ()
      integer :: omp_get_active_level
    end function omp_get_active_level

    function omp_in_explicit_task()
      logical :: omp_in_explicit_task
    end function omp_in_explicit_task

    function omp_in_final ()
      logical :: omp_in_final
    end function omp_in_final

    function omp_get_proc_bind ()
      use omp_lib_kinds
      integer(kind=omp_proc_bind_kind) :: omp_get_proc_bind
    end function omp_get_proc_bind

    function omp_get_num_places ()
      integer :: omp_get_num_places
    end function omp_get_num_places

    function omp_get_place_num_procs (place_num)
      integer, intent(in) :: place_num
      integer :: omp_get_place_num_procs
    end function omp_get_place_num_procs

    subroutine omp_get_place_proc_ids (place_num, ids)
      integer, intent(in) :: place_num
      integer, intent(out) :: ids(*)
    end subroutine omp_get_place_proc_ids

    function omp_get_place_num ()
      integer :: omp_get_place_num
    end function omp_get_place_num

    function omp_get_partition_num_places ()
      integer :: omp_get_partition_num_places
    end function omp_get_partition_num_places

    subroutine omp_get_partition_place_nums (place_nums)
      integer, intent(out) :: place_nums(*)
    end subroutine omp_get_partition_place_nums

    subroutine omp_set_affinity_format (format)
      character(len=*), intent(in) :: format
    end subroutine omp_set_affinity_format

    function omp_get_affinity_format (buffer)
      character(len=*), intent(out) :: buffer
      integer :: omp_get_affinity_format
    end function omp_get_affinity_format

    subroutine omp_display_affinity (format)
      character(len=*), intent(in) :: format
    end subroutine omp_display_affinity

    function omp_capture_affinity (buffer, format)
      character(len=*), intent(out) :: buffer
      character(len=*), intent(in) :: format
      integer :: omp_capture_affinity
    end function omp_capture_affinity

    subroutine omp_display_env (verbose)
      logical, intent(in) :: verbose
    end subroutine omp_display_env

    subroutine omp_set_default_device (device_num)
      integer :: device_num
    end subroutine omp_set_default_device

    function omp_get_default_device ()
      integer :: omp_get_default_device
    end function omp_get_default_device

    function omp_get_num_devices ()
      integer :: omp_get_num_devices
    end function omp_get_num_devices

    function omp_get_device_num ()
      integer :: omp_get_device_num
    end function omp_get_device_num

    function omp_get_num_teams ()
      integer :: omp_get_num_teams
    end function omp_get_num_teams

    function omp_get_team_num ()
      integer :: omp_get_team_num
    end function omp_get_team_num

    function omp_is_initial_device ()
      logical :: omp_is_initial_device
    end function omp_is_initial_device

    function omp_get_initial_device ()
      integer :: omp_get_initial_device
    end function omp_get_initial_device

    function omp_get_max_task_priority ()
      integer :: omp_get_max_task_priority
    end function omp_get_max_task_priority

    function omp_pause_resource (kind, device_num)
      use omp_lib_kinds
      integer(kind=omp_pause_resource_kind) :: kind
      integer :: device_num
      integer :: omp_pause_resource
    end function omp_pause_resource

    function omp_pause_resource_all (kind)
      use omp_lib_kinds
      integer(kind=omp_pause_resource_kind) :: kind
      integer :: omp_pause_resource_all
    end function omp_pause_resource_all

    subroutine omp_set_num_teams (num_teams)
      integer :: num_teams
    end subroutine omp_set_num_teams

    function omp_get_max_teams ()
      integer :: omp_get_max_teams
    end function omp_get_max_teams

    subroutine omp_set_teams_thread_limit (thread_limit)
      integer :: thread_limit
    end subroutine omp_set_teams_thread_limit

    function omp_get_teams_thread_limit ()
      integer :: omp_get_teams_thread_limit
    end function omp_get_teams_thread_limit

    subroutine omp_init_lock (svar)
      use omp_lib_kinds
      integer(kind=omp_lock_kind), intent(out) :: svar
    end subroutine omp_init_lock

    subroutine omp_init_lock_with_hint (svar, hint)
      use omp_lib_kinds
      integer(kind=omp_lock_kind), intent(out) :: svar
      integer(kind=omp_sync_hint_kind), intent(in) :: hint
    end subroutine omp_init_lock_with_hint

    subroutine omp_destroy_lock (svar)
      use omp_lib_kinds
      integer(kind=omp_lock_kind), intent(inout) :: svar
    end subroutine omp_destroy_lock

    subroutine omp_set_lock (svar)
      use omp_lib_kinds
      integer(kind=omp_lock_kind), intent(inout) :: svar
    end subroutine omp_set_lock

    subroutine omp_unset_lock (svar)
      use omp_lib_kinds
      integer(kind=omp_lock_kind), intent(inout) :: svar
    end subroutine omp_unset_lock

    function omp_test_lock (svar)
      use omp_lib_kinds
      logical :: omp_test_lock
      integer(kind=omp_lock_kind), intent(inout) :: svar
    end function omp_test_lock

    subroutine omp_init_nest_lock (nvar)
      use omp_lib_kinds
      integer(kind=omp_nest_lock_kind), intent(out) :: nvar
    end subroutine omp_init_nest_lock

    subroutine omp_init_nest_lock_with_hint (nvar, hint)
      use omp_lib_kinds
      integer(kind=omp_nest_lock_kind), intent(out) :: nvar
      integer(kind=omp_sync_hint_kind), intent(in) :: hint
    end subroutine omp_init_nest_lock_with_hint

    subroutine omp_destroy_nest_lock (nvar)
      use omp_lib_kinds
      integer(kind=omp_nest_lock_kind), intent(inout) :: nvar
    end subroutine omp_destroy_nest_lock

    subroutine omp_set_nest_lock (nvar)
      use omp_lib_kinds
      integer(kind=omp_nest_lock_kind), intent(inout) :: nvar
    end subroutine omp_set_nest_lock

    subroutine omp_unset_nest_lock (nvar)
      use omp_lib_kinds
      integer(kind=omp_nest_lock_kind), intent(inout) :: nvar
    end subroutine omp_unset_nest_lock

    function omp_test_nest_lock (nvar)
      use omp_lib_kinds
      integer :: omp_test_nest_lock
      integer(kind=omp_nest_lock_kind), intent(inout) :: nvar
    end function omp_test_nest_lock

    function omp_get_wtick ()
      double precision :: omp_get_wtick
    end function omp_get_wtick

    function omp_get_wtime ()
      double precision :: omp_get_wtime
    end function omp_get_wtime

    subroutine omp_fulfill_event (event)
      use omp_lib_kinds
      integer(kind=omp_event_handle_kind) :: event
    end subroutine omp_fulfill_event

    function omp_target_alloc (size, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int
      type(c_ptr) :: omp_target_alloc
      integer(kind=c_size_t), value :: size
      integer(kind=c_int), value :: device_num
    end function omp_target_alloc

    subroutine omp_target_free (device_ptr, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      type(c_ptr), value :: device_ptr
      integer(kind=c_int), value :: device_num
    end subroutine omp_target_free

    function omp_target_is_present (ptr, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      integer(kind=c_int) :: omp_target_is_present
      type(c_ptr), value :: ptr
      integer(kind=c_int), value :: device_num
    end function omp_target_is_present

    function omp_target_is_accessible (ptr, size, device_num) bind(c)
      use, intrinsic :: iso_c_binding , only : c_ptr , c_size_t , c_int
      integer(kind=c_int) :: omp_target_is_accessible
      type(c_ptr), value :: ptr
      integer(kind=c_size_t), value :: size
      integer(kind=c_int), value :: device_num
    end function omp_target_is_accessible

    function omp_target_memcpy (dst, src, length, dst_offset, &
                                src_offset, dst_device_num, &
                                src_device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
      integer(kind=c_int) :: omp_target_memcpy
      type(c_ptr), value :: dst, src
      integer(kind=c_size_t), value :: length, dst_offset, src_offset
      integer(kind=c_int), value :: dst_device_num, src_device_num
    end function omp_target_memcpy

    function omp_target_memcpy_rect (dst,src,element_size, num_dims, &
                                     volume, dst_offsets, src_offsets, &
                                     dst_dimensions, src_dimensions, &
                                     dst_device_num, src_device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
      integer(kind=c_int) :: omp_target_memcpy_rect
      type(c_ptr), value :: dst, src
      integer(kind=c_size_t), value :: element_size
      integer(kind=c_int), value :: num_dims, dst_device_num, src_device_num
      integer(kind=c_size_t), intent(in) :: volume(*), dst_offsets(*), &
         src_offsets(*), dst_dimensions(*), src_dimensions(*)
    end function omp_target_memcpy_rect

    function omp_target_memcpy_async (dst, src, length, dst_offset, &
                                      src_offset, dst_device_num, &
                                      src_device_num, depobj_count, &
                                      depobj_list) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
      use omp_lib_kinds
      integer(kind=c_int) :: omp_target_memcpy_async
      type(c_ptr), value :: dst, src
      integer(kind=c_size_t), value :: length, dst_offset, src_offset
      integer(kind=c_int), value :: dst_device_num, src_device_num, depobj_count
      integer(kind=omp_depend_kind), optional :: depobj_list(*)
    end function omp_target_memcpy_async

    function omp_target_memcpy_rect_async (dst,src,element_size, num_dims, &
                                           volume, dst_offsets, src_offsets, &
                                           dst_dimensions, src_dimensions, &
                                           dst_device_num, src_device_num, &
                                           depobj_count, depobj_list) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int, c_size_t
      use omp_lib_kinds
      integer(kind=c_int) :: omp_target_memcpy_rect_async
      type(c_ptr), value :: dst, src
      integer(kind=c_size_t), value :: element_size
      integer(kind=c_int), value :: num_dims, dst_device_num, src_device_num, &
         depobj_count
      integer(kind=c_size_t), intent(in) :: volume(*), dst_offsets(*), &
         src_offsets(*), dst_dimensions(*), src_dimensions(*)
      integer(kind=omp_depend_kind), optional :: depobj_list(*)
    end function omp_target_memcpy_rect_async

    function omp_target_associate_ptr (host_ptr, device_ptr, size, &
                                       device_offset, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_int
      integer(kind=c_int) :: omp_target_associate_ptr
      type(c_ptr), value :: host_ptr, device_ptr
      integer(kind=c_size_t), value :: size, device_offset
      integer(kind=c_int), value :: device_num
    end function omp_target_associate_ptr

    function omp_get_mapped_ptr (ptr, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      type(c_ptr) :: omp_get_mapped_ptr
      type(c_ptr), value :: ptr
      integer(kind=c_int), value :: device_num
    end function omp_get_mapped_ptr

    function omp_target_disassociate_ptr (ptr, device_num) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_int
      integer(kind=c_int) :: omp_target_disassociate_ptr
      type(c_ptr), value :: ptr
      integer(kind=c_int), value :: device_num
    end function omp_target_disassociate_ptr

    function omp_init_allocator (memspace, ntraits, traits)
      use omp_lib_kinds
      integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
      integer, intent(in) :: ntraits
      type(omp_alloctrait), intent(in) :: traits(*)
      integer(kind=omp_allocator_handle_kind) :: omp_init_allocator
    end function omp_init_allocator

    subroutine omp_destroy_allocator (allocator)
      use omp_lib_kinds
      integer(kind=omp_allocator_handle_kind), intent(in) :: allocator
    end subroutine omp_destroy_allocator

    subroutine omp_set_default_allocator (allocator)
      use omp_lib_kinds
      integer(kind=omp_allocator_handle_kind), intent(in) :: allocator
    end subroutine omp_set_default_allocator

    function omp_get_default_allocator ()
      use omp_lib_kinds
      integer(kind=omp_allocator_handle_kind) :: omp_get_default_allocator
    end function omp_get_default_allocator

    function omp_alloc (size, allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
      use omp_lib_kinds
      type(c_ptr) :: omp_alloc
      integer(kind=c_size_t), value :: size
      integer(kind=omp_allocator_handle_kind), value :: allocator
    end function omp_alloc

    function omp_aligned_alloc (alignment, size, allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
      use omp_lib_kinds
      type(c_ptr) :: omp_aligned_alloc
      integer(kind=c_size_t), value :: alignment, size
      integer(kind=omp_allocator_handle_kind), value :: allocator
    end function omp_aligned_alloc

    subroutine omp_free (ptr, allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr
      use omp_lib_kinds
      type(c_ptr), value :: ptr
      integer(kind=omp_allocator_handle_kind), value :: allocator
    end subroutine omp_free

    function omp_calloc (nmemb, size, allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
      use omp_lib_kinds
      type(c_ptr) :: omp_calloc
      integer(kind=c_size_t), value :: nmemb, size
      integer(omp_allocator_handle_kind), value :: allocator
    end function omp_calloc

    function omp_aligned_calloc (alignment, nmemb, size, &
                                 allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
      use omp_lib_kinds
      type(c_ptr) :: omp_aligned_calloc
      integer(kind=c_size_t), value :: alignment, nmemb, size
      integer(omp_allocator_handle_kind), value :: allocator
    end function omp_aligned_calloc

    function omp_realloc (ptr, size, allocator, free_allocator) bind(c)
      use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t
      use omp_lib_kinds
      type(c_ptr) :: omp_realloc
      type(c_ptr), value :: ptr
      integer(kind=c_size_t), value :: size
      integer(omp_allocator_handle_kind), value :: allocator, free_allocator
    end function omp_realloc

    function omp_control_tool (command, modifier)
      use omp_lib_kinds
      integer :: omp_control_tool
      integer(kind=omp_control_tool_kind), intent(in) :: command
      integer, intent(in) :: modifier
    end function omp_control_tool

  end interface

end module omp_lib
