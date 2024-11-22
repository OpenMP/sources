! ******************************************************************
! Copyright (c) 1997-2024 OpenMP Architecture Review Board.        *
!                                                                  *
! Permission to copy without fee all or part of this material is   *
! granted, provided the OpenMP Architecture Review Board copyright *
! notice appears. Notice is given that copying is by permission of *
! the OpenMP Architecture Review Board.                            *
! ******************************************************************

!      the "!" of this comment starts in column 1
!23456

      integer omp_lock_kind
      integer omp_nest_lock_kind
! this selects an integer that is large enough to hold a 64 bit integer
      parameter ( omp_lock_kind = selected_int_kind( 10 ) )
      parameter ( omp_nest_lock_kind = selected_int_kind( 10 ) )

      integer omp_sched_kind
! this selects an integer that is large enough to hold a 32 bit integer
      parameter ( omp_sched_kind = selected_int_kind( 8 ) )
      integer ( omp_sched_kind ) omp_sched_static
      parameter ( omp_sched_static = 1 )
      integer ( omp_sched_kind ) omp_sched_dynamic
      parameter ( omp_sched_dynamic = 2 )
      integer ( omp_sched_kind ) omp_sched_guided
      parameter ( omp_sched_guided = 3 )
      integer ( omp_sched_kind ) omp_sched_auto
      parameter ( omp_sched_auto = 4 )
      integer ( omp_sched_kind ) omp_sched_monotonic
      parameter ( omp_sched_monotonic = ishft( int( 1, omp_sched_kind ),&
     & 31 ) )

      integer omp_proc_bind_kind
      parameter ( omp_proc_bind_kind = selected_int_kind( 8 ) )
      integer ( omp_proc_bind_kind ) omp_proc_bind_false
      parameter ( omp_proc_bind_false = 0 )
      integer ( omp_proc_bind_kind ) omp_proc_bind_true
      parameter ( omp_proc_bind_true = 1 )
      integer ( omp_proc_bind_kind ) omp_proc_bind_primary
      parameter ( omp_proc_bind_primary = 2 )
      integer ( omp_proc_bind_kind ) omp_proc_bind_master ! (deprecated)
      parameter ( omp_proc_bind_master = omp_proc_bind_primary )
      integer ( omp_proc_bind_kind ) omp_proc_bind_close
      parameter ( omp_proc_bind_close = 3 )
      integer ( omp_proc_bind_kind ) omp_proc_bind_spread
      parameter ( omp_proc_bind_spread = 4 )

! omp_lock_hint_kind and the related parameters have been deprecated
      integer omp_sync_hint_kind
      parameter ( omp_sync_hint_kind = selected_int_kind( 10 ) )
      integer omp_lock_hint_kind
      parameter ( omp_lock_hint_kind = omp_sync_hint_kind )
      integer ( omp_sync_hint_kind) omp_sync_hint_none
      parameter ( omp_sync_hint_none = 0 )
      integer ( omp_lock_hint_kind) omp_lock_hint_none
      parameter ( omp_lock_hint_none = omp_sync_hint_none )
      integer ( omp_sync_hint_kind) omp_sync_hint_uncontended
      parameter ( omp_sync_hint_uncontended = 1 )
      integer ( omp_lock_hint_kind) omp_lock_hint_uncontended
      parameter ( omp_lock_hint_uncontended = omp_sync_hint_uncontended &
     & )
      integer ( omp_sync_hint_kind) omp_sync_hint_contended
      parameter ( omp_sync_hint_contended = 2 )
      integer ( omp_lock_hint_kind) omp_lock_hint_contended
      parameter ( omp_lock_hint_contended = omp_sync_hint_contended )
      integer ( omp_sync_hint_kind) omp_sync_hint_nonspeculative
      parameter ( omp_sync_hint_nonspeculative = 4 )
      integer ( omp_lock_hint_kind) omp_lock_hint_nonspeculative
      parameter ( omp_lock_hint_nonspeculative =                        &
     & omp_sync_hint_nonspeculative )
      integer ( omp_sync_hint_kind) omp_sync_hint_speculative
      parameter ( omp_sync_hint_speculative = 8 )
      integer ( omp_lock_hint_kind) omp_lock_hint_speculative
      parameter ( omp_lock_hint_speculative = omp_sync_hint_speculative &
     & )

      integer omp_unassigned_thread
      parameter ( omp_unassigned_thread = -42 )

      integer omp_depend_kind
      parameter ( omp_depend_kind = selected_int_kind( 10 ) )

      integer omp_initial_device
      parameter ( omp_initial_device = -1 )
      integer omp_invalid_device
      parameter ( omp_invalid_device = -42 )

      integer omp_pause_resource_kind
      parameter ( omp_pause_resource_kind = selected_int_kind( 8 ) )
      integer ( omp_pause_resource_kind ) omp_pause_soft
      parameter ( omp_pause_soft = 1 )
      integer ( omp_pause_resource_kind ) omp_pause_hard
      parameter ( omp_pause_hard = 2 )
      integer ( omp_pause_resource_kind ) omp_pause_stop_tool
      parameter ( omp_pause_stop_tool = 3 )

      integer omp_event_handle_kind
      parameter ( omp_event_handle_kind = selected_int_kind( 8 ) )

      integer, parameter :: omp_impex_kind = selected_int_kind( 8 )
      integer (kind=omp_impex_kind), parameter :: omp_not_impex = 0
      integer (kind=omp_impex_kind), parameter :: omp_import = 1
      integer (kind=omp_impex_kind), parameter :: omp_export = 2
      integer (kind=omp_impex_kind), parameter :: omp_impex = 3

! Implementation-defined kind value, e.g.
      integer, parameter :: omp_interop_kind = selected_int_kind( 10 )

      integer (kind=omp_interop_kind) omp_interop_none
      parameter ( omp_interop_none = 0_omp_interop_kind )

! Implementation-defined kind value, e.g.
      integer, parameter :: omp_interop_fr_kind = selected_int_kind( 8 )

! Only omp_ifr_last is required by the OpenMP specification,
! the others are part of the additional definition document.
      integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda = 1
      integer (omp_interop_fr_kind) :: omp_ifr_cuda_driver
      parameter (omp_ifr_cuda_driver = 2)
      integer (omp_interop_fr_kind), parameter :: omp_ifr_opencl = 3
      integer (omp_interop_fr_kind), parameter :: omp_ifr_sycl = 4
      integer (omp_interop_fr_kind), parameter :: omp_ifr_hip = 5
      integer (omp_interop_fr_kind), parameter :: omp_ifr_level_zero = 6
      integer (omp_interop_fr_kind), parameter :: omp_ifr_hsa = 7
      integer (omp_interop_fr_kind), parameter :: omp_ifr_last = 7

! Implementation-defined kind value, e.g.
      integer :: omp_interop_property_kind
      parameter (omp_interop_property_kind = selected_int_kind( 8 ))

      integer (omp_interop_property_kind) omp_ipr_fr_id
      parameter (omp_ipr_fr_id = -1)
      integer (omp_interop_property_kind) omp_ipr_fr_name
      parameter (omp_ipr_fr_name = -2)
      integer (omp_interop_property_kind) omp_ipr_vendor
      parameter (omp_ipr_vendor = -3)
      integer (omp_interop_property_kind) omp_ipr_vendor_name
      parameter (omp_ipr_vendor_name = -4)
      integer (omp_interop_property_kind) omp_ipr_device_num
      parameter (omp_ipr_device_num = -5)
      integer (omp_interop_property_kind) omp_ipr_platform
      parameter (omp_ipr_platform = -6)
      integer (omp_interop_property_kind) omp_ipr_device
      parameter (omp_ipr_device = -7)
      integer (omp_interop_property_kind) omp_ipr_device_context
      parameter (omp_ipr_device_context = -8)
      integer (omp_interop_property_kind) omp_ipr_targetsync
      parameter (omp_ipr_targetsync = -9)
      integer (omp_interop_property_kind) omp_ipr_first
      parameter (omp_ipr_first = omp_ipr_targetsync)

! Implementation-defined kind value, e.g.
      integer, parameter :: omp_interop_rc_kind = selected_int_kind( 8 )
      integer (omp_interop_rc_kind), parameter :: omp_irc_no_value = 1
      integer (omp_interop_rc_kind), parameter :: omp_irc_success = 0
      integer (omp_interop_rc_kind), parameter :: omp_irc_empty = -1
      integer (omp_interop_rc_kind) omp_irc_out_of_range
      parameter (omp_irc_out_of_range = -2)
      integer (omp_interop_rc_kind), parameter :: omp_irc_type_int = -3
      integer (omp_interop_rc_kind), parameter :: omp_irc_type_ptr = -4
      integer (omp_interop_rc_kind), parameter :: omp_irc_type_str = -5
      integer (omp_interop_rc_kind), parameter :: omp_irc_other = -6

      integer omp_memspace_handle_kind
      parameter ( omp_memspace_handle_kind = selected_int_kind( 10 ) )
      integer ( omp_memspace_handle_kind ) omp_default_mem_space
      parameter ( omp_default_mem_space = -1 )
      integer ( omp_memspace_handle_kind ) omp_null_mem_space
      parameter ( omp_null_mem_space = 0 )
      integer ( omp_memspace_handle_kind ) omp_large_cap_mem_space
      parameter ( omp_large_cap_mem_space = 1 )
      integer ( omp_memspace_handle_kind ) omp_const_mem_space
      parameter ( omp_const_mem_space = 2 )
      integer ( omp_memspace_handle_kind ) omp_high_bw_mem_space
      parameter ( omp_high_bw_mem_space = 3 )
      integer ( omp_memspace_handle_kind ) omp_low_lat_mem_space
      parameter ( omp_low_lat_mem_space = 4 )

      integer omp_allocator_handle_kind
      parameter ( omp_allocator_handle_kind = selected_int_kind( 10 ) )
      integer ( omp_allocator_handle_kind ) omp_null_allocator
      parameter ( omp_null_allocator = 0 )
      integer ( omp_allocator_handle_kind ) omp_default_mem_alloc
      parameter ( omp_default_mem_alloc = -12 )
      integer ( omp_allocator_handle_kind ) omp_large_cap_mem_alloc
      parameter ( omp_large_cap_mem_alloc = -11 )
      integer ( omp_allocator_handle_kind ) omp_const_mem_alloc
      parameter ( omp_const_mem_alloc = -10 )
      integer ( omp_allocator_handle_kind ) omp_high_bw_mem_alloc
      parameter ( omp_high_bw_mem_alloc = -9 )
      integer ( omp_allocator_handle_kind ) omp_low_lat_mem_alloc
      parameter ( omp_low_lat_mem_alloc = -8 )
      integer ( omp_allocator_handle_kind ) omp_cgroup_mem_alloc
      parameter ( omp_cgroup_mem_alloc = -7 )
      integer ( omp_allocator_handle_kind ) omp_pteam_mem_alloc
      parameter ( omp_pteam_mem_alloc = -6 )
      integer ( omp_allocator_handle_kind ) omp_thread_mem_alloc
      parameter ( omp_thread_mem_alloc = -5 )

      integer omp_alloctrait_key_kind
      parameter ( omp_alloctrait_key_kind = selected_int_kind( 8 ) )
      integer ( omp_alloctrait_key_kind ) omp_atk_sync_hint
      parameter ( omp_atk_sync_hint = 1 )
      integer ( omp_alloctrait_key_kind ) omp_atk_alignment
      parameter ( omp_atk_alignment = 2 )
      integer ( omp_alloctrait_key_kind ) omp_atk_access
      parameter ( omp_atk_access = 3 )
      integer ( omp_alloctrait_key_kind ) omp_atk_pool_size
      parameter ( omp_atk_pool_size = 4 )
      integer ( omp_alloctrait_key_kind ) omp_atk_fallback
      parameter ( omp_atk_fallback = 5 )
      integer ( omp_alloctrait_key_kind ) omp_atk_fb_data
      parameter ( omp_atk_fb_data = 6 )
      integer ( omp_alloctrait_key_kind ) omp_atk_pinned
      parameter ( omp_atk_pinned = 7 )
      integer ( omp_alloctrait_key_kind ) omp_atk_partition
      parameter ( omp_atk_partition = 8 )
      integer( omp_alloctrait_key_kind ) omp_atk_pin_device
      parameter ( omp_atk_pin_device = 9 )
      integer( omp_alloctrait_key_kind ) omp_atk_preferred_device
      parameter ( omp_atk_preferred_device = 10 )
      integer( omp_alloctrait_key_kind ) omp_atk_device_access
      parameter ( omp_atk_device_access = 11 )
      integer( omp_alloctrait_key_kind ) omp_atk_target_access
      parameter ( omp_atk_target_access = 12 )
      integer( omp_alloctrait_key_kind ) omp_atk_atomic_scope
      parameter ( omp_atk_atomic_scope = 13 )
      integer ( omp_alloctrait_key_kind ) omp_atk_part_size
      parameter ( omp_atk_part_size = 14 )
      integer ( omp_alloctrait_key_kind ) omp_atk_partitioner
      parameter ( omp_atk_partitioner = 15 )
      integer ( omp_alloctrait_key_kind ) omp_atk_partitioner_arg
      parameter ( omp_atk_partitioner_arg = 16 )

      integer omp_alloctrait_val_kind
      parameter ( omp_alloctrait_val_kind = selected_int_kind( 10 ) )
      integer ( omp_alloctrait_val_kind ) omp_atv_default
      parameter ( omp_atv_default = -1 )
      integer ( omp_alloctrait_val_kind ) omp_atv_false
      parameter ( omp_atv_false = 0 )
      integer ( omp_alloctrait_val_kind ) omp_atv_true
      parameter ( omp_atv_true = 1 )
      integer ( omp_alloctrait_val_kind ) omp_atv_contended
      parameter ( omp_atv_contended = 3 )
      integer ( omp_alloctrait_val_kind ) omp_atv_uncontended
      parameter ( omp_atv_uncontended = 4 )
      integer ( omp_alloctrait_val_kind ) omp_atv_serialized
      parameter ( omp_atv_serialized = 5 )
      integer ( omp_alloctrait_val_kind ) omp_atv_sequential
! (deprecated)
      parameter ( omp_atv_sequential = omp_atv_serialized )
      integer ( omp_alloctrait_val_kind ) omp_atv_private
      parameter ( omp_atv_private = 6 )
      integer ( omp_alloctrait_val_kind ) omp_atv_device
      parameter ( omp_atv_device = 7 )
      integer ( omp_alloctrait_val_kind ) omp_atv_thread
      parameter ( omp_atv_thread = 8 )
      integer ( omp_alloctrait_val_kind ) omp_atv_pteam
      parameter ( omp_atv_pteam = 9 )
      integer ( omp_alloctrait_val_kind ) omp_atv_cgroup
      parameter ( omp_atv_cgroup = 10 )
      integer ( omp_alloctrait_val_kind ) omp_atv_default_mem_fb
      parameter ( omp_atv_default_mem_fb = 11 )
      integer ( omp_alloctrait_val_kind ) omp_atv_null_fb
      parameter ( omp_atv_null_fb = 12 )
      integer ( omp_alloctrait_val_kind ) omp_atv_abort_fb
      parameter ( omp_atv_abort_fb = 13 )
      integer ( omp_alloctrait_val_kind ) omp_atv_allocator_fb
      parameter ( omp_atv_allocator_fb = 14 )
      integer ( omp_alloctrait_val_kind ) omp_atv_environment
      parameter ( omp_atv_environment = 15 )
      integer ( omp_alloctrait_val_kind ) omp_atv_nearest
      parameter ( omp_atv_nearest = 16 )
      integer ( omp_alloctrait_val_kind ) omp_atv_blocked
      parameter ( omp_atv_blocked = 17 )
      integer ( omp_alloctrait_val_kind ) omp_atv_interleaved
      parameter ( omp_atv_interleaved = 18 )
      integer( omp_alloctrait_val_kind ) omp_atv_all
      parameter ( omp_atv_all = 19 )
      integer( omp_alloctrait_val_kind ) omp_atv_single
      parameter ( omp_atv_single = 20 )
      integer( omp_alloctrait_val_kind ) omp_atv_multiple
      parameter ( omp_atv_multiple = 21 )
      integer( omp_alloctrait_val_kind ) omp_atv_memspace
      parameter ( omp_atv_memspace = 22 )

! Whether omp_alloctrait derived type is provided in omp_lib.h
! is implementation defined.  The presence of it here could make
! include 'omp_lib.h'
! unusable in semi-strict Fortran 77 compliance compilations.
      type omp_alloctrait
        sequence
        integer ( omp_alloctrait_key_kind ) key
        integer ( omp_alloctrait_val_kind ) value
      end type omp_alloctrait

      integer omp_control_tool_kind
      parameter ( omp_control_tool_kind = selected_int_kind( 8 ) )
      integer ( omp_control_tool_kind ) omp_control_tool_start
      parameter ( omp_control_tool_start = 1 )
      integer ( omp_control_tool_kind ) omp_control_tool_pause
      parameter ( omp_control_tool_pause = 2 )
      integer ( omp_control_tool_kind ) omp_control_tool_flush
      parameter ( omp_control_tool_flush = 3 )
      integer ( omp_control_tool_kind ) omp_control_tool_end
      parameter ( omp_control_tool_end = 4 )
      integer omp_control_tool_result_kind
      parameter ( omp_control_tool_result_kind = selected_int_kind( 8 ) &
     & )
      integer ( omp_control_tool_result_kind ) omp_control_tool_notool
      parameter ( omp_control_tool_notool = -2 )
      integer ( omp_control_tool_result_kind )                          &
     & omp_control_tool_nocallback
      parameter ( omp_control_tool_nocallback = -1 )
      integer ( omp_control_tool_result_kind ) omp_control_tool_success
      parameter ( omp_control_tool_success = 0 )
      integer ( omp_control_tool_result_kind ) omp_control_tool_ignored
      parameter ( omp_control_tool_ignored = 1 )
