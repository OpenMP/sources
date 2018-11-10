! ******************************************************************
! Copyright (c) 1997-2018 OpenMP Architecture Review Board.        *
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
      integer ( omp_proc_bind_kind ) omp_proc_bind_master
      parameter ( omp_proc_bind_master = 2 )
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

      integer omp_depend_kind
      parameter ( omp_depend_kind = selected_int_kind( 10 ) )

      integer omp_pause_resource_kind
      parameter ( omp_pause_resource_kind = selected_int_kind( 8 ) )
      integer ( omp_pause_resource_kind ) omp_pause_soft
      parameter ( omp_pause_soft = 1 )
      integer ( omp_pause_resource_kind ) omp_pause_hard
      parameter ( omp_pause_hard = 2 )

      integer omp_event_handle_kind
      parameter ( omp_event_handle_kind = selected_int_kind( 8 ) )

      integer omp_memspace_handle_kind
      parameter ( omp_memspace_handle_kind = selected_int_kind( 8 ) )
      integer ( omp_memspace_handle_kind ) omp_default_mem_space
      parameter ( omp_default_mem_space = 0 )
      integer ( omp_memspace_handle_kind ) omp_large_cap_mem_space
      parameter ( omp_large_cap_mem_space = 1 )
      integer ( omp_memspace_handle_kind ) omp_const_mem_space
      parameter ( omp_const_mem_space = 2 )
      integer ( omp_memspace_handle_kind ) omp_high_bw_mem_space
      parameter ( omp_high_bw_mem_space = 3 )
      integer ( omp_memspace_handle_kind ) omp_low_lat_mem_space
      parameter ( omp_low_lat_mem_space = 4 )

      integer omp_allocator_handle_kind
      parameter ( omp_allocator_handle_kind = selected_int_kind( 8 ) )
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

      integer omp_alloctrait_val_kind
      parameter ( omp_alloctrait_val_kind = selected_int_kind( 10 ) )
      integer ( omp_alloctrait_val_kind ) omp_atv_false
      parameter ( omp_atv_false = 0 )
      integer ( omp_alloctrait_val_kind ) omp_atv_true
      parameter ( omp_atv_true = 1 )
      integer ( omp_alloctrait_val_kind ) omp_atv_default
      parameter ( omp_atv_default = 2 )
      integer ( omp_alloctrait_val_kind ) omp_atv_contended
      parameter ( omp_atv_contended = 3 )
      integer ( omp_alloctrait_val_kind ) omp_atv_uncontended
      parameter ( omp_atv_uncontended = 4 )
      integer ( omp_alloctrait_val_kind ) omp_atv_sequential
      parameter ( omp_atv_sequential = 5 )
      integer ( omp_alloctrait_val_kind ) omp_atv_private
      parameter ( omp_atv_private = 6 )
      integer ( omp_alloctrait_val_kind ) omp_atv_all
      parameter ( omp_atv_all = 7 )
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

      type omp_alloctrait
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
