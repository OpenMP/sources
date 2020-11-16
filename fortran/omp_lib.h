! ******************************************************************
! Copyright (c) 1997-2020 OpenMP Architecture Review Board.        *
!                                                                  *
! Permission to copy without fee all or part of this material is   *
! granted, provided the OpenMP Architecture Review Board copyright *
! notice appears. Notice is given that copying is by permission of *
! the OpenMP Architecture Review Board.                            *
! ******************************************************************

! default integer type assumed below
! default logical type assumed below
! OpenMP API v5.1

!      the "!" of this comment starts in column 1
!23456

      include 'omp_lib_kinds.h'
      integer openmp_version
      parameter ( openmp_version = 202011 )

      external omp_set_num_threads
      external omp_get_num_threads
      integer omp_get_num_threads
      external omp_get_max_threads
      integer omp_get_max_threads
      external omp_get_thread_num
      integer omp_get_thread_num
      external omp_get_num_procs
      integer omp_get_num_procs
      external omp_in_parallel
      logical omp_in_parallel
      external omp_set_dynamic
      external omp_get_dynamic
      logical omp_get_dynamic
      external omp_get_cancellation
      logical omp_get_cancellation
      external omp_set_nested ! (deprecated)
      external omp_get_nested ! (deprecated)
      logical omp_get_nested
      external omp_set_schedule
      external omp_get_schedule
      external omp_get_thread_limit
      integer omp_get_thread_limit
      external omp_get_supported_active_levels
      integer omp_get_supported_active_levels
      external omp_set_max_active_levels
      external omp_get_max_active_levels
      integer omp_get_max_active_levels
      external omp_get_level
      integer omp_get_level
      external omp_get_ancestor_thread_num
      integer omp_get_ancestor_thread_num
      external omp_get_team_size
      integer omp_get_team_size
      external omp_get_active_level
      integer omp_get_active_level
      external omp_set_affinity_format
      external omp_get_affinity_format
      integer omp_get_affinity_format
      external omp_display_affinity
      external omp_capture_affinity
      integer omp_capture_affinity
      external omp_display_env
      external omp_set_default_device
      external omp_get_default_device
      integer omp_get_default_device
      external omp_get_num_devices
      integer omp_get_num_devices
      external omp_get_device_num
      integer omp_get_device_num
      external omp_get_num_teams
      integer omp_get_num_teams
      external omp_get_team_num
      integer omp_get_team_num
      external omp_is_initial_device
      logical omp_is_initial_device
      external omp_get_initial_device
      integer omp_get_initial_device
      external omp_get_max_task_priority
      integer omp_get_max_task_priority
      external omp_pause_resource
      integer omp_pause_resource
      external omp_pause_resource_all
      integer omp_pause_resource_all
      external omp_set_num_teams
      external omp_get_max_teams
      integer omp_get_max_teams
      external omp_set_teams_thread_limit
      external omp_get_teams_thread_limit
      integer omp_get_teams_thread_limit

      external omp_in_final
      logical omp_in_final

      integer ( omp_proc_bind_kind ) omp_get_proc_bind
      external omp_get_proc_bind
      integer omp_get_num_places
      external omp_get_num_places
      integer omp_get_place_num_procs
      external omp_get_place_num_procs
      external omp_get_place_proc_ids
      integer omp_get_place_num
      external omp_get_place_num
      integer omp_get_partition_num_places
      external omp_get_partition_num_places
      external omp_get_partition_place_nums

      external omp_init_lock
      external omp_init_lock_with_hint
      external omp_destroy_lock
      external omp_set_lock
      external omp_unset_lock
      external omp_test_lock
      logical omp_test_lock

      external omp_init_nest_lock
      external omp_init_nest_lock_with_hint
      external omp_destroy_nest_lock
      external omp_set_nest_lock
      external omp_unset_nest_lock
      external omp_test_nest_lock
      integer omp_test_nest_lock

      external omp_get_wtick
      double precision omp_get_wtick
      external omp_get_wtime
      double precision omp_get_wtime

      external omp_fulfill_event

! Whether omp_target_alloc, omp_target_free, omp_target_is_present,
! omp_target_is_accessible, omp_target_memcpy, omp_target_memcpy_rect,
! omp_target_memcpy_async, omp_target_memcpy_rect_async,
! omp_target_associate_ptr, omp_get_mapped_ptr,
! omp_target_disassociate_ptr, omp_alloc, omp_aligned_alloc,
! omp_free, omp_calloc, omp_aligned_calloc and omp_realloc APIs
! are provided in omp_lib.h is implementation defined.
! Providing them might make
! include 'omp_lib.h'
! unusable in strict Fortran 77, 90 or 95 compliance modes.

      external omp_init_allocator
      integer ( omp_allocator_handle_kind ) omp_init_allocator
      external omp_destroy_allocator
      external omp_set_default_allocator
      external omp_get_default_allocator
      integer ( omp_allocator_handle_kind ) omp_get_default_allocator

      external omp_control_tool
      integer omp_control_tool
