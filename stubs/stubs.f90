! ******************************************************************
! Copyright (c) 1997-2024 OpenMP Architecture Review Board.        *
!                                                                  *
! Permission to copy without fee all or part of this material is   *
! granted, provided the OpenMP Architecture Review Board copyright *
! notice appears. Notice is given that copying is by permission of *
! the OpenMP Architecture Review Board.                            *
! ******************************************************************

subroutine omp_set_num_threads(num_threads)
  integer num_threads
end subroutine

integer function omp_get_num_threads()
  omp_get_num_threads = 1
end function

integer function omp_get_max_threads()
  omp_get_max_threads = 1
end function

integer function omp_get_thread_num()
  omp_get_thread_num = 0
end function

integer function omp_get_num_procs()
  omp_get_num_procs = 1
end function

logical function omp_in_parallel()
  omp_in_parallel = .false.
end function

subroutine omp_set_dynamic(dynamic_threads)
  logical dynamic_threads
end subroutine

logical function omp_get_dynamic()
  omp_get_dynamic = .false.
end function

logical function omp_get_cancellation()
  omp_get_cancellation = .false.
end function

subroutine omp_set_nested(nested)
  logical nested
end subroutine

logical function omp_get_nested()
  omp_get_nested = .false.
end function

subroutine omp_set_schedule(kind, chunk_size)
  include 'omp_lib_kinds.h'
  integer(kind=omp_sched_kind) kind
  integer chunk_size
end subroutine

subroutine omp_get_schedule(kind, chunk_size)
  include 'omp_lib_kinds.h'
  integer(kind=omp_sched_kind) kind
  integer chunk_size
  kind = omp_sched_static
  chunk_size = 0
end subroutine

integer function omp_get_thread_limit()
  omp_get_thread_limit = 1
end function

integer function omp_get_supported_active_levels()
  omp_get_supported_active_levels = 1
end function

subroutine omp_set_max_active_levels(max_levels)
  integer max_levels
end subroutine

integer function omp_get_max_active_levels()
  omp_get_max_active_levels = 0
end function

integer function omp_get_level()
  omp_get_level = 0
end function

integer function omp_get_ancestor_thread_num(level)
  integer level
  if ( level .eq. 0 ) then
     omp_get_ancestor_thread_num = 0
  else
     omp_get_ancestor_thread_num = -1
  end if
end function

integer function omp_get_team_size(level)
  integer level
  if ( level .eq. 0 ) then
     omp_get_team_size = 1
  else
     omp_get_team_size = -1
  end if
end function

integer function omp_get_active_level()
  omp_get_active_level = 0
end function

logical function omp_in_explicit_task()
  omp_in_explicit_task = .false.
end function

logical function omp_in_final()
  omp_in_final = .true.
end function

logical function omp_is_free_agent()
  omp_is_free_agent = .false.
end function

logical function omp_ancestor_is_free_agent()
  omp_ancestor_is_free_agent = .false.
end function

function omp_get_proc_bind()
  include 'omp_lib_kinds.h'
  integer(kind=omp_proc_bind_kind) omp_get_proc_bind
  omp_get_proc_bind = omp_proc_bind_false
end function

integer function omp_get_num_places()
  omp_get_num_places = 0
end function

integer function omp_get_place_num_procs(place_num)
  integer place_num
  omp_get_place_num_procs = 0
end function

subroutine omp_get_place_proc_ids(place_num, ids)
  integer place_num
  integer ids(*)
end subroutine

integer function omp_get_place_num()
  omp_get_place_num = -1
end function

integer function omp_get_partition_num_places()
  omp_get_partition_num_places = 0
end function

subroutine omp_get_partition_place_nums(place_nums)
  integer place_nums(*)
end subroutine

subroutine omp_set_affinity_format(format)
  character(len=*), intent(in)::format
end subroutine

integer function omp_get_affinity_format(buffer)
  character(len=*), intent(out)::buffer
  omp_get_affinity_format = 0
  buffer = ''
end function

subroutine omp_display_affinity(format)
  character(len=*), intent(in)::format
end subroutine

integer function omp_capture_affinity(buffer,format)
  character(len=*), intent(out)::buffer
  character(len=*), intent(in)::format
  omp_capture_affinity = 0
  buffer = ''
end function

subroutine omp_display_env (verbose)
  logical, intent(in) :: verbose
  print *, "OPENMP DISPLAY ENVIRONMENT BEGIN"
  print *, "  _OPENMP = '202411'"
  print *, "  OMP_DYNAMIC = 'FALSE'"
  print *, "  OMP_NESTED = 'TRUE'"
  print *, "  OMP_NUM_THREADS = '1'"
  print *, "  OMP_SCHEDULE = 'DYNAMIC'"
  print *, "  OMP_PROC_BIND = 'FALSE'"
  print *, "  OMP_PLACES = ''"
  print *, "  OMP_STACKSIZE = '8192K'"
  print *, "  OMP_WAIT_POLICY = 'PASSIVE'"
  print *, "  OMP_THREAD_LIMIT = '1'"
  print *, "  OMP_MAX_ACTIVE_LEVELS = '1'"
  print *, "  OMP_CANCELLATION = 'FALSE'"
  print *, "  OMP_DEFAULT_DEVICE = '0'"
  print *, "  OMP_MAX_TASK_PRIORITY = '0'"
  print *, "  OMP_DISPLAY_AFFINITY = 'FALSE'"
  print *, "  OMP_AFFINITY_FORMAT = ''"
  print *, "  OMP_TARGET_OFFLOAD = 'DISABLED'"
  print *, "  OMP_TOOL = 'FALSE'"
  print *, "  OMP_TOOL_LIBRARIES = ''"
  print *, "  OMP_TOOL_VERBOSE_INIT = 'DISABLED'"
  print *, "  OMP_DEBUG = 'DISABLED'"
  print *, "  OMP_ALLOCATOR = 'OMP_DEFAULT_MEM_ALLOC'"
  print *, "  OMP_NUM_TEAMS = '1'"
  print *, "  OMP_TEAMS_THREAD_LIMIT = '1'"
  print *, "OPENMP DISPLAY ENVIRONMENT END"
end subroutine omp_display_env

subroutine omp_set_default_device(device_num)
  integer device_num
end subroutine

integer function omp_get_default_device()
  omp_get_default_device = 0
end function

integer function omp_get_num_devices()
  omp_get_num_devices = 0
end function

integer function omp_get_device_num()
  omp_get_device_num = -10
end function

integer function omp_get_num_teams()
  omp_get_num_teams = 1
end function

integer function omp_get_team_num()
  omp_get_team_num = 0
end function

logical function omp_is_initial_device()
  omp_is_initial_device = .true.
end function

integer function omp_get_initial_device()
  omp_get_initial_device = -10
end function

integer function omp_get_max_task_priority()
  omp_get_max_task_priority = 0
end function

integer function omp_pause_resource(kind, device_num)
  include 'omp_lib_kinds.h'
  integer(kind=omp_pause_resource_kind) kind
  integer device_num
  omp_pause_resource = -1
end function

integer function omp_pause_resource_all(kind)
  include 'omp_lib_kinds.h'
  integer(kind=omp_pause_resource_kind) kind
  omp_pause_resource_all = -1
end function

subroutine omp_set_num_teams (num_teams)
  integer :: num_teams
end subroutine omp_set_num_teams

integer function omp_get_max_teams ()
  omp_get_max_teams = 1
end function omp_get_max_teams

subroutine omp_set_teams_thread_limit (thread_limit)
  integer :: thread_limit
end subroutine omp_set_teams_thread_limit

integer function omp_get_teams_thread_limit ()
  omp_get_teams_thread_limit = 1
end function omp_get_teams_thread_limit

subroutine omp_init_lock(lock)
  ! lock is 0 if the simple lock is not initialized
  !        -1 if the simple lock is initialized but not set
  !         1 if the simple lock is set
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  lock = -1
end subroutine

subroutine omp_init_lock_with_hint(lock, hint)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock
  integer(kind=omp_sync_hint_kind) hint

  call omp_init_lock(lock)
end subroutine

subroutine omp_destroy_lock(lock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  lock = 0
end subroutine

subroutine omp_set_lock(lock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  if (lock .eq. -1) then
    lock = 1
  elseif (lock .eq. 1) then
    print *, 'error: deadlock in using lock variable'
    stop
  else
    print *, 'error: lock not initialized'
    stop
  endif
end subroutine

subroutine omp_unset_lock(lock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  if (lock .eq. 1) then
    lock = -1
  elseif (lock .eq. -1) then
    print *, 'error: lock not set'
    stop
  else
    print *, 'error: lock not initialized'
    stop
  endif
end subroutine

logical function omp_test_lock(lock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  if (lock .eq. -1) then
    lock = 1
    omp_test_lock = .true.
  elseif (lock .eq. 1) then
    omp_test_lock = .false.
  else
    print *, 'error: lock not initialized'
    stop
  endif
end function

subroutine omp_init_nest_lock(nlock)
  ! nlock is
  ! -1 if the nestable lock is initialized but not set
  ! 1 and more if the nestable lock is set (nest count)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  nlock = -1
end subroutine

subroutine omp_init_nest_lock_with_hint(nlock, hint)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock
  integer(kind=omp_sync_hint_kind) hint

  call omp_init_nest_lock(nlock)
end subroutine

subroutine omp_destroy_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock
  nlock = 0
end subroutine

subroutine omp_set_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  if (nlock .eq. -1) then
    nlock = 1
  else if (nlock .gt. 0) then
    nlock = nlock + 1
  else
    print *, 'error: nested lock corrupted or not initialized'
    stop
  endif
end subroutine

subroutine omp_unset_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  if (nlock .eq. 1) then
    nlock = -1
  elseif (nlock .gt. 1) then
    nlock = nlock - 1
  elseif (nlock .eq. -1) then
    print *, 'error: nested lock not set'
    stop
  else
    print *, 'error: nested lock corrupted or not initialized'
    stop
  endif
end subroutine

integer function omp_test_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  call omp_set_nest_lock(nlock)
  omp_test_nest_lock = nlock
end function

double precision function omp_get_wtime()
  ! this function does not provide a working
  ! wall clock timer. replace it with a version
  ! customized for the target machine.

  omp_get_wtime = 0.0d0
end function

double precision function omp_get_wtick()
  ! this function does not provide a working
  ! clock tick function. replace it with
  ! a version customized for the target machine.
  double precision one_year
  parameter (one_year=365.d0*86400.d0)

  omp_get_wtick = one_year
end function

subroutine omp_fulfill_event(event)
  include 'omp_lib_kinds.h'
  integer(kind=omp_event_handle_kind) event
end subroutine

! The omp_target_alloc, omp_target_free, omp_target_is_present,
! omp_target_is_accessible, omp_target_memcpy, omp_target_memcpy_rect,
! omp_target_memcpy_async, omp_target_memcpy_rect_async,
! omp_target_memset, omp_target_memset_async,
! omp_target_associate_ptr, omp_get_mapped_ptr,
! omp_target_disassociate_ptr, omp_alloc, omp_aligned_alloc,
! omp_free, omp_calloc, omp_aligned_calloc and omp_realloc APIs
! are defined in stubs.c in this implementation.

function omp_get_devices_memspace (ndevs, devs, memspace)
  use omp_lib_kinds
  integer(kind=omp_memspace_handle_kind) :: omp_get_devices_memspace
  integer, intent(in) :: ndevs
  integer, intent(in) :: devs(*)
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_devices_memspace = omp_null_mem_space
end function

function omp_get_device_memspace (dev, memspace)
  use omp_lib_kinds
  integer(kind=omp_memspace_handle_kind) :: omp_get_device_memspace
  integer, intent(in) :: dev
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_device_memspace = omp_null_mem_space
end function

function omp_get_devices_and_host_memspace (ndevs, devs, memspace)
  use omp_lib_kinds
  integer(kind=omp_memspace_handle_kind) :: omp_get_devices_and_host_memspace
  integer, intent(in) :: ndevs
  integer, intent(in) :: devs(*)
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_devices_and_host_memspace = omp_null_mem_space
end function

function omp_get_device_and_host_memspace (dev, memspace)
  use omp_lib_kinds
  integer(kind=omp_memspace_handle_kind) :: omp_get_device_and_host_memspace
  integer, intent(in) :: dev
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_device_and_host_memspace = omp_null_mem_space
end function

function omp_get_devices_all_memspace (memspace)
  use omp_lib_kinds
  integer(kind=omp_memspace_handle_kind) :: omp_get_devices_all_memspace
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_devices_all_memspace = omp_null_mem_space
end function

function omp_init_allocator(memspace, ntraits, traits)
  include 'omp_lib_kinds.h'
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  integer, intent(in) :: ntraits
  type(omp_alloctrait), intent(in) :: traits(*)
  integer(kind=omp_allocator_handle_kind) :: omp_init_allocator
  omp_init_allocator = omp_null_allocator
end function omp_init_allocator

subroutine omp_destroy_allocator(allocator)
  include 'omp_lib_kinds.h'
  integer(kind=omp_allocator_handle_kind), intent(in) :: allocator
end subroutine omp_destroy_allocator

subroutine omp_set_default_allocator(allocator)
  include 'omp_lib_kinds.h'
  integer(kind=omp_allocator_handle_kind) allocator
end subroutine

function omp_get_default_allocator()
  include 'omp_lib_kinds.h'
  integer(kind=omp_allocator_handle_kind) omp_get_default_allocator
  omp_get_default_allocator = omp_null_allocator
end function

function omp_get_memspace_num_resources (memspace)
  include 'omp_lib_kinds.h'
  integer omp_get_memspace_num_resources
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  omp_get_memspace_num_resources = 0
end function

function omp_get_submemspace (memspace, num_resources, resources)
  include 'omp_lib_kinds.h'
  integer(kind=omp_memspace_handle_kind) omp_get_submemspace
  integer(kind=omp_memspace_handle_kind), intent(in) :: memspace
  integer, intent(in):: num_resources
  integer, intent(in):: resources(*)
  omp_get_submemspace = omp_null_mem_space
end function

integer function omp_control_tool(command, modifier)
  include 'omp_lib_kinds.h'
  integer(kind=omp_control_tool_kind) command
  integer modifier
  omp_control_tool = omp_control_tool_notool
end function
