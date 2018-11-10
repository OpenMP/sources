subroutine omp_set_num_threads(num_threads)
  integer num_threads
  return
end subroutine

integer function omp_get_num_threads()
  omp_get_num_threads = 1
  return
end function

integer function omp_get_max_threads()
  omp_get_max_threads = 1
  return
end function

integer function omp_get_thread_num()
  omp_get_thread_num = 0
  return
end function

integer function omp_get_num_procs()
  omp_get_num_procs = 1
  return
end function

logical function omp_in_parallel()
  omp_in_parallel = .false.
  return
end function

subroutine omp_set_dynamic(dynamic_threads)
  logical dynamic_threads
  return
end subroutine

logical function omp_get_dynamic()
  omp_get_dynamic = .false.
  return
end function

logical function omp_get_cancellation()
  omp_get_cancellation = .false.
  return
end function

subroutine omp_set_nested(nested)
  logical nested
  return
end subroutine

logical function omp_get_nested()
  omp_get_nested = .false.
  return
end function

subroutine omp_set_schedule(kind, chunk_size)
  include 'omp_lib_kinds.h'
  integer (kind=omp_sched_kind) kind
  integer chunk_size
  return
end subroutine

subroutine omp_get_schedule(kind, chunk_size)
  include 'omp_lib_kinds.h'
  integer (kind=omp_sched_kind) kind
  integer chunk_size
  kind = omp_sched_static
  chunk_size = 0
  return
end subroutine

integer function omp_get_thread_limit()
  omp_get_thread_limit = 1
  return
end function

subroutine omp_set_max_active_levels(max_level)
  integer max_level
end subroutine

integer function omp_get_max_active_levels()
  omp_get_max_active_levels = 0
  return
end function

integer function omp_get_level()
  omp_get_level = 0
  return
end function

integer function omp_get_ancestor_thread_num(level)
  integer level
  if ( level .eq. 0 ) then
     omp_get_ancestor_thread_num = 0
  else
     omp_get_ancestor_thread_num = -1
  end if
  return
end function

integer function omp_get_team_size(level)
  integer level
  if ( level .eq. 0 ) then
     omp_get_team_size = 1
  else
     omp_get_team_size = -1
  end if
  return
end function

integer function omp_get_active_level()
  omp_get_active_level = 0
  return
end function

logical function omp_in_final()
  omp_in_final = .true.
  return
end function

function omp_get_proc_bind()
  include 'omp_lib_kinds.h'
  integer (kind=omp_proc_bind_kind) omp_get_proc_bind
  omp_get_proc_bind = omp_proc_bind_false
end function

integer function omp_get_num_places()
  return 0
end function

integer function omp_get_place_num_procs(place_num)
  integer place_num
  return 0
end function

subroutine omp_get_place_proc_ids(place_num, ids)
  integer place_num
  integer ids(*)
  return
end subroutine

integer function omp_get_place_num()
  return -1
end function

integer function omp_get_partition_num_places()
  return 0
end function

subroutine omp_get_partition_place_nums(place_nums)
  integer place_nums(*)
  return
end subroutine


subroutine omp_set_affinity_format(\plc{format})
   character(len=*),intent(in)::format
   return
end subroutine

integer function omp_get_affinity_format(buffer)
   character(len=*),intent(out)::buffer
   return 0
end function

subroutine omp_display_affinity(format)
   character(len=*),intent(in)::format
   return
end subroutine

integer function omp_capture_affinity(buffer,format)
   character(len=*),intent(out)::buffer
   character(len=*),intent(in)::format
   return 0
end function

subroutine omp_set_default_device(device_num)
  integer device_num
  return
end subroutine

integer function omp_get_default_device()
  omp_get_default_device = 0
  return
end function

integer function omp_get_num_devices()
  omp_get_num_devices = 0
  return
end function

integer function omp_get_device_num()
  omp_get_device_num = -10
  return
end function

integer function omp_get_num_teams()
  omp_get_num_teams = 1
  return
end function

integer function omp_get_team_num()
  omp_get_team_num = 0
  return
end function

logical function omp_is_initial_device()
  omp_is_initial_device = .true.
  return
end function

integer function omp_get_initial_device()
  omp_get_initial_device = -10
  return
end function

integer function omp_get_max_task_priority()
  omp_get_max_task_priority = 0
  return
end function

subroutine omp_init_lock(lock)
  ! lock is 0 if the simple lock is not initialized
  !        -1 if the simple lock is initialized but not set
  !         1 if the simple lock is set
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  lock = -1
  return
end subroutine

subroutine omp_init_lock_with_hint(lock, hint)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock
  integer(kind=omp_sync_hint_kind) hint

  call omp_init_lock(lock)
  return
end subroutine

subroutine omp_destroy_lock(lock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_lock_kind) lock

  lock = 0
  return
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
  return
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
  return
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

  return
end function

subroutine omp_init_nest_lock(nlock)
  ! nlock is
  ! 0 if the nestable lock is not initialized
  ! -1 if the nestable lock is initialized but not set
  ! 1 if the nestable lock is set
  ! no use count is maintained
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  nlock = -1

  return
end subroutine

subroutine omp_init_nest_lock_with_hint(nlock, hint)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock
  integer(kind=omp_sync_hint_kind) hint

  call omp_init_nest_lock(nlock)
  return
end subroutine

subroutine omp_destroy_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  nlock = 0

  return
end subroutine

subroutine omp_set_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  if (nlock .eq. -1) then
    nlock = 1
  elseif (nlock .eq. 0) then
    print *, 'error: nested lock not initialized'
    stop
  else
    print *, 'error: deadlock using nested lock variable'
    stop
  endif

  return
end subroutine

subroutine omp_unset_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  if (nlock .eq. 1) then
    nlock = -1
  elseif (nlock .eq. 0) then
    print *, 'error: nested lock not initialized'
    stop
  else
    print *, 'error: nested lock not set'
    stop
  endif

  return
end subroutine

integer function omp_test_nest_lock(nlock)
  include 'omp_lib_kinds.h'
  integer(kind=omp_nest_lock_kind) nlock

  if (nlock .eq. -1) then
    nlock = 1
    omp_test_nest_lock = 1
  elseif (nlock .eq. 1) then
    omp_test_nest_lock = 0
  else
    print *, 'error: nested lock not initialized'
    stop
  endif

  return
end function

double precision function omp_get_wtime()
  ! this function does not provide a working
  ! wall clock timer. replace it with a version
  ! customized for the target machine.

  omp_get_wtime = 0.0d0

  return
end function

double precision function omp_get_wtick()
  ! this function does not provide a working
  ! clock tick function. replace it with
  ! a version customized for the target machine.
  double precision one_year
  parameter (one_year=365.d0*86400.d0)

  omp_get_wtick = one_year

  return
end function

int function omp_control_tool(command, modifier)
  include 'omp_lib_kinds.h'
  integer (kind=omp_control_tool_kind) command
  integer (kind=omp_control_tool_kind) modifier

  return omp_control_tool_notool
end function

subroutine omp_set_default_allocator(allocator)
  include 'omp_lib_kinds.h'
  integer (kind=omp_allocator_kind) allocator
  return
end subroutine

function omp_get_default_allocator
  include 'omp_lib_kinds.h'
  integer (kind=omp_allocator_kind) omp_get_default_allocator
  omp_get_default_allocator = omp_null_allocator
end function
