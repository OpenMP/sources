/*******************************************************************
* Copyright (c) 1997-2020 OpenMP Architecture Review Board.        *
*                                                                  *
* Permission to copy without fee all or part of this material is   *
* granted, provided the OpenMP Architecture Review Board copyright *
* notice appears. Notice is given that copying is by permission of *
* the OpenMP Architecture Review Board.                            *
*******************************************************************/

#define _POSIX_C_SOURCE 200112L
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "omp.h"

#define OMP_STUBS_DEFAULT_DEVICE_NUM 0
#define OMP_STUBS_DEVICE_NUM 0
#define OMP_STUBS_NUM_DEVICES 0
#define OMP_STUBS_INITIAL_DEVICE_NUM OMP_STUBS_NUM_DEVICES

void omp_set_num_threads(int num_threads)
{
}

int omp_get_num_threads(void)
{
  return 1;
}

int omp_get_max_threads(void)
{
  return 1;
}

int omp_get_thread_num(void)
{
  return 0;
}

int omp_get_num_procs(void)
{
  return 1;
}

int omp_in_parallel(void)
{
  return 0;  /* false */
}

void omp_set_dynamic(int dynamic_threads)
{
}

int omp_get_dynamic(void)
{
  return 0;
}

int omp_get_cancellation(void)
{
  return 0;
}

void omp_set_nested(int nested)
{
}

int omp_get_nested(void)
{
  return 0;
}

void omp_set_schedule(omp_sched_t kind, int chunk_size)
{
}

void omp_get_schedule(omp_sched_t *kind, int *chunk_size)
{
  *kind = omp_sched_static;
  *chunk_size = 0;
}

int omp_get_thread_limit(void)
{
  return 1;
}

int omp_get_supported_active_levels(void)
{
  return 1;
}

void omp_set_max_active_levels(int max_levels)
{
}

int omp_get_max_active_levels(void)
{
  return 0;
}

int omp_get_level(void)
{
  return 0;
}

int omp_get_ancestor_thread_num(int level)
{
  if (level == 0)
  {
    return 0;
  }
  else
  {
    return -1;
  }
}

int omp_get_team_size(int level)
{
  if (level == 0)
  {
    return 1;
  }
  else
  {
    return -1;
  }
}

int omp_get_active_level(void)
{
  return 0;
}

int omp_in_final(void)
{
  return 1;   /* true */
}

omp_proc_bind_t omp_get_proc_bind(void)
{
  return omp_proc_bind_false;
}

int omp_get_num_places(void)
{
  return 0;
}

int omp_get_place_num_procs(int place_num)
{
  return 0;
}

void omp_get_place_proc_ids(int place_num, int *ids)
{
}

int omp_get_place_num(void)
{
  return -1;
}

int omp_get_partition_num_places(void)
{
  return 0;
}

void omp_get_partition_place_nums(int *place_nums)
{
}

void omp_set_affinity_format(const char *format)
{
}

size_t omp_get_affinity_format(char *buffer, size_t size)
{
  if (size)
    *buffer = 0;
  return 0;
}

void omp_display_affinity(const char *format)
{
}

size_t omp_capture_affinity(char *buffer, size_t size, const char *format)
{
  if (size)
    *buffer = 0;
  return 0;
}

void omp_display_env(int verbose)
{
  /* Just an example of possible values.  */
  fprintf(stderr, "\
OPENMP DISPLAY ENVIRONMENT BEGIN\n\
  _OPENMP = '202011'\n\
  OMP_DYNAMIC = 'FALSE'\n\
  OMP_NESTED = 'TRUE'\n\
  OMP_NUM_THREADS = '1'\n\
  OMP_SCHEDULE = 'DYNAMIC'\n\
  OMP_PROC_BIND = 'FALSE'\n\
  OMP_PLACES = ''\n\
  OMP_STACKSIZE = '8192K'\n\
  OMP_WAIT_POLICY = 'PASSIVE'\n\
  OMP_THREAD_LIMIT = '1'\n\
  OMP_MAX_ACTIVE_LEVELS = '1'\n\
  OMP_CANCELLATION = 'FALSE'\n\
  OMP_DEFAULT_DEVICE = '0'\n\
  OMP_MAX_TASK_PRIORITY = '0'\n\
  OMP_DISPLAY_AFFINITY = 'FALSE'\n\
  OMP_AFFINITY_FORMAT = ''\n\
  OMP_TARGET_OFFLOAD = 'DISABLED'\n\
  OMP_TOOL = 'FALSE'\n\
  OMP_TOOL_LIBRARIES = ''\n\
  OMP_TOOL_VERBOSE_INIT = 'DISABLED'\n\
  OMP_DEBUG = 'DISABLED'\n\
  OMP_ALLOCATOR = 'OMP_DEFAULT_MEM_ALLOC'\n\
  OMP_NUM_TEAMS = '1'\n\
  OMP_TEAMS_THREAD_LIMIT = '1'\n\
OPENMP DISPLAY ENVIRONMENT END\n");
}

void omp_set_default_device(int device_num)
{
}

int omp_get_default_device(void)
{
  return OMP_STUBS_DEFAULT_DEVICE_NUM;
}

int omp_get_num_devices(void)
{
  return OMP_STUBS_NUM_DEVICES;
}

int omp_get_device_num(void)
{
  return OMP_STUBS_DEVICE_NUM;
}

int omp_get_num_teams(void)
{
  return 1;
}

int omp_get_team_num(void)
{
  return 0;
}

int omp_is_initial_device(void)
{
  return 1;  /* true */
}

int omp_get_initial_device(void)
{
  return OMP_STUBS_INITIAL_DEVICE_NUM;
}

int omp_get_max_task_priority(void)
{
  return 0;
}

int omp_pause_resource(omp_pause_resource_t kind, int device_num)
{
  return ENOTSUP;
}

int omp_pause_resource_all(omp_pause_resource_t kind)
{
  return ENOTSUP;
}

void omp_set_num_teams(int num_teams)
{
}

int omp_get_max_teams(void)
{
  return 1;
}

void omp_set_teams_thread_limit(int thread_limit)
{
}

int omp_get_teams_thread_limit(void)
{
  return 1;
}

struct __omp_lock
{
  int lock;
};

enum { UNLOCKED = -1, INIT, LOCKED };

void omp_init_lock(omp_lock_t *arg)
{
  struct __omp_lock *lock = (struct __omp_lock *)arg;
  lock->lock = UNLOCKED;
}

void omp_init_lock_with_hint(omp_lock_t *arg, omp_sync_hint_t hint)
{
  omp_init_lock(arg);
}

void omp_destroy_lock(omp_lock_t *arg)
{
  struct __omp_lock *lock = (struct __omp_lock *)arg;
  lock->lock = INIT;
}

void omp_set_lock(omp_lock_t *arg)
{
  struct __omp_lock *lock = (struct __omp_lock *)arg;
  if (lock->lock == UNLOCKED)
  {
    lock->lock = LOCKED;
  }
  else if (lock->lock == LOCKED)
  {
    fprintf(stderr, "error: deadlock in using lock variable\n");
    exit(1);
  }

  else
  {
    fprintf(stderr, "error: lock not initialized\n");
    exit(1);
  }
}

void omp_unset_lock(omp_lock_t *arg)
{
  struct __omp_lock *lock = (struct __omp_lock *)arg;
  if (lock->lock == LOCKED)
  {
    lock->lock = UNLOCKED;
  }
  else if (lock->lock == UNLOCKED)
  {
    fprintf(stderr, "error: lock not set\n");
    exit(1);
  }
  else
  {
    fprintf(stderr, "error: lock not initialized\n");
    exit(1);
  }
}

int omp_test_lock(omp_lock_t *arg)
{
  struct __omp_lock *lock = (struct __omp_lock *)arg;
  if (lock->lock == UNLOCKED)
  {
    lock->lock = LOCKED;
    return 1;
  }
  else if (lock->lock == LOCKED)
  {
    return 0;
  }
  else
  {
    fprintf(stderr, "error: lock not initialized\n");
    exit(1);
  }
}

struct __omp_nest_lock
{
  short owner;
  short count;
};

enum { NOOWNER = -1, MASTER = 0 };

void omp_init_nest_lock(omp_nest_lock_t *arg)
{
  struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
  nlock->owner = NOOWNER;
  nlock->count = 0;
}

void omp_init_nest_lock_with_hint(
  omp_nest_lock_t *arg,
  omp_sync_hint_t hint
)
{
  omp_init_nest_lock(arg);
}

void omp_destroy_nest_lock(omp_nest_lock_t *arg)
{
  struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
  nlock->owner = NOOWNER;
  nlock->count = UNLOCKED;
}

void omp_set_nest_lock(omp_nest_lock_t *arg)
{
  struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
  if (nlock->owner == MASTER && nlock->count >= 1)
  {
    nlock->count++;
  }
  else if (nlock->owner == NOOWNER && nlock->count == 0)
  {
    nlock->owner = MASTER;
    nlock->count = 1;
  }
  else
  {
    fprintf(stderr, "error: lock corrupted or not initialized\n");
    exit(1);
  }
}

void omp_unset_nest_lock(omp_nest_lock_t *arg)
{
  struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
  if (nlock->owner == MASTER && nlock->count >= 1)
  {
    nlock->count--;
    if (nlock->count == 0)
    {
      nlock->owner = NOOWNER;
    }
  }
  else if (nlock->owner == NOOWNER && nlock->count == 0)
  {
    fprintf(stderr, "error: lock not set\n");
    exit(1);
  }
  else
  {
    fprintf(stderr, "error: lock corrupted or not initialized\n");
    exit(1);
  }
}

int omp_test_nest_lock(omp_nest_lock_t *arg)
{
  struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
  omp_set_nest_lock(arg);
  return nlock->count;
}

double omp_get_wtime(void)
{
  /* This function does not provide a working
   * wallclock timer. Replace it with a version
   * customized for the target machine.
   */
  return 0.0;
}

double omp_get_wtick(void)
{
  /* This function does not provide a working
   * clock tick function. Replace it with
   * a version customized for the target machine.
   */
  return 365. * 86400.;
}

void omp_fulfill_event(omp_event_handle_t event)
{
}

int omp_get_num_interop_properties(const omp_interop_t interop)
{
  return 0;
}

omp_intptr_t omp_get_interop_int(
  const omp_interop_t interop,
  omp_interop_property_t property_id, int *ret_code
)
{
  if (ret_code)
  {
    if (interop == omp_interop_none)
    {
      *ret_code = omp_irc_empty;
    }
    else
    {
      *ret_code = omp_irc_no_value;
    }
  }
  return 0;
}

void *omp_get_interop_ptr(
  const omp_interop_t interop,
  omp_interop_property_t property_id, int *ret_code
)
{
  if (ret_code)
  {
    if (interop == omp_interop_none)
    {
      *ret_code = omp_irc_empty;
    }
    else
    {
      *ret_code = omp_irc_no_value;
    }
  }
  return NULL;
}

const char *omp_get_interop_str(
  const omp_interop_t interop,
  omp_interop_property_t property_id, int *ret_code
)
{
  if (ret_code)
  {
    if (interop == omp_interop_none)
    {
      *ret_code = omp_irc_empty;
    } else
    {
      *ret_code = omp_irc_no_value;
    }
  }
  return NULL;
}

const char *omp_get_interop_name(
  const omp_interop_t interop,
  omp_interop_property_t property_id
)
{
  switch (property_id)
  {
    case omp_ipr_fr_id:
      return "fr_id";
    case omp_ipr_fr_name:
      return "fr_name";
    case omp_ipr_vendor:
      return "vendor";
    case omp_ipr_vendor_name:
      return "vendor_name";
    case omp_ipr_device_num:
      return "device_num";
    case omp_ipr_platform:
      return "platform";
    case omp_ipr_device:
      return "omp_ipr_device";
    case omp_ipr_device_context:
      return "device_context";
    case omp_ipr_targetsync:
      return "targetsync";
  }
  return NULL;
}

const char *omp_get_interop_type_desc(
  const omp_interop_t interop,
  omp_interop_property_t property_id
)
{
  if (interop == omp_interop_none)
    return NULL;
  else
    return "Invalid interop object provided";
}

const char *omp_get_interop_rc_desc(
  const omp_interop_t interop,
  omp_interop_rc_t ret_code
)
{
  switch (ret_code)
  {
    case omp_irc_no_value:
      return "Parameters valid, no meaningful value available";
    case omp_irc_success:
      return "Successful, value is usable";
    case omp_irc_empty:
      return "The object provided is equal to omp_interop_none";
    case omp_irc_out_of_range:
      return "Property ID is out of range";
    case omp_irc_type_int:
      return "Property type is int";
    case omp_irc_type_ptr:
      return "Property type is pointer";
    case omp_irc_type_str:
      return "Property type is string";
    case omp_irc_other:
      return "Other error";
  }
  return NULL;
}

void *omp_target_alloc(size_t size, int device_num)
{
  if (device_num != OMP_STUBS_INITIAL_DEVICE_NUM)
    return NULL;
  return malloc(size);
}

void omp_target_free(void *device_ptr, int device_num)
{
  free(device_ptr);
}

int omp_target_is_present(const void *ptr, int device_num)
{
  return (device_num == OMP_STUBS_INITIAL_DEVICE_NUM);
}

int omp_target_is_accessible(
  const void *ptr,
  size_t size,
  int device_num
)
{
  return (device_num == OMP_STUBS_INITIAL_DEVICE_NUM);
}

int omp_target_memcpy(
  void *dst,
  const void *src,
  size_t length,
  size_t dst_offset,
  size_t src_offset,
  int dst_device,
  int src_device
)
{
  /* Only the default device is valid in a stub.  */
  if (
    dst_device != OMP_STUBS_INITIAL_DEVICE_NUM
    || src_device != OMP_STUBS_INITIAL_DEVICE_NUM
    || ! dst || ! src )
  return EINVAL;
  memcpy((char *)dst + dst_offset,
    (const char *)src + src_offset,
    length);
  return 0;
}

int omp_target_memcpy_rect(
  void *dst,
  const void *src,
  size_t element_size,
  int num_dims,
  const size_t *volume,
  const size_t *dst_offsets,
  const size_t *src_offsets,
  const size_t *dst_dimensions,
  const size_t *src_dimensions,
  int dst_device_num,
  int src_device_num
)
{
  int ret=0;
  /* Both null, return number of dimensions supported,
   * this stub supports an arbitrary number.
   */
  if (dst == NULL && src == NULL) return INT_MAX;

  if (
    !volume || !dst_offsets || !src_offsets
    || !dst_dimensions || !src_dimensions
    || num_dims < 1 ) {
    ret = EINVAL;
    goto done;
  }
  if (num_dims == 1) {
    ret = omp_target_memcpy(
      dst,
      src,
      element_size * volume[0],
      dst_offsets[0] * element_size,
      src_offsets[0] * element_size,
      dst_device_num,
      src_device_num
    );
    if (ret) goto done;
  } else {
    size_t dst_slice_size = element_size;
    size_t src_slice_size = element_size;
    for (int i=1; i < num_dims; i++) {
      dst_slice_size *= dst_dimensions[i];
      src_slice_size *= src_dimensions[i];
    }
    size_t dst_off = dst_offsets[0] * dst_slice_size;
    size_t src_off = src_offsets[0] * src_slice_size;
    for (size_t i=0; i < volume[0]; i++) {
      ret = omp_target_memcpy_rect(
        (char *)dst + dst_off + dst_slice_size*i,
        (const char *)src + src_off + src_slice_size*i,
        element_size,
        num_dims - 1,
        volume + 1,
        dst_offsets + 1,
        src_offsets + 1,
        dst_dimensions + 1,
        src_dimensions + 1,
        dst_device_num,
        src_device_num);
      if (ret) goto done;
    }
  }
  done:
  return ret;
}

int omp_target_memcpy_async(
  void *dst,
  const void *src,
  size_t length,
  size_t dst_offset,
  size_t src_offset,
  int dst_device,
  int src_device,
  int depobj_count,
  omp_depend_t *depobj_list
)
{
  return omp_target_memcpy(
    dst, src, length, dst_offset, src_offset,
    dst_device, src_device);
}

int omp_target_memcpy_rect_async(
  void *dst,
  const void *src,
  size_t element_size,
  int num_dims,
  const size_t *volume,
  const size_t *dst_offsets,
  const size_t *src_offsets,
  const size_t *dst_dimensions,
  const size_t *src_dimensions,
  int dst_device_num,
  int src_device_num,
  int depobj_count,
  omp_depend_t *depobj_list
)
{
  return omp_target_memcpy_rect(
    dst, src, element_size, num_dims, volume,
    dst_offsets, src_offsets, dst_dimensions, src_dimensions,
    dst_device_num, src_device_num);
}

int omp_target_associate_ptr(
  const void *host_ptr,
  const void *device_ptr,
  size_t size,
  size_t device_offset,
  int device_num
)
{
  /* No association is possible because all host pointers
   * are considered present.  */
  return EINVAL;
}

int omp_target_disassociate_ptr(const void *ptr, int device_num)
{
  return EINVAL;
}

void *omp_get_mapped_ptr(const void *ptr, int device_num)
{
  return (void *)ptr;
}

static omp_allocator_handle_t omp_allocator = omp_null_allocator;

omp_allocator_handle_t omp_init_allocator(
  omp_memspace_handle_t memspace,
  int ntraits,
  const omp_alloctrait_t traits[]
)
{
  return omp_null_allocator;
}

void omp_destroy_allocator(omp_allocator_handle_t allocator)
{
}

void omp_set_default_allocator(omp_allocator_handle_t allocator)
{
  omp_allocator = allocator;
}

omp_allocator_handle_t omp_get_default_allocator(void)
{
  return omp_allocator;
}

void *omp_alloc(size_t size, omp_allocator_handle_t allocator)
{
  if (size == 0)
    return NULL;
  return malloc(size);
}

void *omp_aligned_alloc(
  size_t alignment,
  size_t size,
  omp_allocator_handle_t allocator
)
{
  void *ret;
  if (size == 0)
    return NULL;
  /* The following is needed for posix_memalign.  aligned_alloc,
     memalign etc. could be used instead.  */
  if (alignment < sizeof(void *))
    alignment = sizeof(void *);
  if (posix_memalign(&ret, alignment, size) == 0)
    return ret;
  return NULL;
}

void omp_free(void *ptr, omp_allocator_handle_t allocator)
{
  if (ptr != NULL)
    free(ptr);
}

void *omp_calloc(size_t nmemb, size_t size, omp_allocator_handle_t allocator)
{
  if (size == 0 || nmemb == 0)
    return NULL;
  if (SIZE_MAX / size < nmemb)
    return NULL;
  return calloc(nmemb, size);
}

void *omp_aligned_calloc(
  size_t alignment,
  size_t nmemb,
  size_t size,
  omp_allocator_handle_t allocator
)
{
  void *ret;
  if (size == 0 || nmemb == 0)
    return NULL;
  if (SIZE_MAX / size < nmemb)
    return NULL;
  /* The following is needed for posix_memalign.  aligned_alloc,
     memalign etc. could be used instead.  */
  if (alignment < sizeof(void *))
    alignment = sizeof(void *);
  if (posix_memalign(&ret, alignment, size * alignment) == 0)
  {
    memset(ret, '\0', size * alignment);
    return ret;
  }
  return NULL;
}

void *omp_realloc(
  void *ptr,
  size_t size,
  omp_allocator_handle_t allocator,
  omp_allocator_handle_t free_allocator
)
{
  if (ptr == NULL)
    return omp_alloc(size, allocator);
  if (size == 0)
  {
    free(ptr);
    return NULL;
  }
  return realloc(ptr, size);
}

int omp_control_tool(int command, int modifier, void *arg)
{
  return omp_control_tool_notool;
}
