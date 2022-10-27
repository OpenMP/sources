/*******************************************************************
* Copyright (c) 1997-2021 OpenMP Architecture Review Board.        *
*                                                                  *
* Permission to copy without fee all or part of this material is   *
* granted, provided the OpenMP Architecture Review Board copyright *
* notice appears. Notice is given that copying is by permission of *
* the OpenMP Architecture Review Board.                            *
*******************************************************************/

#ifndef _OMP_H
#define _OMP_H

/*
 * For size_t definition.  Implementations might handle size_t differently
 * and not actually define this type when including omp.h.
 */
#include <stddef.h>

/*
 * For uintptr_t and intptr_t definitions, which are used just as an
 * implementation detail in this implementation.
 */
#include <stdint.h>

/*
 * define the lock data types
 */
typedef void *omp_lock_t;

typedef void *omp_nest_lock_t;

/*
 * define the synchronization hints
 */
typedef enum omp_sync_hint_t {
  omp_sync_hint_none = 0x0,
  omp_lock_hint_none = omp_sync_hint_none,
  omp_sync_hint_uncontended = 0x1,
  omp_lock_hint_uncontended = omp_sync_hint_uncontended,
  omp_sync_hint_contended = 0x2,
  omp_lock_hint_contended = omp_sync_hint_contended,
  omp_sync_hint_nonspeculative = 0x4,
  omp_lock_hint_nonspeculative = omp_sync_hint_nonspeculative,
  omp_sync_hint_speculative = 0x8,
  omp_lock_hint_speculative = omp_sync_hint_speculative
  /* ,
   Add vendor specific constants for lock hints here,
   starting from the most-significant bit. */
} omp_sync_hint_t;

/* omp_lock_hint_t has been deprecated */
typedef omp_sync_hint_t omp_lock_hint_t;

/*
 * define the schedule kinds
 */
typedef enum omp_sched_t
{
  omp_sched_static = 0x1,
  omp_sched_dynamic = 0x2,
  omp_sched_guided = 0x3,
  omp_sched_auto = 0x4,
  /* Add vendor specific schedule constants here */
  omp_sched_monotonic = 0x80000000u
} omp_sched_t;

/*
 * define the proc bind values
 */
typedef enum omp_proc_bind_t
{
  omp_proc_bind_false = 0,
  omp_proc_bind_true = 1,
  omp_proc_bind_primary = 2,
  omp_proc_bind_master = omp_proc_bind_primary, /* (deprecated) */
  omp_proc_bind_close = 3,
  omp_proc_bind_spread = 4
} omp_proc_bind_t;

typedef void *omp_depend_t;

/*
 * define interop properties
 */
typedef enum omp_interop_property_t
{
  omp_ipr_fr_id = -1,
  omp_ipr_fr_name = -2,
  omp_ipr_vendor = -3,
  omp_ipr_vendor_name = -4,
  omp_ipr_device_num = -5,
  omp_ipr_platform = -6,
  omp_ipr_device = -7,
  omp_ipr_device_context = -8,
  omp_ipr_targetsync = -9,
  omp_ipr_first = -9
} omp_interop_property_t;

/*
 * define interop return code properties
 */
typedef enum omp_interop_rc_t
{
  omp_irc_no_value = 1,
  omp_irc_success = 0,
  omp_irc_empty = -1,
  omp_irc_out_of_range = -2,
  omp_irc_type_int = -3,
  omp_irc_type_ptr = -4,
  omp_irc_type_str = -5,
  omp_irc_other = -6
} omp_interop_rc_t;

typedef void *omp_interop_t;
#define omp_interop_none ((omp_interop_t) 0)

/*
 * define memory management types
 */
typedef uintptr_t omp_uintptr_t;
typedef intptr_t omp_intptr_t;

typedef enum omp_memspace_handle_t {
  omp_default_mem_space,
  omp_large_cap_mem_space,
  omp_const_mem_space,
  omp_high_bw_mem_space,
  omp_low_lat_mem_space
  /* ,
     Add vendor specific constants for memory spaces here.  */
} omp_memspace_handle_t;

typedef enum omp_allocator_handle_t {
  omp_null_allocator = 0,
  /* The rest of the enumerators have
     implementation specific values.  */
  omp_default_mem_alloc = -12,
  omp_large_cap_mem_alloc,
  omp_const_mem_alloc,
  omp_high_bw_mem_alloc,
  omp_low_lat_mem_alloc,
  omp_cgroup_mem_alloc,
  omp_pteam_mem_alloc,
  omp_thread_mem_alloc
  /* ,
     Some range for dynamically allocated handles.  */
} omp_allocator_handle_t;

typedef enum omp_alloctrait_key_t {
  omp_atk_sync_hint = 1,
  omp_atk_alignment = 2,
  omp_atk_access = 3,
  omp_atk_pool_size = 4,
  omp_atk_fallback = 5,
  omp_atk_fb_data = 6,
  omp_atk_pinned = 7,
  omp_atk_partition = 8
} omp_alloctrait_key_t;

typedef enum omp_alloctrait_value_t {
  omp_atv_false = 0,
  omp_atv_true = 1,
  omp_atv_contended = 3,
  omp_atv_uncontended = 4,
  omp_atv_serialized = 5,
  omp_atv_sequential = omp_atv_serialized, /* (deprecated) */
  omp_atv_private = 6,
  omp_atv_all = 7,
  omp_atv_thread = 8,
  omp_atv_pteam = 9,
  omp_atv_cgroup = 10,
  omp_atv_default_mem_fb = 11,
  omp_atv_null_fb = 12,
  omp_atv_abort_fb = 13,
  omp_atv_allocator_fb = 14,
  omp_atv_environment = 15,
  omp_atv_nearest = 16,
  omp_atv_blocked = 17,
  omp_atv_interleaved = 18
} omp_alloctrait_value_t;

#define omp_atv_default ((omp_uintptr_t) -1)
/* Or
   enum {
     omp_atv_default = (omp_uintptr_t) -1
   };
   or in C++
   static const omp_uintptr_t omp_atv_default = -1;
   etc.
 */

typedef struct omp_alloctrait_t {
  omp_alloctrait_key_t key;
  omp_uintptr_t value;
} omp_alloctrait_t;

/* Define device-number enums; the value of
 * omp_invalid_device is implementation defined.
 */

enum {
  omp_initial_device = -1,
  omp_invalid_device = -42
};

/*
 * define kinds of relinguishing resources
 */
typedef enum omp_pause_resource_t {
  omp_pause_soft = 1,
  omp_pause_hard = 2
} omp_pause_resource_t;

typedef enum omp_event_handle_t {
  /* Vendor specific enumerators, e.g.:  */
  __omp_event_min = 0,
  __omp_event_max = ~0u
} omp_event_handle_t;

/*
 * define the tool control commands
 */
typedef enum omp_control_tool_t
{
  omp_control_tool_start = 1,
  omp_control_tool_pause = 2,
  omp_control_tool_flush = 3,
  omp_control_tool_end = 4
} omp_control_tool_t;

typedef enum omp_control_tool_result_t {
  omp_control_tool_notool = -2,
  omp_control_tool_nocallback = -1,
  omp_control_tool_success = 0,
  omp_control_tool_ignored = 1
} omp_control_tool_result_t;

/*
 * exported OpenMP functions
 */
#ifdef __cplusplus
extern "C"
{
#endif

extern void omp_set_num_threads(int num_threads);
extern int omp_get_num_threads(void);
extern int omp_get_max_threads(void);
extern int omp_get_thread_num(void);
extern int omp_get_num_procs(void);
extern int omp_in_parallel(void);
extern void omp_set_dynamic(int dynamic_threads);
extern int omp_get_dynamic(void);
extern int omp_get_cancellation(void);
/* The following two routines are deprecated.  */
extern void omp_set_nested(int nested);
extern int omp_get_nested(void);
extern void omp_set_schedule(omp_sched_t kind, int chunk_size);
extern void omp_get_schedule(omp_sched_t *kind, int *chunk_size);
extern int omp_get_thread_limit(void);
extern int omp_get_supported_active_levels(void);
extern void omp_set_max_active_levels(int max_levels);
extern int omp_get_max_active_levels(void);
extern int omp_get_level(void);
extern int omp_get_ancestor_thread_num(int level);
extern int omp_get_team_size(int level);
extern int omp_get_active_level(void);
extern int omp_in_explicit_task(void);
extern int omp_in_final(void);
extern omp_proc_bind_t omp_get_proc_bind(void);
extern int omp_get_num_places(void);
extern int omp_get_place_num_procs(int place_num);
extern void omp_get_place_proc_ids(int place_num, int *ids);
extern int omp_get_place_num(void);
extern int omp_get_partition_num_places(void);
extern void omp_get_partition_place_nums(int *place_nums);

extern void omp_set_affinity_format(const char *format);
extern size_t omp_get_affinity_format(char *buffer, size_t size);
extern void omp_display_affinity(const char *format);
extern size_t omp_capture_affinity(
  char *buffer,
  size_t size,
  const char *format
);
extern void omp_display_env(int verbose);

extern void omp_set_default_device(int device_num);
extern int omp_get_default_device(void);

extern int omp_get_num_devices(void);
extern int omp_get_device_num(void);
extern int omp_get_num_teams(void);
extern int omp_get_team_num(void);
extern int omp_is_initial_device(void);
extern int omp_get_initial_device(void);
extern int omp_get_max_task_priority(void);
extern int omp_pause_resource(omp_pause_resource_t kind, int device_num);
extern int omp_pause_resource_all(omp_pause_resource_t kind);

extern void omp_set_num_teams(int num_teams);
extern int omp_get_max_teams(void);
extern void omp_set_teams_thread_limit(int thread_limit);
extern int omp_get_teams_thread_limit(void);

extern void omp_init_lock(omp_lock_t *lock);
extern void omp_init_lock_with_hint(
  omp_lock_t *lock,
  omp_sync_hint_t hint
);
extern void omp_destroy_lock(omp_lock_t *lock);
extern void omp_set_lock(omp_lock_t *lock);
extern void omp_unset_lock(omp_lock_t *lock);
extern int omp_test_lock(omp_lock_t *lock);

extern void omp_init_nest_lock(omp_nest_lock_t *lock);
extern void omp_init_nest_lock_with_hint(
  omp_nest_lock_t *lock,
  omp_sync_hint_t hint
);
extern void omp_destroy_nest_lock(omp_nest_lock_t *lock);
extern void omp_set_nest_lock(omp_nest_lock_t *lock);
extern void omp_unset_nest_lock(omp_nest_lock_t *lock);
extern int omp_test_nest_lock(omp_nest_lock_t *lock);

extern double omp_get_wtime(void);
extern double omp_get_wtick(void);

extern void omp_fulfill_event(omp_event_handle_t event);
  
extern int omp_get_num_interop_properties(const omp_interop_t interop);
extern omp_intptr_t omp_get_interop_int(
  const omp_interop_t interop,
  omp_interop_property_t property_id, int *ret_code
);
extern void* omp_get_interop_ptr(
  const omp_interop_t interop,
  omp_interop_property_t property_id,
  int *ret_code
);
extern const char* omp_get_interop_str(
  const omp_interop_t interop,
  omp_interop_property_t property_id,
  int *ret_code
);
extern const char* omp_get_interop_name(
  const omp_interop_t interop,
  omp_interop_property_t property_id
);
extern const char* omp_get_interop_type_desc(
  const omp_interop_t interop,
  omp_interop_property_t property_id
);
extern const char* omp_get_interop_rc_desc(
  const omp_interop_t interop,
  omp_interop_rc_t ret_code
);

extern void *omp_target_alloc(size_t size, int device_num);
extern void omp_target_free(void *device_ptr, int device_num);
extern int omp_target_is_present(const void *ptr, int device_num);
extern int omp_target_is_accessible(
  const void *ptr,
  size_t size,
  int device_num
);
extern int omp_target_memcpy(
  void *dst,
  const void *src,
  size_t length,
  size_t dst_offset,
  size_t src_offset,
  int dst_device_num,
  int src_device_num
);
extern int omp_target_memcpy_rect(
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
);
extern int omp_target_memcpy_async(
  void *dst,
  const void *src,
  size_t length,
  size_t dst_offset,
  size_t src_offset,
  int dst_device_num,
  int src_device_num,
  int depobj_count,
  omp_depend_t *depobj_list
);
extern int omp_target_memcpy_rect_async(
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
);
extern int omp_target_associate_ptr(
  const void *host_ptr,
  const void *device_ptr,
  size_t size,
  size_t device_offset,
  int device_num
);
extern void *omp_get_mapped_ptr(const void *ptr, int device_num);
extern int omp_target_disassociate_ptr(
  const void *ptr,
  int device_num
);

extern omp_allocator_handle_t omp_init_allocator(
  omp_memspace_handle_t memspace,
  int ntraits,
  const omp_alloctrait_t traits[]
);
extern void omp_destroy_allocator(omp_allocator_handle_t allocator);
extern void omp_set_default_allocator(omp_allocator_handle_t allocator);
extern omp_allocator_handle_t omp_get_default_allocator(void);

#ifdef __cplusplus
extern void *omp_alloc(
  size_t size,
  omp_allocator_handle_t allocator = omp_null_allocator
);
extern void *omp_aligned_alloc(
  size_t alignment,
  size_t size,
  omp_allocator_handle_t allocator = omp_null_allocator
);
extern void omp_free(
  void *ptr,
  omp_allocator_handle_t allocator = omp_null_allocator
);
extern void *omp_calloc(
  size_t nmemb,
  size_t size,
  omp_allocator_handle_t allocator = omp_null_allocator
);
extern void *omp_aligned_calloc(
  size_t alignment,
  size_t nmemb,
  size_t size,
  omp_allocator_handle_t allocator = omp_null_allocator
);
extern void *omp_realloc(
  void *ptr,
  size_t size,
  omp_allocator_handle_t allocator = omp_null_allocator,
  omp_allocator_handle_t free_allocator = omp_null_allocator
);
#else
extern void *omp_alloc(size_t size, omp_allocator_handle_t allocator);
extern void *omp_aligned_alloc(
  size_t alignment,
  size_t size,
  omp_allocator_handle_t allocator);
extern void omp_free(void *ptr, omp_allocator_handle_t allocator);
extern void *omp_calloc(
  size_t nmemb,
  size_t size,
  omp_allocator_handle_t allocator);
extern void *omp_aligned_calloc(
  size_t alignment,
  size_t nmemb,
  size_t size,
  omp_allocator_handle_t allocator);
extern void *omp_realloc(
  void *ptr,
  size_t size,
  omp_allocator_handle_t allocator,
  omp_allocator_handle_t free_allocator);
#endif

extern int omp_control_tool(int command, int modifier, void *arg);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include <memory>

namespace omp {
  namespace allocator {
    template<class T> struct default_mem : public std::allocator<T> {};
    template<class T> struct large_cap_mem : public std::allocator<T> {};
    template<class T> struct const_mem : public std::allocator<T> {};
    template<class T> struct high_bw_mem : public std::allocator<T> {};
    template<class T> struct low_lat_mem : public std::allocator<T> {};
    template<class T> struct cgroup_mem : public std::allocator<T> {};
    template<class T> struct pteam_mem : public std::allocator<T> {};
    template<class T> struct thread_mem : public std::allocator<T> {};
  }
}
#endif

#endif /* ! _OMP_H */
