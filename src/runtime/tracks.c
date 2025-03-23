#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include "genesis/sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "gc.h"
#include "lispregs.h"
#include "genesis/gc-tables.h"
#include "thread.h"
#include "genesis/instance.h"

static page_index_t close_heap_region_(struct alloc_region* r, int WITH_TRACK(page_type)) {
    page_index_t result = -1;
    if (r->start_addr) {
        result = find_page_index(r->start_addr);
        gc_close_region(r, WITH_TRACK(page_type), 4);
    }
    return result;
}

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
int inhibit_track_use = 0;
void switch_to_track(lispobj track)
{
    if (inhibit_track_use) return;
    uint64_t tr_ = track >> N_FIXNUM_TAG_BITS;
    gc_assert(tr_ < TRACKS_END);
    gc_assert(tr_ == (tr_ & TRACK_MASK));
    track_index_t tr = tr_ & TRACK_MASK;
    struct thread* th = get_sb_vm_thread();
    struct extra_thread_data *extra_data = thread_extra_data(th);
    if (th->arena)
        lose("track error: can't use track together with arena in the same thread");
    // Page table lock guards the page table
    acquire_gc_page_table_lock();
    // Close only the non-system regions
    extra_data->mixed_page_hint[th->track] =
        close_heap_region_(&th->mixed_tlab, TR_PT_ARG(th->track, PAGE_TYPE_MIXED));
    extra_data->cons_page_hint[th->track] =
        close_heap_region_(&th->cons_tlab, TR_PT_ARG(th->track, PAGE_TYPE_CONS));
    release_gc_page_table_lock();
    /*
    } else {
        gc_assert(th->track != DEFAULT_TRACK); // must have been a non-default track in use
        // Indicate that the tlabs have no space remaining.
        gc_set_region_empty(&th->mixed_tlab);
        gc_set_region_empty(&th->cons_tlab);
    }
    */
    th->track = (uword_t)tr;
}
#else
void switch_to_track(lispobj __attribute__((unused)) tr) {}
#endif
