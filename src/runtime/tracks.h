/*
 * separate allocation tracks for dynamic space
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#ifndef _TRACKS_H_
#define _TRACKS_H_

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
#define TRACK_BITS 8
#define TRACK_MASK ((1 << TRACK_BITS) - 1)
#define TRACKS_END (1 << TRACK_BITS) /* upper bound (exclusive) */

#define DEFAULT_TRACK 0
#define UNUSED_TRACK (TRACKS_END - 1)
#define INITIAL_TRACK DEFAULT_TRACK
#define PRIVATE_CONS_TRACK UNUSED_TRACK
#define RESERVED_TRACK (TRACKS_END - 2)
#endif

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
#define PAGE_TRACK(page) \
    (page_tracks)[page]

#define PAGE_TRACK_SET(page, tr) \
    /*                                                                  \
    track_index_t tr_old = PAGE_TRACK(page);                            \
    if (tr_old != tr) {                                                 \
        fprintf(stderr, "Page %d: track %x -> %x\n", page, tr_old, tr); \
    }                                                                   \
    */                                                                  \
    (page_tracks)[page] = (tr)
#else
#define PAGE_TRACK(page)
#define PAGE_TRACK_SET(page, tr)
#endif

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
#define TRACK_ARG(arg)  arg,
#else
#define TRACK_ARG(arg)
#endif

#ifdef LISP_FEATURE_ALLOCATION_TRACKS
#define WITH_TRACK_INDEX(var, i)  (var)[i]
#else
#define WITH_TRACK_INDEX(var, i)  (var)
#endif

#endif /* _TRACKS_H_ */
