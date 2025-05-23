#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#include "genesis/sbcl.h"
#include "globals.h"
#include "os.h"
#include "interr.h"
#include "sys_mmap.inc"

static void dumpmaps()
{
    FILE *maps = fopen("/proc/self/maps","r");
    if (maps) {
        fprintf(stderr, "Dump of /proc/self/maps:\n");
        char line[512];
        while (fgets(line, sizeof line, maps))
            ignore_value(write(2, line, strlen(line)));
        fclose(maps);
    }
}

os_vm_address_t
os_alloc_gc_space(int __attribute__((unused)) space_id,
                  int attributes, os_vm_address_t addr, os_vm_size_t len)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t actual;

#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = sbcl_mmap(addr, len, protection, flags, -1, 0);
    if (actual == MAP_FAILED) {
        // TODO: If ths is FIXEDOBJ space, then actually read /proc/self/maps
        // to find a hole. There's no chance that ENOMEM is the truth.
        if (errno == ENOMEM)
            fprintf(stderr, "os_alloc_gc_space(%d,%p,%zu) failed with ENOMEM\n",
                    attributes, addr, len);
        else
            perror("mmap");
        dumpmaps();
        return 0;               /* caller should check this */
    }

    // If requested addr was 0, the MOVABLE attribute means nothing.
    if (addr && !(attributes & MOVABLE) && (addr != actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        dumpmaps();
        return 0;
    }

    return actual;
}
