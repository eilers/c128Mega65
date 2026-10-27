// The firmware's HyperRAM heap: from the end of its image, as the boot ROM loaded it, to
// the end of its region (doc/hyperram-map.md).
#include "heap.h"
#include "m2m_rv.h"
#include "shell.h"

#ifndef FW_HEAP
extern char __crt0_copy_data_src_begin[], __crt0_copy_data_dst_begin[], __crt0_copy_data_dst_end[];
#define FW_HEAP     ((uintptr_t)__crt0_copy_data_src_begin + \
                     (__crt0_copy_data_dst_end - __crt0_copy_data_dst_begin))
#define FW_HEAP_END M2M_FW_END
#endif

void *heap_alloc(uint32_t bytes)
{
   static uintptr_t next;
   if (!next)
      next = (FW_HEAP + 15) & ~(uintptr_t)15;
   uintptr_t p = next;
   next = (next + bytes + 15) & ~(uintptr_t)15;
   if (next > FW_HEAP_END)
      shell_fatal("Out of HyperRAM for the firmware\n", bytes);
   return (void *)p;
}
