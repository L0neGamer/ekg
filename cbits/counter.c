#include <stdlib.h>
#include "HsFFI.h"

StgInt* hs_counter_new(void) {
  StgInt* counter = malloc(sizeof(StgInt));
  *counter = 0;
  return counter;
}
void hs_counter_add(volatile StgInt* counter, StgInt n) {
  StgInt temp = n;
#if SIZEOF_VOID_P == 8
  __asm__ __volatile__("lock; xaddq %0,%1"
#elif SIZEOF_VOID_P == 4
  __asm__ __volatile__("lock; xaddl %0,%1"
#else
# error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
                       : "+r" (temp), "+m" (*counter)
                       : : "cc", "memory");
}

StgInt hs_counter_read(volatile const StgInt* counter) {
  return *counter;
}
