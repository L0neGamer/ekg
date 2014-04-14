#include "HsFFI.h"

void hs_atomic_add(volatile StgInt* atomic, StgInt n) {
  __sync_fetch_and_add(atomic, n);
}

void hs_atomic_subtract(volatile StgInt* atomic, StgInt n) {
  __sync_fetch_and_sub(atomic, n);
}

StgInt hs_atomic_read(volatile const StgInt* atomic) {
  return *atomic;
}

void hs_atomic_write(volatile StgInt* atomic, StgInt n) {
  *atomic = n;
}
