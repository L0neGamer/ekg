#include "HsFFI.h"

void hs_atomic_add(volatile StgInt* atomic, StgInt n) {
  __sync_fetch_and_add(atomic, n);
}

StgInt hs_atomic_read(volatile const StgInt* atomic) {
  return *atomic;
}
