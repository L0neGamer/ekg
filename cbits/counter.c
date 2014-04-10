#include "HsFFI.h"

void hs_counter_add(volatile StgInt* counter, StgInt n) {
  __sync_fetch_and_add(counter, n);
}

StgInt hs_counter_read(volatile const StgInt* counter) {
  return *counter;
}
