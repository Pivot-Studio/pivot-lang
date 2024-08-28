// dso_handle.c
#include <stddef.h>

const void *const __dso_handle __attribute__((weak)) = &__dso_handle;
const void *const _dso_handle __attribute__((weak)) = &__dso_handle;
const void *const dso_handle __attribute__((weak)) = &__dso_handle;