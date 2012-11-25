#include "erl_nif.h"
typedef ERL_NIF_TERM (*nif_fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
extern "C" {
#endif

  void* native_dispatch_create_table(ErlNifEnv *env, ErlNifFunc *funcs, size_t funcs_size);
  void native_dispatch_free_table(void *table);
  nif_fptr native_dispatch_table_lookup(ERL_NIF_TERM name, int arity, void *table);

#ifdef __cplusplus
}
#endif
