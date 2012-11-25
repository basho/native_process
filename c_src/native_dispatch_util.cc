// Internal utilities used by native_dispatch
//
// Not included as part of the header-file because we want these to be C++
// and the header file can be included in pure C code.
//
// Yes, these will be linked globally between all libraries that use
// native_dispatch. There are some games we can play to solve that, but meh.

#include "native_dispatch_util.h"
#include <map>
#include <utility>

typedef std::map<std::pair<ERL_NIF_TERM, unsigned>, nif_fptr> dispatch_table_t;

void* native_dispatch_create_table(ErlNifEnv *env, ErlNifFunc *funcs, size_t funcs_size) {
  dispatch_table_t &dispatch = *(new dispatch_table_t());
  size_t num = funcs_size/sizeof(ErlNifFunc);
  for(size_t i = 0; i < num; ++i) {
    ErlNifFunc func = funcs[i];
    dispatch[std::make_pair(enif_make_atom(env, func.name), func.arity)] = func.fptr;
    // printf("(%s, %d): %p\n", func.name, func.arity, func.fptr);
  }
  return (void*)&dispatch;
}

void native_dispatch_free_table(void *table) {
  dispatch_table_t *dispatch = (dispatch_table_t*)table;
  delete dispatch;
}

nif_fptr native_dispatch_table_lookup(ERL_NIF_TERM name, int arity, void *table) {
  dispatch_table_t &dispatch = *((dispatch_table_t*)table);
  if(dispatch.count(std::make_pair(name, arity))) {
    return dispatch[std::make_pair(name, arity)];
  }
  return NULL;
}
