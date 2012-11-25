#include "native_process.h"

typedef struct {
  int count;
} ustate;

static void *init() {
  ustate *us = enif_alloc(sizeof(ustate));
  us->count = 0;
  return us;
}

static void destroy(void *p) {
  ustate *us = (ustate*)p;
  erts_printf("Count when GC'd: %d\n", us->count);
  enif_free(us);
}

static void receive(ErlNifEnv* env, ERL_NIF_TERM msg, void *p) {
  ustate *us = (ustate*)p;
  erts_printf("Received/%d: %T\n", us->count, msg);
  us->count++;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  setup_native_process(env, priv_data, load_info);
  return 0;
}

static ErlNifFunc nif_funcs[] = {NATIVE_PROCESS_NIFS};
ERL_NIF_INIT(native_process_test, nif_funcs, &on_load, NULL, NULL, NULL);
