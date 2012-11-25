#include "native_process.h"
#include "native_dispatch_util.h"

static ERL_NIF_TERM NP_ATOM_DISPATCH;
static ERL_NIF_TERM NP_ATOM_DBG_DISPATCH;

// User-defined callback
static ErlNifFunc* native_dispatch_async_nifs();
static size_t native_dispatch_async_nifs_size();

#define NATIVE_DISPATCH_TABLE(NIFS)                                      \
static ErlNifFunc* native_dispatch_async_nifs() { return NIFS; }         \
static size_t native_dispatch_async_nifs_size() { return sizeof(NIFS); }

typedef struct {
  ErlNifEnv *env;
  void *dispatch_table;
} ustate;

int setup_native_dispatch(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  setup_native_process(env, priv_data, load_info);
  NP_ATOM_DISPATCH = enif_make_atom(env, "dispatch");
  NP_ATOM_DBG_DISPATCH = enif_make_atom(env, "dbg_dispatch");
  return 0;
}

static void* init() {
  ustate *us = (ustate*) enif_alloc(sizeof(ustate));
  us->env = enif_alloc_env();
  us->dispatch_table = native_dispatch_create_table(us->env,
                                                    native_dispatch_async_nifs(),
                                                    native_dispatch_async_nifs_size());
  return us;
}

static void destroy(void *p) {
  ustate *us = (ustate*)p;
  enif_free_env(us->env);
  enif_free(us);
}

static ERL_NIF_TERM do_dispatch(int debug, ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv, ustate *us) {
  unsigned int arity;
  ErlNifPid pid;

  if(!enif_is_ref(env, argv[1]))
    return NP_ATOM_ERROR;
  if(!enif_get_local_pid(env, argv[2], &pid))
    return NP_ATOM_ERROR;
  if(!enif_is_atom(env, argv[3]))
    return NP_ATOM_ERROR;
  if(!enif_is_list(env, argv[4]))
    return NP_ATOM_ERROR;
  if(!enif_get_list_length(env, argv[4], &arity))
    return NP_ATOM_ERROR;

  ERL_NIF_TERM fname = argv[3]; /* atom */

  enif_clear_env(us->env);
  ERL_NIF_TERM *new_argv = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * arity);
  ERL_NIF_TERM head, tail = argv[4];
  int i = 0;
  while(enif_get_list_cell(env, tail, &head, &tail)) {
    new_argv[i] = enif_make_copy(us->env, head);
    ++i;
  }

  ERL_NIF_TERM result, msg, send_ref;

  nif_fptr fptr = native_dispatch_table_lookup(fname, arity, us->dispatch_table);
  if(!fptr) {
    if(debug)
      erts_printf("thread=%p: Unable to find NIF to call for %T/%d\n",
                  enif_thread_self(), fname, arity);
    result = NP_ATOM_ERROR;
  }
  else {
    if(debug)
      erts_printf("thread=%p: Dispatching %T/%d to function %p\n",
                  enif_thread_self(), fname, arity, fptr);
    result = fptr(us->env, arity, new_argv);
  }
  send_ref = enif_make_copy(us->env, argv[1]);
  msg = enif_make_tuple2(us->env, send_ref, result);

  if(debug)
    erts_printf("thread=%p: Sending %T to %T\n",
                enif_thread_self(), msg, pid);

  enif_send(NULL, &pid, us->env, msg);

  return NP_ATOM_OK;
}

static ERL_NIF_TERM dispatch(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv, ustate *us) {
  return do_dispatch(0, env, argc, argv, us);
}

static ERL_NIF_TERM dbg_dispatch(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv, ustate *us) {
  return do_dispatch(1, env, argc, argv, us);
}


static void receive(ErlNifEnv* env, ERL_NIF_TERM msg, void *p) {
  /* erts_printf("Yar, received: %T\n", msg); */

  int argc;
  const ERL_NIF_TERM* argv;

  if(enif_get_tuple(env, msg, &argc, &argv)) {
    if(enif_is_atom(env, argv[0])) {
      if(argv[0] == NP_ATOM_DISPATCH) {
        dispatch(env, argc, argv, (ustate*)p);
      }
      if(argv[0] == NP_ATOM_DBG_DISPATCH) {
        dbg_dispatch(env, argc, argv, (ustate*)p);
      }
    }
  }
}
