// Current design exits as soon as possible. Alternatively, we could drain the mailbox
// and then exit. Not sure what semantics we want here. Maybe make it configurable.

#include "erl_nif.h"
#include <unistd.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

int erts_printf(const char *, ...);

#ifdef __cplusplus
}
#endif

ErlNifResourceType* native_process_RESOURCE;

static ERL_NIF_TERM NP_ATOM_ERROR;
static ERL_NIF_TERM NP_ATOM_OK;
static ERL_NIF_TERM NP_ATOM_WOULD_BLOCK;
static ERL_NIF_TERM NP_ATOM_EMPTY;
static ERL_NIF_TERM NP_ATOM_SENDMAIL;

// User-defined callbacks
static void* init();
static void destroy(void*);
static void receive(ErlNifEnv*, ERL_NIF_TERM, void*);

typedef struct
{
  ERL_NIF_TERM mailbox;
  ErlNifEnv *mailbox_env;
  ErlNifEnv *send_env;
  ErlNifPid pid;
  ErlNifMutex* lock;
  ErlNifCond* cond;
  ErlNifTid thread;
  int should_exit;
  void *user_state;
} native_process;

static void native_process_resource_cleanup(ErlNifEnv* env, void* arg)
{
  /* erts_printf("freeing\n"); */
  native_process *state = (native_process*)arg;

  enif_mutex_lock(state->lock);
  state->should_exit = 1;
  enif_cond_signal(state->cond);
  enif_mutex_unlock(state->lock);
  enif_thread_join(state->thread, NULL);

  enif_mutex_destroy(state->lock);
  enif_cond_destroy(state->cond);
  if(state->mailbox_env)
    enif_free_env(state->mailbox_env);
  enif_free_env(state->send_env);
  destroy(state->user_state);
  /* erts_printf("freed\n"); */
}

static int setup_native_process(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  native_process_RESOURCE = enif_open_resource_type(env, NULL, "native_process_resource",
                                                    &native_process_resource_cleanup,
                                                    flags, NULL);

  NP_ATOM_ERROR = enif_make_atom(env, "error");
  NP_ATOM_OK = enif_make_atom(env, "ok"); 
  NP_ATOM_WOULD_BLOCK = enif_make_atom(env, "would_block");
  NP_ATOM_EMPTY = enif_make_atom(env, "empty");
  NP_ATOM_SENDMAIL = enif_make_atom(env, "sendmail");
  return 0;
}

static int process_mailbox(ErlNifEnv *env, ERL_NIF_TERM mailbox, native_process *state) {
  int should_exit = 0;

  if(!enif_is_list(env, mailbox) || enif_is_empty_list(env, mailbox))
    return 0;

  /*
  unsigned mbox_size;
  if(enif_get_list_length(env, mailbox, &mbox_size))
    erts_printf("Mailbox size: %d\n", mbox_size);
  */

  /* erts_printf("Mailbox: %T\n", mailbox); */

  ERL_NIF_TERM head, tail = mailbox;
  while(enif_get_list_cell(env, tail, &head, &tail)) {
    receive(env, head, state->user_state);

    enif_mutex_lock(state->lock);
    should_exit = state->should_exit;
    enif_mutex_unlock(state->lock);

    if(should_exit)
      return 1;
  }
  return 0;
}

static void request_mail(native_process *state) {
  ERL_NIF_TERM msg;
  msg = enif_make_copy(state->send_env, NP_ATOM_SENDMAIL);
  enif_send(NULL, &state->pid, state->send_env, msg);
  enif_clear_env(state->send_env);
}

static void *thread_body(void *arg) {
  ERL_NIF_TERM mailbox;
  ErlNifEnv *mailbox_env;
  int should_exit;
  native_process *state = (native_process*) arg;

  while(1) {
    enif_mutex_lock(state->lock);
    while((state->mailbox == NP_ATOM_EMPTY) && !state->should_exit) { /* empty mailbox */
      request_mail(state);
      enif_cond_wait(state->cond, state->lock);
    }
    mailbox = state->mailbox;
    mailbox_env = state->mailbox_env;
    should_exit = state->should_exit;
    state->mailbox = NP_ATOM_EMPTY;
    state->mailbox_env = NULL;
    enif_mutex_unlock(state->lock);

    if(should_exit) {
      return NULL;
    }

    if(mailbox_env) {
      should_exit = process_mailbox(mailbox_env, mailbox, state);
      enif_free_env(mailbox_env);
      if(should_exit)
        return NULL;
    }
  }
}

static ERL_NIF_TERM spawn_native(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  native_process* obj = (native_process*)enif_alloc_resource(native_process_RESOURCE, sizeof(native_process));

  obj->mailbox = NP_ATOM_EMPTY;
  obj->mailbox_env = NULL;
  obj->send_env = enif_alloc_env();
  enif_self(env, &obj->pid);
  obj->lock = enif_mutex_create((char*)"native_process_lock");
  obj->cond = enif_cond_create((char*)"native_process_cond");
  obj->should_exit = 0;
  obj->user_state = init();

  // enif_mutex_lock(obj->lock);
  if(enif_thread_create((char*)"native_process", &obj->thread, thread_body, obj, NULL)) {
    // enif_mutex_unlock(obj->lock);
    return NP_ATOM_ERROR;
  }
  // enif_mutex_unlock(obj->worker_lock);

  term = enif_make_resource(env, obj);
  enif_release_resource(obj);
  return term;
}

static ERL_NIF_TERM send_native(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  native_process *state;

  if(!enif_get_resource(env, argv[0], native_process_RESOURCE, (void**)&state))
    return NP_ATOM_ERROR;

  ERL_NIF_TERM new_mailbox = argv[1];
  if(!enif_is_list(env, new_mailbox))
    return NP_ATOM_ERROR;

  if(enif_mutex_trylock(state->lock)) {
    // This really shouldn't happen...
    return NP_ATOM_WOULD_BLOCK;
  }

  if(state->mailbox_env != NULL) {
    // Already have mail that the native thread hasn't taken
    enif_mutex_unlock(state->lock);
    return NP_ATOM_ERROR;
  }

  state->mailbox_env = enif_alloc_env();
  state->mailbox = enif_make_copy(state->mailbox_env, new_mailbox);
  enif_cond_signal(state->cond);
  enif_mutex_unlock(state->lock);
  return NP_ATOM_OK;
}

#define NATIVE_PROCESS_NIFS          \
  {"spawn_native", 0, spawn_native}, \
  {"send_native", 2, send_native}
