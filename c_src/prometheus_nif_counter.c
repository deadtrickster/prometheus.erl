#define _GNU_SOURCE
#include "erl_nif.h"
#include <dirent.h>
#include <unistd.h>
#include <stdatomic.h>

typedef union {
  double d;
  unsigned long l;
} value;

typedef _Atomic struct {
  double counter;
} counter_t;

static ErlNifResourceType * prom_counter;

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);

#define MAXBUFLEN 1024

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM ret;

  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
      return enif_make_atom(env, atom);
    }

  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
make_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 0)
    {
      return enif_make_badarg(env);
    }
  
  counter_t* counter;

  counter = (counter_t*)enif_alloc_resource(prom_counter, sizeof(counter_t));

  counter->counter = 0;

  ERL_NIF_TERM term = enif_make_resource(env, counter);

  enif_release_resource(counter);

  return term;
}

static ERL_NIF_TERM
inc_counter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1)
    {
      return enif_make_badarg(env);
    }

  counter_t* counter;

  enif_get_resource(env, argv[0], prom_counter, (void**)&counter);

  counter_t expected = *counter;
  counter_t desired;

  do {
    desired.counter = expected.counter + 1;
  } while (!atomic_compare_exchange_strong(counter, &expected, desired));

  return enif_make_double(env, desired.counter);
}

static ERL_NIF_TERM
get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1)
    {
      return enif_make_badarg(env);
    }

  counter_t* counter;

  enif_get_resource(env, argv[0], prom_counter, (void**)&counter);

  return enif_make_double(env, counter->counter);
}

static ERL_NIF_TERM
reset (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1)
    {
      return enif_make_badarg(env);
    }

  counter_t* counter;

  enif_get_resource(env, argv[0], prom_counter, (void**)&counter);

  counter->counter = 0;

  return enif_make_double(env, counter->counter);
}


static ErlNifFunc nif_funcs[] = {
  {"make_counter", 0, make_counter},
  {"inc_counter", 1, inc_counter},
  {"value", 1, get_value},
  {"reset", 1, reset}
};

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  prom_counter = enif_open_resource_type(env, NULL, "prom_counter", NULL,
					 ERL_NIF_RT_CREATE, NULL);
  return 0;
}

ERL_NIF_INIT(prometheus_nif_counter, nif_funcs, &load, NULL, NULL, NULL);
