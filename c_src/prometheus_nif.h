/* Copyright (c) 2017, Benoit Chesneau <bchesneau@gmail.com>.
 *
 * This file is part of instrument released under the MIT license.
 * See the NOTICE for more information.
 */


#ifndef INCL_instrument_H
#define INCL_instrument_H

extern "C" {
#include "erl_nif.h"

ERL_NIF_TERM instrument_new_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM instrument_inc_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM instrument_dec_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM instrument_set_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM instrument_get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

namespace prometheus {

ERL_NIF_TERM NewValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM IncValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM DecValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM SetValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM GetValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

} // namespace instrument

#endif
