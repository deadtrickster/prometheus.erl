/* Copyright (c) 2017, Benoit Chesneau <bchesneau@gmail.com>.
 *
 * This file is part of prometheus released under the MIT license.
 * See the NOTICE for more information.
 */

#include <syslog.h>

#include <new>
#include <stdexcept>

#include "prometheus_nif.h"

#include "erl_nif.h"

#ifndef ATOMS_H
#include "atoms.h"
#endif

#include "value.h"


static ErlNifFunc nif_funcs[] =
{
    {"new_value", 0, prometheus::NewValue},
    {"inc_value", 1, prometheus::IncValue},
    {"inc_value", 2, prometheus::IncValue},
    {"dec_value", 1, prometheus::DecValue},
    {"dec_value", 2, prometheus::DecValue},
    {"set_value", 2, prometheus::SetValue},
    {"get_value", 1, prometheus::GetValue}
};

namespace prometheus {

    ERL_NIF_TERM ATOM_OK;
    ERL_NIF_TERM ATOM_ERROR;
    ERL_NIF_TERM ATOM_EINVAL;
    ERL_NIF_TERM ATOM_BADARG;

    
    ErlNifResourceType *m_Value_RESOURCE;

    void
    value_resource_cleanup(ErlNifEnv *env, void *res)
    {

    }

    void
    CreateValueType(ErlNifEnv *env)
    {
        ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
        m_Value_RESOURCE = enif_open_resource_type(env, NULL, "prometheus_Value", value_resource_cleanup, flags, NULL);
        return;
    }

    ERL_NIF_TERM
    NewValue(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[])
    {
        prometheus::Value * value;
        void *alloc_ptr;
        alloc_ptr = enif_alloc_resource(m_Value_RESOURCE, sizeof(prometheus::Value));
        value = new(alloc_ptr) prometheus::Value();
        ERL_NIF_TERM result = enif_make_resource(env, value);
        enif_release_resource(value);
        return enif_make_tuple2(env, ATOM_OK, result);
    }

    ERL_NIF_TERM
    IncValue(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[])
    {
        prometheus::Value* value_ptr;

        if(!enif_get_resource(env, argv[0], m_Value_RESOURCE, (void **) &value_ptr))
            return enif_make_badarg(env);

        if(argc > 1)
        {
            double v;
            if (!enif_get_double(env, argv[1], &v))
                return enif_make_badarg(env);    
            value_ptr->Increment(v);
        }
        else
        {
            value_ptr->Increment();
        }

        return ATOM_OK;
    }

    ERL_NIF_TERM
    DecValue(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[])
    {
        prometheus::Value* value_ptr;

        if(!enif_get_resource(env, argv[0], m_Value_RESOURCE, (void **) &value_ptr))
            return enif_make_badarg(env);

        if(argc > 1)
        {
            double v;
            if (!enif_get_double(env, argv[1], &v))
                return enif_make_badarg(env);    
            value_ptr->Decrement(v);
        }
        else
        {
            value_ptr->Decrement();
        }

        return ATOM_OK;
    }

    ERL_NIF_TERM
    SetValue(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[])
    {
        prometheus::Value* value_ptr;

        if(!enif_get_resource(env, argv[0], m_Value_RESOURCE, (void **) &value_ptr))
            return enif_make_badarg(env);

        double v;
        if (!enif_get_double(env, argv[1], &v))
            return enif_make_badarg(env);    
        value_ptr->Set(v);

        return ATOM_OK;
    }

    ERL_NIF_TERM
    GetValue(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[])
    {
        prometheus::Value* value_ptr;

        if(!enif_get_resource(env, argv[0], m_Value_RESOURCE, (void **) &value_ptr))
            return enif_make_badarg(env);

        double v;
        v = value_ptr->GetValue();
        return enif_make_double(env, v);
    }
    
} // namespace prometheus


/*nif initialization  */

static void on_unload(ErlNifEnv *env, void *priv_data)
{
}


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    try
    {
        prometheus::CreateValueType(env);

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
        // initialize the atoms
        ATOM(prometheus::ATOM_OK, "ok");
        ATOM(prometheus::ATOM_ERROR, "error");
        ATOM(prometheus::ATOM_EINVAL, "einval");
        ATOM(prometheus::ATOM_BADARG, "badarg");
#undef ATOM

        return 0;
        
    }
    catch(...)
    {
    return -1;
    }
}

extern "C" {
    ERL_NIF_INIT(prometheus_value, nif_funcs, &on_load, NULL, NULL, &on_unload);
}
