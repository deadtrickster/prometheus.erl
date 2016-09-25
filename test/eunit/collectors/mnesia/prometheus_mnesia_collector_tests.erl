-module('prometheus_mnesia_collector_tests').

-include_lib("eunit/include/eunit.hrl").

prometheus_mnesia_off_test_() ->
  {setup,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_mnesia_off_collector/0]}.

test_mnesia_off_collector() ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  Metrics = prometheus_text_format:format(),
  ?assertMatch(nomatch, re:run(Metrics, "erlang_mnesia_held_locks")).

prometheus_mnesia_on_test_() ->
  {setup,
   fun() -> mnesia:start(),
            prometheus_eunit_common:start() end,
   fun(X) -> mnesia:stop(),
             prometheus_eunit_common:stop(X) end,
   [fun test_mnesia_on_collector/0,
    fun test_mnesia_on_collector_env_on/0,
    fun test_mnesia_on_collector_env_off/0]}.

test_mnesia_on_collector_env_on() ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  application:set_env(prometheus,mnesia_collector_metrics,
                      [transaction_coordinators]),
  Metrics = prometheus_text_format:format(),
  ?assertMatch({match,_}, re:run(Metrics, "erlang_mnesia_transaction_coord")),
  application:unset_env(prometheus,mnesia_collector_metrics,[]).

test_mnesia_on_collector_env_off() ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  application:set_env(prometheus,mnesia_collector_metrics,[]),
  Metrics = prometheus_text_format:format(),
  ?assertMatch(nomatch, re:run(Metrics, "erlang_mnesia_held_locks")),
  application:unset_env(prometheus,mnesia_collector_metrics,[]).

test_mnesia_on_collector() ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  Metrics = prometheus_text_format:format(),
  ?assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_held_locks")).
