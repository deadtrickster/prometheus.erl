-module('prometheus_mnesia_collector_tests').

-include_lib("eunit/include/eunit.hrl").

prometheus_mnesia_off_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_mnesia_off_collector/1]}.

test_mnesia_off_collector(_) ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch(nomatch, re:run(Metrics, "erlang_mnesia_held_locks"))
  ].

prometheus_mnesia_on_test_() ->
  {foreach,
   fun() -> mnesia:start(),
            prometheus_eunit_common:start() end,
   fun(X) -> mnesia:stop(),
             prometheus_eunit_common:stop(X) end,
   [fun test_mnesia_on_collector/1]}.

test_mnesia_on_collector(_) ->
  prometheus_registry:register_collector(prometheus_mnesia_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_held_locks"))
  ].
