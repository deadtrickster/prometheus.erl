-module(prometheus_vm_microstates_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [
    fun test_default_metrics/1,
    fun test_custom_metrics/1
   ]
  }.

test_default_metrics(_) ->
  prometheus_registry:register_collector(prometheus_vm_microstates_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_thread_microstates_microseconds"))
  ].

test_custom_metrics(_) ->
  Metrics = try
    application:set_env(prometheus, vm_microstates_collector_unit, second),
    prometheus_registry:register_collector(prometheus_vm_microstates_collector),
    prometheus_text_format:format()
  after
    application:unset_env(prometheus, vm_microstates_collector_unit)
  end,

  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_thread_microstates_seconds"))
  ].

