-module(prometheus_vm_system_info_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_process_collector/1]}.

test_process_collector(_) ->
  prometheus_registry:register_collector(prometheus_vm_system_info_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_limit")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors_available")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors_online")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_port_count")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_port_limit")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_process_count")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_process_limit")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_schedulers")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_schedulers_online")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_smp_support")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_threads")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_thread_pool_size")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_time_correction"))
  ].
