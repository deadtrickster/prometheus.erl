-module(prometheus_vm_memory_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_process_collector/1]}.

test_process_collector(_) ->
  prometheus_registry:register_collector(prometheus_vm_memory_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_tables")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dets_tables"))
  ].
