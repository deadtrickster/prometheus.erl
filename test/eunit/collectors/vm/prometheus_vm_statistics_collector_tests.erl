-module(prometheus_vm_statistics_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_process_collector/1]}.

test_process_collector(_) ->
  prometheus_registry:register_collector(prometheus_vm_statistics_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _},
                 re:run(Metrics,
                        "erlang_vm_statistics_garbage_collection_number_of_gcs")),
   ?_assertMatch({match, _},
                 re:run(Metrics,
                        "erlang_vm_statistics_garbage_collection_words_reclaimed")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_bytes_received_total")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_bytes_output_total")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_reductions_total")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_run_queues_length_total")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_runtime_milliseconds")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_context_switches")),
   ?_assertMatch({match, _},
                 re:run(Metrics, "erlang_vm_statistics_wallclock_time_milliseconds"))
  ].
