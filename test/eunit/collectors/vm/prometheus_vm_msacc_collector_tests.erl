-module(prometheus_vm_msacc_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_default_metrics/1,
    fun test_all_metrics/1,
    fun test_custom_metrics/1,
    fun test_global_labels/1]}.

test_default_metrics(_) ->
  prometheus_registry:register_collector(prometheus_vm_msacc_collector),
  Metrics = prometheus_text_format:format(),
  [
   %% Base.
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep_microseconds")),
   %% Extra.
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send_microseconds")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers_microseconds"))
  ].


test_all_metrics(_) ->
  try
    application:set_env(prometheus, vm_msacc_collector_metrics,
                        [
                         %% Base.
                         aux_microseconds,
                         check_io_microseconds,
                         emulator_microseconds,
                         gc_microseconds,
                         other_microseconds,
                         port_microseconds,
                         sleep_microseconds,
                         %% Extra.
                         alloc_microseconds,
                         bif_microseconds,
                         busy_wait_microseconds,
                         ets_microseconds,
                         gc_full_microseconds,
                         nif_microseconds,
                         send_microseconds,
                         timers_microseconds
                        ]),
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    Metrics = prometheus_text_format:format(),
    [
     %% Base.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep_microseconds")),
     %% Extra.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers_microseconds"))
    ]

  after
    application:unset_env(prometheus, vm_msacc_collector_metrics)
  end.


test_custom_metrics(_) ->
  try
    application:set_env(prometheus, vm_msacc_collector_metrics,
                        [
                         aux_microseconds,
                         emulator_microseconds,
                         gc_microseconds,
                         gc_full_microseconds,
                         nif_microseconds
                        ]),
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    Metrics = prometheus_text_format:format(),
    [
     %% Base.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_check_io_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_other_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_port_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_sleep_microseconds")),
     %% Extra.
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_alloc_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_bif_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_busy_wait_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_ets_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_microseconds")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_send_microseconds")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_timers_microseconds"))
    ]

  after
    application:unset_env(prometheus, vm_msacc_collector_metrics)
  end.


test_global_labels(_) ->
  Metrics = try
    prometheus:start(),
    application:set_env(prometheus, global_labels, [{node, node()}]),
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    prometheus_text_format:format()
  after
    application:unset_env(prometheus, global_labels)
  end,
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_microseconds{node="))
  ].
