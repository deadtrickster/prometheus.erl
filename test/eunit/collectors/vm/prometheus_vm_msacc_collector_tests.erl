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
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep")),
   %% Extra.
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers"))
  ].


test_all_metrics(_) ->
  try
    application:set_env(prometheus, vm_msacc_collector_metrics,
                        [
                         %% Base.
                         aux,
                         check_io,
                         emulator,
                         gc,
                         other,
                         port,
                         sleep,
                         %% Extra.
                         alloc,
                         bif,
                         busy_wait,
                         ets,
                         gc_full,
                         nif,
                         send,
                         timers
                        ]),
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    Metrics = prometheus_text_format:format(),
    [
     %% Base.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep")),
     %% Extra.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers"))
    ]

  after
    application:unset_env(prometheus, vm_msacc_collector_metrics)
  end.


test_custom_metrics(_) ->
  try
    application:set_env(prometheus, vm_msacc_collector_metrics,
                        [
                         aux,
                         emulator,
                         gc,
                         gc_full,
                         nif
                        ]),
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    Metrics = prometheus_text_format:format(),
    [
     %% Base.
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_check_io")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_other")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_port")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_sleep")),
     %% Extra.
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_alloc")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_bif")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_busy_wait")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_ets")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_send")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_timers"))
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
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux{node="))
  ].
