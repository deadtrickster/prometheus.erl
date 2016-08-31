-module(prometheus_registry_tests).

-export([deregister_cleanup/1]).

-include_lib("eunit/include/eunit.hrl").

prometheus_registry_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   fun(ok) ->
       {inparallel, [default_registry(),
                     clear_registry()]}
   end}.

start() ->
  prometheus:start(),
  ok.

stop(ok) ->
  ok.

default_registry() ->
  ?_assertEqual([prometheus_vm_statistics_collector,
                 prometheus_vm_memory_collector,
                 prometheus_summary,
                 prometheus_histogram,
                 prometheus_gauge,
                 prometheus_counter],
                prometheus_registry:collectors(default)).

clear_registry() ->
  %% test whole register cleanup
  ok = prometheus_registry:register_collector(custom_registry, prometheus_registry_tests),
  Collectors = prometheus_registry:collectors(custom_registry),
  prometheus_registry:clear(custom_registry),

  %% remove just this one collector from the register
  ok = prometheus_collector:register(prometheus_registry_tests, custom_registry1),
  Collectors1 = prometheus_registry:collectors(custom_registry1),
  prometheus_collector:deregister(prometheus_registry_tests, custom_registry1),

  [?_assertEqual([prometheus_registry_tests], Collectors),
   ?_assertEqual([], prometheus_registry:collectors(custom_registry)),
   ?_assertEqual([prometheus_registry_tests], Collectors1),
   ?_assertEqual([], prometheus_registry:collectors(custom_registry1))].

deregister_cleanup(_) -> ok.
