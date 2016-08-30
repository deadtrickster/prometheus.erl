-module(prometheus_vm_memory_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(prometheus_collector:callback(), atom()) -> ok.
%% @private
collect_mf(Callback, _Registry) ->
  Memory = erlang:memory(),

  Callback(create_gauge(erlang_vm_memory_bytes_total,
                        "The total amount of memory currently allocated. "
                        "This is the same as the sum of the memory size "
                        "for processes and system.",
                        Memory)),

  Callback(create_gauge(erlang_vm_memory_processes_bytes_total,
                        "The total amount of memory currently allocated "
                        "for the Erlang processes.",
                        Memory)),

  Callback(create_gauge(erlang_vm_memory_system_bytes_total,
                        "The total amount of memory currently allocated "
                        "for the emulator that is not directly related "
                        "to any Erlang process. Memory presented as processes "
                        "is not included in this memory.",
                        Memory)),

  Callback(create_gauge(erlang_vm_memory_atom_bytes_total,
                        "The total amount of memory currently allocated "
                        "for atoms. This memory is part of the memory "
                        "presented as system memory.",
                        Memory)),

  Callback(create_gauge(erlang_vm_ets_tables,
                        "Erlang VM ETS Tables count",
                        Memory)),

  Callback(create_gauge(erlang_vm_dets_tables,
                        "Erlang VM DETS Tables count",
                        Memory)).

%% @private
collect_metrics(erlang_vm_memory_bytes_total, Memory) ->
  gauge_metrics([
                 {[{kind, system}], proplists:get_value(system,  Memory)},
                 {[{kind, processes}], proplists:get_value(processes, Memory)}
                ]);
collect_metrics(erlang_vm_memory_processes_bytes_total, Memory) ->
  gauge_metrics([
                 {[{usage, used}], proplists:get_value(processes_used, Memory)},
                 {[{usage, free}],
                  proplists:get_value(processes, Memory)
                  - proplists:get_value(processes_used, Memory)}
                ]);
collect_metrics(erlang_vm_memory_system_bytes_total, Memory) ->
  gauge_metrics([
                 {[{usage, atom}], proplists:get_value(atom, Memory)},
                 {[{usage, binary}], proplists:get_value(binary, Memory)},
                 {[{usage, code}], proplists:get_value(code, Memory)},
                 {[{usage, ets}], proplists:get_value(ets, Memory)},
                 {[{usage, other}], memory_other(Memory)}
                ]);
collect_metrics(erlang_vm_memory_atom_bytes_total, Memory) ->
  gauge_metrics([
                 {[{usage, used}], proplists:get_value(atom_used, Memory)},
                 {[{usage, free}],
                  proplists:get_value(atom, Memory)
                  - proplists:get_value(atom_used, Memory)}
                ]);
collect_metrics(erlang_vm_ets_tables, _MFData) ->
  gauge_metric(length(ets:all()));
collect_metrics(erlang_vm_dets_tables, _MFData) ->
  gauge_metric(length(dets:all())).

%%====================================================================
%% Private Parts
%%====================================================================

memory_other(Memory) ->
  proplists:get_value(system, Memory)
    - proplists:get_value(atom, Memory)
    - proplists:get_value(binary, Memory)
    - proplists:get_value(code, Memory)
    - proplists:get_value(ets, Memory).

-spec create_gauge(Name, Help, Data) -> prometheus_model:'MetricFamily'() when
    Name :: atom(),
    Help :: string(),
    Data :: prometheus_collector:data().
create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).
