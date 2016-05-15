-module(prometheus_vm_memory_collector).
-export([register/0,
         register/1,
         deregister/1,
         collect_mf/2,
         collect_metrics/3]).

-behaviour(prometheus_collector).
-include("prometheus.hrl").

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister(_) -> ok.

collect_mf(Callback, _Registry) ->
  Memory = erlang:memory(),
  Callback(gauge, erlang_vm_memory_bytes_total, [kind], "The total amount of memory currently allocated. This is the same as the sum of the memory size for processes and system.", Memory),
  Callback(gauge, erlang_vm_memory_processes_bytes_total, [usage], "The total amount of memory currently allocated for the Erlang processes.", Memory),
  Callback(gauge, erlang_vm_memory_system_bytes_total, [usage], "The total amount of memory currently allocated for the emulator that is not directly related to ay Erlang process. Memory presented as processes is not included in this memory.", Memory),
  Callback(gauge, erlang_vm_memory_atom_bytes_total, [usage], "The total amount of memory currently allocated for atoms. This memory is part of the memory presented as system memory.", Memory),
  Callback(gauge, erlang_vm_ets_tables, [], "Erlang VM ETS Tables count", []),
  Callback(gauge, erlang_vm_dets_tables, [], "Erlang VM DETS Tables count", []).

collect_metrics(erlang_vm_memory_bytes_total, Callback, Memory) ->
  Callback([system], proplists:get_value(system, Memory)),
  Callback([processes], proplists:get_value(processes, Memory));
collect_metrics(erlang_vm_memory_processes_bytes_total, Callback, Memory) ->
  Callback([used], proplists:get_value(processes_used, Memory)),
  Callback([free], proplists:get_value(processes, Memory) - proplists:get_value(processes_used, Memory));
collect_metrics(erlang_vm_memory_system_bytes_total, Callback, Memory) ->
  Callback([atom], proplists:get_value(atom, Memory)),
  Callback([binary], proplists:get_value(binary, Memory)),
  Callback([code], proplists:get_value(code, Memory)),
  Callback([ets], proplists:get_value(ets, Memory)),
  Callback([other], proplists:get_value(system, Memory) - proplists:get_value(atom, Memory)
           - proplists:get_value(binary, Memory) - proplists:get_value(code, Memory) - proplists:get_value(ets, Memory));
collect_metrics(erlang_vm_memory_atom_bytes_total, Callback, Memory) ->
  Callback([used], proplists:get_value(atom_used, Memory)),
  Callback([free], proplists:get_value(atom, Memory) - proplists:get_value(atom_used, Memory));
collect_metrics(erlang_vm_ets_tables, Callback, _MFData) ->
  Callback([], length(ets:all()));
collect_metrics(erlang_vm_dets_tables, Callback, _MFData) ->
  Callback([], length(dets:all())).
