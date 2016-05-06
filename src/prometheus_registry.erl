-module(prometheus_registry).
-export([collect/2,
         register_collector/2,
         register_collector/5,
         register_collector/6,
         collector_registeredp/4]).

-include("prometheus.hrl").

collect(Name, Callback) ->
  [apply(Callback, MF) || MF <-  ets:match(?PROMETHEUS_TABLE, {{Name, '$1', '$2', '_'}, '$3', '$4', '_'})].

register_collector(Registry, Collector) ->
  register_collector(Registry, Collector, Collector, [], "").

register_collector(Registry, Type, Name, Labels, Help) ->
  register_collector(Registry, Type, Name, Labels, Help, undefined).

register_collector(Registry, Type, Name, Labels, Help, Data) ->
  ets:insert_new(?PROMETHEUS_TABLE, {{Registry, Type, Name, length(Labels)}, Labels, Help, Data}),
  ok.

collector_registeredp(Registry, Type, Name, LabelValues) ->
  case ets:lookup(?PROMETHEUS_TABLE, {Registry, Type, Name, length(LabelValues)}) of
    [MF] -> {ok, MF};
    [] -> undefined
  end.
