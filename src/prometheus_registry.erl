-module(prometheus_registry).
-export([collect/2,
         register_collector/2,
         register_collector/5]).

-include("prometheus.hrl").

collect(Name, Callback) ->
  [apply(Callback, MF) || MF <-  ets:match(?PROMETHEUS_TABLE, {{Name, '$1', '$2', '_'}, '$3', '$4'})].

register_collector(Registry, Collector) ->
  ets:insert_new(?PROMETHEUS_TABLE, {{Registry, Collector, Collector, 0}, [], ""}),
  ok.

register_collector(Registry, Type, Name, Labels, Help) ->  
  ets:insert_new(?PROMETHEUS_TABLE, {{Registry, Type, Name, length(Labels)}, Labels, Help}),
  ok.
