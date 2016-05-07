-module(prometheus_registry).
-export([collect/2,
         register_collector/2,
         collector_registeredp/2]).

-include("prometheus.hrl").

collect(Registry, Callback) ->
  [Callback(Registry, Collector) || {_, Collector} <- ets:lookup(?PROMETHEUS_TABLE, Registry)].

register_collector(Registry, Collector) ->
  ets:insert(?PROMETHEUS_TABLE, {Registry, Collector}),
  ok.

collector_registeredp(Registry, Collector) ->
  case ets:match(?PROMETHEUS_TABLE, {Registry, Collector}) of
    [] -> false;
    _ -> true
  end.
