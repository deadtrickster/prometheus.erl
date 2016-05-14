-module(prometheus_registry).
-export([collect/2,
         collectors/1,
         register_collector/2,
         collector_registeredp/2,
         clear/0,
         clear/1]).

-include("prometheus.hrl").

collect(Registry, Callback) ->
  [Callback(Registry, Collector) || {_, Collector} <- ets:lookup(?PROMETHEUS_TABLE, Registry)].

collectors(Registry) ->
  [Collector || {_, Collector} <- ets:lookup(?PROMETHEUS_TABLE, Registry)].

register_collector(Registry, Collector) ->
  ets:insert(?PROMETHEUS_TABLE, {Registry, Collector}),
  ok.

clear() ->
  clear(default).

clear(Registry) ->
  ets:delete(?PROMETHEUS_TABLE, Registry).

collector_registeredp(Registry, Collector) ->
  case ets:match(?PROMETHEUS_TABLE, {Registry, Collector}) of
    [] -> false;
    _ -> true
  end.
