-module(prometheus_registry).
-export([collect/2,
         collectors/1,
         register_collector/2,
         collector_registeredp/2,
         clear/0,
         clear/1]).

-include("prometheus.hrl").

-define(TABLE, ?PROMETHEUS_REGISTRY_TABLE).

collect(Registry, Callback) ->
  [Callback(Registry, Collector) || {_, Collector} <- ets:lookup(?TABLE, Registry)].

collectors(Registry) ->
  [Collector || {_, Collector} <- ets:lookup(?TABLE, Registry)].

register_collector(Registry, Collector) ->
  ets:insert(?TABLE, {Registry, Collector}),
  ok.

clear() ->
  clear(default).

clear(Registry) ->
  [Collector:deregister(Registry) || {_, Collector} <- ets:take(?TABLE, Registry)].

collector_registeredp(Registry, Collector) ->
  case ets:match(?TABLE, {Registry, Collector}) of
    [] -> false;
    _ -> true
  end.
