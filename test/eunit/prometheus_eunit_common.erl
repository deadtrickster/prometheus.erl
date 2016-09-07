-module(prometheus_eunit_common).

-export([start/0,
         stop/1,
         collect_mf_to_list/1,
         collect_mf_to_list/2]).

start() ->
  prometheus:start(),
  Collectors = prometheus_registry:collectors(default),
  prometheus_registry:clear(default),
  prometheus_registry:clear(qwe),
  erase(),
  Collectors.

stop(DefaultCollectors) ->
  prometheus_registry:clear(default),
  prometheus_registry:clear(qwe),
  prometheus_registry:register_collectors(default, DefaultCollectors),
  erase(),
  ok.

collect_mf_to_list(Collector) ->
  collect_mf_to_list(default, Collector).

collect_mf_to_list(Registry, Collector) ->
  try
    Callback = fun (MF) ->
                   put(Collector, [MF|get_list(Collector)])
               end,
    prometheus_collector:collect_mf(Registry, Collector, Callback),

    get_list(Collector)
  after
    erase(Collector)
  end.

get_list(Key) ->
  case get(Key) of
    undefined ->
      [];
    Value ->
      Value
  end.
