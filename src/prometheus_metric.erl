-module(prometheus_metric).
-export([mf_metrics/2,
         insert_mf/1,
         check_mf_exists/4]).

-include("prometheus.hrl").

mf_metrics({Registry, Type, Name, Labels}, Callback) ->
    [apply(Callback, [Name, Labels] ++ Metric) || Metric <- ets:match(metric_table(Type), {{Registry, Name, '$1'}, '$3'})].

metric_table(counter) ->
    ?PROMETHEUS_COUNTER_TABLE.

insert_mf(MF) ->
    ets:insert_new(?PROMETHEUS_TABLE, MF).

check_mf_exists(Registry, Type,  Name, LabelValues) ->
  prometheus_registry:collector_registeredp(Registry, Type, Name, LabelValues).
