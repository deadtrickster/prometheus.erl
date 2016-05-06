-module(prometheus_metric).
-export([insert_mf/1,
         check_mf_exists/4]).

-include("prometheus.hrl").
insert_mf(MF) ->
    ets:insert_new(?PROMETHEUS_TABLE, MF).

check_mf_exists(Registry, Type,  Name, LabelValues) ->
  prometheus_registry:collector_registeredp(Registry, Type, Name, LabelValues).
