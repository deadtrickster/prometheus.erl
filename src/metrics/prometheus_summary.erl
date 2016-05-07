-module(prometheus_summary).
-export([register/0,
         register/1,
         new/1,
         new/2,
         observe/2,
         observe/3,
         observe/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3,
         collect_mf/2,
         collect_metrics/3]).

-include("prometheus.hrl").
-behaviour(prometheus_collector).
-behaviour(prometheus_metric).

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

new(Spec) ->
  new(Spec, default).

new(Spec, Registry) ->
  Name = proplists:get_value(name, Spec),
  Labels = proplists:get_value(labels, Spec, []),
  Help = proplists:get_value(help, Spec, ""),
  %Value = proplists:get_value(value, Spec),
  register(Registry),
  prometheus_metric:insert_mf(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, Labels, Help).

observe(Name, Value) ->
  observe(default, Name, [], Value).

observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

observe(Registry, Name, LabelValues, Value) ->
  try
    ets:update_counter(?PROMETHEUS_GAUGE_TABLE, {Registry, Name, LabelValues}, {2, Value}),
    ets:update_counter(?PROMETHEUS_GAUGE_TABLE, {Registry, Name, LabelValues}, {3, 1})
  catch error:badarg ->
      prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
      case ets:insert_new(?PROMETHEUS_GAUGE_TABLE, {{Registry, Name, LabelValues}, Value, 1}) of
        false -> %% some sneaky process already inserted
          observe(Registry, Name, LabelValues, Value);
        true ->
          ok
      end
  end.

reset(Name) ->
  reset(default, Name, []).
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues),
  ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{2, 0}, {3, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Sum, Count}] = ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}),
  {Sum, Count}.

collect_mf(Callback, Registry) ->
  [Callback(summary, Name, Labels, Help, [Registry]) || [Name, Labels, Help] <- prometheus_metric:metrics(?PROMETHEUS_SUMMARY_TABLE, Registry)].

collect_metrics(Name, Callback, [Registry]) ->
  [emit_summary_stat(Name, LabelValues, Sum, Count, Callback) || [LabelValues, Sum, Count] <- ets:match(?PROMETHEUS_COUNTER_TABLE, {{Registry, Name, '$1'}, '$2', '$3'})].

emit_summary_stat(Name, LabelValues, Sum, Count, Callback) ->
  Callback({atom_to_list(Name) ++ "_count" , LabelValues}, Count),
  Callback({atom_to_list(Name) ++ "_sum" , LabelValues}, Sum).
