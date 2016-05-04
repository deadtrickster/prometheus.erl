-module(prometheus_summary).
-export([register/0,
         register/1,
         register/2,
         observe/2,
         observe/3,
         observe/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3,
         collect_mf/5,
         collect_metrics/3]).

-include("prometheus.hrl").
-compile({no_auto_import,[register/2]}).
-behaviour(prometheus_collector).

register() ->
  erlang:error(invalid_register_call).

register(Spec) ->
  register(Spec, default).

register(Spec, Registry) ->
  Name = proplists:get_value(name, Spec),
  Labels = proplists:get_value(labels, Spec, []),
  Help = proplists:get_value(help, Spec, ""),
                                                %Value = proplists:get_value(value, Spec),
  ok = prometheus_registry:register_collector(Registry, prometheus_summary, Name, Labels, Help).

observe(Name, Value) ->
  observe(default, Name, [], Value).

observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

observe(Registry, Name, LabelValues, Value) ->
  observe(?PROMETHEUS_SUMMARY_TABLE, Registry, Name, LabelValues, Value).

observe(Table, Registry, Name, LabelValues, Value) ->
  update_summary_counter(Table, Registry, Name, LabelValues, Value),
  ok.

update_summary_counter(Table, Registry, Name, LabelValues, Value) ->
  try
    ets:update_counter(Table, {Registry, Name, LabelValues}, {2, Value}),
    ets:update_counter(Table, {Registry, Name, LabelValues}, {3, 1})
  catch error:badarg ->
      ok = prometheus_metric:check_mf_exists(Registry, prometheus_summary, Name, length(LabelValues)),
      case ets:insert_new(Table, {{Registry, Name, LabelValues}, Value, 1}) of
        false -> %% some sneaky process already inserted
          update_summary_counter(Table, Registry, Name, LabelValues, Value);
        true ->
          ok
      end
  end.

reset(Name) ->
  reset(default, Name, []).
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
  ok = prometheus_metric:check_mf_exists(Registry, prometheus_summary, Name, length(LabelValues)),
  ets:update_element(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}, [{2, 0}, {3, 0}]).

value(Name) ->
  value(default, Name, []).

value(Name, LabelValues) ->
  value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
  [{_Key, Sum, Count}] = ets:lookup(?PROMETHEUS_SUMMARY_TABLE, {Registry, Name, LabelValues}),
  {Sum, Count}.

collect_mf(Callback, Registry, Name, Labels, Help) ->
  Callback(counter, Name, Labels, Help, ets:match(?PROMETHEUS_SUMMARY_TABLE, {{Registry, Name, '$1'}, '$2', '$3'})).

collect_metrics(Name, Callback, Values) ->
  [emit_summary_stat(Name, LabelValues, Sum, Count, Callback) || [LabelValues, Sum, Count] <- Values].

emit_summary_stat(Name, LabelValues, Sum, Count, Callback) ->
  Callback({atom_to_list(Name) ++ "_count" , LabelValues}, Count),
  Callback({atom_to_list(Name) ++ "_sum" , LabelValues}, Sum).
