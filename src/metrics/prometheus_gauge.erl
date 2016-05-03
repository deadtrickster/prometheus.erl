-module(prometheus_gauge).

-include("prometheus.hrl").
-export([new/2,
				 new/3,
         set/2,
         set/3,
         set/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3]).

new(Name, Labels) ->
		new(Name, Labels, default).

new(Name, Labels, Registry) ->
		prometheus_metric:insert_mf({{Registry, gauge, Name, length(Labels)}, Labels, "HELP STRING"}),
    {Registry, gauge, Name, Labels}.

set({Registry, Name, LabelValues}, Value) ->
    set(Registry, Name, LabelValues, Value);
set(Name, LabelValues) when is_list(LabelValues) ->
    set(default, Name, LabelValues, 1);
set(Name, Value) ->
    set(default, Name, [], Value).

set(Name, LabelValues, Value) ->
    set(default, Name, LabelValues, Value).

set(Registry, Name, LabelValues, Value) ->
    set(?PROMETHEUS_GAUGE_TABLE, Registry, Name, LabelValues, Value).

set(Table, Registry, Name, LabelValues, Value) ->
    case ets:update_element(Table, {Registry, Name, LabelValues}, {1, Value}) of
        false ->
            ok = prometheus_metric:check_mf_exists(Registry, gauge, Name, length(LabelValues)),
            case ets:insert_new(Table, {{Registry, Name, LabelValues}, Value}) of
                false -> %% some sneaky process already inserted
                    set(Table, Registry, Name, LabelValues, Value);
                true ->
                    ok
            end;
        true ->
            ok
    end,
    ok.

reset({Registry, Name, LabelValues}) ->
    reset(Registry, Name, LabelValues);
reset(Name) ->
    reset(default, Name, []).

reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
    set(Registry, Name, LabelValues, 0).

value({Registry, Name, LabelValues}) ->
    value(Registry, Name, LabelValues);
value(Name) ->
    value(default, Name, []).

value(Name, LabelValues) ->
    value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
    [{_Key, Value}] = ets:lookup(?PROMETHEUS_GAUGE_TABLE, {Registry, Name, LabelValues}),
    Value.
