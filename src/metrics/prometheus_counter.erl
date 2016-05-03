-module(prometheus_counter).

-include("prometheus.hrl").
-export([new/2,
				 new/3,
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3]).

new(Name, Labels) ->
		new(Name, Labels, default).

new(Name, Labels, Registry) ->
		prometheus_metric:insert_mf({{Registry, counter, Name, length(Labels)}, Labels, "HELP STRING"}),
    {Registry, counter, Name, Labels}.

inc({Registry, Name, LabelValues}) ->
    inc(Registry, Name, LabelValues, 1);
inc(Name) ->
    inc(default, Name, [], 1).

inc({Registry, Name, LabelValues}, Value) ->
    inc(Registry, Name, LabelValues, Value);
inc(Name, LabelValues) when is_list(LabelValues) ->
    inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
    inc(default, Name, [], Value).

inc(Name, LabelValues, Value) ->
    inc(default, Name, LabelValues, Value).

inc(Registry, Name, LabelValues, Value) ->
    inc(?PROMETHEUS_COUNTER_TABLE, Registry, Name, LabelValues, Value).

inc(Table, Registry, Name, LabelValues, Value) ->
    try ets:update_counter(Table, {Registry, Name, LabelValues}, Value)
    catch error:badarg ->
            ok = prometheus_metric:check_mf_exists(Registry, counter, Name, length(LabelValues)),
            case ets:insert_new(Table, {{Registry, Name, LabelValues}, Value}) of
                false -> %% some sneaky process already inserted
                    inc(Table, Registry, Name, LabelValues, Value);
                true ->
                    ok
            end
    end,
    ok.

reset({Registry, Name, LabelValues}) ->
    reset(Registry, Name, LabelValues);
reset(Name) ->
    reset(default, Name, []).

reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

reset(Registry, Name, LabelValues) ->
    ok = prometheus_metric:check_mf_exists(Registry, counter, Name, length(LabelValues)),
    ets:update_counter(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}, [{2, 1, 0, 0}]).

value({Registry, Name, LabelValues}) ->
    value(Registry, Name, LabelValues);
value(Name) ->
    value(default, Name, []).

value(Name, LabelValues) ->
    value(default, Name, LabelValues).

value(Registry, Name, LabelValues) ->
    [{_Key, Value}] = ets:lookup(?PROMETHEUS_COUNTER_TABLE, {Registry, Name, LabelValues}),
    Value.
