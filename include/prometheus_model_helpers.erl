-module(prometheus_model_helpers).

-export([create_mf/5,
         label_pairs/1,
         gauge_metrics/1,
         gauge_metric/1,
         gauge_metric/2,
         counter_metric/1,
         counter_metric/2]).

-include("prometheus_model.hrl").

create_mf(Name, Help, Type, Collector, CollectorData) ->
  #'MetricFamily'{name = Name,
                  help = Help,
                  type = Type,
                  metric = ensure_list(Collector:collect_metrics(Name, CollectorData))}.

label_pairs([]) ->
  [];
label_pairs(Labels) ->
  lists:map(fun({Name, Value}) ->
                #'LabelPair'{name = Name,
                             value = Value}
            end,
            Labels).

gauge_metrics(Metrics) ->
  lists:map(fun(Spec) ->
                case Spec of
                  {Labels, Value} ->
                    gauge_metric(Labels, Value);
                  {Value} ->
                    gauge_metric(Value);
                  Value ->
                    gauge_metric(Value)
                end
            end,
            Metrics).

gauge_metric(Value) ->
  gauge_metric([], Value).

gauge_metric(Labels, Value) ->
  #'Metric'{label = label_pairs(Labels),
            gauge = #'Gauge'{value=Value}}.

counter_metric(Value) ->
  counter_metric([], Value).

counter_metric(Labels, Value) ->
  #'Metric'{label = label_pairs(Labels),
            gauge = #'Counter'{value=Value}}.

ensure_list(Val) when is_list(Val) ->
  Val;
ensure_list(Val) ->
  [Val].
