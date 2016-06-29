-module(prometheus_model_helpers).

-export([create_mf/5,
         label_pairs/1,
         gauge_metrics/1,
         gauge_metric/1,
         gauge_metric/2,
         counter_metric/1,
         counter_metric/2,
         summary_metric/3,
         histogram_metric/4]).

-include("prometheus_model.hrl").

create_mf(Name, Help, Type, Collector, CollectorData) ->
  #'MetricFamily'{name = Name,
                  help = Help,
                  type = Type,
                  metric = ensure_list(Collector:collect_metrics(Name, CollectorData))}.

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

summary_metric(Labels, Count, Sum) ->
  #'Metric'{label = label_pairs(Labels),
            gauge = #'Summary'{sample_count=Count,
                               sample_sum=Sum}}.

histogram_metric(Labels, Buckets, Count, Sum) ->
  #'Metric'{label = label_pairs(Labels),
            gauge = #'Histogram'{sample_count=Count,
                                 sample_sum=Sum,
                                 bucket=histogram_buckets(Buckets)}}.


label_pairs([]) ->
  [];
label_pairs(Labels) ->
  lists:map(fun({Name, Value}) ->
                #'LabelPair'{name = Name,
                             value = Value}
            end,
            Labels).

histogram_buckets(Buckets) ->
  lists:map(fun({Bound, Count}) ->
                   #'Bucket'{upper_bound = Bound,
                             cumulative_count = Count}
               end,
            Buckets).

ensure_list(Val) when is_list(Val) ->
  Val;
ensure_list(Val) ->
  [Val].
