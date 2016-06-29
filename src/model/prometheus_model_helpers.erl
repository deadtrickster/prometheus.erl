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
  #'MetricFamily'{name = ensure_binary(Name),
                  help = ensure_binary(Help),
                  type = ensure_mf_type(Type),
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
            counter = #'Counter'{value=Value}}.

summary_metric(Labels, Count, Sum) ->
  #'Metric'{label = label_pairs(Labels),
            summary = #'Summary'{sample_count=Count,
                                 sample_sum=Sum}}.

histogram_metric(Labels, Buckets, Count, Sum) ->
  #'Metric'{label = label_pairs(Labels),
            histogram = #'Histogram'{sample_count=Count,
                                     sample_sum=Sum,
                                     bucket=histogram_buckets(Buckets)}}.


label_pairs([]) ->
  [];
label_pairs(Labels) ->
  lists:map(fun({Name, Value}) ->
                #'LabelPair'{name = ensure_binary(Name),
                             value = ensure_binary(Value)}
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

ensure_binary(Val) when is_atom(Val) ->
  atom_to_binary(Val, utf8);
ensure_binary(Val) when is_list(Val) ->
  list_to_binary(Val);
ensure_binary(Val) when is_binary(Val) ->
  Val;
ensure_binary(Val) ->
  list_to_binary(io_lib:format("~p", [Val])).


ensure_mf_type(gauge) ->
  'GAUGE';
ensure_mf_type(counter) ->
  'COUNTER';
ensure_mf_type(summary) ->
  'SUMMARY';
ensure_mf_type(histogram) ->
  'HISTOGRAM';
ensure_mf_type(untyped) ->
  'UNTYPED';
ensure_mf_type(Type) ->
  Type.
