%% @doc
%%
%% Serializes Prometheus registry using the latest
%% [text format](http://bit.ly/2cxSuJP).
%%
%% Example output:
%% <pre>
%%   # TYPE http_request_duration_milliseconds histogram
%%   # HELP http_request_duration_milliseconds Http Request execution time
%%   http_request_duration_milliseconds_bucket{method="post",le="100"} 0
%%   http_request_duration_milliseconds_bucket{method="post",le="300"} 1
%%   http_request_duration_milliseconds_bucket{method="post",le="500"} 3
%%   http_request_duration_milliseconds_bucket{method="post",le="750"} 4
%%   http_request_duration_milliseconds_bucket{method="post",le="1000"} 5
%%   http_request_duration_milliseconds_bucket{method="post",le="+Inf"} 6
%%   http_request_duration_milliseconds_count{method="post"} 6
%%   http_request_duration_milliseconds_sum{method="post"} 4350
%% </pre>
%% @end

-module(prometheus_text_format).
-export([content_type/0,
         format/0,
         format/1]).

-ifdef(TEST).
-export([escape_metric_help/1,
         escape_label_value/1,
         emit_mf_prologue/2,
         emit_mf_metrics/2
        ]).
-endif.

-include("prometheus.hrl").
-include("prometheus_model.hrl").

-behaviour(prometheus_format).

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% Format API
%%====================================================================

-spec content_type() -> binary().
%% @doc
%% Returns content type of the latest [text format](http://bit.ly/2cxSuJP).
%% @end
content_type() ->
  <<"text/plain; version=0.0.4">>.

%% @equiv format(default)
-spec format() -> binary().
%% @doc
%% Formats `default' registry using the latest text format.
%% @end
format() ->
  format(default).

-spec format(Registry :: prometheus_registry:registry()) -> binary().
%% @doc
%% Formats `Registry' using the latest text format.
%% @end
format(Registry) ->
  {ok, Fd} = ram_file:open("", [write, read, binary]),
  Callback = fun (_, Collector) ->
                 registry_collect_callback(Fd, Registry, Collector)
             end,
  prometheus_registry:collect(Registry, Callback),
  file:write(Fd, "\n"),
  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

%%====================================================================
%% Private Parts
%%====================================================================

registry_collect_callback(Fd, Registry, Collector) ->
  Callback = fun (MF) ->
                 emit_mf_prologue(Fd, MF),
                 emit_mf_metrics(Fd, MF)
             end,
  prometheus_collector:collect_mf(Registry, Collector, Callback).

%% @private
emit_mf_prologue(Fd, #'MetricFamily'{name=Name, help=Help, type=Type}) ->
  Bytes = ["# TYPE ", Name, " ", string_type(Type), "\n# HELP ",
           Name, " ", escape_metric_help(Help), "\n"],
  file:write(Fd, Bytes).

%% @private
emit_mf_metrics(Fd, #'MetricFamily'{name=Name, metric = Metrics}) ->
  [emit_metric(Fd, Name, Metric) || Metric <- Metrics].

emit_metric(Fd, Name, #'Metric'{label=Labels,
                                counter=#'Counter'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                gauge=#'Gauge'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                untyped=#'Untyped'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                summary=#'Summary'{sample_count=Count,
                                                   sample_sum=Sum,
                                                   quantile=Quantiles}}) ->
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum),
  [
    emit_series(
      Fd, [Name],
      Labels ++ [#'LabelPair'{name="quantile", value=io_lib:format("~p", [QN])}],
      QV)
    || #'Quantile'{quantile = QN, value = QV} <- Quantiles
  ];
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                histogram=#'Histogram'{sample_count=Count,
                                                       sample_sum=Sum,
                                                       bucket=Buckets}}) ->
  [emit_histogram_bucket(Fd, Name, Labels, Bucket) || Bucket <- Buckets],
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum).

emit_histogram_bucket(Fd, Name, Labels, #'Bucket'{cumulative_count=BCount,
                                                  upper_bound=BBound}) ->
  BLValue = bound_to_label_value(BBound),
  emit_series(Fd, [Name, "_bucket"],
              Labels ++ [#'LabelPair'{name="le", value=BLValue}], BCount).

string_type('COUNTER') ->
  "counter";
string_type('GAUGE') ->
  "gauge";
string_type('SUMMARY') ->
  "summary";
string_type('HISTOGRAM') ->
  "histogram";
string_type('UNTYPED') ->
  "untyped".

labels_string([])     -> "";
labels_string(Labels) ->
  Fun = fun (#'LabelPair'{name=Name, value=Value}) ->
            [Name, "=\"", escape_label_value(Value), "\""]
        end,
  ["{", join(",", lists:map(Fun, Labels)), "}"].

emit_series(Fd, Name, Labels, undefined) ->
  LString = labels_string(Labels),
  file:write(Fd, [Name, LString, " NaN\n"]);
emit_series(Fd, Name, Labels, Value) when is_integer(Value) ->
  LString = labels_string(Labels),
  file:write(Fd, [Name, LString, " ", integer_to_list(Value) , "\n"]);
emit_series(Fd, Name, Labels, Value) ->
  LString = labels_string(Labels),
  file:write(Fd, [Name, LString, " ", io_lib:format("~p", [Value]) , "\n"]).

%% @private
escape_metric_help(Help) ->
  escape_string(fun escape_help_char/1, Help).

%% @private
escape_help_char($\\ = X) ->
  <<X, X>>;
escape_help_char($\n) ->
  <<$\\, $n>>;
escape_help_char(X) ->
  <<X>>.

bound_to_label_value(Bound) when is_integer(Bound) ->
  integer_to_list(Bound);
bound_to_label_value(Bound) when is_float(Bound) ->
  float_to_list(Bound);
bound_to_label_value(infinity) ->
  "+Inf".

-spec escape_label_value(binary() | iolist() | undefined) -> binary().
%% @private
escape_label_value(LValue) when is_list(LValue); is_binary(LValue) ->
  escape_string(fun escape_label_char/1, LValue);
escape_label_value(Value) ->
  erlang:error({wtf, Value}).

%% @private
escape_label_char($\\ = X) ->
  <<X, X>>;
escape_label_char($\n) ->
  <<$\\, $n>>;
escape_label_char($" = X) ->
  <<$\\, X>>;
escape_label_char(X) ->
  <<X>>.

%% @private
escape_string(Fun, Str) when is_binary(Str) ->
  << <<(Fun(X))/binary>> || <<X:8>> <= Str >>;
escape_string(Fun, Str) ->
  escape_string(Fun, iolist_to_binary(Str)).

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-spec join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
    T :: term().

join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep, H|join_prepend(Sep, T)].
