-module(prometheus_text_format).
-export([content_type/0,
         format/0,
         format/1]).

-ifdef(TEST).
-export([escape_metric_help/1,
         escape_label_value/1]).
-endif.

-include("prometheus.hrl").
-include("prometheus_model.hrl").

-behaviour(prometheus_format).

%%====================================================================
%% Format API
%%====================================================================

-spec content_type() -> binary().
content_type() ->
  <<"text/plain; version=0.0.4">>.

%% @equiv format(default)
-spec format() -> binary().
format() ->
  format(default).

-spec format(Registry :: atom()) -> binary().
format(Registry) ->
  {ok, Fd} = ram_file:open("", [write,read,binary]),
  prometheus_registry:collect(Registry, fun (_, Collector) ->
                                            registry_collect_callback(Fd, Registry, Collector)
                                        end),
  file:write(Fd, io_lib:format("\n", [])),
  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

%%====================================================================
%% Private Parts
%%====================================================================

registry_collect_callback(Fd, Registry, Collector) ->
  Collector:collect_mf(
    fun (MF) ->
        emit_mf_prologue(Fd, MF),
        emit_mf_metrics(Fd, MF)
    end,
    Registry).

emit_mf_prologue(Fd, #'MetricFamily'{name=Name, help=Help, type=Type}) ->
  file:write(Fd, io_lib:format("# TYPE ~s ~s\n# HELP ~s ~s\n", [Name, string_type(Type), Name, escape_metric_help(Help)])).

emit_mf_metrics(Fd, #'MetricFamily'{name=Name, metric = Metrics}) ->
  [emit_metric(Fd, Name, Metric) || Metric <- Metrics].

emit_metric(Fd, Name, #'Metric'{label=Labels, counter=#'Counter'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels, gauge=#'Gauge'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels, summary=#'Summary'{sample_count=Count, sample_sum=Sum}}) ->
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum);
emit_metric(Fd, Name, #'Metric'{label=Labels, histogram=#'Histogram'{sample_count=Count,
                                                                     sample_sum=Sum,
                                                                     bucket=Buckets}}) ->
  [emit_histogram_bucket(Fd, Name, Labels, Bucket) || Bucket <- Buckets],
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum).

emit_histogram_bucket(Fd, Name, Labels, #'Bucket'{cumulative_count=BCount, upper_bound=BBound}) ->
  BLValue = bound_to_label_value(BBound),
  emit_series(Fd, [Name, "_bucket"], Labels++[#'LabelPair'{name="le", value=BLValue}], BCount).

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

labels_string(Labels) ->
  case length(Labels) of
    0 ->
      "";
    _ ->
      "{" ++ string:join(lists:map(fun (#'LabelPair'{name=Name, value=Value}) ->
                                       io_lib:format("~s=\"~s\"", [Name, escape_label_value(Value)])
                                   end,
                                   Labels),
                         ",") ++ "}"
  end.

emit_series(Fd, Name, Labels, '') ->
  LString = labels_string(Labels),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " NaN\n", [Name]));
emit_series(Fd, Name, Labels, undefined) ->
  LString = labels_string(Labels),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " NaN\n", [Name]));
emit_series(Fd, Name, Labels, Value) ->
  LString = labels_string(Labels),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " ~p\n", [Name, Value])).

escape_metric_help(Help) ->
  sub(sub(Help, "\\", "\\\\\\\\"), "\n", "\\\\n").

bound_to_label_value(Bound) when is_number(Bound) ->
  Bound;
bound_to_label_value(infinity) ->
  "+Inf".

-spec escape_label_value(binary() | iolist() | undefined) -> string().
escape_label_value(LValue) when is_list(LValue)->
  sub(sub(sub(LValue, "\\", "\\\\\\\\"), "\n", "\\\\n"), "\"", "\\\\\"");
escape_label_value(LValue) when is_binary(LValue) ->
  escape_label_value(binary_to_list(LValue));
escape_label_value(LValue) ->
  escape_label_value(io_lib:format("~p", [LValue])).

-spec sub(string() | atom(), string(), string()) -> string().
sub(Str, Old, New) when is_atom(Str) ->
  sub(atom_to_list(Str), Old, New);
sub(Str,Old,New) ->
  RegExp = "\\Q"++Old++"\\E",
  re:replace(Str,RegExp,New,[global, {return, list}]).
