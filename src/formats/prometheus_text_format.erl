-module(prometheus_text_format).
-export([content_type/0,
         format/0,
         format/1,
         registry_collect_callback/6,
         collector_metrics_callback/5]).

-include("prometheus.hrl").

-behaviour(prometheus_format).

content_type() ->
  <<"text/plain; version=0.0.4">>.

format() ->
  format(default).

format(Registry) ->
  {ok, Fd} = ram_file:open("", [write,read,binary]),
  prometheus_registry:collect(Registry, fun (Type, Name, Labels, Help) ->
                                            registry_collect_callback(Fd, Registry, Type, Name, Labels, Help)
                                        end),
  file:write(Fd, io_lib:format("\n", [])),
  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

emit_mf_prologue(Fd, Type, Name, Help) ->
  file:write(Fd, io_lib:format("# TYPE ~s ~s\n# HELP ~s ~s\n", [Name, Type, Name, escape_metric_help(Help)])).

registry_collect_callback(Fd, Registry, Type, Name, Labels, Help) -> 
  Type:collect_mf(
    fun (Type1, MFName, Labels1, Help1, MFData) ->
        emit_mf_prologue(Fd, Type1, MFName, Help1),
        Type:collect_metrics(MFName,
                             fun (Series, Value) ->
                                 case Series of
                                   {Name1, LabelValues} ->
                                     collector_metrics_callback(Fd, Name1, Labels1, LabelValues, Value);
                                   {Name1, NewLabels, LabelValues} ->
                                     collector_metrics_callback(Fd, Name1, Labels1 ++ NewLabels, LabelValues, Value);
                                   LabelValues ->
                                     collector_metrics_callback(Fd, MFName, Labels1, LabelValues, Value)
                                   end
                             end,
                             MFData)
    end,
    Registry, Name, Labels, Help).

labels_string(Labels, LabelValues) ->
  case length(Labels) of
    0 ->
      "";
    _ ->
      LabelsPList = lists:zip(Labels, LabelValues),
      "{" ++ string:join(lists:map(fun ({Label, Value1}) ->
                                       io_lib:format("~s=\"~s\"", [Label, escape_label_value(Value1)])
                                   end,
                                   LabelsPList),
                         ",") ++ "}"
  end.


collector_metrics_callback(Fd, Name, Labels, LabelValues, '') ->
  LString = labels_string(Labels, LabelValues),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " NaN\n", [Name]));
collector_metrics_callback(Fd, Name, Labels, LabelValues, undefined) ->
  LString = labels_string(Labels, LabelValues),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " NaN\n", [Name]));
collector_metrics_callback(Fd, Name, Labels, LabelValues, Value) ->
  LString = labels_string(Labels, LabelValues),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " ~p\n", [Name, Value])).

escape_metric_help(Help) ->
  sub(sub(Help, "\\", "\\\\"), "\n", "\\n").

escape_label_value(LValue) when is_list(LValue)->
  sub(sub(sub(LValue, "\\", "\\\\"), "\n", "\\n"), "\"", "\\\"");
escape_label_value('+Inf') ->
  "+Inf";
escape_label_value(LValue) ->
  escape_label_value(io_lib:format("~p", [LValue])).

sub(Str, Old, New) when is_atom(Str) ->
  sub(atom_to_list(Str), Old, New);
sub(Str,Old,New) ->
  RegExp = "\\Q"++Old++"\\E",
  re:replace(Str,RegExp,New,[multiline, {return, list}]).
