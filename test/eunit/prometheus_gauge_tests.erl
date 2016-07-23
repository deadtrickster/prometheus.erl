-module(prometheus_gauge_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_set/1,
    fun test_set_to_current_time/1,
    fun test_track_inprogress/1]}.

test_registration(_)->
  Name = pool_size,
  Spec = [{name, Name}, {help, ""}],
  [?_assertEqual(true,
                 prometheus_counter:declare(Spec)),
   ?_assertEqual(false,
                 prometheus_counter:declare(Spec)),
   ?_assertError({mf_already_exists, {default, Name}, "maybe you could try declare?"},
                 prometheus_counter:new(Spec))].

test_errors(_) ->
  prometheus_gauge:new([{name, pool_size}, {help, ""}]),
  [%% basic name/labels/help validations test, lets hope new is using extract_common_params
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_gauge:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"}, prometheus_gauge:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_gauge:new([{name, "qwe"}, {help, 12}])),
   %% gauge specific errors,
   ?_assertError({invalid_value, "qwe", "set accepts only numbers"}, prometheus_gauge:set(pool_size, "qwe"))
  ].

test_set(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:set(pool_size, [mongodb], 100),
  Value = prometheus_gauge:value(pool_size, [mongodb]),
  prometheus_gauge:reset(pool_size, [mongodb]),
  RValue = prometheus_gauge:value(pool_size, [mongodb]),
  [?_assertEqual(100, Value),
   ?_assertEqual(0, RValue)].

test_set_to_current_time(_) ->
  prometheus_gauge:new([{name, cur_time}, {labels, []}, {help, ""}]),
  Timestamp = os:system_time(seconds),
  prometheus_gauge:set_to_current_time(cur_time),
  STimestamp = prometheus_gauge:value(cur_time),
  [?_assertEqual(Timestamp, STimestamp)].

test_track_inprogress(_) ->
  prometheus_gauge:new([{name, fun_executing_gauge}, {help, ""}]),
  Value = prometheus_gauge:track_inprogress(fun_executing_gauge, fun () ->
                                                                     prometheus_gauge:value(fun_executing_gauge)
                                                                 end),

  try prometheus_gauge:track_inprogress(fun_executing_gauge, fun () ->
                                                                 erlang:error({qwe})
                                                             end)
  catch _:_ -> ok
  end,

  [?_assertEqual(1, Value),
   ?_assertEqual(0, prometheus_gauge:value(fun_executing_gauge))].
