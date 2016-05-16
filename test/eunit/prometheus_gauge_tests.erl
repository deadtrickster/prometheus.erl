-module(prometheus_gauge_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_errors/1,
    fun test_set/1]}.

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
