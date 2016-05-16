-module(prometheus_histogram_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_errors/1,
    fun test_int/1,
    fun test_double/1]}.

test_errors(_) ->
  prometheus_histogram:new([{name, orders_summary}, {bounds, [100, 300, 500, 750, 1000]}, {help, "Track orders count/total sum"}]),
  [%% basic name/labels/help validations test, lets hope new is using extract_common_params
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_histogram:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"}, prometheus_histogram:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_histogram:new([{name, "qwe"}, {help, 12}])),
   %% summary specific errors
   ?_assertError({invalid_value, 1.5, "observe accepts only integers"}, prometheus_histogram:observe(orders_summary, 1.5)),
   ?_assertError({invalid_value, "qwe", "observe accepts only integers"}, prometheus_histogram:observe(orders_summary, "qwe")),
   ?_assertError({invalid_value, "qwe", "dobserve accepts only numbers"}, prometheus_histogram:dobserve(orders_summary, "qwe"))
  ].

test_int(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {bounds, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"}]),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950),
  Value = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
  prometheus_histogram:reset(http_request_duration_milliseconds, [get]),
  RValue = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
  [?_assertEqual({[3, 3, 1, 1, 1, 0], 2622}, Value),
   ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)].


test_double(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {bounds, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"}]),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 500.2),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 150.4),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 450.5),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 850.3),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 750.9),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 1650.23),
  timer:sleep(10), %% dobserve is async so let's make sure gen_server processed our increment request
  Value = prometheus_histogram:value(http_request_duration_milliseconds, [post]),
  prometheus_histogram:reset(http_request_duration_milliseconds, [post]),
  RValue = prometheus_histogram:value(http_request_duration_milliseconds, [post]),
  [?_assertEqual({[0, 1, 1, 1, 2, 1], 4352.53}, Value),
   ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)].
