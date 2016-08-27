-module(prometheus_summary_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_int/1,
    fun test_double/1,
    fun test_observe_duration/1,
    fun test_undefined_value/1]}.

test_registration(_)->
  Name = orders_summary,
  Spec = [{name, Name}, {help, "Track orders count/total sum"}],
  [?_assertEqual(true,
                 prometheus_counter:declare(Spec)),
   ?_assertEqual(false,
                 prometheus_counter:declare(Spec)),
   ?_assertError({mf_already_exists, {default, Name}, "Consider using declare instead."},
                 prometheus_counter:new(Spec))].

test_errors(_) ->
  prometheus_summary:new([{name, orders_summary}, {help, "Track orders count/total sum"}]),
  prometheus_summary:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),
  [%% basic name/labels/help validations test, lets hope new is using extract_common_params
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_summary:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"}, prometheus_summary:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_label_name, "quantile", "summary cannot have a label named \"quantile\""},
                 prometheus_summary:new([{name, "qwe"}, {labels, ["qua", "quantile"]}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_summary:new([{name, "qwe"}, {help, 12}])),
   %% mf/arity errors
   ?_assertError({unknown_metric, default, unknown_metric}, prometheus_summary:observe(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1}, prometheus_summary:observe(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric}, prometheus_summary:dobserve(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1}, prometheus_summary:dobserve(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric}, prometheus_summary:observe_duration(unknown_metric, fun() -> 1 end)),
   ?_assertError({invalid_metric_arity, 2, 1}, prometheus_summary:observe_duration(db_query_duration, [repo, db], fun() -> 1 end)),
   ?_assertError({unknown_metric, default, unknown_metric}, prometheus_summary:reset(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1}, prometheus_summary:reset(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric}, prometheus_summary:value(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1}, prometheus_summary:value(db_query_duration, [repo, db])),
   %% summary specific errors
   ?_assertError({invalid_value, 1.5, "observe accepts only integers"}, prometheus_summary:observe(orders_summary, 1.5)),
   ?_assertError({invalid_value, "qwe", "observe accepts only integers"}, prometheus_summary:observe(orders_summary, "qwe")),
   ?_assertError({invalid_value, "qwe", "dobserve accepts only numbers"}, prometheus_summary:dobserve(orders_summary, "qwe"))
  ].

test_int(_) ->
  prometheus_summary:new([{name, orders_summary}, {labels, [department]}, {help, "Track orders count/total sum"}]),
  prometheus_summary:observe(orders_summary, [electronics], 10),
  prometheus_summary:observe(orders_summary, [electronics], 15),
  Value = prometheus_summary:value(orders_summary, [electronics]),
  prometheus_summary:reset(orders_summary, [electronics]),
  RValue = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual({2, 25}, Value),
   ?_assertEqual({0, 0}, RValue)].


test_double(_) ->
  prometheus_summary:new([{name, orders_summary}, {labels, [department]}, {help, "Track orders count/total sum"}]),
  prometheus_summary:dobserve(orders_summary, [electronics], 1.5),
  prometheus_summary:dobserve(orders_summary, [electronics], 2.7),
  timer:sleep(10), %% dobserve is async so lets make sure gen_server processed our increment request
  Value = prometheus_summary:value(orders_summary, [electronics]),
  prometheus_summary:reset(orders_summary, [electronics]),
  RValue = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual({2, 4.2}, Value),
   ?_assertEqual({0, 0}, RValue)].

test_observe_duration(_) ->
  prometheus_summary:new([{name, fun_executing_summary}, {help, ""}]),
  prometheus_summary:observe_duration(fun_executing_summary, fun () ->
                                                                 timer:sleep(1000)
                                                             end),
  timer:sleep(10),
  {Count, Sum} = prometheus_summary:value(fun_executing_summary),

  try prometheus_summary:observe_duration(fun_executing_summary, fun () ->
                                                                     erlang:error({qwe})
                                                                 end)
  catch _:_ -> ok
  end,

  timer:sleep(10),
  {CountE, SumE} = prometheus_summary:value(fun_executing_summary),

  [?_assertEqual(1, Count),
   ?_assertEqual(2, CountE),
   ?_assertMatch(true, 0.9 < Sum andalso Sum < 1.2),
   ?_assertMatch(true, 0.9 < SumE andalso SumE < 1.2)].

test_undefined_value(_) ->  
  prometheus_summary:new([{name, orders_summary}, {labels, [department]}, {help, "Track orders count/total sum"}]),
  Value = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual(undefined, Value)].
