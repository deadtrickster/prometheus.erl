-module(prometheus_summary_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_int/1,
    fun test_double/1]}.

test_registration(_)->
  Name = orders_summary,
  Spec = [{name, Name}, {help, "Track orders count/total sum"}],
  [?_assertEqual(true,
                 prometheus_counter:declare(Spec)),
   ?_assertEqual(false,
                 prometheus_counter:declare(Spec)),
   ?_assertError({mf_already_exists, {default, Name}, "maybe you could try declare?"},
                 prometheus_counter:new(Spec))].

test_errors(_) ->
  prometheus_summary:new([{name, orders_summary}, {help, "Track orders count/total sum"}]),
  [%% basic name/labels/help validations test, lets hope new is using extract_common_params
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_summary:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"}, prometheus_summary:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_label_name, "quantile", "summary cannot have a label named \"quantile\""},
                 prometheus_summary:new([{name, "qwe"}, {labels, ["qua", "quantile"]}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_summary:new([{name, "qwe"}, {help, 12}])),
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
  timer:sleep(10), %% dobserve is async so let's make sure gen_server processed our increment request
  Value = prometheus_summary:value(orders_summary, [electronics]),
  prometheus_summary:reset(orders_summary, [electronics]),
  RValue = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual({2, 4.2}, Value),
   ?_assertEqual({0, 0}, RValue)].
