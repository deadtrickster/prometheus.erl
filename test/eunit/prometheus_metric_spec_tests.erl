-module(prometheus_metric_spec_tests).

-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
  Spec = [{name, "qwe"}],

  ?assertMatch(undefined,
               prometheus_metric_spec:get_value(labels, Spec)),
  ?assertMatch([default],
               prometheus_metric_spec:get_value(labels, Spec, [default])),

  ?assertEqual("qwe", prometheus_metric_spec:get_value(name, Spec)).

fetch_value_test() ->
  Spec = [{name, "qwe"}],

  ?assertError({missing_metric_spec_key, labels, Spec},
               prometheus_metric_spec:fetch_value(labels, Spec)),

  ?assertEqual("qwe", prometheus_metric_spec:fetch_value(name, Spec)).
