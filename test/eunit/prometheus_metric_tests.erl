-module(prometheus_metric_tests).

-include_lib("eunit/include/eunit.hrl").

validate_metric_name_test() ->
  ?assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_metric:validate_metric_name(12)),
  ?assertError({invalid_metric_name, <<0,0,123>>, "metric name is invalid string"}, prometheus_metric:validate_metric_name(<<0,0,123>>)),
  ?assertError({invalid_metric_name, "1qwe", "metric name doesn't match regex ^[a-zA-Z_:][a-zA-Z0-9_:]*$"}, prometheus_metric:validate_metric_name("1qwe")),

  ?assertEqual('qwe_:qwe', prometheus_metric:validate_metric_name('qwe_:qwe')),
  ?assertEqual("qwe_:qwe", prometheus_metric:validate_metric_name("qwe_:qwe")),
  ?assertEqual(<<"qwe_:qwe">>, prometheus_metric:validate_metric_name(<<"qwe_:qwe">>)).

validate_metric_label_names_test() ->
  ?assertError({invalid_metric_labels, 12, "not list"}, prometheus_metric:validate_metric_label_names(12)),
  ?assertError({invalid_metric_label_name, 12, "metric label is not a string"}, prometheus_metric:validate_metric_label_names([12])),
  ?assertError({invalid_metric_label_name, [0,0,123], "metric label is invalid string"}, prometheus_metric:validate_metric_label_names([<<0,0,123>>])),
  ?assertError({invalid_metric_label_name, "__qwe", "metric label can't start with __"}, prometheus_metric:validate_metric_label_names(["__qwe"])),
  ?assertError({invalid_metric_label_name, "qwe:", "metric label doesn't match regex ^[a-zA-Z_][a-zA-Z0-9_]*$"}, prometheus_metric:validate_metric_label_names(["qwe:"])),

  ?assertEqual(["_qwe", "weq123"], prometheus_metric:validate_metric_label_names([<<"_qwe">>, 'weq123'])),
  ?assertEqual(["_qwe", "weq123"], prometheus_metric:validate_metric_label_names([<<"_qwe">>, "weq123"])).

validate_metric_help_test() ->
  ?assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_metric:validate_metric_help(12)),
  ?assertError({invalid_metric_help, [0,0,123], "metric help is invalid string"}, prometheus_metric:validate_metric_help(<<0,0,123>>)),

  ?assertEqual("qwe_:qwe", prometheus_metric:validate_metric_help("qwe_:qwe")),
  ?assertEqual("qwe_:qwe", prometheus_metric:validate_metric_help(<<"qwe_:qwe">>)).

extract_common_params_test() ->
  ?assertError({invalid_metric_name, 12, "metric name is not a string"}, prometheus_metric:extract_common_params([{name, 12}])),
  ?assertError({invalid_metric_labels, 12, "not list"}, prometheus_metric:extract_common_params([{name, "qwe"}, {labels, 12}])),
  ?assertError({invalid_metric_help, 12, "metric help is not a string"}, prometheus_metric:extract_common_params([{name, "qwe"}, {labels, ["qwe"]}, {help, 12}])),

  ?assertEqual({default, "qwe", [], "qwe"}, prometheus_metric:extract_common_params([{name, "qwe"}, {help, "qwe"}])),
  ?assertEqual({qwe, "qwe", ["qwe"], "qwe"}, prometheus_metric:extract_common_params([{name, "qwe"}, {labels, ["qwe"]}, {help, "qwe"}, {registry, qwe}])).
