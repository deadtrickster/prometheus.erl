-module(prometheus_text_format_tests).

-include_lib("eunit/include/eunit.hrl").

escape_metric_help_test() ->
  ?assertEqual("qwe\\\\qwe\\nqwe", prometheus_text_format:escape_metric_help("qwe\\qwe\nqwe")).

escape_label_value_test()->
  ?assertEqual("qwe\\\\qwe\\nqwe\\\"qwe", prometheus_text_format:escape_label_value("qwe\\qwe\nqwe\"qwe")).
