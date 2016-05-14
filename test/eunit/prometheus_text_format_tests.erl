-module(prometheus_text_format_tests).

-include_lib("eunit/include/eunit.hrl").

escape_metric_help_test() ->
  ?assertEqual("qwe\\\\qwe\\nqwe", prometheus_text_format:escape_metric_help("qwe\\qwe\nqwe")).

escape_label_value_test()->
  ?assertEqual("qwe\\\\qwe\\nqwe\\\"qwe", prometheus_text_format:escape_label_value("qwe\\qwe\nqwe\"qwe")).

prometheus_format_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   fun(_) ->
       {inorder, [test_format()]}
   end}.

start() ->
  prometheus:start(),
  Collectors = prometheus_registry:collectors(default),
  prometheus_registry:clear(default),
  Collectors.

stop(DefaultCollectors) ->
  prometheus_registry:clear(custom_registry),
  [prometheus_registry:register_collector(default, Collector) || Collector <- DefaultCollectors],
  ok.

test_format() ->
  prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
  prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]),
  prometheus_summary:new([{name, orders_summary}, {help, "Track orders count/total sum"}]),
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {bounds, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"}]),
  prometheus_gauge:set(pool_size, 365),
  prometheus_counter:inc(http_requests_total),
  prometheus_summary:observe(orders_summary,  10),
  prometheus_summary:observe(orders_summary,  15),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 500),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 150),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 450),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 850),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 750),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 1650),
  ?_assertEqual("# TYPE pool_size gauge\n# HELP pool_size MongoDB Connections pool size\npool_size 365\n# TYPE http_requests_total counter\n# HELP http_requests_total Http request count\nhttp_requests_total 1\n# TYPE orders_summary summary\n# HELP orders_summary Track orders count/total sum\norders_summary_count 2\norders_summary_sum 25\n# TYPE http_request_duration_milliseconds histogram\n# HELP http_request_duration_milliseconds Http Request execution time\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"100\"} 3\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"300\"} 6\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"500\"} 7\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"750\"} 8\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"1000\"} 9\nhttp_request_duration_milliseconds_bucket{method=\"get\",le=\"+Inf\"} 9\nhttp_request_duration_milliseconds_count{method=\"get\"} 9\nhttp_request_duration_milliseconds_sum{method=\"get\"} 2622\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"100\"} 0\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"300\"} 1\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"500\"} 3\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"750\"} 4\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"1000\"} 5\nhttp_request_duration_milliseconds_bucket{method=\"post\",le=\"+Inf\"} 6\nhttp_request_duration_milliseconds_count{method=\"post\"} 6\nhttp_request_duration_milliseconds_sum{method=\"post\"} 4350\n\n",
                binary_to_list(prometheus_text_format:format())).
