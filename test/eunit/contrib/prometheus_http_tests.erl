-module(prometheus_http_tests).

-include_lib("eunit/include/eunit.hrl").

-define(RFC_ACCEPT, "text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
        "text/html;level=2;q=0.4, */*;q=0.5").

-define(CHROME_ACCEPT, "application/xml,application/xhtml+xml,"
        "text/html;q=0.9,text/plain;q=0.8,image/png,image/*;q=0.9,*/*;q=0.5").

-define(PROMETHEUS_ACCEPT, "application/vnd.google.protobuf;"
        "proto=io.prometheus.client.MetricFamily;encoding=delimited;q=0.7,"
        "text/plain;version=0.0.4;q=0.3,"
        "application/json;schema=\"prometheus/telemetry\";version=0.0.2;q=0.2,"
        "*/*;q=0.1").

microseconds_duration_buckets_test() ->
  ?assertMatch([10, 25, 50, 100, 250, 500,
                1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
                1000000, 2500000, 5000000, 10000000],
               prometheus_http:microseconds_duration_buckets()).

status_class_test() ->
  ?assertError({invalid_value, "qwe", "status code must be a positive integer"},
               prometheus_http:status_class("qwe")),
  ?assertError({invalid_value, 1.2, "status code must be a positive integer"},
               prometheus_http:status_class(1.2)),
  ?assertError({invalid_value, -10, "status code must be a positive integer"},
               prometheus_http:status_class(-10)),
  ?assertMatch("unknown", prometheus_http:status_class(65)),
  ?assertMatch("informational", prometheus_http:status_class(155)),
  ?assertMatch("success", prometheus_http:status_class(255)),
  ?assertMatch("redirection", prometheus_http:status_class(355)),
  ?assertMatch("client-error", prometheus_http:status_class(455)),
  ?assertMatch("server-error", prometheus_http:status_class(565)),
  ?assertMatch("unknown", prometheus_http:status_class(655)).


parse_accept_test() ->
  ?assertMatch([{media_range, "text", "org", 0.5, []},
                {media_range, "text", "html", 1, []},
                {media_range, "*", "*", 1, [{"name", "value"}]},
                {media_range, "*", "*", 1, [{"name", "value"}, {"name1", "value1"}]},
                {media_range, "image", "*", 1, [{"name", "value"}]},
                {media_range, "image", "png", 1, [{"name", "value"}]},
                {media_range, "image", "*", 1, [{"name", "value"}, {"name1", "value1"}]},
                {media_range, "text", "plain", 0, [{"name", "value"}]}],
               prometheus_http:parse_accept("text/org;q=0.5,"
                                            "text/html,"
                                            "*/*;name=value,"
                                            "*/*;name=value;name1=value1,"
                                            "image/*;name=value,"
                                            "image/png;name=value,"
                                            "image/*;name=value;name1=value1,"
                                            "text/plain;q=qwe;name=value;orphaned")),

  ?assertMatch([{media_range, "image", "*", 1, [{"name", "value"}, {"name1", "value1"}]},
                {media_range, "image", "png", 1, [{"name", "value"}]}],
               prometheus_http:parse_accept("image/*;name=value;name1=value1,"
                                            "image/png;name=value")).

content_negotiation_test() ->

  ?assertEqual("text/html;level=1",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain",
                                                       "text/html;level=2",
                                                       "image/jpeg",
                                                       "text/html",
                                                       "text/html;level=3",
                                                       "text/html;level=1"])),

  ?assertEqual("text/html",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain",
                                                       "text/html;level=2",
                                                       "image/jpeg",
                                                       "text/html"])),

  ?assertEqual("text/html;level=3",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain",
                                                       "text/html;level=2",
                                                       "image/jpeg",
                                                       "text/html;level=3"])),

  ?assertEqual("image/jpeg",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain",
                                                       "text/html;level=2",
                                                       "image/jpeg"])),

  ?assertEqual("text/html;level=2",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain",
                                                       "text/html;level=2"])),

  ?assertEqual("text/plain",
               prometheus_http:negotiate(?RFC_ACCEPT, ["text/plain"])),


  ?assertEqual("image/png",
               prometheus_http:negotiate(?CHROME_ACCEPT, ["text/html",
                                                          "image/png"])),
  ?assertEqual("text/html",
               prometheus_http:negotiate(?CHROME_ACCEPT, ["text/html",
                                                          "text/plain",
                                                          "text/n3"])),
  ?assertEqual("text/plain",
               prometheus_http:negotiate(?CHROME_ACCEPT, ["text/n3",
                                                          "text/plain"])),
  ?assertEqual("image/jpg",
               prometheus_http:negotiate(?CHROME_ACCEPT, ["text/plain",
                                                          "image/jpg"])),
  ?assertEqual("text/n3",
               prometheus_http:negotiate(?CHROME_ACCEPT, ["text/n3",
                                                          "application/rdf+xml"])),

  ?assertEqual("text/n3",
               prometheus_http:negotiate("text/*;q=0.5",
                                         ["text/n3", "text/rdf+xml", "app/qwe"])),


  ?assertEqual(prometheus_protobuf_format,
               prometheus_http:negotiate(?PROMETHEUS_ACCEPT,
                                         [{prometheus_text_format:content_type(),
                                           prometheus_text_format},
                                          {prometheus_protobuf_format:content_type(),
                                           prometheus_protobuf_format}])),

  ?assertEqual(prometheus_text_format,
               prometheus_http:negotiate(?PROMETHEUS_ACCEPT,
                                         [{prometheus_text_format:content_type(),
                                           prometheus_text_format}])).
