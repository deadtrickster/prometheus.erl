-module(prometheus_http).

-export([microseconds_duration_buckets/0,
         status_class/1]).

%%====================================================================
%% Http instrumentation helpers
%%====================================================================

%% @doc default microseconds buckets for measuring http requests duration
microseconds_duration_buckets() ->
  [10, 100, 1000, 10000, 100000, 300000, 500000,
   750000, 1000000, 1500000, 2000000, 3000000].

%% @doc returns status class for http status code
status_class(StatusCode) when StatusCode < 100 ->
  "unknown";
status_class(StatusCode) when StatusCode < 200 ->
  "informational";
status_class(StatusCode) when StatusCode < 300 ->
  "success";
status_class(StatusCode) when StatusCode < 400 ->
  "redirection";
status_class(StatusCode) when StatusCode < 500 ->
  "client-error";
status_class(StatusCode) when StatusCode < 600 ->
  "server-error";
status_class(StatusCode) when StatusCode >= 600 ->
  "unknown".
