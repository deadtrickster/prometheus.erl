%% @doc HTTP instrumentation helpers
-module(prometheus_http).

-export([microseconds_duration_buckets/0,
         status_class/1]).

-export_type([status_code/0,
              status_class/0]).

%%====================================================================
%% Types
%%====================================================================

-type status_code() :: pos_integer().
-type status_class() :: prometheus_model_helpers:label_value().

%%====================================================================
%% Public API
%%====================================================================

-spec microseconds_duration_buckets() -> prometheus_buckets:buckets().
%% @doc default microseconds buckets for measuring http requests duration
microseconds_duration_buckets() ->
  [10, 100, 1000, 10000, 100000, 300000, 500000,
   750000, 1000000, 1500000, 2000000, 3000000].

%% FIXME: throw invalid_value if StatusCode isn't a positive integer
-spec status_class(SCode) -> StatusClass when
    SCode :: status_code(),
    StatusClass :: status_class().
%% @doc returns status class for http status code
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 100 ->
  "unknown";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 200 ->
  "informational";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 300 ->
  "success";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 400 ->
  "redirection";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 500 ->
  "client-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 600 ->
  "server-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode >= 600 ->
  "unknown";
status_class(C) ->
  erlang:error({invalid_value, C, "status code must be a positive integer"}).
