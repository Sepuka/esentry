-module(esentry_protocol).

-include_lib("esentry/include/esentry.hrl").

-export([send/1]).

-define(SENTRY_TIMESTAMP, qdate:unixtime()).
-define(SENTRY_CLIENT, io_lib:format("~s/~p", [?CLIENT_NAME, ?SENTRY_VERSION])).
-define(X_SENTRY_AUTH, io_lib:format(
  "Sentry sentry_version=~B,sentry_client=~s,sentry_timestamp=~B,sentry_key=~s,sentry_secret=~s",
  [?SENTRY_VERSION, ?SENTRY_CLIENT, ?SENTRY_TIMESTAMP, esentry_config:get_public_key(), esentry_config:get_secret_key()]
)).

headers() ->
  [
    {"X-Sentry-Auth", ?X_SENTRY_AUTH},
    {"Content-Type", "application/json"},
    {"User-Agent", ?ESENTRY_USER_AGENT}
  ].

url() ->
  ProjectId = esentry_config:get_project_id(),
  Path = <<"/api/", ProjectId/binary, "/store/">>,
  Host = esentry_config:get_host(),
  lists:flatten(io_lib:format("http://~s~s", [Host, Path])).

send(Body) when is_map(Body) ->
  JSON = jsx:encode(Body),
  send(JSON);
send(Body) ->
  httpc:request(post, {url(), headers(), "application/json", Body}, [], []).