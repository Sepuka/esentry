-module(esentry_protocol).

-include_lib("esentry/include/esentry.hrl").

-export([send/1]).

-define(SENTRY_TIMESTAMP, qdate:unixtime()).
-define(SENTRY_CLIENT, <<?CLIENT_NAME/binary, "/", ?ESENTRY_VERSION>>).
-define(X_SENTRY_AUTH, iolist_to_binary(
  [
    "Sentry sentry_version=", ?SENTRY_VERSION, ",",
    "sentry_client=", ?SENTRY_CLIENT, ",",
    "sentry_timestamp=", ?SENTRY_TIMESTAMP, ",",
    "sentry_key=", esentry_config:get_public_key(), ",",
    "sentry_secret=", esentry_config:get_secret_key()
  ]
)).

headers() ->
  [
    {<<"X-Sentry-Auth">>, ?X_SENTRY_AUTH},
    {<<"Content-Type">>, <<"application/json">>},
    {<<"User-Agent">>, ?ESENTRY_USER_AGENT}
  ].

url() ->
  ProjectId = esentry_config:get_project_id(),
  Path = <<"/api/", ProjectId, "/store/">>,
  Host = esentry_config:get_host(),
  <<"https://", Host, Path>>.

send(Body) when is_map(Body) ->
  JSON = jsx:encode(Body),
  send(JSON);
send(Body) ->
  httpc:request(post, {url(), headers(), <<"application/json">>, Body}).