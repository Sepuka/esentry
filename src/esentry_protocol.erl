-module(esentry_protocol).

-include_lib("esentry/include/esentry.hrl").

-export([headers/0]).
-export([send/0]).

-define(X_SENTRY_AUTH,
  <<"Sentry sentry_version=", ?SENTRY_VERSION, ",",
  <<"sentry_client=">>, ?SENTRY_CLIENT>>, ",",
  <<"sentry_timestamp ">>, erlang:timestamp(), ",",
  <<"sentry_key=">>, esentry_config:get_public_key(), ",",
  <<"sentry_secret=">>, esentry_config:get_secret_key()
).
-define(SENTRY_CLIENT, <<?CLIENT_NAME, "/", ?ESENTRY_VERSION>>).

headers() ->
  [
    {<<"X-Sentry-Auth">>, ?X_SENTRY_AUTH},
    {<<"Content-Type">>, <<"application/json">>},
    {<<"User-Agent">>, ?ESENTRY_USER_AGENT}
  ].

send() ->
  httpc:request(post, {<<"Url">>, headers(), <<"ContentType">>, <<"Body">>}).