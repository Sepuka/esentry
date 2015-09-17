-module(esentry_config).

-export([get_public_key/0]).
-export([get_secret_key/0]).

get_public_key() ->
  Settings = get_protocol_settings(),
  proplists:get_value(public_key, Settings).

get_secret_key() ->
  Settings = get_protocol_settings(),
  proplists:get_value(secret_key, Settings).

get_protocol_settings() ->
  {ok, ProtocolSettings} = application:get_env(esentry, protocol),
  ProtocolSettings.