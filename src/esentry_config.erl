-module(esentry_config).

-export([get_public_key/0]).
-export([get_secret_key/0]).
-export([get_host/0]).
-export([get_project_id/0]).

get_public_key() ->
  Settings = get_protocol_settings(),
  proplists:get_value(public_key, Settings).

get_secret_key() ->
  Settings = get_protocol_settings(),
  proplists:get_value(secret_key, Settings).

get_host() ->
  Settings = get_protocol_settings(),
  proplists:get_value(host, Settings).

get_project_id() ->
Settings = get_protocol_settings(),
proplists:get_value(project_id, Settings).

get_protocol_settings() ->
  {ok, ProtocolSettings} = application:get_env(esentry, protocol),
  ProtocolSettings.