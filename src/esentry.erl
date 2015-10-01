-module(esentry).

-include_lib("esentry/include/esentry.hrl").

-export([fatal/1]).
-export([fatal/2]).
-export([err/1]).
-export([err/2]).
-export([warning/1]).
-export([warning/2]).
-export([info/1]).
-export([info/2]).
-export([debug/1]).
-export([debug/2]).

fatal(Msg) ->
  Request = build_request(Msg, ?LEVEL_FATAL),
  esentry_protocol:send(Request).

fatal(Template, Args) ->
  Msg = build_msg(Template, Args),
  fatal(Msg).

err(Msg) ->
  Request = build_request(Msg, ?LEVEL_ERROR),
  esentry_protocol:send(Request).

err(Template, Args) ->
  Msg = build_msg(Template, Args),
  err(Msg).

warning(Msg) ->
  Request = build_request(Msg, ?LEVEL_WARNING),
  esentry_protocol:send(Request).

warning(Template, Args) ->
  Msg = build_msg(Template, Args),
  warning(Msg).

info(Msg) ->
  Request = build_request(Msg, ?LEVEL_INFO),
  esentry_protocol:send(Request).

info(Template, Args) ->
  Msg = build_msg(Template, Args),
  info(Msg).

debug(Msg) ->
  Request = build_request(Msg, ?LEVEL_DEBUG),
  esentry_protocol:send(Request).

debug(Template, Args) ->
  Msg = build_msg(Template, Args),
  debug(Msg).

build_msg(Template, Args) ->
  io_lib:format(Template, Args).

%% @private
-spec event_id() -> binary().
event_id() ->
  <<Byte1:8, Byte2:8, Byte3:8, Byte4:8, Byte5:8, Byte6:8, Byte7:8, Byte8:8,
  Byte9:8, Byte10:8, Byte11:8, Byte12:8, Byte13:8, Byte14:8, Byte15:8, Byte16:8>> = uuid:get_v4(),
  ByteList = io_lib:format(
    "~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b~.16b",
    [Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]
  ),
  list_to_binary(lists:flatten(io_lib:format("~32..0s", [ByteList]))).

build_request(Msg, Level) when is_list(Msg) ->
  build_request(list_to_binary(Msg), Level);
build_request(Msg, Level) ->
  #{
    event_id => event_id(),
    message => Msg,
    timestamp => qdate:unixtime(),
    level => Level,
    loger => ?LOGER_NAME,
    platform => <<"erlang">>
  }.
