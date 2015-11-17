-module(esentry).

-include_lib("esentry/include/esentry.hrl").

-export([log/2]).
-export([log/3]).
-export([fatal/2]).
-export([err/2]).
-export([warning/2]).
-export([info/2]).
-export([debug/2]).

log(emergency, Message) ->
  fatal(Message, #{});
log(alert, Message) ->
  error(Message, #{});
log(critical, Message) ->
  fatal(Message, #{});
log(error, Message) ->
  err(Message, #{});
log(warning, Message) ->
  warning(Message, #{});
log(notice, Message) ->
  warning(Message, #{});
log(info, Message) ->
  info(Message, #{});
log(debug, Message) ->
  debug(Message, #{}).

log(emergency, Message, Metadata) ->
  fatal(Message, Metadata);
log(alert, Message, Metadata) ->
  error(Message, Metadata);
log(critical, Message, Metadata) ->
  fatal(Message, Metadata);
log(error, Message, Metadata) ->
  err(Message, Metadata);
log(warning, Message, Metadata) ->
  warning(Message, Metadata);
log(notice, Message, Metadata) ->
  warning(Message, Metadata);
log(info, Message, Metadata) ->
  info(Message, Metadata);
log(debug, Message, Metadata) ->
  debug(Message, Metadata).

fatal(Msg, Metadata) ->
  Request = build_request(Msg, ?LEVEL_FATAL, Metadata),
  esentry_protocol:send(Request).

err(Msg, Metadata) ->
  Request = build_request(Msg, ?LEVEL_ERROR, Metadata),
  esentry_protocol:send(Request).

warning(Msg, Metadata) ->
  Request = build_request(Msg, ?LEVEL_WARNING, Metadata),
  esentry_protocol:send(Request).

info(Msg, Metadata) ->
  Request = build_request(Msg, ?LEVEL_INFO, Metadata),
  esentry_protocol:send(Request).

debug(Msg, Metadata) ->
  Request = build_request(Msg, ?LEVEL_DEBUG, Metadata),
  esentry_protocol:send(Request).

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

build_request(Msg, Level, Metadata) when is_list(Msg) ->
  build_request(list_to_binary(Msg), Level, Metadata);
build_request(Msg, Level, Metadata) ->
  Metadata#{
    event_id => event_id(),
    message => Msg,
    timestamp => qdate:unixtime(),
    level => Level,
    logger => ?LOGER_NAME,
    platform => <<"erlang">>
  }.
