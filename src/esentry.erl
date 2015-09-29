-module(esentry).

-include_lib("esentry/include/esentry.hrl").

-export([fatal/1]).
-export([fatal/2]).
-export([error/1]).
-export([error/2]).
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

error(Msg) ->
  Request = build_request(Msg, ?LEVEL_ERROR),
  esentry_protocol:send(Request).

error(Template, Args) ->
  Msg = build_msg(Template, Args),
  error(Msg).

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
  <<Part1:8, "-", Part2:4, "-", Part3:4, "-", Part4:4, "-", Part5:12>> = uuid:get_v4(),
  <<Part1, Part2, Part3, Part4, Part5>>.

build_request(Msg) ->
  build_request(Msg, ?LEVEL_ERROR).

build_request(Msg, Level) ->
  #{
    event_id => event_id(),
    message => Msg,
    timestamp => erlang:timestamp(),
    level => Level,
    loger => ?LOGER_NAME,
    platform => <<"erlang">>
  }.
