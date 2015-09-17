-module(esentry).

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
  Request = build_request(Msg),
  esentry_protocol:send(Request).

fatal(Template, Args) ->
  Msg = build_msg(Template, Args),
  fatal(Msg).

error(Msg) ->
  Request = build_request(Msg),
  esentry_protocol:send(Request).

error(Template, Args) ->
  Msg = build_msg(Template, Args),
  error(Msg).

warning(Msg) ->
  Request = build_request(Msg),
  esentry_protocol:send(Request).

warning(Template, Args) ->
  Msg = build_msg(Template, Args),
  warning(Msg).

info(Msg) ->
  Request = build_request(Msg),
  esentry_protocol:send(Request).

info(Template, Args) ->
  Msg = build_msg(Template, Args),
  info(Msg).

debug(Msg) ->
  Request = build_request(Msg),
  esentry_protocol:send(Request)..

debug(Template, Args) ->
  Msg = build_msg(Template, Args),
  debug(Msg).

build_msg(Template, Args) ->
  io_lib:format(Template, Args).

%% @private
event_id() ->
  ok.

build_request(Msg) ->
  #{
    event_id => event_id(),
    message => Msg,
    timestamp => erlang:timestamp()
  }.
