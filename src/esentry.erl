-module(esentry).

fatal(Msg) ->
  ok.

fatal(Template, Args) ->
  ok.

error(Msg) ->
  ok.

error(Template, Args) ->
  ok.

warning(Msg) ->
  ok.

warning(Template, Args) ->
  ok.

info(Msg) ->
  ok.

info(Template, Args) ->
  ok.

debug(Msg) ->
  ok.

debug(Template, Args) ->
  ok.

build_msg(Template, Args) ->
  io_lib:format(Template, Args).

%% @private
event_id() ->
  ok.