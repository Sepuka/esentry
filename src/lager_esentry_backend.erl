-module(lager_esentry_backend).

-behaviour(gen_event).

-export([
  init/1,
  handle_call/2,
  handle_event/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {level}).

-include_lib("lager/include/lager.hrl").
-define(TERSE_FORMAT,[time, " ", "[", severity,"] ", message]).

%% @private
init([{level, Level}]) ->
  {ok, #state{level=lager_util:config_to_mask(Level)}}.

handle_call(_Request, State) ->
  {ok, ok, State}.

%% @private
handle_event({log, Message}, #state{level=Level} = State) ->
  case lager_util:is_loggable(Message, Level, ?MODULE) of
    true ->
      esentry:log(lager_msg:severity(Message), lager_msg:message(Message)),
      {ok, State};
    false ->
      {ok, State}
  end;
handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.