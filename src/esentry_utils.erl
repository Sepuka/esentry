-module(esentry_utils).

-export([format_reason/1]).

format_reason({Reason, Details}) ->
  BinaryReason = atom_to_binary(Reason, utf8),
  BinaryDetails = to_binary(Details),
  Trace = format_stacktrace(erlang:get_stacktrace()),
  <<BinaryReason/binary, ":", BinaryDetails/binary, 10, 13, "StackTrace:", Trace/binary>>;
format_reason(Reason) ->
  BinaryReason = atom_to_binary(Reason, utf8),
  Trace = format_stacktrace(erlang:get_stacktrace()),
  <<BinaryReason/binary, ":", 10, 13, "StackTrace:", Trace/binary>>.

to_binary(Map) when is_map(Map) ->
  try
    jsx:encode(Map)
  catch
    error:badarg ->
      List = maps:to_list(Map),
      jsx:encode(process_list(List))
  end.

process_list(Details) ->
  process_list(Details, []).

process_list([], Proplist) ->
  Proplist;
process_list([{Key, List} | Tail], Acc) when is_list(List) ->
  process_list(Tail, [{Key, format_stacktrace(List)}] ++ Acc);
process_list([{Key, Value} | Tail], Acc) ->
  process_list(Tail, [{Key, Value}] ++ Acc).

%% @private
format_stacktrace([]) ->
  <<>>;
format_stacktrace([{Module, Func, Arity, [{file, Path}, {line, Line}]} | Tail]) when is_integer(Arity) ->
  FuncName = iolist_to_binary([atom_to_binary(Module, utf8), ":", atom_to_binary(Func, utf8), "/", integer_to_binary(Arity)]),
  Step = iolist_to_binary([Path, ":", integer_to_binary(Line), " in fun ", FuncName, ";"]),
  iolist_to_binary([Step, 10, 13, format_stacktrace(Tail)]);
format_stacktrace([{Module, Func, Args, [{file, Path}, {line, Line}]} | Tail]) ->
  StrArgs = args_to_string(Args),
  FuncName = iolist_to_binary([atom_to_binary(Module, utf8), ":", atom_to_binary(Func, utf8), "(", StrArgs, ")"]),
  Step = iolist_to_binary([Path, ":", integer_to_binary(Line), " in fun ", FuncName, ";"]),
  iolist_to_binary([Step, 10, 13, format_stacktrace(Tail)]);
format_stacktrace([{Module, Func, Args0, Args1} | Tail]) ->
  FuncName = iolist_to_binary([atom_to_binary(Module, utf8), ":", atom_to_binary(Func, utf8), "(", args_to_string(Args0), ",", args_to_string(Args1), "])"]),
  iolist_to_binary([FuncName, 10, 13, format_stacktrace(Tail)]).

%% @private
args_to_string(Args) ->
  lists:flatten(io_lib:format("~p", [Args])).