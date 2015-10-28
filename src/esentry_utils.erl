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

to_binary(Map) ->
  jsx:encode(Map).

format_stacktrace([]) ->
  <<>>;
format_stacktrace([{Module, Func, Arity, [{file, Path}, {line, Line}]} | Tail]) ->
  FuncName = iolist_to_binary([atom_to_binary(Module, utf8), ":", atom_to_binary(Func, utf8), "/", integer_to_binary(Arity)]),
  Step = iolist_to_binary([Path, ":", integer_to_binary(Line), " in fun ", FuncName, ";"]),
  iolist_to_binary([Step, 10, 13, format_stacktrace(Tail)]);
format_stacktrace([{Module, Func, Args, _} | Tail]) ->
  FuncName = iolist_to_binary([atom_to_binary(Module, utf8), ":", atom_to_binary(Func, utf8)]),
  Step = io_lib:format("~s(~p)~n", [FuncName, Args]),
  iolist_to_binary([Step, 10, 13, format_stacktrace(Tail)]).