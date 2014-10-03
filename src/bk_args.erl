%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <anton@alferov.me>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 02 Oct 2014 by Anton I Alferov <anton@alferov.me>
%%%-------------------------------------------------------------------

-module(bk_args).
-export([read/1]).

read(Args) -> complete([Result || Arg <- Args, {_, Result} <- [read_arg(Arg)]]).
read_arg(Arg) -> (hd(lists:dropwhile(fun not_succeeded/1, readers(Arg))))().

complete(Args) -> lists:flatten([
	[{Key, Value} || Value <- Completer()] || {Key, Completer} <- completers(),
	not lists:keymember(Key, 1, Args)
]) ++ Args.

readers(Arg) -> [
	fun() -> {lists:member(Arg, bk_utils:years()), {year, Arg}} end,
	fun() -> {lists:member(Arg, bk_utils:months()), {month, Arg}} end,
	fun() -> {true, result(Arg)} end
].

completers() -> [
	{year, fun bk_utils:years/0},
	{month, fun bk_utils:months/0}
].

result([$d|Param]) -> {day, Param};
result([$g|Param]) -> {group, [$g|Param]};
result([$s|Param]) -> {string, Param}.

not_succeeded(Reader) -> {Succeeded, _} = Reader(), not Succeeded.
