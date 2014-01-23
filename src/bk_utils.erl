%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 12 Dec 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_utils).

-export([read_file/1, read_file/2]).
-export([eval/1]).
-export([flatten/1, merge/2]).
-export([dir/1, file/2]).
-export([last_month_day/2]).

-include("bk_config.hrl").

-define(LF, "\n").
-define(Comment, $#).

read_file(FileName) -> read_file(FileName, ?LF).
read_file(FileName, LineSep) -> case file:read_file(FileName) of
	{ok, Binary} ->
		[Stripped || Token <- string:tokens(binary_to_list(Binary), LineSep),
			Stripped <- [strip_after(Token, ?Comment)], Stripped =/= []];
	_Error -> []
end.

eval("=" ++ Expr) -> eval(Expr);
eval(Expr) ->
	{ok, Scanned, _} = erl_scan:string(Expr ++ "."),
	{ok, Parsed} = erl_parse:parse_exprs(Scanned),
	{value, Value, _} = erl_eval:exprs(Parsed, []), Value.

flatten(L) -> flatten(L, []).
flatten([{H1, H2}|T], Acc) -> flatten(T, [H2,H1|Acc]);
flatten([{H1, H2, H3}|T], Acc) -> flatten(T, [H3,H2,H1|Acc]);
flatten([{H1, H2, H3, H4}|T], Acc) -> flatten(T, [H4,H3,H2,H1|Acc]);
flatten([], Acc) -> lists:reverse(Acc).

merge(L1, L2) -> merge(L1, L2, []).
merge([{H1, H2}|T12], [H3|T3], Acc) -> merge(T12, T3, [{H1, H2, H3}|Acc]);
merge([{H1, H2, H3}|T123], [H4|T4], Acc) ->
	merge(T123, T4, [{H1, H2, H3, H4}|Acc]);
merge([], [], Acc) -> lists:reverse(Acc).

strip_after(L, Char) -> strip_after(L, Char, []).
strip_after([Char|_], Char, Acc) -> lists:reverse(Acc);
strip_after([H|T], Char, Acc) -> strip_after(T, Char, [H|Acc]);
strip_after([], _Char, Acc) -> lists:reverse(Acc).

dir(Year) when is_integer(Year) -> dir(integer_to_list(Year));
dir(Year) -> filename:join(?DataPath, Year).

file(Year, Month) when is_integer(Month) ->
	file(Year, lists:nth(Month, ?Months));
file(Year, Month) ->
	true = lists:member(Month, ?Months),
	filename:join(dir(Year), Month).

last_month_day(Year, Month) when is_list(Year) ->
	last_month_day(list_to_integer(Year), Month);
last_month_day(Year, Month) when is_list(Month) -> last_month_day(
	Year, length(lists:takewhile(fun(M) -> M /= Month end, ?Months)) + 1);
last_month_day(Year, Month) -> calendar:last_day_of_the_month(Year, Month).
