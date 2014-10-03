%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 20 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_slice).
-export([read/2]).

read(Data, Args) -> lists:foldl(fun({ReadKey, Process, FilterKey}, NewData) ->
	filter(FilterKey, NewData, Process(proplists:get_all_values(ReadKey, Args)))
end, Data, rules()).

rules() -> [
	{day, fun(Days) -> [list_to_integer(D) || D <- Days] end, days},
	{group, fun(Groups) -> [list_to_atom(G) || G <- Groups] end, groups},
	{string, fun(Strings) -> Strings end, strings}
].

filter(days, Data, Days) -> [
	X || X = {Day, _} <- Data,
	Days == [] orelse lists:member(Day, Days)
];
filter(groups, Data, Groups) -> [{Day, [
	X || X = {Group, _} <- DataGroups,
	Groups == [] orelse lists:member(Group, Groups)
]} || {Day, DataGroups} <- Data];

filter(strings, Data, Strings) -> [{Day, [{Group, [
	X || X = {Name, _} <- Items,
	Strings == [] orelse in_string(Name, Strings)
]} || {Group, Items} <- Groups]} || {Day, Groups} <- Data].

in_string(Str, Subs) -> lists:foldl(fun
	(_, true) -> true;
	(Sub, false) -> string:str(Str, Sub) > 0
end, false, Subs).
