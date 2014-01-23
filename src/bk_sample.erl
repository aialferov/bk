%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 12 Dec 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_sample).

-export([exists/0]).
-export([create/3, remove/0]).
-export([read/0]).

-export([file/0]).

-include("bk_config.hrl").

-define(GroupIndent, " ").
-define(ItemIndent, "  ").

-define(PriceSep, " ").

exists() -> filelib:is_file(?SampleFile).

create(Day, GroupsInfo, SampleTmpl = {_Header, _DayTmpl, _GroupTmpl}) ->
	file:write_file(?SampleFile, format(Day, GroupsInfo, SampleTmpl)).

remove() -> file:delete(?SampleFile).

read() -> read(bk_utils:read_file(?SampleFile), []).

file() -> ?SampleFile.

format(Day, GroupsInfo, {Header, DayTmpl, GroupTmpl}) -> io_lib:format(
	lists:flatten([Header, DayTmpl|[GroupTmpl || _ <- GroupsInfo]]),
	[Day|bk_utils:flatten(GroupsInfo)]
).

read([?ItemIndent ++ Item|T], [{Day, [{Group, Items}|Groups]}|Acc]) ->
	read(T, [{Day, [{Group, [read_item(Item)|Items]}|Groups]}|Acc]);
read([?GroupIndent ++ Group|T], [{Day, [{PrevGroup, Items}|Groups]}|Acc]) ->
	read(T, [{Day, [{list_to_atom(string:strip(Group)), []},
		{PrevGroup, lists:reverse(Items)}|Groups]}|Acc]);
read([?GroupIndent ++ Group|T], [{Day, Groups}|Acc]) ->
	read(T, [{Day, [{list_to_atom(string:strip(Group)), []}|Groups]}|Acc]);
read([Day|T], [{PrevDay, Groups}|Acc]) -> read(
	T, [{list_to_integer(Day), []}, {PrevDay, lists:reverse(Groups)}|Acc]);
read([Day|T], []) -> read(T, [{list_to_integer(Day), []}]);
read([], [{Day, Groups}|T]) -> lists:reverse([{Day, lists:reverse(Groups)}|T]).

read_item(Item) -> read_item(lists:reverse(Item), []).
read_item(?PriceSep ++ T, Price) -> {lists:reverse(T), Price};
read_item([H|T], Price) -> read_item(T, [H|Price]).
