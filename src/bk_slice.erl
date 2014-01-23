%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 20 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_slice).
-export([read/2, read/3]).

read(Data, {day, Day}) when is_list(Day) ->
	read(Data, {day, list_to_integer(Day)});
read(Data, {day, Day}) -> [X || X = {DataDay, _} <- Data, DataDay == Day];
read(Data, {group, Group}) when is_list(Group) ->
	read(Data, {group, list_to_atom(Group)});
read(Data, {group, Group}) ->
	[{Day, [X || X = {DataGroup, _} <- Groups, DataGroup == Group]} ||
		{Day, Groups} <- Data].
read(Data, {day, Day}, {group, Group}) ->
	read(read(Data, {day, Day}), {group, Group}).
