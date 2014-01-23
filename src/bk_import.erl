-module(bk_import).
-export([tabtxt/1]).

-define(BOM, [239, 187, 191]).

-define(CRLF, "\r\n").
-define(TAB, "\t").

tabtxt(FileName) -> read(bk_utils:read_file(FileName, ?CRLF)).

read(Content) ->
	GroupNames = read_group_names(hd(Content)),
	{GroupNames, [{Day, [{lists:nth(GroupNum, GroupNames), Items} ||
		{GroupNum, Items} <- Groups]} ||
		{Day, Groups} <- read_data(tl(Content))
	]}.

read_group_names(?BOM ++ [H|T]) -> read_group_names(T, [], [H]).
read_group_names([$",H|T], Names, []) -> read_group_names(T, Names, [H]);
read_group_names([_|T], Names, []) -> read_group_names(T, Names, []);
read_group_names([$"|T], Names, Acc) ->
	read_group_names(T, [tl(lists:reverse(tl(Acc)))|Names], []);
read_group_names([H|T], Names, Acc) -> read_group_names(T, Names, [H|Acc]);
read_group_names([], Names, _) -> tl(lists:reverse(Names)).

read_data(Text) -> read_data(Text, []).
read_data(L = [H|_], Days) -> read_data(L, Days, head_to_day(H));
read_data([], Days) -> lists:reverse(Days).
read_data([H|T], [{Day, Groups}|Days], false) ->
	read_data(T, [{Day, update_groups(Groups, read_items(H))}|Days]);
read_data([_|T], Days, Day) -> read_data(T, [{Day, []}|Days]).

update_groups(Groups, Items) -> lists:foldl(fun update_group/2, Groups, Items).

update_group({GroupNum, Item}, Groups) ->
	case lists:keyfind(GroupNum, 1, Groups) of
		{GroupNum, Items} -> lists:keyreplace(
			GroupNum, 1, Groups, {GroupNum, Items ++ [Item]});
		false -> lists:keysort(1, [{GroupNum, [Item]}|Groups])
	end.

read_items(Items) -> read_items(Items, 0, [], []).
read_items(?TAB ++ T, Pos, [], Items) -> read_items(T, Pos + 1, [], Items);
read_items(?TAB ++ T, Pos, Acc, [{Item, []}|Items]) ->
	read_items(T, Pos + 1, [],
		[{group_num(Pos), {unquote_item(Item), lists:reverse(Acc)}}|Items]);
read_items(?TAB ++ T, Pos, Acc, Items) ->
	read_items(T, Pos + 1, [], [{lists:reverse(Acc), []}|Items]);
read_items([H|T], Pos, Acc, Items) -> read_items(T, Pos, [H|Acc], Items);
read_items([], Pos, Acc, [{Item, []}|Items]) -> lists:reverse(
	[{group_num(Pos), {unquote_item(Item), lists:reverse(Acc)}}|Items]);
read_items([], _, _, Items) -> lists:reverse(Items).

unquote_item(Item) -> [X || X <- Item, X /= $"].

head_to_day([H1,H2|_]) ->
	try list_to_integer([H1,H2]) catch _:_ ->
	try list_to_integer([H1]) catch _:_ -> false end end.

group_num(Pos) -> Pos - (Pos div 2 + 1).
