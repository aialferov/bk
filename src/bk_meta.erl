%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 09 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_meta).

-export([groups_info/0, ordered_groups_info/1]).
-export([update_groups_info/1]).

-include("bk_config.hrl").

groups_info() -> groups_info(meta()).
groups_info(Meta) -> case lists:keyfind(groups_info, 1, Meta) of
	{groups_info, GroupsInfo} -> GroupsInfo; false -> [] end.

ordered_groups_info(GroupNames) -> Info = groups_info(), lists:foldr(fun
	(_Name, false) -> false;
	(Name, Acc) -> case lists:keyfind(Name, 2, Info) of
		false -> false; GroupInfo -> [GroupInfo|Acc] end
end, [], GroupNames).

update_groups_info(GroupNames) -> Meta = meta(),
	update_file(update_meta(update_groups_info(
		GroupNames, groups_info(Meta)), Meta)).

update_groups_info(GroupNames, []) ->
	lists:zip([n2g(N) || N <- lists:seq(1, length(GroupNames))], GroupNames);
update_groups_info(GroupNames, GroupsInfo) ->
	update_groups_info(GroupNames, GroupsInfo, {[], length(GroupsInfo) + 1}).

update_groups_info([GroupName|T], GroupsInfo, {Acc, NextGroupNo}) ->
	update_groups_info(T, GroupsInfo,
		case lists:keymember(GroupName, 2, GroupsInfo) of
			true -> {Acc, NextGroupNo};
			false -> {[{n2g(NextGroupNo), GroupName}|Acc], NextGroupNo + 1}
		end
	);
update_groups_info([], GroupsInfo, {Acc, _NextGroupNo}) -> GroupsInfo ++ Acc.

meta() -> case file:consult(?MetaFile) of
	{ok, Meta} -> Meta; {error, _Reason} -> [] end.

update_meta(GroupsInfo, Meta) ->
	[{groups_info, GroupsInfo}|lists:keydelete(groups_info, 1, Meta)].

update_file(Meta) -> file:write_file(?MetaFile,
	io_lib:format(lists:flatten(["~p.~n" || _ <- Meta]), Meta)).

n2g(N) -> list_to_atom([$g|integer_to_list(N)]).
