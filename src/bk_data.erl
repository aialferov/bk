%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 26 Nov 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_data).

-export([exists/2]).
-export([create/4]).
-export([read/2, write/5]).

-export([merge/2]).

-include("bk_config.hrl").

-define(DataDepth, 2).

% DataTmpl = {Header, DayTmpl, GroupTmpl, ItemTmpl}

exists(Year, Month) -> filelib:is_file(bk_utils:file(Year, Month)).

create(Year, Month, GroupsInfo, DataTmpl) -> write(Year, Month,
	template(Year, Month, GroupsInfo), GroupsInfo, DataTmpl).

read(Year, Month) -> read_file(bk_utils:file(Year, Month)).

write(Year, Month, Data, GroupsInfo, DataTmpl) ->
	filelib:ensure_dir(bk_utils:dir(Year) ++ "/"),
	file:write_file(bk_utils:file(Year, Month),
		format(GroupsInfo, Data, DataTmpl)).

merge(Data, Sample) -> merge(Data, Sample, ?DataDepth).

format(GroupsInfo, Data, {Header, DayTmpl, GroupTmpl, ItemTmpl}) ->
	io_lib:format(
		lists:flatten([Header|[DayTmpl || _ <- Data]]),
		bk_utils:flatten([{Day, io_lib:format(
			lists:flatten([GroupTmpl || _ <- Groups]),
			bk_utils:flatten(bk_utils:merge([{
				Group, proplists:get_value(Group, GroupsInfo),
				io_lib:format(
					lists:flatten([ItemTmpl || _ <- Items]),
					bk_utils:flatten(bk_utils:merge(Items, commas(Items)))
				)
			} || {Group, Items} <- Groups], commas(Groups)))
		)} || {Day, Groups} <- Data])
	).

template(Year, Month, GroupsInfo) ->
	[{Day, [{Group, []} || {Group, _} <- GroupsInfo]} ||
		Day <- lists:seq(1, bk_utils:last_month_day(Year, Month))].

merge(Data, Sample, 0) -> Data ++ Sample;
merge(Data, Sample, Depth) -> lists:foldl(fun
	({SampleKey, SampleList}, OldData) ->
		{DataKey, DataList} = lists:keyfind(SampleKey, 1, OldData),
		lists:keyreplace(DataKey, 1, OldData,
			{DataKey, merge(DataList, SampleList, Depth - 1)})
end, Data, Sample).

commas([]) -> "";
commas(L) -> lists:duplicate(length(L) - 1, ", ") ++ [" "].

read_file(File) -> case file:consult(File) of
	{ok, Data} -> Data; {error, _Error} -> [] end.
