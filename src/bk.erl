%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 12 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk).

-export([start/0, stop/0]).

-export([sum/0, sum/1]).
-export([sample_create/0, sample_remove/0, sample_sum/0]).
-export([merge/0, merge/1, merge/2, merge/3]).

-export([groups_create/0, groups_info/0]).
-export([groups_file/0, sample_file/0]).

-export([years/0, months/0]).
-export([message/1]).

-export([import/3]).

-include("bk_config.hrl").

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

sum() -> sum(lists:zip([year, month], tuple_to_list(year_month()))).
sum(Args) -> lists:sum([
	bk_calc:sum(bk_slice:read(bk_data:read(Year, Month), Args)) ||
	Year <- proplists:get_all_values(year, Args),
	Month <- proplists:get_all_values(month, Args)
]).

sample_create() -> sample_create(day()).
sample_create(Day) -> sample_create(Day, bk_groups:read(),
	bk_config:read([sample_header, sample_day_tmpl, sample_group_tmpl])).

sample_create(_Day, [], _SampleTmpl) -> {error, groups_not_found};
sample_create(Day, GroupNames, SampleTmpl) ->
	SampleCreate = fun() -> bk_sample:create(
		Day, groups_info(GroupNames), SampleTmpl) end,
	if_(bk_sample:exists(), {error, already_exists}, SampleCreate).

sample_remove() -> bk_sample:remove().
sample_sum() -> if_sample(fun() -> bk_calc:sum(bk_sample:read()) end).

merge() -> merge([]).
merge(Year, Month) -> merge([], Year, Month).

merge(Tag) -> {Year, Month} = year_month(), merge(Tag, Year, Month).
merge(Tag, Year, Month) -> merge(Tag, Year, Month, bk_groups:read(),
	bk_config:read([header, day_tmpl, group_tmpl, item_tmpl])).

merge(_Tag, _Year, _Month, [], _DataTmpl) -> {error, groups_not_found}; 
merge(Tag, Year, Month, GroupNames, DataTmpl) -> if_sample(fun() -> data_merge(
	Year, Month, data_tag(bk_sample:read(), Tag),
	groups_info(GroupNames), DataTmpl
) end).

groups_create() -> bk_groups:create(bk_config:read([groups_header])).

groups_info() -> groups_info(bk_groups:read()).
groups_info(Names) -> case bk_meta:ordered_groups_info(Names) of
	false -> bk_meta:update_groups_info(Names), groups_info(Names);
	Info -> Info
end.

groups_file() -> bk_groups:file().
sample_file() -> bk_sample:file().

years() -> bk_utils:years().
months() -> bk_utils:months().

message(Type) -> ?Message(Type).

import(Type, Path, Years) when is_atom(Type) ->
	Import = [{Year, Month, import(Type, Path, Year, Month)} ||
		Year <- Years, Month <- bk_utils:months()],
	{ImportData, ImportGroupNames} = lists:foldr(fun
		({Year, Month, {GroupNames, Data}}, {AccYmd, AccGn}) ->
			{[{Year, Month, Data}|AccYmd], GroupNames ++ AccGn}
	end, {[], []}, Import),
	GroupNames = sets:to_list(sets:from_list(ImportGroupNames)),
	GroupsInfo = groups_info(GroupNames),
	DataTmpl = bk_config:read([header, day_tmpl, group_tmpl, item_tmpl]),
	bk_groups:write(GroupNames),
	[import(Year, Month, Data, GroupsInfo, DataTmpl) ||
		{Year, Month, Data} <- ImportData].

import(Year, Month, Data, GroupsInfo, DataTmpl) ->
	data_merge(Year, Month, [{Day, [{group(GroupName, GroupsInfo), Items} ||
		{GroupName, Items} <- Groups]} ||
		{Day, Groups} <- Data, Day =< bk_utils:last_month_day(Year, Month)
	], GroupsInfo, DataTmpl).

import(Type, Path, Year, Month) ->
	bk_import:Type(filename:join([Path, Year, Month])).

data_merge(_Year, _Month, [], _GroupsInfo, _DataTmpl) -> ok;
data_merge(Year, Month, Data, GroupsInfo, DataTmpl) ->
	data_ensure(Year, Month, GroupsInfo, DataTmpl),
	bk_data:write(Year, Month, bk_data:merge(bk_data:read(Year, Month), Data),
		GroupsInfo, DataTmpl).

data_ensure(Year, Month, GroupsInfo, DataTmpl) ->
	DataCreate = fun() -> bk_data:create(Year, Month, GroupsInfo, DataTmpl) end,
	if_(bk_data:exists(Year, Month), ok, DataCreate).

data_tag(Data, Tag) -> bk_data:tag(Data, Tag, bk_config:read([tag_format])).

if_sample(Fun) -> if_(bk_sample:exists(), Fun, {error, sample_not_found}).

day() -> {{_, _, Day}, {_, _, _}} = calendar:local_time(), Day.
year_month() ->
	{{Year, Month, _}, {_, _, _}} = calendar:local_time(), {Year, Month}.

group(GroupName, GroupsInfo) ->
	{Group, GroupName} = lists:keyfind(GroupName, 2, GroupsInfo), Group.

if_(true, Then, _Else) -> if is_function(Then) -> Then(); true -> Then end;
if_(false, _Then, Else) -> if is_function(Else) -> Else(); true -> Else end.
