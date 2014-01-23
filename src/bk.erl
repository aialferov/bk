%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 12 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk).

-export([start/0, stop/0]).

-export([sum/0, sum/1, sum/2, sum/3, sum/4]).
-export([sample_create/0, sample_remove/0, sample_sum/0]).
-export([merge/0, merge/2]).

-export([groups_create/0, groups_info/0]).
-export([groups_file/0, sample_file/0]).

-export([months/0]).
-export([message/1]).

-export([import/3]).

-include("bk_config.hrl").

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

sum() -> {Year, Month} = year_month(), sum(Year, Month).
sum(Year) -> lists:sum([sum(Year, Month) || Month <- ?Months]).
sum(Year, Month) -> bk_calc:sum(bk_data:read(Year, Month)).
sum(Year, Month, DayOrGroup) ->
	bk_calc:sum(bk_slice:read(bk_data:read(Year, Month), DayOrGroup)).
sum(Year, Month, Day, Group) ->
	bk_calc:sum(bk_slice:read(bk_data:read(Year, Month), Day, Group)).

sample_create() -> sample_create(day()).
sample_create(Day) -> sample_create(Day, bk_groups:read(),
	bk_config:read([sample_header, sample_day_tmpl, sample_group_tmpl])).

sample_create(_Day, [], _SampleTmpl) -> {error, groups_not_found};
sample_create(Day, GroupNames, SampleTmpl) -> case bk_sample:exists() of
	true -> {error, already_exists};
	false -> bk_sample:create(Day, groups_info(GroupNames), SampleTmpl)
end.

sample_remove() -> bk_sample:remove().
sample_sum() -> if_sample(fun() -> bk_calc:sum(bk_sample:read()) end).

merge() -> {Year, Month} = year_month(), merge(Year, Month).
merge(Year, Month) -> merge(Year, Month, bk_groups:read(),
	bk_config:read([header, day_tmpl, group_tmpl, item_tmpl])).

merge(_Year, _Month, [], _DataTmpl) -> {error, groups_not_found}; 
merge(Year, Month, GroupNames, DataTmpl) -> if_sample(fun() -> data_merge(
	Year, Month, groups_info(GroupNames), bk_sample:read(), DataTmpl) end).

groups_create() -> bk_groups:create(bk_config:read([groups_header])).

groups_info() -> groups_info(bk_groups:read()).
groups_info(Names) -> case bk_meta:ordered_groups_info(Names) of
	false -> bk_meta:update_groups_info(Names), groups_info(Names);
	Info -> Info
end.

groups_file() -> bk_groups:file().
sample_file() -> bk_sample:file().

months() -> ?Months.
message(Type) -> ?Message(Type).

import(Type, Dir, Years) ->
	GroupsInfo = groups_info(), 
	DataTmpl = bk_config:read([header, day_tmpl, group_tmpl, item_tmpl]),
	[import(Type, Dir, Year, Month, GroupsInfo, DataTmpl) ||
		Year <- Years, Month <- ?Months].

import(Type, Dir, Year, Month, GroupsInfo, DataTmpl) ->
	data_merge(Year, Month, GroupsInfo,
		import(Type, Dir, Year, Month, GroupsInfo), DataTmpl).

import(tabtxt, Dir, Year, Month, GroupsInfo) ->
	[{Day, [{group(GroupName, GroupsInfo), Items} ||
		{GroupName, Items} <- Groups]} ||
		{Day, Groups} <- bk_import:tabtxt(filename:join([Dir, Year, Month])),
		Day =< bk_utils:last_month_day(Year, Month)
	].

data_merge(Year, Month, GroupsInfo, Data, DataTmpl) ->
	data_ensure(Year, Month, GroupsInfo, DataTmpl),
	bk_data:write(Year, Month, GroupsInfo,
		bk_data:merge(bk_data:read(Year, Month), Data), DataTmpl).

data_ensure(Year, Month, GroupsInfo, DataTmpl) ->
	case bk_data:exists(Year, Month) of
		true -> ok;
		false -> bk_data:create(Year, Month, GroupsInfo, DataTmpl)
	end.

if_sample(Fun) -> case bk_sample:exists() of
	true -> Fun(); false -> {error, sample_not_found} end.

day() -> {{_, _, Day}, {_, _, _}} = calendar:local_time(), Day.
year_month() ->
	{{Year, Month, _}, {_, _, _}} = calendar:local_time(), {Year, Month}.

group(GroupName, GroupsInfo) ->
	{Group, GroupName} = lists:keyfind(GroupName, 2, GroupsInfo), Group.
