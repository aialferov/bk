%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 15 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_calc).
-export([sum/1]).

sum(Data) -> lists:sum([bk_utils:eval(Price) || Price <- prices(Data)]).

prices(Data) -> lists:append(lists:append(
	[[[Price || {_Name, Price} <- Items] ||
		{_Group, Items} <- Groups] || {_Day, Groups} <- Data]
)).
