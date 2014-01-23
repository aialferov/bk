%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 12 Dec 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_config).
-export([read/1]).

-include("bk_config.hrl").

read(Keys) ->
	{ok, Config} = read(),
	case list_to_tuple([proplists:get_value(Key, Config) || Key <- Keys]) of
		{Value} -> Value; Other -> Other end.

read() -> lists:foldl(fun
	(_Path, {ok, Config}) -> {ok, Config};
	(Path, {error, _Error}) -> file:consult(filename:join(Path, ?ConfigFile))
end, {error, error}, ?ConfigPaths).
