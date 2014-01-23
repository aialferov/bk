-module(bk_test).

-include_lib("eunit/include/eunit.hrl").

-define(Config, "tests/bk.test").

merge_test() ->
	{Data, Sample, Result} = config([merge_data, merge_sample, merge_result]),
	?assert(bk_data:merge(Data, Sample) =:= Result).

config(Keys) ->
	{ok, Config} = file:consult(?Config),
	case list_to_tuple([proplists:get_value(Key, Config) || Key <- Keys]) of
		{Value} -> Value; Other -> Other end.
