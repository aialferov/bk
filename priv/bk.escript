#!/usr/bin/env escript
%%! -smp enable -pa /usr/local/lib/bk/ebin ebin

main(["sum"]) -> message(sum, bk:sum());
main(["sum"|Args]) -> message(sum, bk:sum(bk_args:read(Args)));

main(["sample"]) -> sample(bk:sample_create());
main(["sample", "sum"]) -> message(sum, bk:sample_sum());

main(["merge"]) -> merge(bk:merge());
main(["merge", Year, Month]) -> merge(bk:merge(Year, Month));

main(["groups"]) -> message(groups, bk:groups_info());
main(["months"]) -> message(months, bk:months());

main(["sample", "file"]) -> message(file, bk:sample_file());
main(["groups", "file"]) -> message(file, bk:groups_file());

main(["import", Dir]) ->
	{ok, Files} = file:list_dir(Dir),
	bk:import(tabtxt, Dir, [Year || Year <- Files,
		filelib:is_dir(filename:join(Dir, Year))]);

main(_) -> message(usage).

sample(ok) -> ok;
sample({error, already_exists}) -> ok;
sample({error, groups_not_found}) -> bk:groups_create(), halt(1).

merge(ok) -> bk:sample_remove();
merge({error, sample_not_found}) -> message(sample_not_found);
merge({error, groups_not_found}) -> bk:groups_create(), halt(1).

message(sum, {error, Error}) -> message(Error);
message(sum, Sum) -> io:format("~s~n",
	[float_to_list(float(Sum), [{decimals, 2}])]);
message(file, File) -> io:format("~s~n", [File]);
message(groups, Groups) -> io:format(
	lists:flatten(["~p ~ts~n" || _ <- Groups]), bk_utils:flatten(Groups));
message(months, Months) -> io:format(
	lists:flatten(["~s " || _ <- Months] ++ "~n"), Months).
message(Type) -> io:format(bk:message(Type)).
