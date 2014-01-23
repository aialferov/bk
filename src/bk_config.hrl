%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 12 Dec 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-define(MetaFile, ".bk.meta").
-define(ConfigFile, "bk.conf").
-define(ConfigPaths, [".", "/etc"]).
-define(GroupsFile, "bk.groups").
-define(SampleFile, "bk.sample").

-define(DataPath, ".").

-define(Months, [
	"jan", "feb", "mar", "apr", "may", "jun",
	"jul", "aug", "sep", "oct", "nov", "dec"
]).

-define(Message(Type), case Type of
	usage ->
		"usage: bk <command>~n~n"
		"Commands:~n"
		"    sample        Create (if does not exists) and open sample file~n"
		"    sample sum    Show sum of the sample file~n"
		"    merge         Merge sample file with the whole data~n"
		"    sum           Show sum of the current year and month~n"
		"    groups        Show relations between groups and their names~n"
		"    months        Show supported month names~n"
	;
	sample_not_found -> "sample not found~n"
end).
