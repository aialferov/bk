%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 14 Jan 2014 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(bk_groups).

-export([exists/0]).
-export([create/1]).
-export([read/0]).

-export([file/0]).

-include("bk_config.hrl").

exists() -> filelib:is_file(?GroupsFile).
create(Header) -> file:write_file(?GroupsFile, io_lib:format(Header, [])).
read() -> bk_utils:read_file(?GroupsFile).

file() -> ?GroupsFile.
