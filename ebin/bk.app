%%%-------------------------------------------------------------------
%%% Created: 23 Jan 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, bk, [
	{id, "bk"},
	{vsn, "0.0.1"},
	{description, "Simple finance manager"},
	{modules, [
		bk,
		bk_calc,
		bk_data,
		bk_meta,
		bk_test,
		bk_utils
		bk_slice,
		bk_config,
		bk_groups,
		bk_import,
		bk_sample
	]},
	{applications, [kernel, stdlib]}
]}.
