%% @author yangchaojun
%% @doc @todo Add description to ets_handler.


-module(ets_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([foreach_key/2,foreach_line/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================


foreach_key(Tab,Fun) ->
	ets:safe_fixtable(Tab, true),
	do_foreach_key(Tab, ets:first(Tab) , Fun),
	ets:safe_fixtable(Tab, false).
do_foreach_key(_Tab, '$end_of_table' , _ ) ->
	ok;
do_foreach_key(Tab,  Idx ,Fun ) ->
	Fun( Idx ),
	do_foreach_key(Tab, ets:next(Tab, Idx),  Fun ).


foreach_line(Tab,Fun) ->
	ets:safe_fixtable(Tab, true),
	do_foreach_line(Tab, ets:first(Tab) , Fun),
	ets:safe_fixtable(Tab, false).
do_foreach_line(_Tab, '$end_of_table' , _ ) ->	
	ok;
do_foreach_line(Tab,  Idx ,Fun ) ->
	[ X ] = ets:lookup(Tab, Idx),
	Fun(  X   ),
	do_foreach_line(Tab, ets:next(Tab, Idx),  Fun ).