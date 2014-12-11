%% @author yangchaojun
%% @doc @todo Add description to ets_handler.


-module(ets_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([table/2,foreach_key/2,foreach_line/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================
table( Name,Options ) ->
	Tab = ets:new(Name,Options),
	do_table( Tab ).
do_table( Tab) ->
	receive
		{request_tab,Pid} ->
			Pid ! { reply_tab,Tab };
		{ add,Record } ->
			ets:insert(Tab, Record );
		{add_all,List} ->
			lists:foreach(  fun( Record ) -> ets:insert(Tab, Record ) end,  List);
		{del,Key }  ->
			ets:delete(Tab,Key);
		{del_alll, List} ->
			lists:foreach(fun(Key) -> ets:delete(Tab,Key) end, List);
		_ ->
			ignore
	end,
	do_table(Tab).


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