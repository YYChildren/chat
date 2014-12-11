-module(chat_interface).
-export([start/0,stop/0,send/3,tell_connect/1]).
-record(player, {name,zone="world",time=none}).
start() -> 
    chat_server3:start(?MODULE).

stop()->
    chat_server3:stop(?MODULE).

send( Socket,Player,Data ) ->
	chat_server3:process_msg(?MODULE, Socket,Player,Data ).
tell_connect( Socket ) ->
    chat_server3:do_connect(?MODULE, Socket ).


%% %%可封装成独立模块
%% client_count() ->
%% 	manage_clients  !  { request_ets, self()  },
%% 	receive
%% 		{ reply_ets,TableID }  ->
%% 			do_client_count( TableID );
%% 		_ ->
%% 			timer:sleep(2000),
%% 			client_count()
%% 	end.
%% do_client_count( TableID ) ->
%% 	timer:sleep(2000),
%% 	io:format("process indo:~p~n",  [ erlang:process_info(  erlang:whereis( get_name( zone_pid,"world") ))]),
%% 	io:format("info of table: ~p~n ",  [ ets:info(TableID) ] ),
%% 	io:format("World~p~n", [ ets:info(  get_name(table,"world"))  ]),
%% 	%io:format("country1~p~n", [ ets:info(  get_name(table,"country1"))  ]),
%% 	%io:format("country2~p~n", [ ets:info(  get_name(table,"country2"))  ]),
%% 	%io:format("country3~p~n", [ ets:info(  get_name(table,"country3"))  ]),
%% 	do_client_count( TableID ).