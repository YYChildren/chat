%% @author yangchaojun
%% @doc @todo Add description to receive_client.


-module(receive_client_server).
-define(CHAT_SERVER,chat_server).
%% ====================================================================
%% API functions
%% ====================================================================
-export([receive_msg/2]).
%% ====================================================================
%% Internal functions
%% ====================================================================
receive_msg( Socket,ClientName ) ->
	register(ClientName, spawn_link( fun() -> do_receive_msg(Socket) end) ).
do_receive_msg(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
			%% player进程 这里不要并发，一个palyer 登录  -》 发言本来就是串行的
			?CHAT_SERVER:send(?CHAT_SERVER,Socket,Data),
			do_receive_msg(Socket);
        {error, Why} ->
			io:format("~p ~p~n",[Socket,Why]),
			%% gen_tcp:close(Socket),
			catch erlang:port_close(Socket),
    		receive 
				{'EXIT',Socket,_} -> ok 
			after 0 -> ok 
			end,
			?CHAT_SERVER:disconnect(?CHAT_SERVER,Socket)
    end.

do_send(Socket) ->
	gen_tcp:send(Socket, [ hello ]),
	do_send(Socket).

