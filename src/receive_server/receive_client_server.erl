%% @author yangchaojun
%% @doc @todo Add description to receive_client.


-module(receive_client_server).
-define(CHAT_SERVER,chat_server3).
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
			?CHAT_SERVER:send(Socket,Data),
			do_receive_msg(Socket);
        {error, Why} ->
			io:format("~p ~p~n",[Socket,Why]),
			?CHAT_SERVER:disconnect(Socket)
    end.

