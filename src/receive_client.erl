%% @author yangchaojun
%% @doc @todo Add description to receive_client.


-module(receive_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([receive_msg/1]).
%% ====================================================================
%% Internal functions
%% ====================================================================
receive_msg(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
			%% player进程 这里不要并发，一个palyer 登录  -》 发言本来就是串行的
			chat_server3:send(Socket,Data),
			receive_msg(Socket);
        {error, Why} ->
			io:format("~p~n~n",[Why]),
			chat_server3:disconnect(Socket)
    end.