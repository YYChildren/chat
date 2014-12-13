%% @author yangchaojun
%% @doc @todo Add description to accept_client.


-module(accept_client).
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
%% ====================================================================
%% API functions
%% ====================================================================
-export( [accept/3] ).



%% ====================================================================
%% Internal functions
%% ====================================================================
%%新建连接,只有新连接时才调用
%%创建监听
accept(AcceptProName,Port,TcpOptions)->
	{Tag, LSocket}=gen_tcp:listen(Port, TcpOptions),
	case Tag of
        ok ->
            %%统一接收Z
            register(AcceptProName , spawn_link(fun() -> do_accept( LSocket ) end));
        error ->
            %%出错，退出连接
            exit({stop, exit})                               
    end.
do_accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} -> 
            %%创建进程处理响应
			io:format("Socket ~p connected~n",[ Socket]),
			ClientName = common_name:get_name(  client_socket,Socket ),
			chat_server3:connect(Socket),
			io:format("~p Client: ~p~n", [?MODULE,ClientName]);
			%% client_server:start( ClientName,Socket );
			%% register(ClientName,spawn_link(fun() -> receive_client(Socket)end));
        _ ->
            ok
    end,
	do_accept(LSocket). 
