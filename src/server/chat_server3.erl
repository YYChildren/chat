-module(chat_server3).
-behaviour(gen_server).
%% gen_server回调
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export( [start/0,stop/0,send/2,connect/1,disconnect/1]).
-export( [start/1,stop/1,send/3,connect/2,disconnect/2]).

-define(MSG_SERVER,msg_server).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表

%% -record(state, {name,loop,socket}).
-record( state,{table,lsocket} ).
-record(player, {name,zone="world",time=none}).


start() -> 
    start(?MODULE).
stop()->
    stop(?MODULE).
send( Socket,Data ) ->
	send(?MODULE, Socket,Data ).
connect( Socket ) ->
    connect(?MODULE, Socket ).
disconnect(Socket) ->
	disconnect(?MODULE,Socket).


%%回调gen_server创建服务
start(Name) ->                                 
    gen_server:start_link({local,Name},?MODULE,[],[]). 

send( ServerRef, Socket,Data ) ->
	gen_server:call(ServerRef, {msg,  Socket,Data  }).

connect(ServerRef, Socket ) ->
	gen_server:call(ServerRef, {connect,Socket}).

disconnect(ServerRef,Socket) ->
	gen_server:call(ServerRef, {disconnect,Socket}).


%%停止服务
stop(Name)  -> 
    gen_server:call(Name,stop),
	%%等待所有连接断开
    timer:sleep(2000),
    manage_clients ! {exit}.


init( [] ) -> 
	%%设成系统进程，不让子进程关闭
    erlang:process_flag(trap_exit, true),
    %%启动数据库
    chat_db:start(),
    io:format("~p Database is started!~n",[?MODULE]),
	
	%%MSG server
	chat_send_server:start( ?MSG_SERVER, chat_channel_manage:load_channel() ),
    
	%%维护队列
	ets:new(user_in_mem,[set,public,named_table]),
    
	%%默认8080
    {Tag, LSocket}=gen_tcp:listen(8080, ?TCP_OPTIONS),
	State = #state{ table=user_in_mem,lsocket=LSocket },
    %%创建监听
    case Tag of
        ok ->
            %%统一接收Z
			AcceptProName = chat_server3_accept,
            register(AcceptProName , spawn_link(fun() -> do_accept( State ) end));
        error ->
            %%出错，退出连接
            exit({stop, exit})                               
    end,
    {ok,State}.

%%新建连接,只有新连接时才调用
do_accept(State) ->
    case gen_tcp:accept(State#state.lsocket) of
        {ok, Socket} -> 
            %%创建进程处理响应
			io:format("Socket ~p connected~n",[ Socket]),
			ClientName = common_name:get_name(  client_socket,Socket ),
			chat_server3:connect(Socket),
			io:format("~p Client: ~p~n", [?MODULE,ClientName]),
			%% client_server:start( ClientName,Socket );
			register(ClientName,spawn_link(fun() -> receive_client(Socket)end));
        _ ->
            ok
    end,
	do_accept(State). 
receive_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
			%% player进程 这里不要并发，一个palyer 登录  -》 发言本来就是串行的
			chat_server3:send(Socket,Data),
			receive_client(Socket);
        {error, Why} ->
			io:format("~p~n~n",[Why]),
			chat_server3:disconnect(Socket)
    end.


handle_call({msg,  Socket,Data  },_From,State) ->
	io:format("call~n"),
	Reply = chat_client(State,Socket,Data),
	{reply, Reply, State};
handle_call({connect,Socket} , _From,State) ->
	Tab = State#state.table,
	Reply = ets:insert( Tab, {Socket,null} ),
	{reply, Reply, State};
handle_call({disconnect,Socket},_From,State) ->
	Tab = State#state.table,
	ets:delete(Tab, Socket),
	io:format("~p ~p ~n",[?MODULE,?LINE]),
	Reply = chat_send_server:remove_record(?MSG_SERVER, Socket),
	{reply, Reply, State};
handle_call(stop,_From,State) ->
    {stop,normal,stopped,State};
handle_call(Request,_From,State) ->
	io:format("Request: ~p~n", [ Request ]),
	{reply,argument_error,State}.

handle_cast(stop,State) ->
    {stop, normal, State};

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_) ->
    {ok,State}.

%%------------------------------------
%% -------------私有函数--------------
%% -----------------------------------
chat_client(State,Socket,Data) ->
		io:format("chat_client~n"),
		TableID = State#state.table,
        case ets:lookup(TableID,Socket) of
			[{Socket,null}]	->
				Login = string:substr(Data, 1,8),
				io:format("~p login tag: ~p~n", [ ?MODULE,Login] ),
				case Login of
					%% 登录标记段
					?LOGIN_TAG	->
		                [UserName,PassWord] = string:tokens( string:substr(Data, 9)," "),
						io:format("~p User: ~p ~p~n",[?MODULE,UserName,PassWord]),
						
						%% 登录或连接
						case res_or_login( TableID,Socket,UserName,PassWord ) of 
							register ->
								gen_tcp:send( Socket,["Welcome new user! Please remember your username and password"] );
							ok ->
								gen_tcp:send( Socket,[ string:concat( UserName," logined")] );
							exist ->
								gen_tcp:send( Socket,[ lists:concat( [UserName ," is exist and the password is wrong"] )] );
							_ ->
								error
						end;
				
		    		_	->
						gen_tcp:send(Socket,"Please login")
				end;
			[{Socket,Player1}]	->
				case string:substr(Data,1,8) of
			            %%切换频道标记段
			            ?SWITCH_TAG ->
			                Zone = string:substr(Data,9),
			                case chat_channel_manage:load_channel( Zone ) of
			                    [ { channel,Zone,_Public ,_Timeout } ]  ->
									%% 切换频道
									switch_channel(TableID,Socket,Zone),
									gen_tcp:send( Socket, [ lists:concat(  [ "switch to ", Zone ] ) ] );
								_ ->
									send_data( TableID, Socket,Player1,Data )
 			                end;
						_->
							send_data( TableID, Socket,Player1,Data )
 				end;
			_	->
				io:format("Format")
        end.
			
%% 登录或注册
res_or_login( TableID,Socket,UserName,PassWord ) ->
	case ets:lookup(TableID, Socket) of
		[ { Socket, null  } ] ->
		    case chat_user_manage:res_or_login(UserName,PassWord) of
		        register ->
					Player = #player{name=UserName,time=time_handler:timestamp() },
		            ets:insert(TableID, { Socket,  Player}),
					[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
					%% 给MSG Server发送新增socket
					chat_send_server:add_record(?MSG_SERVER, Zone, Socket),
					register;
		        ok       ->
					Player = #player{name=UserName,time=time_handler:timestamp() },
		            ets:insert(TableID, { Socket,  Player }),
					[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
					%% 给MSG Server发送新增socket
					chat_send_server:add_record(?MSG_SERVER, Zone, Socket),
		            ok;
		        exist    ->
					exist
		    end;
		_ ->
			error
	end.
	
%% 切换频道
switch_channel(TableID,Socket,Zone) ->
	case ets:lookup(TableID, Socket) of
		%% 确保是该进程的动作
		[ { Socket,Player1 } ] ->
			chat_send_server:switch_channel( ?MSG_SERVER, Socket, Player1#player.zone, Zone),
			%% 更改频道和时间戳
			case Zone =:= Player1#player.zone of
				true ->
					ok;
				false	->
					Player = Player1#player{zone=Zone, time=time_handler:timestamp() },
					ets:insert(TableID,  { Socket, Player } ),
					switched
			end;
		_->
			error
	end.

send_data( TableID, Socket,Player,Data ) ->
    [{channel,_Zone,_Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
    case  ( Lest = time_handler:timestamp() - Player#player.time ) >= Time of
        true ->
            %% 插入最后发言时间
            Player1 = Player#player{ time=time_handler:timestamp() } ,
            ets:insert( TableID,{Socket,Player1} ),
			io:format("insert time ~n~n"),
			%%
			%% 把消息发给  群发进程
            chat_send_server:send( ?MSG_SERVER,Socket, Player, Data );
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.