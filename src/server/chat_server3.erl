-module(chat_server3).

-behaviour(gen_server).

-export([start/1,stop/1]).

%% gen_server回调

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,

         terminate/2,code_change/3]).

-compile(export_all).

-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表

%% -record(state, {name,loop,socket}).
-record( state,{table,msg_server,lsocket} ).
-record(player, {name,zone="world",time=none}).

%%回调gen_server创建服务
start(Name) ->                                 
    gen_server:start_link({local,Name},?MODULE,[],[]). 

process_msg( ServerRef, Socket,Player,Data ) ->
	gen_server:call(ServerRef, {msg,  Socket,Player,Data  }).

do_connect(ServerRef, Socket ) ->
	gen_server:call(ServerRef, {connect,Socket}).



%%停止服务
stop(Name)  -> 
    gen_server:call(Name,stop),
	%%等待所有连接断开
    timer:sleep(2000),
    manage_clients ! {exit}.


init( [] ) -> 
    
    %%启动数据库
    chat_db:start(),
    io:format("~p Database is started!~n",[?MODULE]),
	
	%%MSG server
	chat_send_server:start( ?MODULE, chat_channel_manage:load_channel() ),
    
	%%维护队列
	ets:new(user_in_mem,[set,public,named_table]),
    
	%%默认8080
    {Tag, LSocket}=gen_tcp:listen(8080, ?TCP_OPTIONS),
	State = #state{ table=user_in_mem, lsocket=LSocket },
    %%创建监听
    case Tag of
        ok ->
            %%统一接收
            spawn(fun() -> do_accept( State ) end);
        error ->
            %%出错，退出连接
            exit({stop, exit})                               
    end,
    {ok,State}.

handle_call({msg,  Socket,Player,Data  },_From,State) ->
	chat_client(State,Socket,Player,Data);
handle_call({connnect,Socket} , _From,State) ->
	Tab = State#state.table,
	ets:insert( Tab, {Socket,null} ),
	{ok,null,State};
handle_call(stop,_From,State) ->
    {stop,normal,stopped,State}.

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
chat_client(State,Socket,Player,Data) ->
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
				norecords
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
					chat_send_server:add_record(?MODULE, Zone, Socket),
					register;
		        ok       ->
					Player = #player{name=UserName,time=time_handler:timestamp() },
		            ets:insert(TableID, { Socket,  Player }),
					[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
					%% 给MSG Server发送新增socket
					chat_send_server:add_record(?MODULE, Zone, Socket),
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
			chat_send_server:switch_channel( ?MODULE, Socket, Player1#player.zone, Zone),
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
            chat_send_server:send( ?MODULE,Socket, Player, Data );
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.

%%新建连接,只有新连接时才调用
do_accept(State) ->
    case gen_tcp:accept(State#state.lsocket) of
        {ok, Socket} -> 
            %%创建进程处理响应
			ClientName = common_name:get_name(  client_socket,Socket ),
			client_server:start( ClientName,Socket );
        _ ->
            ok
    end,
	do_accept(State). 