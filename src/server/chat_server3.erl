-module(chat_server3).
%% -include("info.hrl").
-behaviour(gen_server).
%% gen_server回调
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export( [start/0,stop/0,send/2,connect/1,disconnect/1]).
-export( [start/1,stop/1,send/3,connect/2,disconnect/2]).

-define(CHAT_SEND_SERVER,chat_send_server).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表
-record( state,{table} ).
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
	erlang:send(ServerRef, {msg,  Socket,Data}).

connect(ServerRef, Socket ) ->
	erlang:send(ServerRef, {connect,Socket}).

disconnect(ServerRef,Socket) ->
	File = "D:\\tt\\chat_server3_disconnect..txt",
	{ ok,S }= file:open(File, [  append ]),
	io:format(  S , "~p ~p ~p~n",   [ time(),Socket, ServerRef]),
	file:close( S ),
	erlang:send(ServerRef, {disconnect,Socket}).


%%停止服务
stop(Name)  -> 
    gen_server:call(Name,stop),
	%%等待所有连接断开
    timer:sleep(2000).


init( [] ) -> 
	%%设成系统进程，不让子进程关闭
    erlang:process_flag(trap_exit, true),    
	%% 启动数据库
	chat_db:start(),
	%%维护队列
	erase(),
	ets:new(user_in_mem,[set,public,named_table]),
	State = #state{ table=user_in_mem},
    {ok,State}.


handle_call(stop,_From,State) ->
    {stop,normal,stopped,State};
handle_call(Request,_From,State) ->
	io:format("Request: ~p~n", [ Request ]),
	{reply,argument_error,State}.

handle_cast(stop,State) ->
    {stop, normal, State};
handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info({msg,  Socket,Data},State) ->
	chat_client(State,Socket,Data),
	{noreply, State};
handle_info({connect,Socket},State) ->
	Tab = State#state.table,
	ets:insert( Tab, {Socket,null} ),
	{noreply, State};
handle_info({disconnect,Socket},State) ->
	File = "D:\\tt\\chat_server3.txt",
	{ ok,S }= file:open(File, [  append ]),
	io:format(  S , "~p ~p~n",   [ time(),Socket]),
	file:close( S ),
	Tab = State#state.table,
	ets:delete(Tab, Socket),
	?CHAT_SEND_SERVER:remove_record(?CHAT_SEND_SERVER, Socket),
	{noreply, State};
handle_info(_Info,State) ->
	io:format("chat  server Format: ~p~n",[_Info]),
	
    {noreply,State}.

terminate(_Reason,_State) ->
	File = "D:\\tt\\chat_server3_stop.txt",
	{ ok,S }= file:open(File, [  append ]),
	io:format(  S , "~p ~p~n",   [ time(),stop]),
	file:close( S ),
    ok.

code_change(_OldVsn,State,_) ->
    {ok,State}.

%%------------------------------------
%% -------------私有函数--------------
%% -----------------------------------
chat_client(State,Socket,Data) ->
		TableID = State#state.table,
        case ets:lookup(TableID,Socket) of
			[{Socket,null}]	->
				Login = string:substr(Data, 1,8),
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
									switch_channel(TableID,Socket,Player1,Zone);
								_ ->
									send_data( TableID, Socket,Player1,Data )
 			                end;
						_->
							send_data( TableID, Socket,Player1,Data )
 				end;
			_	->
				ignore
        end.
			
%% 登录或注册
res_or_login( TableID,Socket,UserName,PassWord ) ->
    case chat_user_manage:res_or_login(UserName,PassWord) of
        register ->
			Player = #player{name=UserName,time=time_handler:timestamp() },
            ets:insert(TableID, { Socket,  Player}),
			[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
			%% 给MSG Server发送新增socket
			?CHAT_SEND_SERVER:add_record(?CHAT_SEND_SERVER, Zone, Socket),
			register;
        ok       ->
			Player = #player{name=UserName,time=time_handler:timestamp() },
            ets:insert(TableID, { Socket,  Player }),
			[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
			%% 给MSG Server发送新增socket
			?CHAT_SEND_SERVER:add_record(?CHAT_SEND_SERVER, Zone, Socket),
            ok;
        exist    ->
			exist
	end.
	
%% 切换频道
switch_channel(TableID,Socket,Player1,Zone) ->
	%% 更改频道和时间戳
	case Zone =:= Player1#player.zone of
		true ->
			gen_tcp:send( Socket, [ lists:concat(  [ "You are in ", Zone,". Don't have to switch" ] ) ] ),
			ok;
		false	->
			Player = Player1#player{zone=Zone, time=time_handler:timestamp() },
			ets:insert(TableID,  { Socket, Player } ),
			?CHAT_SEND_SERVER:switch_channel( ?CHAT_SEND_SERVER, Socket, Player1#player.zone, Zone),
			gen_tcp:send( Socket, [ lists:concat(  [ "switch to ", Zone ] ) ] ),
			switched
	end.

send_data( TableID, Socket,Player,Data ) ->
    [{channel,_Zone,_Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
    case  ( Lest = time_handler:timestamp() - Player#player.time ) >= Time of
        true ->
            %% 插入最后发言时间
            Player1 = Player#player{ time=time_handler:timestamp() } ,
            ets:insert( TableID,{Socket,Player1} ),
			io:format("------------------------------------------~n~n"),
			%%
			%% 把消息发给  群发进程
            ?CHAT_SEND_SERVER:send( ?CHAT_SEND_SERVER,Socket, Player, Data );
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.