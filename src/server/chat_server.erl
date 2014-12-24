-module(chat_server).
-behaviour(gen_server).
%% gen_server回调
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export( [start/1,stop/1,send/3,connect/2,disconnect/2,get_player/2,set_player/2]).

-define(CHAT_SEND_SERVER,chat_send_server).
-define(CHAT_MSG_CAST_SERVER,chat_msg_cast_server).
-define(STARTED,started).
-define(NORMAL,normal).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表+
%%改用进程字典, 进程字典是  socket  -》 { Pid, player }
-record(state,{}).
-record(player,{playername, current_zone, player_info}).
%% player_info   list{ zone,  time } 


%%回调gen_server创建服务
start(Name) ->                                 
    gen_server:start_link({local,Name},?MODULE,[],[]). 

send( ServerRef, Socket,Data ) ->
	erlang:send(ServerRef, {msg,  Socket,Data}).

connect(ServerRef, Socket ) ->
	erlang:send(ServerRef, {connect,Socket}).

disconnect(ServerRef,Socket) ->
	erlang:send(ServerRef, {disconnect,Socket}).

%% put_cast_stop( ServerRef,  StopInfo) ->
%% 	{Socket,  { Reason, Player }} = StopInfo,
%% 	erlang:send( ServerRef, { put, {Socket,  {Reason, Player }  }} ).
%% 
%% put_normal( ServerRef , {Socket, Player} ) ->
%% 	erlang:send( ServerRef, {put, {Socket, {?STARTED, Player } }} ).

get_player(  ServerRef, Playername) ->
	 gen_server:call( ServerRef, {get_player, Playername} ).
set_player( ServerRef,{Playername, { Socket,{ Reason,Player } } }) ->
	erlang:send(ServerRef,   { set_player, {Playername, { Socket,{ Reason,Player } } }}).
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
	State = #state{},
    {ok,State}.

handle_call({get_player,Playername}, _From,State ) ->
	case get(Playername) of
		undefined ->
			Reply = undefined;
		Socket ->
			Reply =  get(Socket)
	end,
	{reply,Reply,State};
handle_call(stop,_From,State) ->
    {stop,normal,stopped,State};
handle_call(Request,_From,State) ->
	io:format("Request: ~p~n", [ Request ]),
	{reply,argument_error,State}.

handle_cast(stop,State) ->
    {stop, normal, State};
handle_cast(_Msg,State) ->
    {noreply,State}.

chat_msg_cast_pid(Socket) ->
	list_to_atom( string:concat(  "chat_msg_cast", erlang:port_to_list(Socket) ) ).
handle_info({msg,  Socket,Data},State) ->
	Record = get(Socket),
	ChatCastPid = chat_msg_cast_pid(Socket), 
	case erlang:whereis( ChatCastPid ) of
		 undefined ->
			 %% 创建进程
			 chat_msg_cast_server:start_link( ChatCastPid ,Socket),
			 %% 这里可能存在时序问题
			 case Record of 
				%% 未登录
				null ->
					?CHAT_MSG_CAST_SERVER:send(ChatCastPid,   { Data} );
				%% 已登录，cast进程正常
				{?STARTED,_Player} ->
					?CHAT_MSG_CAST_SERVER:send(ChatCastPid,   { Data} );
				 %% 进程被正常关闭,同步进入上个用户的数据库
 				{?NORMAL,_Player} ->
					%% player发言信息持久化，可延时持久化
					{ _Reason, Player } = erlang:erase(  Socket ),
					catch(erase(   Player#player.playername  )),
					chat_player_info_manage:insert( Player ),
					%%插入新的资料
					put(Socket,null),
					?CHAT_MSG_CAST_SERVER:send(ChatCastPid,   { Data} );
				 %% cast进程被异常关闭,
				{_,Player} ->
					?CHAT_MSG_CAST_SERVER:recover_send(ChatCastPid,  { Player, Data});
				 _ ->
					 ignore
			 end;
		_ ->
			?CHAT_MSG_CAST_SERVER:send(ChatCastPid,   { Data} )
	end,
	{noreply, State};
handle_info( { set_player, {Playername, { Socket,{ Reason,Player } } }}  ,State) ->
	Socket1 = get(Playername),
	case get( Socket1) of
		{_Reason1, Player1} ->
			Playername1 =Player1#player.playername,
			%% 删除原有的记录
			erase(Playername1);
		_ ->
			undefined
	end,
	erase(Socket1),
	%% 插入新的记录
	put(Socket,{Reason, Player}),
	put(Playername,Socket),
	{noreply, State};
handle_info({connect,Socket},State) ->
	put(Socket,null),
	{noreply, State};
handle_info({disconnect,Socket},State) ->
	ChatCastPid = chat_msg_cast_pid(Socket),
	%%关闭事务处理进程
	case erlang:whereis( ChatCastPid ) of
		undefined ->
		  ok;
		_ ->
			?CHAT_MSG_CAST_SERVER:stop( ChatCastPid )
	end,
	% 在群发进程删除相应记录，并关闭发送进程
	?CHAT_SEND_SERVER:remove_record(?CHAT_SEND_SERVER, Socket),
	%% 进程字典暂存用户退出信息，
	{noreply, State};
handle_info(_Info,State) ->
	%% io:format("chat ----- server: ~p~n",[_Info]),
    {noreply,State}.

terminate(_Reason,_State) ->
	List = get(),
	%% 持久化
	ListPlayer = get_dict_socket_record( [],  List),
	chat_player_info_manage:insert_all(ListPlayer),
    ok.
get_dict_socket_record(NewList,[]) ->
	NewList;
get_dict_socket_record(NewList,List) ->
	[Record | Others] = List,
	case Record of 
	{ Socket,{ _Reason,Player}} when is_port(Socket) ->
		  get_dict_socket_record( [ Player| NewList ] , Others );
	  _ ->
		  get_dict_socket_record(  NewList  , Others )
   end.

code_change(_OldVsn,State,_) ->
    {ok,State}.

