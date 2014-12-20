-module(chat_server).
-behaviour(gen_server).
%% gen_server回调
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export( [start/1,stop/1,send/3,connect/2,disconnect/2,do_cast_stop/2]).

-define(CHAT_SEND_SERVER,chat_send_server).
-define(CHAT_MSG_CAST_SERVER,chat_msg_cast_server).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表+
%%改用进程字典, 进程字典是  socket  -》 player
-record(state,{}).
-record(player,{playername, current_zone, player_info}).
%% user_tab     ets{ socket, cast_pid, playername }
%% players       ets{ playername, current ,player_info }
%%player_info   dict{ zone,  time } 


%%回调gen_server创建服务
start(Name) ->                                 
    gen_server:start_link({local,Name},?MODULE,[],[]). 

send( ServerRef, Socket,Data ) ->
	erlang:send(ServerRef, {msg,  Socket,Data}).

connect(ServerRef, Socket ) ->
	erlang:send(ServerRef, {connect,Socket}).

disconnect(ServerRef,Socket) ->
	erlang:send(ServerRef, {disconnect,Socket}).

do_cast_stop( ServerRef,  StopInfo) ->
	{Reason, {Socket,Player}} = StopInfo,
	erlang:send( ServerRef, {put, {Socket,Player}} ).

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
	ets:new(socket_tab_in_mem,[set,protected,named_table]),
	ets:new(player_tab_in_mem, [set,protected,named_table]),
	State = #state{  socket_tab =  socket_tab_in_mem ,player_tab = player_tab_in_mem  },
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

set_player( Player ) ->
	ok.
chat_msg_cast_pid(Socket) ->
	string:concat(  "chat_msg_cast", erlang:port_to_list(Socket) ).
handle_info({msg,  Socket,Data},State) ->
	SocketTab = State#state.socket_tab,
	{ Socket, ChatCastPid, Playername } = ets:lookup(SocketTab, Socket),
	case erlang:whereis( ChatCastPid ) =:=undefined of
		 false ->
			 %% 创建进程
			 NewChatCastPid = chat_msg_cast_pid(Socket),
			chat_msg_cast_server:start_link( NewChatCastPid ),
			 %% 这里肯呢过存在时序问题
			 case Playername of
				 %% 说明是还未登录
				 null ->
					ets:insert( SocketTab, {Socket,  NewChatCastPid ,null}),
					?CHAT_MSG_CAST_SERVER:send(NewChatCastPid,   {Socket,Data} );
				 %% 说明是进程崩溃
				 _ ->
					 PlayerTab = State#state.player_tab,
					 Player = ets:lookup(PlayerTab, Playername),
					ets:insert( SocketTab, {Socket,  NewChatCastPid ,Playername}),
					 %% 设置Player
					?CHAT_MSG_CAST_SERVER:recover_send(NewChatCastPid,   {Socket,Player,Data} )
			 end;
		_ ->
			?CHAT_MSG_CAST_SERVER:send(ChatCastPid,   { Socket, Data} )
	end,
	{noreply, State};
handle_info({connect,Socket},State) ->
	Tab = State#state.socket_tab,
	ets:insert( Tab, {Socket,null,null} ),
	{noreply, State};
handle_info({disconnect,Socket},State) ->
 	Tab = State#state.socket_tab,
 	ets:delete(Tab, Socket),
	% ?CHAT_SEND_SERVER:remove_record(?CHAT_SEND_SERVER, Socket),
	{noreply, State};
handle_info(_Info,State) ->
	io:format("chat  server Format: ~p~n",[_Info]),
	
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_) ->
    {ok,State}.

