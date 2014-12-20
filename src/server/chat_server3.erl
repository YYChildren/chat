-module(chat_server3).
%% %% -include("info.hrl").
%% -behaviour(gen_server).
%% %% gen_server回调
%% -export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
%% 
%% -export( [start/0,stop/0,send/2,connect/1,disconnect/1]).
%% -export( [start/1,stop/1,send/3,connect/2,disconnect/2]).
%% 
%% -define(CHAT_SEND_SERVER,chat_send_server).
%% -define(LOGIN_TAG,"bG9naW4=").
%% -define(SWITCH_TAG,"c3dpdGNo").
%% -define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
%% 
%% %%状态表
%% -record( socket_palyer, { socket,   })
%% -record( state,{ player, chat_casts } ).
%% -record(player, {name,zone="world",time=none}).
%% 
%% 
%% start() -> 
%%     start(?MODULE).
%% stop()->
%%     stop(?MODULE).
%% send( Socket,Data ) ->
%% 	send(?MODULE, Socket,Data ).
%% connect( Socket ) ->
%%     connect(?MODULE, Socket ).
%% disconnect(Socket) ->
%% 	disconnect(?MODULE,Socket).
%% 
%% 
%% %%回调gen_server创建服务
%% start(Name) ->                                 
%%     gen_server:start_link({local,Name},?MODULE,[],[]). 
%% 
%% send( ServerRef, Socket,Data ) ->
%% 	erlang:send(ServerRef, {msg,  Socket,Data}).
%% 
%% connect(ServerRef, Socket ) ->
%% 	erlang:send(ServerRef, {connect,Socket}).
%% 
%% disconnect(ServerRef,Socket) ->
%% 	erlang:send(ServerRef, {disconnect,Socket}).
%% 
%% 
%% %%停止服务
%% stop(Name)  -> 
%%     gen_server:call(Name,stop),
%% 	%%等待所有连接断开
%%     timer:sleep(2000).
%% 
%% 
%% init( [] ) -> 
%% 	%%设成系统进程，不让子进程关闭
%%     erlang:process_flag(trap_exit, true),    
%% 	%% 启动数据库
%% 	chat_db:start(),
%% 	ets:new(user_in_mem,[set,public,named_table]),
%% 	ChatCast = sets:new(),
%% 	State = #state{ players = user_in_mem,chat_casts = ChatCast},
%%     {ok,State}.
%% 
%% 
%% handle_call(stop,_From,State) ->
%%     {stop,normal,stopped,State};
%% handle_call(Request,_From,State) ->
%% 	io:format("Request: ~p~n", [ Request ]),
%% 	{reply,argument_error,State}.
%% 
%% handle_cast(stop,State) ->
%%     {stop, normal, State};
%% handle_cast(_Msg,State) ->
%%     {noreply,State}.
%% 
%% chat_msg_cast_pid(Socket) ->
%% 	string:concat(  "chat_msg_cast", erlang:port_to_list(Socket) ).
%% handle_info({msg,  Socket,Data},State) ->
%% 	ChatMsgCastPid = chat_msg_cast_pid(Socket),
%% 	TableID = State#state.players,
%% 	Player = ets:lookup(TableID, Socket),
%% 	case erlang:whereis( ChatMsgCastPid ) of
%% 		undefined -> 
%% 			chat_msg_cast_server:start_link( ChatMsgCastPid ),
%% 			ChatCasts = State#state.chat_casts,
%% 			case sets:is_element(ChatMsgCastPid , ChatCasts) of 
%% 				false ->
%% 					NewSet = sets:add_element(ChatMsgCastPid , ChatCasts),
%% 					NewState = State#state{ chat_casts = NewSet },
%% 					{noreply, NewState};
%% 				true ->
%% 					{noreply, State}
%% 			end;
%% 		_ ->
%% 			chat_msg_cast_server:send(ChatMsgCastPid,   {TableID,Socket,Player,Data} ),
%% 			{noreply, State}
%% 	end;
%% handle_info({connect,Socket},State) ->
%% %% 	Tab = State#state.players,
%% %% 	ets:insert( Tab, {Socket,null} ),
%% 	{noreply, State};
%% handle_info({disconnect,Socket},State) ->
%% 	File = "D:\\tt\\chat_server3.txt",
%% 	{ ok,S }= file:open(File, [  append ]),
%% 	io:format(  S , "~p ~p~n",   [ time(),Socket]),
%% 	file:close( S ),
%% %% 	Tab = State#state.table,
%% %% 	ets:delete(Tab, Socket),
%% 	?CHAT_SEND_SERVER:remove_record(?CHAT_SEND_SERVER, Socket),
%% 	{noreply, State};
%% handle_info(_Info,State) ->
%% 	io:format("chat  server Format: ~p~n",[_Info]),
%% 	
%%     {noreply,State}.
%% 
%% terminate(_Reason,_State) ->
%%     ok.
%% 
%% code_change(_OldVsn,State,_) ->
%%     {ok,State}.
%% 
