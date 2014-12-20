%% @author yangchaojun
%% @doc @todo Add description to chat_msg_cast_server.


-module(chat_msg_cast_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ start_link/2,send/2,recover_send/2,stop/1]).
start_link( ServerRef,Socket ) ->                                 
    gen_server:start_link({local, ServerRef },?MODULE,[Socket],[]).

send(  ServerRef , { Data } ) ->
	Msg = { msg, Data },
	erlang:send( ServerRef , Msg).
%% 一般是进程崩溃
recover_send(  ServerRef , { Player,Data } ) ->
	Msg = { recover_msg, Player,Data },
	erlang:send( ServerRef , Msg).

stop( ServerRef ) ->
	erlang:send(ServerRef, stop).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-define(CHAT_SERVER,chat_server).
-define(CHAT_SEND_SERVER,chat_send_server).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
%%状态表
-record( state,{  socket, playername=null,  current_zone="world",  player_info=null,  channels }).
-record(player,{playername, current_zone,player_info }).
%% player_info  ets{  zone,time  }


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init( Socket ) ->
	Channels = chat_channel_manage:load_channel(),
    {ok, #state{socket = Socket, channels= Channels }}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info( {msg,Data}, State) ->
	NewState = chat_client(Data,State),
	{noreply, NewState};
handle_info( {recover_msg,Player,Data}, State) ->
	%崩溃重启后更新State
	State1 = State#state{
					playername = Player#player.playername, 
					current_zone = Player#player.current_zone , 
					player_info = Player#player.player_info },
	NewState = chat_client( Data,State1 ),
	{noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.


chat_client( Data,State ) ->
	Socket = State#state.socket,
	PlayerInfo = State#state.player_info,
	Channels = State#state.channels,
	case PlayerInfo of
		null ->
			Login = string:substr(Data, 1,8),
			case Login of
				%% 登录标记段
				?LOGIN_TAG	->
		            [UserName,PassWord] = string:tokens( string:substr(Data, 9)," "),
					%% 登录或连接
					case res_or_login( Socket, UserName, PassWord, Channels ) of 
						{register , NewPlayerInfo} ->
							gen_tcp:send( Socket,["Welcome new user! Please remember your username and password"] ),
							NewState = State#state{ player_info =NewPlayerInfo },
							NewState;
						{ok,NewPlayerInfo} ->
							gen_tcp:send( Socket,[ string:concat( UserName," logined")] ),
							NewState = State#state{ player_info =NewPlayerInfo },
							NewState;
						exist ->
							gen_tcp:send( Socket,[ lists:concat( [UserName ," is exist and the password is wrong"] )] ),
							State;
						_ ->
							State
					end;
				_	->
					gen_tcp:send(Socket,"Please login"),
					State
			end;
		_ ->
			case string:substr(Data,1,8) of
			            %%切换频道标记段
			            ?SWITCH_TAG ->
			                Zone = string:substr(Data,9),
			                case chat_channel_manage:load_channel( Zone ) of
			                    [ { channel,Zone,_Public ,_Timeout } ]  ->
									%% 切换频道
									case switch_channel(  State ,Socket, Zone) of
										{ keep, State} ->
											gen_tcp:send( Socket, [ lists:concat(  [ "You are in ", Zone,". Don't have to switch" ] ) ] ),
											State;
										{switched,NewState} ->
											gen_tcp:send( Socket, [ lists:concat(  [ "switch to ", Zone ] ) ] ),
											NewState
									end;
								_ ->
									NewState = send_data( State, Socket, Data ),
									NewState
 			                end;
						_->
							Playername = State#state.playername,
							Zone = State#state.current_zone,
							?CHAT_SEND_SERVER:send(?CHAT_SEND_SERVER,  Socket,  Playername, Zone, Data)
 				end
	end.

send_data( State,Socket,Data ) ->
	Zone = State#state.current_zone,
	PlayerInfo = State#state.player_info,
	{Zone,OldTime} = ets:lookup( PlayerInfo, Zone),
    [{channel,_Zone,_Public,Timeout}] = chat_channel_manage:load_channel( Zone ),
	CurrentTime = time_handler:timestamp(),
    case  ( Lest = CurrentTime - OldTime ) >= Timeout of
        true ->
            %% 插入最后发言时间
			ets:insert(PlayerInfo, {Zone, CurrentTime}),
			%% 把消息发给  群发进程
            ?CHAT_SEND_SERVER:send(?CHAT_SEND_SERVER, 
								   Socket, State#state.playername, State#state.current_zone, Data),
			io:format("------------------------------------------~n~n"),
			State;
        false ->
			%%多少秒后可发言
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Timeout - Lest), " seconds."])),
			State
    end.

%% 切换频道
switch_channel( State,Socket,Zone) ->
	%% 更改频道和时间戳
	OldZone = State#state.current_zone,
	case Zone =:= OldZone of
		true ->
			{ keep, State};
		false	->
			?CHAT_SEND_SERVER:switch_channel( ?CHAT_SEND_SERVER, Socket, OldZone, Zone),
			NewState = State#state{ current_zone =Zone},
			{switched,NewState}
	end.


get_trans_tab(Socket) ->
	erlang:list_to_atom(  lists:concat("trans", erlang:port_to_list(Socket), "_tab") ).
%% 登录或注册
res_or_login( Socket,UserName,PassWord ,Channels) ->
    case chat_user_manage:res_or_login(UserName,PassWord) of
        register ->
			NewPlayerInfo = get_trans_tab( Socket ),
			ets:new( NewPlayerInfo ,  [ set, protected,named_table ]),
			Timestamp = time_handler:timestamp(),
			InitPlayerInfoFun = fun(  {channel, Zone, Public, _Timeout} ) ->
										ets:insert( NewPlayerInfo ,  { Zone , Timestamp} ),
										case Public of 
											true ->
												?CHAT_SEND_SERVER:add_record(?CHAT_SEND_SERVER, Zone, Socket);
											_ ->ok
										end
								end,
			lists:foreach(InitPlayerInfoFun, Channels),
			{register , NewPlayerInfo};
        ok       ->
			NewPlayerInfo = get_trans_tab( Socket ),
			ets:new( NewPlayerInfo ,  [ set, protected,named_table ]),
			Timestamp = time_handler:timestamp(),
			InitPlayerInfoFun = fun(  {channel, Zone, Public, _Timeout} ) ->
										ets:insert( NewPlayerInfo ,  { Zone , Timestamp} ),
										case Public of 
											true ->
												?CHAT_SEND_SERVER:add_record(?CHAT_SEND_SERVER, Zone, Socket);
											_ ->ok
										end
								end,
			lists:foreach(InitPlayerInfoFun, Channels),
            {ok,NewPlayerInfo};
        exist    ->
			exist
	end.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	Record = { 
			  State#state.socket,     
			   #player{
					   playername=State#state.playername,
					   current_zone=State#state.current_zone,
					   player_info=State#state.player_info
					  }            
			 },
	?CHAT_SERVER:do_cast_stop(?CHAT_SERVER,  {Reason, Record} ),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


