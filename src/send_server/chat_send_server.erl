%% @author yangchaojun
%% @doc @todo Add description to chat_pro.


-module(chat_send_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,start/2,send/4,add_record/3,del_record/3,switch_channel/4,remove_record/2]).

start(ServerRef) ->
	ok.
start(Name,Channels) -> 
    gen_server:start_link({local,Name},?MODULE,Channels,[]).
send( ServerRef,Socket, Player, Data ) ->
	gen_server:call(ServerRef, {send,Socket, Player, Data}).
add_record(ServerRef, Zone, Socket) ->
	gen_server:call(ServerRef,   {add,Zone,Socket} ).
del_record( ServerRef, Zone, Socket) ->
	gen_server:call(ServerRef,   {del,Zone,Socket}  ).
remove_record( ServerRef, Socket) ->
	io:format("~p ~p ~n",[?MODULE,?LINE]),
	gen_server:call(ServerRef, {remove_record,Socket}).
switch_channel( ServerRef, Socket,OldZone,Zone) -> 
%% 	OldZone = Player#player.zone,
	case OldZone =:= Zone of 
		%% 原纪录和新纪录相同
		true ->
			ok;
		false ->
			
			case chat_channel_manage:load_channel( OldZone ) of
				%% 原纪录是public
				[ {channel,OldZone,true,_Timeout} ] ->
					case chat_channel_manage:load_channel(Zone) of
						%% 新纪录不是public
						[ {channel,Zone,false,_Timeout1} ] ->
							add_record(ServerRef,Zone,Socket);
						_ ->
							ok
					end;
				%% 原纪录不是public
				_ ->
					del_record( ServerRef, OldZone, Socket),
					case chat_channel_manage:load_channel(Zone) of
						%% 新纪录不是public
						[ {channel,Zone,false,_Timeout1} ] ->
							add_record(ServerRef,Zone,Socket);
						_ ->
							ok
					end
			end
	end.
			


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {table_pids}).
% -record(player, {name,zone="world",time=none}).

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
init( Channels ) ->
	%%设成系统进程，不让子进程关闭
    %%erlang:process_flag(trap_exit, true),
	TablePids = create_tab_server( Channels,[]),
    {ok, #state{table_pids = TablePids}}.
create_tab_server( [],TablePids ) ->
	TablePids;
create_tab_server(Channels,TablePids) ->
	[ {channel,Zone,_Public, _Timeout} | Others ] = Channels,
	Table = common_name:get_name(table, Zone),
	TablePid = common_name:get_name(table_pid, Zone),
	chat_tab_server:start( TablePid  ,  Table ),
	create_tab_server(Others,[TablePid | TablePids]).

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
handle_call(  {send,Socket, Player, Data} ,  _From,  State) ->
	SendRegName = common_name:get_name(  send_socket,Socket ),
	case erlang:whereis(  SendRegName ) of
		undefined -> 
			chat_send_per_server:start( SendRegName),
			chat_send_per_server:send( SendRegName , Player, Data);
		_ ->
			chat_send_per_server:send( SendRegName , Player, Data)
	end,
	Reply = send_data,
    {reply, Reply, State};
handle_call( {add,Zone,Socket},  _From,  State ) ->
	TablePid = common_name:get_name(table_pid, Zone),
	Reply = chat_tab_server:add_record( TablePid,{Socket} ),
	{reply, Reply, State};
handle_call( {del,Zone,Socket},  _From,  State ) ->
	TablePid = common_name:get_name(table_pid, Zone),
	Reply = chat_tab_server:del_record( TablePid,Socket ),
	{reply, Reply, State};
handle_call( {remove_record,Socket},_From,State ) ->
	io:format("~p ~p ~n",[?MODULE,?LINE]),
	TablePids = State#state.table_pids,
	%%在每个ets表上删除记录
	do_remove_record(TablePids,Socket),
	%%关闭进程
	SendRegName = common_name:get_name(  send_socket,Socket ),
	chat_send_per_server:stop( SendRegName),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

do_remove_record([],Socket) ->
	{removed,Socket};
do_remove_record(TablePids,Socket) ->
	[TablePid | Others] = TablePids,
	chat_tab_server:del_record( TablePid,Socket ),
	do_remove_record(Others,Socket).

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
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
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

