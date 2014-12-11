-module(chat2).
-export([start/0,manage_clients/0,stop/0,client_count/0]).
-define(LOGIN_TAG,"bG9naW4=").
-define(SWITCH_TAG,"c3dpdGNo").
-record(player, {name,zone="world",time=none}).

start() -> 
    chat_server2:start(?MODULE,manage_clients,fun chat_client/3).

stop()->
    chat_server2:stop(?MODULE),
    %%等待所有连接断开
    timer:sleep(2000),    
    manage_clients ! {exit}.

%%可封装成独立模块
client_count() ->
	manage_clients  !  { request_ets, self()  },
	receive
		{ reply_ets,TableID }  ->
			do_client_count( TableID );
		_ ->
			timer:sleep(2000),
			client_count()
	end.
do_client_count( TableID ) ->
	timer:sleep(2000),
	io:format("process indo:~p~n",  [ erlang:process_info(  erlang:whereis( get_name( zone_pid,"world") ))]),
	io:format("info of table: ~p~n ",  [ ets:info(TableID) ] ),
	io:format("World~p~n", [ ets:info(  get_name(table,"world"))  ]),
	%io:format("country1~p~n", [ ets:info(  get_name(table,"country1"))  ]),
	%io:format("country2~p~n", [ ets:info(  get_name(table,"country2"))  ]),
	%io:format("country3~p~n", [ ets:info(  get_name(table,"country3"))  ]),
	do_client_count( TableID ).
	

manage_clients() ->
    %%用ets表保存Socket  对应  User
    TableID = ets:new(user_in_mem,[set,public]),
	Channel = chat_channel_manage:load_channel(),
	create_channel_tab( Channel ),
    loop(TableID).

create_channel_tab([]) ->
	ok;
create_channel_tab(Channel) ->
	[ {channel, Zone, _Public, _Timeout} | Other] = Channel,
	%% 创建群发进程
	%% 发送消息的进程名是  Zone
	%% 表名是 Zone_tab
	%% 管理ets表的进程名是  TabPid = Zone_tab_pid
	register( get_name( zone_pid,Zone) ,spawn( fun() -> group_chat(  Zone )  end ) ),
	create_channel_tab(Other).
	
group_chat( Zone )  ->
	%% 发送消息的进程名是  Zone
	%% 表名是 Zone_tab
	%% 管理ets表的进程名是  TabPid = Zone_tab_pid
	Tab = get_name( table,Zone),
	TabPid = get_name( table_pid,Zone),
	%% io:format("table pid ~p~n", [ TabPid ]),
	%% 加上named_table选项
	register( TabPid,   spawn( fun() ->  ets_handler:table( Tab,  [set,protected,named_table] ) end) ),
	do_group_chat( Zone,Tab ).
		
do_group_chat( Zone,Tab)->
	receive 
		{ PlayerName, Data} ->
			ets_handler:foreach_key(Tab,   fun(Socket) -> gen_tcp:send(Socket, [ lists:concat(  [Zone, "->",PlayerName,": ",Data]  ) ] )  end);
			%ets_handler:foreach_key(Tab,fun(Key) ->io:format("Key ~p~n", [ Key ]) end);
		_	->
			error
	end,
	do_group_chat( Zone, Tab ).


get_name(Type,Zone) ->
	case Type of
		zone_pid ->
			erlang:list_to_atom(Zone);
		table ->
			 erlang:list_to_atom( string:concat(Zone, "_tab") );
		table_pid ->
			erlang:list_to_atom( string:concat(Zone, "_tab_pid")  )
	end.
	

%% 主进程
loop(TableID) ->
    receive
        {connect, Socket} ->
            io:format("~p Socket connected: ~p~n", [?MODULE,Socket]),		
            ets:insert(TableID,{Socket, null } );
            
        {disconnect, Socket} ->
            io:format("~p Socket disconnected: ~p~n", [?MODULE,Socket]),
			[{Socket,_Player}] = ets:lookup(TableID, Socket),
%% 			case Player of 
%% 				null ->
%% 					null;
%% 				_ ->
%% 					Zone  = Player#player.zone,
%% 					get_name( table_pid, Zone) ! { del,Socket },
%% 					Channels = chat_channel_manage:load_channel(Zone),
%% 					%% public的删除
%% 					%% 非public的删除
%% 					del_socket( Channels ,TableID,Socket)
%% 			end,
			Channels = chat_channel_manage:load_channel(),
			del_socket( Channels ,TableID,Socket),
            ets:delete(TableID,Socket);
		
		{ request_ets,Pid } ->
			Pid ! { reply_ets,TableID };
		
        {exit}->
            ets:delete(TableID),
            exit({stop, exit})
    end,
    loop(TableID).

del_socket( [] ,_TableID,_Socket) ->
	finish;
del_socket( Channels ,TableID,Socket) ->
	
 	[ { channel,Zone,_Public, _Timeout} | Other ] = Channels,
	get_name( table_pid, Zone) ! { del,Socket },
%% 	case Public of
%% 		true ->
%% 			get_name( table_pid, Zone) ! { del,Socket };
%% 		false ->
%% 			ok
%% 	end,
	del_socket( Other ,TableID,Socket).
			

chat_client(TableID,Socket,Data) ->
		
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
			[{Socket,Player}]	->
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
									send_data( TableID, Socket,Player,Data )
 			                end;
						_->
							send_data( TableID, Socket,Player,Data )
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
					get_name( table_pid, Zone) !  { add,{ Socket} },
					register;
		        ok       ->
					Player = #player{name=UserName,time=time_handler:timestamp() },
		            ets:insert(TableID, { Socket,  Player }),
					[ {channel, Zone ,_Public,_Timeout  } ] = chat_channel_manage:load_channel(  Player#player.zone  ),
					get_name( table_pid, Zone) !  { add, {Socket} },
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
			%% 更改频道和时间戳
			case Zone =:= Player1#player.zone of
				true ->
					ok;
				false	->
					Zone1 = Player1#player.zone,
					[ { channel, Zone1 ,Public1,_Timeout1 }  ] = chat_channel_manage:load_channel(  Zone1  ),
					case Public1 of 
						false ->
							get_name( table_pid , Zone1  ) ! { del , Socket } ;
						_ ->
							ok
					end,
					[ { channel, Zone ,Public,_Timeout } ]  = chat_channel_manage:load_channel(Zone),
					case Public of 
						false ->
							get_name( table_pid,Zone) ! {add ,{ Socket } };
						_ ->
							ok
					end,
					Player = Player1#player{zone=Zone, time=time_handler:timestamp() },
					ets:insert(TableID,  { Socket, Player } ),
					switched
			end;
		_->
			error
	end.

send_data( TableID, Socket,Player,Data ) ->
    io:format("~p Data: ~p~n",[?MODULE,Data]),
	io:format("~p ~n", [Player]),
    [{channel,Zone,Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
    io:format("~p ~p ~n",[?MODULE,    [{channel,Zone,Public,Time}]     ]),

    Name = Player#player.name,
    case  ( Lest = time_handler:timestamp() - Player#player.time ) >= Time of
        true ->
            %% 插入最后发言时间
            Player1 = Player#player{ time=time_handler:timestamp() } ,
            ets:insert( TableID,{Socket,Player1} ),
			io:format("insert time ~n~n"),
			%% 把消息发给  群发进程
            get_name( zone_pid, Zone ) ! { Name,Data };
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.