-module(chat2).
-export([start/0,manage_clients/0,stop/0,client_count/0]).
-record(player, {name,zone="world",time=none}).


start() -> 
    chat_server2:start(?MODULE,manage_clients,fun chat_client/3).


stop()->
    chat_server2:stop(?MODULE),
    %%等待所有连接断开
    timer:sleep(2000),    
    manage_clients!{exit}.

client_count() ->
	timer:sleep(2000),
	manage_clients  !  { request_ets, self()  },
	receive
		{ reply_ets,TableID }  ->
			io:format("info of table: ~p~n ",  [ ets:info(TableID) ] );
		_ ->
			ignore
	end,
	client_count().
	

manage_clients() ->
    %%用ets表保存Socket  对应  User
    TableID = ets:new(user_in_mem,[set,public]),
    loop(TableID).


%% 主进程
loop(TableID) ->
    receive
        {connect, Socket} ->
            io:format("~p Socket connected: ~p~n", [?MODULE,Socket]),		
            ets:insert(TableID,{Socket, null } );
            
        {disconnect, Socket} ->
            io:format("~p Socket disconnected: ~p~n", [?MODULE,Socket]),
            ets:delete(TableID,Socket);
		
		{ request_ets,Pid } ->
			Pid ! { reply_ets,TableID };
		
        {exit}->
            ets:delete(TableID),
            exit({stop, exit})
    end,
    loop(TableID).

chat_client(TableID,Socket,Data) ->
		%% io:format("~p ~n", [ ets:lookup(TableID,Socket) ]),
        case ets:lookup(TableID,Socket) of
			[{Socket,null}]	->
				Login = string:substr(Data, 1,8),
				io:format("~p login tag： ~p~n", [ ?MODULE,Login] ),
				case Login of
					%% 登录标记段
					"bG9naW4="	->
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
			            "c3dpdGNo" ->
			                Zone = string:substr(Data,9),
			                case chat_channel_manage:load_channel( Zone ) of
			                    [ { channel,Zone,_,_ } ]  ->
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
		            ets:insert(TableID, { Socket, #player{name=UserName,time=time_handler:timestamp() } }),
					register;
		        ok       ->
		            ets:insert(TableID, { Socket,  #player{name=UserName,time=time_handler:timestamp()  } }),
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
		[ { Socket,Player } ] ->
			%% 更改频道和时间戳
			Player1 = Player#player{zone=Zone, time=time_handler:timestamp() },
			ets:insert(TableID,  { Socket, Player1 } ),
			switched;
		_->
			error
	end.

send_data( TableID, Socket,Player,Data ) ->
    io:format("~p Data: ~p~n",[?MODULE,Data]),
	io:format("~p ~n", [Player]),
	io:format("~p ~n", [ chat_channel_manage:load_channel(Player#player.zone) ]),
    [{channel,Zone,Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
    io:format("~p ~p ~p ~p~n",[?MODULE,Zone,Public,Time]),

    Name = Player#player.name,
    SendData =  case Public of
                    %%代表世界
                    true ->
                        fun(SocketUser) ->
							io:format("SocketUsert ~p~n", [SocketUser]),
	                        {X, { player,_ , _ , _ } } = SocketUser,
	                        io:format("~p ~n",   [gen_tcp:send( X, [ lists:concat( [Zone,"->", Name , ": " , Data]) ] )] ),
							io:format("World chat~n~n~n")
                        end;

                    %%代表每个国家
                    false ->
                        fun(SocketUser) ->
							io:format("SocketUser ~p~n", [SocketUser]),
                            case SocketUser of
                                {X, { player,_ , Zone , _ } }  ->
                                    gen_tcp:send( X, [  lists:concat( [Zone,"->", Name , ": " , Data]) ]);
                                _   -> 
                                    ok
                            end,
							io:format("Country chat~n~n~n")
                        end
                end,
    case  ( Lest = time_handler:timestamp() - Player#player.time ) >= Time of
        true ->
            %% 插入最后发言时间
            Player1 = Player#player{ time=time_handler:timestamp() } ,
            ets:insert( TableID,{Socket,Player1} ),
			io:format("insert time ~n~n"),
            ets_handler:foreach_line(TableID, SendData);
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.


%% manage_client_chat( TableID,Pid,Socket ) ->

%% 		{ switched, Pid, Zone}                ->
%% 			
%% 		Error ->
%% 					io:format( "~p~n",Error)
%% 	end,
%% 	manage_client_chat( TableID,Pid,Socket ).

%% 对客户端广播,player发言
%%还需要优化遍历
