-module(chat).

% -export([start/0,manage_clients/0,stop/0,find/1]).
-export([start/0,manage_clients/0,stop/0]).

-record(player, {name,zone="world",time=none}).


start() -> 
    chat_server:start(?MODULE,manage_clients).


stop()->
    chat_server:stop(?MODULE),
    %%等待所有连接断开
    timer:sleep(2000),    
    manage_clients!{exit}.


manage_clients() ->
    %%用ets表保存Socket  对应  User
    TableID = ets:new(user_in_mem,[set,protected]),
    loop(TableID).


%% 主进程
loop(TableID) ->
    receive
        {connect, Socket} ->
            io:format("~p Socket connected: ~p~n", [?MODULE,Socket]),
		
			Pid = spawn( ?MODULE, manage_client_chat,  [  TableID, self() ,Socket  ] ),
			%% TableID ->  (    Scoket   -> (    Pid   ->   Player ) )
            ets:insert(TableID,{Socket, {Pid ,null } } ),
            io:format("~p test ~p~n",[?MODULE,ets:lookup(TableID,Socket)]);
            %%把Socket插入ets表

        {disconnect, Socket} ->
            io:format("~p Socket disconnected: ~p~n", [?MODULE,Socket]),
            %%查找socket对应的记录
            gen_tcp:send(Socket,[ "Session is closed!"]),
            ets:delete(TableID,Socket);

		%% 把Data发给相应进程，单独处理
        {chat,Socket,Data} ->
            case ets:lookup(TableID,Socket) of
				{Pid,Player}	->
					Pid ! { chat, self() ,Player, Data };
				_					->
					null
            end;
		
		{res_or_login,Socket,Pid,UserName,PassWord} ->
			res_or_login( TableID,Socket,Pid,UserName,PassWord );
		
		%% 切换频道
		{switch_channel,Socket,Pid,Zone} ->
			switch_channel(TableID,Socket,Pid,Zone);
		
        {exit}->
            ets:delete(TableID),
            exit({stop, exit})                                        
    end,
    loop(TableID).

%% 登录或注册
res_or_login( TableID,Socket,Pid,UserName,PassWord ) ->
	case ets:lookup(TableID, Socket) of
		%% 确保是该用户进程的动作
		[ { Socket, { Pid, _  } } ] ->
		    case chat_user_manage:res_or_login(UserName,PassWord) of
		        register ->
		            ets:insert(TableID, { Socket, { Pid,#player{name=UserName,time=time_handler:timestamp()} } }),
					Pid ! { registered, self()  ,UserName};
		        ok       ->
		            ets:insert(TableID, { Socket, { Pid,#player{name=UserName,time=time_handler:timestamp()} } }),
		            Pid ! { logined , self()  ,UserName};
		        exist    ->
					Pid ! { exist, self()  ,UserName}
		    end;
		_ ->
			Pid ! { login_error, self(), UserName}
	end.
	
%% 切换频道
switch_channel(TableID,Socket,Pid,Zone) ->
	case ets:lookup(TableID, Socket) of
		%% 确保是该进程的动作
		[ { Socket, { Pid, Player  } } ] ->
			%% 更改频道和时间戳
			Player1 = Player#player{zone=Zone, time=time_handler:timestamp() },
			ets:insert(TableID,  { Socket, { Pid,Player1 } } ),
			Pid ! { switched,  self(), Zone };
		_->
			Pid ! { switch_error,self() ,Zone }
	end.

manage_client_chat( TableID,Pid,Socket ) ->
	receive
		{chat, Pid, Socket, Player,Data} ->
			case Player of
				%% 注册或登录
				null ->
					Login = string:substr(Data, 1,8),
					io:format("~p login tag： ~p~n", [ ?MODULE,Login] ),
					case Login of
						%% 登录标记段
						"bG9naW4="	->
			                [UserName,PassWord] = string:tokens( string:substr(Data, 9)," "),
							io:format("~p User : ~p ~p~n",[?MODULE,UserName,PassWord]),
							%% 把登录注册信息发给主进程
			                Pid ! { res_or_login,Socket, erlang:self(), UserName, PassWord };
			    		_	->
							gen_tcp:send(Socket,"Please login")
					end;
				{ player,Name,Zone,Time } ->
					case string:substr(Data,1,8) of
			            %%切换频道标记段
			            "c3dpdGNo" ->
			                Zone = string:substr(Data,9),
			                case chat_channel_manage:load_channel( Zone ) of
			                    [ { channel,Zone,_,_ } ]  ->
									%% 把切换频道信息发给主线程
									Pid ! {switch_channel, Socket, self(), Zone } ;
								_ ->
									send_data( TableID, Socket,Player,Data )
 			                end;
						_->
							send_data( TableID, Socket,Player,Data )
 					end;
				Error ->
					io:format( "~p~n",Error)
 			end;
		{ registered,Pid,Socket } ->
			gen_tcp:send( Socket,["Welcome new user! Please remember your username and password"] );
		{ logined,Pid,Socket,UserName} ->
			gen_tcp:send( Socket,[ lists:concat(UserName," logined")] );
		{ exist, Pid, UserName }   ->
			gen_tcp:send( Socket,[ lists:concat( [UserName ," is exist and the password is wrong"] )] );
		{ switched, Pid, Zone}                ->
			gen_tcp:send( Socket, [ lists:concat(  [ "switch to ", Zone ] ) ] );
		Error ->
					io:format( "~p~n",Error)
	end,
	manage_client_chat( TableID,Pid,Socket ).

%% 对客户端广播,player发言
%%还需要优化遍历
send_data( TableID, Socket,Player,Data ) ->
    io:format("~p Data: ~p~n",[?MODULE,Data]),
    %% 取出{Socket,User}的列表
    SocketUsers = ets:tab2list(TableID),
    io:format("~p SocketUsers: ~p~n",[?MODULE,SocketUsers]),

    [{channel,Zone,Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
    io:format("~p ~p ~p ~p~n",[?MODULE,Zone,Public,Time]),

    Name = Player#player.name,
    SendData =  case Public of

                    %%代表世界
                    true ->
                        fun(SocketUser) ->
                            {X, { player,_ , _ , _ } } = SocketUser,
                            gen_tcp:send( X, lists:concat( [Zone,"->", Name , ": " , Data] ))
                        end;

                    %%代表每个国家
                    false ->
                        fun(SocketUser) ->
                            case SocketUser of
                                {X, { player,_ , Zone , _ } }  ->
                                    gen_tcp:send( X, lists:concat( [Zone,"->", Name , ": " , Data] ));
                                _   -> 
                                    ok
                            end
                        end
                end,
    case  ( Lest = time_handler:timestamp() - Player#player.time ) >= Time of
        true ->
            %% 插入最后发言时间
            Player1 = Player#player{ time=time_handler:timestamp() } ,
            ets:insert( TableID,{Socket,Player1} ),
			io:format("insert time ~n~n"),
            lists:foreach( SendData, SocketUsers);
        false ->
            gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
            %%多少秒后可发言
    end.



%%对客户端进行广播
% send_data(Players, Data) ->
%     SendData = fun(Player) ->
%                        gen_tcp:send(Player#player.socket, Data) ,
%                        io:format("Data: ~p~n",[Data])                        
%                end,
%     lists:foreach(SendData, Players).

%%查找指定记录
% find_Socket(Socket, Players) ->
%     {value, Player} = lists:keysearch(Socket, #player.socket, Players),
%     Player.

% find_player(Uname, Players) ->
%     case lists:keysearch(Uname, #player.id, Players) of
%         {value, Player}->
%             io:fwrite("Find a player: ~p~n", [Player]);
%         _->
%             io:fwrite("No found.~n")
%     end.

%%查找用户
% find(Uname)->
%     client_manager ! {list,Uname}.
