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
    TableID = ets:new(user_in_mem,[set,public]),
    register(res_or_login,spawn(fun res_or_login/0)),
    register(send_data,spawn(fun send_data/0)),
    loop(TableID).

loop(TableID) ->
    receive
        {connect, Socket} ->
            io:fwrite("Socket connected: ~w~n", [Socket]),
            gen_tcp:send(Socket,[ "You Have connected.Please input your usernamae and password to login.\n" ]),
            ets:insert(TableID,{Socket,null}),
            io:format("test ~w~n",[ets:lookup(TableID,Socket)]);
            %%把Socket插入ets表

        {disconnect, Socket} ->
            io:fwrite("Socket disconnected: ~w~n", [Socket]),
            %%查找socket对应的记录
            gen_tcp:send(Socket,[ "Session is closed!"]),
            ets:delete(TableID,Socket);

        {chat,Socket,Data} ->
            case ets:lookup(TableID,Socket) of

                %% 注册或登录
                [{Socket,null}] -> 
                    io:format("User : ~w~n",[string:tokens(Data," ")]),
                    [UserName,PassWord] = string:tokens(Data," "),
                    res_or_login ! { TableID,Socket,UserName,PassWord };
                
                [{Socket,Player}]      ->
                    case string:substr(Data,1,8) of

                        %%切换频道
                        "c3dpdGNo" ->
                            Zone = string:substr(Data,9),
                            case chat_channel_manage:load_channel( Zone ) of
                                [ { channel,Zone,_,_ } ]  ->
                                    Player1 = Player#player{zone=Zone},
                                    ets:insert(TableID, { Socket, Player1 }),
                                    gen_tcp:send(Socket,string:concat("switch to ",Zone));
                                _                         ->   
                                    send_data ! { TableID, Socket,Player,Data }
                            end;    
                        _          ->

                            send_data ! { TableID, Socket, Player, Data }
                    end
            end;

        {exit}->
            ets:delete(TableID),
            exit({stop, exit})                                        
    end,
    loop(TableID).


%% 登录或注册
res_or_login() ->
    receive
    {TableID,Socket,UserName,PassWord} ->
            case chat_user_manage:res_or_login(UserName,PassWord) of
                register ->
                    ets:insert(TableID, { Socket, #player{name=UserName,time=time_handler:timestamp() } }),
                    gen_tcp:send(Socket,["Welcome new user! Please remember your username and password"]);
                ok       ->
                    ets:insert(TableID, { Socket, #player{name=UserName,time=time_handler:timestamp() } }),
                    gen_tcp:send(Socket,[string:concat(UserName," logined")]);
                exist    ->
                    gen_tcp:send(Socket,["The user is exist and the password is wrong"])
            end
    end,
    res_or_login().


%% 对客户端广播
send_data() ->
    receive
        {TableID,Socket,Player,Data} ->
            io:format("Data: ~w~n",[Data]),
            %% 取出{Socket,User}的列表
            SocketUsers = ets:tab2list(TableID),
            io:format("SocketUsers: ~w~n",[SocketUsers]),

            [{channel,Zone,Public,Time}] = chat_channel_manage:load_channel(Player#player.zone),
            io:format("~w ~w ~w~n",[Zone,Public,Time]),

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
                    lists:foreach( SendData, SocketUsers);
                false ->
                    gen_tcp:send(Socket,lists:concat(["Please wait ",integer_to_list(Time - Lest), " seconds."]))
                    %%多少秒后可发言
            end
        end,

    send_data().



%%对客户端进行广播
% send_data(Players, Data) ->
%     SendData = fun(Player) ->
%                        gen_tcp:send(Player#player.socket, Data) ,
%                        io:format("Data: ~w~n",[Data])                        
%                end,
%     lists:foreach(SendData, Players).

%%查找指定记录
% find_Socket(Socket, Players) ->
%     {value, Player} = lists:keysearch(Socket, #player.socket, Players),
%     Player.

% find_player(Uname, Players) ->
%     case lists:keysearch(Uname, #player.id, Players) of
%         {value, Player}->
%             io:fwrite("Find a player: ~w~n", [Player]);
%         _->
%             io:fwrite("No found.~n")
%     end.



%%查找用户
% find(Uname)->
%     client_manager ! {list,Uname}.
