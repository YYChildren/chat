-module(chat_server2).

-behaviour(gen_server).

-export([start/3,stop/1]).


%% gen_server回调

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,

         terminate/2,code_change/3]).

-compile(export_all).


-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  

%%状态表

-record(state, {name,loop,chat_client,socket}).


%%回调gen_server创建服务
start(Name,Loop,LoopClient) -> 
    %%定为记录name= module     loop=item()     loop_client = fun
    State=#state{name=Name,loop=Loop,chat_client=LoopClient},                                
    gen_server:start_link({local,Name},?MODULE,State,[]).                        


%%停止服务
stop(Name)  -> 
    gen_server:call(Name,stop).


init(State) -> 
    
    %%启动数据库
    chat_db:start(),
    io:format("~p Database is started!~n",[?MODULE]),

    %%维护队列 
    register(State#state.loop, spawn(fun() -> (State#state.name):(State#state.loop)() end)),

    %%默认8080
    {Tag, LSocket}=gen_tcp:listen(8080, ?TCP_OPTIONS), 
    %%创建监听
    case Tag of
        ok ->
            %%统一接收
            spawn(fun() -> do_accept(State#state{socket=LSocket}) end);        
        error ->
            %%出错，退出连接
            exit({stop, exit})                               
    end,
    {ok,LSocket}.

handle_call(stop,_From,Tab) ->
    {stop,normal,stopped,Tab}.

handle_cast(stop,State) ->
    {stop, normal, State};

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_) ->
    {ok,State}.

%%------------------------------------
%% -------------私有函数--------------
%% -----------------------------------

%%新建连接,只有新连接时才调用
do_accept(State) ->
    case gen_tcp:accept(State#state.socket) of
        {ok, Socket} -> 
            %%创建进程处理响应
            spawn(fun() -> handle_client(State,Socket) end),
            %%创建新连接,持久化
            State#state.loop ! {connect, Socket},
            do_accept(State);
        _ ->
            ok
    end. 
                                 

handle_client(State,Socket) ->
	TableID = request_ets(State#state.loop),
	io:format("~p ~n", [TableID]),
	loop_client(TableID,State,Socket).

request_ets( Pid ) ->
	Pid ! { request_ets,self() },
	receive
		{ reply_ets, TableID }  ->
			TableID;
		_ ->
			%% 循环请求Table
			request_ets( Pid )
	end.

loop_client(TableID,State,Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
			%% player进程 这里不要并发，一个palyer 登录  -》 发言本来就是串行的
			io:format("~p ~p ~p ~p ~p ~n", [?MODULE,TableID,Socket,Data,State]),
			(State#state.chat_client)(TableID,Socket,Data),
            loop_client(TableID,State,Socket);
        {error, _} ->
            State#state.loop ! {disconnect, Socket}
    end.
