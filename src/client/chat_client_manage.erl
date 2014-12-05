-module(chat_client_manage).
-export([start/1,stop/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).
-behaviour(gen_server).


-define(IP,"127.0.0.1").
-define(PORT,8080).
-define(CLIENT,chat_client2).
-define(FUN,start).
-define(TYPE,{1,2,3,4} ).

%%回调gen_server创建服务
start(Number) -> 
    %%定为记录
    gen_server:start_link({local,?MODULE},?MODULE,Number,[]).                        

%%停止服务
stop()  -> 
    gen_server:call(?MODULE,stop).


init(Number) ->
	%%总共Types种类型
	Types = erlang:size(?TYPE),
	%%每种类型有NumberPType个
	NumberPType = Number div Types,
	
	do_start_manage(NumberPType,Types).


do_start_manage(Number,Typeth) -> 
	%%创建用户进程
	%% Typeth 表示第Typeth种
	spawn(?MODULE,manage_client,[ Number,Typeth ]),
	do_start_manage(Number,Typeth -1);
do_start_manage(Number,0) ->
	all_finished.

manage_client(0, Typeth) ->
	output(lists:concat( "Type: " ,Typeth ," finieshed"  ));
manage_client(Number,Typeth) ->
	spawn( ?CLIENT, ?FUN , 
		   [ ?IP, ?PORT, integer_to_list(Number * Typeth), integer_to_list(Number * Typeth),
			 erlang:element(Typeth, ?TYPE)] ),
	%% erlang:element(Typeth, ?TYPE)] 这是第Typeth种类型
	output( lists:concat(["Client:",Number," Type: ", erlang:element(Typeth, ?TYPE) ]  ) ),
	manage_client( Number - 1, Typeth ).

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

output(Msg) ->
	io:format("~p Msg: ~p~n",[?MODULE,Msg]).