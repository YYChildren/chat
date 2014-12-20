%% @author yangchaojun
%% @doc @todo Add description to chat_client2.


-module(chat_client2).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
-define(ZONE,{"world","country1","country2","country3"}).

%% ====================================================================
%% Internal functions
%% ====================================================================

start(Ip,Port,UserName,PassWord,Type) ->
	case do_connect(Ip,Port) of
		%% 连接成功
		{ok,Socket}	->
			timer:sleep(10000),
			output(" connected!"),
			do_login(Socket,UserName,PassWord),
			
			case gen_tcp:recv(Socket, 0) of
				%% 登录成功
				{ok,Data} ->
					outputServerMsg(Data),
					
					%% 必须要做
					Pid = spawn( ?MODULE, do_recv_data , [Socket] ) ,
					gen_tcp:controlling_process(Socket, Pid),
					
					Size = erlang:size(?ZONE),
					do_chat(Socket,Type,Size);
				{error,Why} ->
					output(Why);
				_ ->
					ignore
			end;
		{error,Why}	->
			{ error,Why }
	end.
	
do_connect(Ip,Port) ->
	gen_tcp:connect(Ip,Port,?TCP_OPTIONS).

do_chat(Socket,Type,Size) ->
	Type1 = case Type rem Size of
				0	->
					Size;
				X	->
					X
			end,
	output(Type1),
	
	%%switch_channel(Socket, erlang:element(Type1, ?ZONE)),
	%% 并发进程导致ets的tab_to_list有不同备份
	timer:sleep(10000),
	send_msg(Socket,   binary_to_list( unicode:characters_to_binary(  "msg" ) )),
	do_chat(Socket,Type1 + 1,Size).
	
switch_channel(Socket,Channel) ->
	send_msg(Socket,string:concat("c3dpdGNo", Channel)).

do_recv_data(Socket) ->
	case gen_tcp:recv(Socket, 0) of
        {error, closed} ->
            ok;
		Data	->
			outputServerMsg(Data)
    end,
    do_recv_data(Socket).

do_login(Socket,UserName,PassWord) ->
	send_msg(Socket, lists:concat( ["bG9naW4=",UserName," ",PassWord] )),
	output(lists:concat( ["bG9naW4=",UserName," ",PassWord]) ).

output(Msg) ->
	io:format("~p Msg: ~p~n",[?MODULE,Msg]).

outputServerMsg(Msg) ->
	io:format("~p Msg From Server: ~tp~n",[?MODULE,Msg]).

send_msg(Socket,Msg) ->
	gen_tcp:send(Socket, Msg ).