-module(chat_client).
-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
-compile(export_all).


start(Ip,Port,UserName,PassWord) ->
	case do_connect(Ip,Port) of 
		{ok, Socket} ->
			io:format("Socket"),
			register(do_recv_data,spawn( ?MODULE, fun do_recv_data/1 , Socket )),
			do_login(Socket,UserName,PassWord),
			do_chat(Socket),
			do_disconnect(Socket);

		{error,Why} ->
			output(Why),
			do_recv_data ! exit,
			exit({exit,Why})
	end.
	
do_connect(Ip,Port) ->
	gen_tcp:connect(Ip,Port,?TCP_OPTIONS).

do_chat(Socket) -> 
	timer:sleep(15000),
	send_msg(Socket,"0"),
	send_msg(Socket,"c3dpdGNocountry1"),
	send_msg(Socket,"1"),
	send_msg(Socket,"c3dpdGNocountry2"),
	send_msg(Socket,"2"),
	send_msg(Socket,"c3dpdGNocountry3"),
	send_msg(Socket,"3"),
	send_msg(Socket,"c3dpdGNocountry4"),
	send_msg(Socket,"4").

do_recv_data(Socket) ->
	case gen_tcp:recv(Socket, 0) of
        {Data} ->
            io:format("~p~n",[Data]);
        {error, closed} ->
            ok
    end,
    do_recv_data(Socket).

do_login(Socket,UserName,PassWord) ->
	send_msg(Socket, lists:concat( [UserName," ",PassWord] )).

do_disconnect(Socket) ->
	gen_tcp:close(Socket).

output(Msg) ->
	io:format("Msg: ~p~n",[Msg]).


send_msg(Socket,Msg) ->
	gen_tcp:send(Socket,Msg).
