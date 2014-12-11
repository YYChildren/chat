-module(chat_interface).
-export([start/0,stop/0,send/2,tell_connect/1,disconnect/1]).

start() -> 
    chat_server3:start(?MODULE).

stop()->
    chat_server3:stop(?MODULE).
send( Socket,Data ) ->
	chat_server3:process_msg(?MODULE, Socket,Data ).
tell_connect( Socket ) ->
    chat_server3:do_connect(?MODULE, Socket ).
disconnect(Socket) ->
	chat_server3:disconnect(Socket).