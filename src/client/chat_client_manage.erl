-module(chat_client_manage).
-export([start/1]).

-define(IP,"192.168.15.139").
-define(PORT,8080).
-define(CLIENT,chat_client).
-define(FUN,start).

start(Number) ->
	manage_client(Number).

manage_client(0) ->
	finish;
manage_client(Number) ->
	spawn( ?CLIENT, ?FUN , [ ?IP, ?PORT, integer_to_list(Number), integer_to_list(Number)] ),
	io:format(lists:concat(["client",Number,"~n"])),
	manage_client(Number -1).
