%% @author yangchaojun
%% @doc @todo Add description to common_name.


-module(common_name).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_name/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

get_name(name_pid,Name) ->
	erlang:list_to_atom(Name);
get_name(table,Name) ->
	erlang:list_to_atom( string:concat(Name, "_tab") );
get_name(table_pid,Name) ->
	erlang:list_to_atom( string:concat(Name, "_tab_pid") );
get_name(send_socket,Socket ) ->
	get_name( name_pid, string:concat( "send",erlang:port_to_list( Socket) ) );
get_name(client_socket,Socket) ->
	get_name( name_pid,string:concat( "client",erlang:port_to_list( Socket) ) );
get_name(_,_) ->
	no_match_error.