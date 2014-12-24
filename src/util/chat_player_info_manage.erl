%% @author yangchaojun
%% @doc @todo Add description to chat_player_info_manage.


-module(chat_player_info_manage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([load/0,load/1,insert/1,insert_all/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
load() ->
	chat_db:select_player().
load(Playername) ->
	chat_db:select_player(Playername).
insert( Player ) ->
	chat_db:insert( Player ).
insert_all( ListPlayer) ->
	chat_db:insert_all( ListPlayer ).
