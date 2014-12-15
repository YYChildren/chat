
-module(chat_db).
-import(lists, [foreach/2]).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(user, {username, password}).
-record(channel,{zone,public,timeout}).

do_this_once() ->
    mnesia:create_schema([erlang:node()]),
    mnesia:start(),
    mnesia:create_table(user,   [{attributes, record_info(fields, user)},{disc_copies, [erlang:node()]} ]),
    mnesia:create_table(channel,   [{attributes, record_info(fields, channel)},{disc_copies, [erlang:node()]} ]),
    %% 初始化频道信息
    init_channel(),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([user], 20000),
    mnesia:wait_for_tables([channel], 20000),
	io:format("~p Database is started!~n",[?MODULE]).

stop() ->
    mnesia:stop().




demo(select_channel)->
    do(qlc:q([X || X <- mnesia:table(channel)]));
demo(select_user) ->
    do(qlc:q([X || X <- mnesia:table(user)])).

demo(select_channel,Zone)->
    do(qlc:q([X || X <- mnesia:table(channel),
                    X#channel.zone =:= Zone
        ]));
demo(select_user,UserName) ->
    do(qlc:q([X || X <- mnesia:table(user),
                    X#user.username =:=  UserName 
        ])).
%% demo( Tab ,Fields,Value ) ->
%% 	do(qlc:q([X || X <- mnesia:table(Tab),
%%                     X#Tab.Fields =:=  Value 
%%         ])).



do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

add_user(UserName, Password) ->
    Row = #user{username=UserName, password=Password},
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

remove_user(UserName) ->
    Oid = {user, UserName},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    mnesia:transaction(F).

reset_tables() ->
    mnesia:clear_table(user),
    mnesia:clear_table(channel),
	init_user(),
    init_channel().

init_channel() ->
    F = fun() ->
        foreach(fun mnesia:write/1, channel_tables())
    end,
    mnesia:transaction(F).
init_user()->
	F = fun() ->
        foreach(fun mnesia:write/1, user_example_tables())
    end,
	mnesia:transaction(F).

user_example_tables() ->
	[
	 {user,"example1","example1"},
	 {user,"example2","example2"}
	 ].

channel_tables() ->
    [
     {channel, "world",     true,   3 },
     {channel, "country1",  false,  0  },
     {channel, "country2",  false,  0  },
     {channel, "country3",  false,  0  }
    ].