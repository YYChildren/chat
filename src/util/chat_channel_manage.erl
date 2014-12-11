-module(chat_channel_manage).
-compile(export_all).

load_channel() ->
	chat_db:demo(select_channel).
load_channel(Zone) ->
	chat_db:demo(select_channel,Zone).
