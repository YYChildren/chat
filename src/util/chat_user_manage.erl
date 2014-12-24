-module(chat_user_manage).
-compile(export_all).

res_or_login(UserName,PassWord) ->
	Users = chat_db:demo(select_user,UserName),
	case Users of
		[] ->
			chat_db:add_user(UserName,PassWord),
			io:format("~p NewUser ~p~n",[?MODULE,UserName]),
			%%Register a new user
			register;
		[{user,UserName,PassWord1}] ->
			case PassWord1 =:= PassWord of
				true -> 
					io:format("~p OldUser: ~p~n",[?MODULE,UserName]),
					ok;
				false -> 
					io:format("~p OldUser with wrong password~n",[?MODULE]),
					
					exist
			end
	end.

