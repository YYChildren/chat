%% @author yangchaojun
%% @doc @todo Add description to monitor.


-module(monitor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([monitor/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


monitor( Node ) ->
	rpc:call(Node, chat2, client_count, []).
	