%% @author yangchaojun
%% @doc @todo Add description to do_client_sup.


-module(chat_tab_sup).
-behaviour(supervisor).
-export([init/1]).
-define(CHAT_TAB_SERVER,chat_tab_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Client = {
			  	?CHAT_TAB_SERVER, {?CHAT_TAB_SERVER, start_link, []}, 
           		transient, brutal_kill, worker, [?CHAT_TAB_SERVER]
			 },
    {ok,{{simple_one_for_one,0,1}, [Client]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


