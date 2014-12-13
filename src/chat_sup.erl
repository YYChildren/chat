%% @author yangchaojun
%% @doc @todo Add description to chat_supersior.


-module(chat_sup).
-behaviour(supervisor).
-export([init/1]).
-define(PORT,8080).
-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).  
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_in_shell_for_testing/0,start_link/1,start/0]).
start() ->
	    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).
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
    ChatServer ={chat_server3, 
	    {chat_server3, start, [ chat_server3 ]},
	    permanent, 
	    10000, 
	    worker, 
	    [chat_server3]},
	AcceptClient = {accep_client, 
	    {accept_client, accept, [ accep_client,?PORT,?TCP_OPTIONS ]},
	    permanent, 
	    10000, 
	    worker, 
	    []},
	SendServer = {chat_send_server, 
	    {chat_send_server, start, [ chat_send_server ]},
	    permanent, 
	    10000, 
	    worker, 
	    []},
	ReceiveClientSupervisor ={receive_client_sup, 
	    {receive_clinet_sup, start_link, []},
	    permanent, 
	    10000, 
	    supervisor, 
	    [receive_clinet_sup]},
	SendClientSupervisor ={send_client_sup, 
	    {receive_clinet_sup, start_link, []},
	    permanent, 
	    10000, 
	    supervisor, 
	    [send_clinet_sup]},
	ChatTabSupervisor ={chat_tab_sup, 
	    {chat_tab_server, start_link, []},
	    permanent, 
	    10000, 
	    supervisor, 
	    [chat_tab_sup]},
	{ok, {{one_for_one, 3, 10},
		  [ChatServer,
		   AcceptClient,
		   SendServer,
		   ReceiveClientSupervisor,
		   SendClientSupervisor,
		   SendClientSupervisor,
		   ChatTabSupervisor
		  ]}}.
%% ====================================================================
%% Internal functions
%% ====================================================================


