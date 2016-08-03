-module(botbop_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(WEBSOCKET_SERVER(P), list_to_atom("websocket_server_" ++ integer_to_list(P))).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ ok, Port } = application:get_env(port),
	{ ok, { #{ 
		strategy => one_for_one, 
		intensity => 5, 
		period => 10 
	},[#{
		id => { local, botbop_server },
		start => { botbop_server, start_link, [Port]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ botbop_server ] 
	}]}}.

