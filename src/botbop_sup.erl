-module(botbop_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ ok, { #{ 
		strategy => one_for_one, 
		intensity => 5, 
		period => 10 
	},[#{
		id => { local, botbop_server },
		start => { botbop_server, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ botbop_server ] 
	},#{
		id => { local, botbop_auth },
		start => { botbop_auth, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ botbop_auth ]
	}]}}.

