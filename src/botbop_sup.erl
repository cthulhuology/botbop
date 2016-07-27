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
	{ ok, Port } = application:get_env(botbop,port),
	{ ok, TLSPort } = application:get_env(botbop,tls_port),
	{ ok, TLSCert } = application:get_env(botbop,tls_cert),
	{ ok, TLSKey } = application:get_env(botbop,tls_key),
	{ ok, { 
		#{ 
			strategy => one_for_one, 
			intensity => 5, 
			period => 10 
		},[
	#{
		id => botbop_server,
		start => { botbop_server, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ botbop_server ] 
	},#{
		id => ?WEBSOCKET_SERVER(Port),
		start => { websocket_server, start_link, [botbop_server,dispatch,Port]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ websocket_server, websocket, websocket_rfc6455 ] 
	}
%%,#{
%%		id => ?WEBSOCKET_SERVER(TLSPort),
%%		start => { websocket_server, start_link, [botbop_server,dispatch,TLSPort,TLSCert,TLSKey]},
%%		restart => permanent,
%%		shutdown => brutal_kill,
%%		type => worker,
%%		modules => [ websocket_server, websocket, websocket_rfc6455 ] 

%%	}
	]}}.

