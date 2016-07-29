-module(botbop_auth).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, install/2, auth/2, stream/1, user/4, provision/2, paid/2, ban/1, reactivate/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(botbop_auth, {}).
-record(botbop_users, { id, name, email, address, created, paid, active, tokens=[], streams=[] }).
-record(botbop_streams, { id, name, active, tokens=[] }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [code:priv_dir(botbop)], []).

stop() ->
	gen_server:call(?MODULE,stop).

install(Database,Nodes) ->
	application:set_env(mnesia,dir,Database),
	case mnesia:delete_schema(Nodes) of
		ok ->
			io:format("removed old db in ~p~n", [ Database ]);
		{ error, Reason } -> 
			io:format("could not remove old db in ~p because ~p~n", [ Database, Reason ])
	end,
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes,application,start,[ mnesia ]),	% start all the nodes at once
	{ atomic, ok } = mnesia:create_table(botbop_users, [
		{ attributes, record_info(fields,botbop_users) },
		{ disc_copies, Nodes }]),
	{ atomic, ok } = mnesia:create_table(botbop_streams, [
		{ attributes, record_info(fields,botbop_streams) },
		{ disc_copies, Nodes }]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]),	% stop all of the nodes to force a flush	
	ok.

%% tests for authentication
auth(Token,Stream) ->
	gen_server:call(?MODULE, { auth, Token, Stream }).

%% creates a new stream ID
stream(Stream) ->
	gen_server:call(?MODULE, { stream, Stream }).

%% creates a user entry
user(Name,Email,Address,Paid) ->
	gen_server:call(?MODULE, { user, Name, Email, Address, Paid }).

%% provisions a new token for a User & Stream
provision(User,Stream) ->
	gen_server:call(?MODULE, { provision, User, Stream }).

%% updates the paid timestamp for the user
paid(User,Time) ->
	gen_server:call(?MODULE, { paid, User, Time }).

%% deactivates a user
ban(User) ->
	gen_server:call(?MODULE, { ban, User }).

%% reactivate a user
reactivate(User) ->
	gen_server:call(?MODULE, { reactivate, User }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([Database]) ->
	io:format("Loading database in ~p~n", [ Database ]),
	application:set_env(mnesia,dir,Database),
	application:start(mnesia),
	mnesia:wait_for_tables([ botbop_users, botbop_streams ], 5000),	% wait 5 seconds for mnesia to startup
	{ ok, #botbop_auth{}}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call({ auth, Token, Stream }, _From, State) ->
	F = fun() ->
		case mnesia:read(botbop_streams, Stream) of
			[ #botbop_streams{ tokens = Tokens, active = true } ] -> 
				lists:member(Token,Tokens);
			_ ->
				false
		end
	end,
	Authed = mnesia:activity(transaction,F),
	{ reply, Authed, State };

handle_call({ stream, Stream }, _From, State ) ->
	Id = uuid:new(),
	F = fun() ->
		ok = mnesia:write(#botbop_streams{ id = Id, name = Stream, active = true, tokens = [] })
	end,
	ok = mnesia:activity(transaction,F),
	{ reply, Id, State };	

handle_call({ user, Name, Email, Address, Paid }, _From, State) ->
	Id = uuid:new(),
	F = fun() ->
		ok = mnesia:write(#botbop_users{ id = Id, name = Name, email = Email, address = Address, paid = Paid, 
			created = erlang:system_time(), active = true, tokens = [], streams = [] })
	end,
	ok = mnesia:activity(transaction,F),
	{ reply, Id, State };
			
handle_call({ provision, User, Stream }, _From, State) ->
	Token = uuid:new(),
	F = fun() ->
		case mnesia:read(botbop_users, User) of 
			[ Bob = #botbop_users{ tokens = Tokens, streams = Streams, active = true } ] ->
				ok = mnesia:write( Bob#botbop_users{ tokens = [ Token | Tokens], streams = [ Stream | lists:delete(Stream,Streams) ]});
			[] -> 
				mnesia:abort(no_such_user)
		end,
		case mnesia:read(botbop_streams, Stream) of
			[ Flow = #botbop_streams{ tokens = Tokes } ] ->
				mnesia :write( Flow#botbop_streams{ tokens = [ Token | Tokes ] });
			[] -> 
				mnesia:abort(no_such_stream)
		end
	end,
	mnesia:activity(transaction,F),
	{ reply, Token, State };

handle_call({ paid, User, Time }, _From, State) ->
	F = fun() ->
		case mnesia:read(botbop_users, User) of
			[ Bob = #botbop_users{} ] ->
				ok = mnesia:write( Bob#botbop_users{ paid =  Time, active = true } );	
			[] ->
				mnesia:abort(no_such_user)
		end
	end,
	{ reply, mnesia:activity(transaction,F), State };

handle_call({ ban, User }, _From, State) ->
	F = fun() ->
		case mnesia:read(botbop_users, User) of
			[ Bob = #botbop_users{ streams = Streams } ] ->
				ok = mnesia:write(Bob#botbop_users{ active = false }),
				[ 
					case mnesia:read(botbop_streams,Stream) of
						[ S = #botbop_streams{}] -> 
							mnesia:write(S#botbop_streams{ active = false }); 
						[] ->
							ok
					end
				|| Stream <- Streams ];
			[] ->
				mnesia:abort(no_such_user)
		end
	end,
	{ reply, mnesia:activity(transaction,F), State };

handle_call({ reactivate, User }, _From, State) ->
	F = fun() ->
		case mnesia:read(botbop_users, User) of
			[ Bob = #botbop_users{ streams = Streams } ] ->
				ok = mnesia:write(Bob#botbop_users{ active = true }),	
				[
					case mnesia:read(botbop_streams,Stream) of
						[ S = #botbop_streams{}] ->
							mnesia:write(S#botbop_streams{ active = true });
						[] ->
							ok
					end
				|| Stream <- Streams ];
			[] ->
				mnesia:abort(no_such_user)
		end
	end,
	{ reply, mnesia:activity(transaction,F), State };

			
handle_call(Message,_From,State) ->
	io:format("[botbop_auth] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(Message,State) ->
	io:format("[botbop_auth] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[botbop_auth] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
