-module(botbop_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, dispatch/2, rooms/0, users/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(botbop_server, { rooms }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

dispatch(Pid,Message) ->
	Path = websocket:path(Pid),
	io:format("message to ~p is ~p~n", [ Path,Message ]),
	gen_server:cast(?MODULE, { Path, Pid, Message }).

rooms() ->
	gen_server:call(?MODULE, rooms).

users(Path) ->
	gen_server:call(?MODULE, { users, Path }).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, #botbop_server{ rooms = [] }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(rooms,_From,State = #botbop_server{ rooms = Rooms }) ->
	{ reply, Rooms, State };

handle_call({users,Path},_From,State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ reply, [], State };
		{ Path, Users } ->
			{ reply, Users, State }
	end;

handle_call(Message,_From,State) ->
	io:format("[botbop_server] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({ Path, Pid, connected}, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none -> 
			{ noreply, State#botbop_server{ rooms = [ { Path, [ Pid ]} | Rooms ] }};
		{ Path, Users } ->
			{ noreply, State#botbop_server{ rooms = [ { Path, [ Pid | Users ] } | proplists:delete(Path,Rooms)]}}
	end;

handle_cast({ Path, Pid, closed}, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ noreply, State };
		{ Path, Users } ->
			{ noreply, State#botbop_server{ rooms = [ { Path, lists:delete(Pid,Users) } | proplists:delete(Path,Rooms)]}}
	end;

handle_cast({ Path, Pid, Message }, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ noreply, State };
		{ Path, Users } ->
			lists:map(fun(U) -> websocket:send(U,Message) end,
				lists:filter(fun(U) -> U =/= Pid end, Users)),
			{ noreply, State }
	end;
	
handle_cast(Message,State) ->
	io:format("[botbop_server] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[botbop_server] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
