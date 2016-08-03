-module(botbop_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, stop/0, dispatch/3, rooms/0, users/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(botbop_server, { rooms }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Port) ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [Port], []).

stop() ->
	gen_server:call(?MODULE,stop).

dispatch(Pid,Path,Message) ->
	io:format("dispatching ~p to ~p for ~p~n", [ Message, Path, Pid ]),
	gen_server:cast(?MODULE, { Path, Pid, Message }).

rooms() ->
	gen_server:call(?MODULE, rooms).

users(Path) ->
	gen_server:call(?MODULE, { users, Path }).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([Port]) ->
	websocket:server(?MODULE,dispatch,Port),
	{ ok, #botbop_server{ rooms = [] }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(rooms,_From,State = #botbop_server{ rooms = Rooms }) ->
	{ reply, [ Path || { Path,_Room } <- Rooms ], State };

handle_call({users,Path},_From,State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ reply, [], State };
		{ Path, Room } ->
			{ reply, botbop_room:users(Room), State }
	end;

handle_call(Message,_From,State) ->
	io:format("[botbop_server] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({ Path, User, connected}, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none -> 
			{ ok, Room } = botbop_room:start_link(Path),
			botbop_room:join(Room,User),
			{ noreply, State#botbop_server{ rooms = [ { Path, Room } | Rooms ] }};
		{ Path, Room } ->
			botbop_room:join(Room,User),
			{ noreply, State }
	end;

handle_cast({ Path, User, closed}, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ noreply, State };
		{ Path, Room } ->
			botbop_room:leave(Room,User),
			{ noreply, State }
	end;

handle_cast({ Path, User, Message }, State = #botbop_server{ rooms = Rooms }) ->
	case proplists:lookup(Path,Rooms) of
		none ->
			{ noreply, State };
		{ Path, Room } ->
			botbop_room:send(Room,User,Message),
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
