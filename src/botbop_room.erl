-module(botbop_room).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ install/1, start_link/1, stop/0, users/1, path/1, join/2, leave/2, send/3, send/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(botbop_room, {
	path,
	users = [],
	stats = []
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

install(Nodes) ->
	Database = code:priv_dir(botbop),
	rpc:multicall(Nodes,application,set_env, [ mnesia,dir,Database]),
	rpc:multicall(Nodes,application,start, [ mnesia ]),
	mnesia:create_table(botbop_room, [
		{ attributes, record_info(fields,botbop_room) },
		{ disc_copies, Nodes }]),
	rpc:multicall(Nodes,application,stop, [ mnesia ]),
	ok.

start_link(Path) ->
	gen_server:start_link({ local, list_to_atom(?MODULE_STRING ++ "_" ++ Path) }, ?MODULE, [Path], []).

stop() ->
	gen_server:call(?MODULE,stop).

users(Room) ->
	gen_server:call(Room,users).

path(Room) ->
	gen_server:call(Room,path).

join(Room,User) ->
	gen_server:cast(Room, { join, User }).

leave(Room,User) ->
	gen_server:cast(Room, { leave, User }).

send(Room,User,Message) ->
	gen_server:cast(Room, { send, User, Message }).

send(Room,Message) ->
	gen_server:cast(Room, { send, Message }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([Path]) ->
	F = fun() ->
		mnesia:delete(botbop_room,Path,write),
		mnesia:write(#botbop_room{ path = Path, users = [], stats = []})
	end,
	mnesia:activity(transaction,F),
	{ ok, #botbop_room{ path = Path, users = []}}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(users,_From,State = #botbop_room{ users = Users }) ->
	{ reply, Users, State };

handle_call(path,_From,State = #botbop_room{ path = Path }) ->
	{ reply, Path, State };

handle_call(Message,_From,State) ->
	io:format("[botbop_room] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({join,User},State = #botbop_room{ users = Users }) ->
	{ noreply, State#botbop_room{ users = [ User | lists:delete(User,Users) ]}};

handle_cast({leave,User},State = #botbop_room{ users = Users }) ->
	{ noreply, State#botbop_room{ users = lists:delete(User,Users) }}; 

handle_cast({send,User,Message},State  = #botbop_room{ users = Users }) ->
	lists:map(fun(U) -> webpage_websocket:send(U,Message) end,
		lists:filter(fun(U) -> U =/= User end, Users)),
	{ noreply, State };

handle_cast({send,Message},State  = #botbop_room{ users = Users }) ->
	lists:map(fun(U) -> webpage_websocket:send(U,Message) end, Users),
	{ noreply, State };

handle_cast(Message,State) ->
	io:format("[botbop_room] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[botbop_room] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
