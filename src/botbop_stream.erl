-module(botbop_stream).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-export([ create/2 ]).
-record(botbop_stream, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

create(User,Stream) when is_list(User), is_list(Stream) ->
	Path = "/" ++ Stream ++ "/*",
	webpage_auth:grant(User,Path),
	webpage_router:add(Path,
		[{ webpage_auth, auth, []}, 
		{ webpage_websocket, [ botbop_server, dispatch, []]}]).
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%
