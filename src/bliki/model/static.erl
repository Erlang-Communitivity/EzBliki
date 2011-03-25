-module(bliki.model.static).

-export([view_static/1,defaultModel/1]).

-import(proplists).
-import(error_logger).
-import(lists).
-import(file).
-import(yaws_api).

-include("routes.hrl").


view_static({ParsedRoute, Options}) ->
	%%error_logger:info_msg("+~p:view_static, with ~n[~n~p,~n~p~n]~n", [?MODULE, "_ParsedRoute", Options]),
	FileName = proplists:get_value(file, Options),
	FileType = proplists:get_value(type, Options),
	FilePath = "resources/" ++ FileType ++ "/" ++ FileName, 
	case file:read_file(FilePath) of
		{error, enoent} -> 
			error_logger:info_msg("|~p:view_static: File not found ~p~n", [?MODULE, FilePath]),
			{ParsedRoute, {error, not_found}};
		{ok, Content} -> 
			Model = {content, yaws_api:mime_type(FileName), Content},
			Route = ParsedRoute#parsed_route.route,
			RouteFinal = Route#route{no_render = true},
			ParsedRouteFinal = ParsedRoute#parsed_route{route = RouteFinal},
			{ParsedRouteFinal, Model}
	end.
	
defaultModel({ParsedRoute, Options}) ->
	{ParsedRoute, Options}.