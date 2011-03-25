-module(bliki.routes).
-export([dispatch/5, route_for/3, test/0,test/1]).

-author("=Bill.Barnhill").
-created("2008-Feb-25").
-copyright("Communitivity,Inc.").
-license("Mozilla Public License 1.1").

-include("routes.hrl").
-include("yaws_api.hrl").


-import(string).
-import(lists).
-import(error_logger).
-import(io).
-import(filename).
-import(proplists).
-import(bliki.model.openid).

test() ->
	test("/2008/03/14/index").
	
test(Path) ->
	Routes = bliki.appmod.main:routes(),
	{ok, Parsed} = routes:route_for(Path, http_get, Routes),
	extract_bindings(Parsed).

model_for_route(ParsedRoute, PresetBindings) ->
	Route = ParsedRoute#parsed_route.route,
	case Route#route.model_factory of
		undefined -> 
			error_logger:info_msg("*** 12 +++~n~p~n*** 12---~n", [Route]),
			[];
		ModelFactory ->
			error_logger:info_msg("*** 13 ***~n", []),
			case Route#route.action of
				undefined ->
					error_logger:info_msg("*** 14 ***~n", []),
				 	case has_func(ModelFactory, defaultModel, 2) of
						false -> 
							error_logger:info_msg("*** 15 ***~n", []),
							[];
						true -> 
							error_logger:info_msg("*** 16 ***~n", []),
							ModelFactory:defaultModel(ParsedRoute, PresetBindings)
					end;
				ModelFunc ->
					error_logger:info_msg("*** 17 ***~n", []),
			 		case has_func(ModelFactory, ModelFunc, 1) of
						true -> 
							error_logger:info_msg("*** 18 ***~n", []),
							ModelFactory:ModelFunc({ParsedRoute, PresetBindings});
						false -> 
							error_logger:error_msg("routes:model_for_route: Model func ~p:~p not found~n",
						  		[ModelFactory, ModelFunc]),
							[]
					end
			end
	end.
			
		
dispatch(Path, Method, Routes,Renderer, Options) ->
	error_logger:info_msg(
		"**~nroutes:dispatch(~p, ~p, ~p, ~p, ~p)~n**~n", 
		[Path, Method, Routes,Renderer, Options]),
	case route_for(Path, Method, Routes) of
		{ok, ParsedRoute} ->
			error_logger:info_msg("*** 1 ***~n", []),
			Arg = proplists:get_value(yaws_arg, Options),
			error_logger:info_msg("*** 2 ***~n", []),
			Headers = Arg#arg.headers,
			error_logger:info_msg("*** 3 ***~n", []),
			ThisUrl = "http://" ++ Headers#headers.host ++ Path,
			error_logger:info_msg("*** 4 ***~n", []),
			Identity = openid:get_identity(ParsedRoute, Options),
			error_logger:info_msg("*** 5 ***~n", []),
			PresetBindings = lists:merge(Options, 
								lists:merge(
									extract_bindings(ParsedRoute),
									Identity ++ [{this_url, ThisUrl}]
									)
								), 
			error_logger:info_msg("*** 6 ***~n", []),
			{ParsedRouteFinal, Model} = model_for_route(ParsedRoute, PresetBindings),
			error_logger:info_msg("*** 7 ***~n", []),
			Route = ParsedRouteFinal#parsed_route.route,
			error_logger:info_msg("*** 8 ***~n", []),
			if 
				Route#route.no_render -> 
					error_logger:info_msg("*** 9 ***~n", []),
					Model;
				true ->
					error_logger:info_msg("*** 10 ***~n", []),
					ViewName = Route#route.view_name,
					Renderer:render_view(ViewName, Model)
			end;
		{error, not_found} -> 
			error_logger:info_msg("*** 11 ***~n", []),
			{error, not_found}
	end.	
	
has_func(Module, Func, _Arity) ->
	%% For now we ignore arity, as all our model gen funcs are arity 2
	Exports = proplists:get_value(exports, Module:module_info()),
	proplists:is_defined(Func, Exports).
	
is_var_part(RoutePart) ->
	%%error_logger:info_msg("Arg:~p~n",[RoutePart]),
	element(1, RoutePart) == route_part_var.

var_part_to_binding(#route_part_var{name=Name, value=Value}) ->
	{Name, Value}.

% Get a template engine compatible array of 
% variable bindings from parsed route
% Currently this just grabs from var route parts,
% nothing is taken from the route portion of the 
% parsed route, such as path, etc.
extract_bindings(ParsedRoute) ->
	VarParts = lists:filter(fun is_var_part/1, ParsedRoute#parsed_route.parts),
	FromVars = lists:map(fun var_part_to_binding/1, VarParts),
	Route = ParsedRoute#parsed_route.route,
	case Route#route.options of
		undefined -> FromVars;
		RouteOptions -> lists:merge(FromVars, RouteOptions)
	end.

route_for(Path, Method, Routes)  ->
	% split path into Segments by tokenizing on /
	Segments = string:tokens(Path, "/"),
	case match_route(Method, Segments, Routes) of 
		{match, ParsedRoute} -> {ok, ParsedRoute};
		{nomatch} -> {error, not_found}
	end.
		
%TODO: Flattening the RouteParts is something of a hack, find prob and fix
match_route(Method, Segments, [Route | RoutesLeft]) ->
	RouteMethod = Route#route.method,
	%%error_logger:info_msg("~p:match_route(~p, _, ~p)~n", [?MODULE, Method, Route]),
	if
		RouteMethod == Method ->
			case match_parts(Segments, parse_route(Route), []) of
				{match, RouteParts} -> 
					{match, #parsed_route{route=Route,parts=lists:flatten(RouteParts)}}; 
				{nomatch} -> match_route(Method, Segments, RoutesLeft)
			end;
		true -> match_route(Method, Segments, RoutesLeft)
	end;
	
match_route(_Method, _Segments, []) ->
	{nomatch}.
	
match_parts([], [_Part | _RoutePartsLeft], _Result) ->
	{nomatch};

match_parts([_Segment | _SegmentsLeft], [], _Result) ->
	{nomatch};

match_parts([], [],  [{nomatch} | _Result]) ->
	{nomatch};

% If we find a segment that doesn't match then the whole
% thing doesn't match.
%
match_parts([_Segment | _SegmentsLeft], [_Part | _RoutePartsLeft], [{nomatch} | _Result]) ->
	{nomatch};
	
match_parts([Segment | SegmentsLeft], [Part | RoutePartsLeft], Result) ->
	match_parts(
		SegmentsLeft, 
		RoutePartsLeft, 
		[match_part(Segment, Part) | Result]
	);

match_parts([], [], Result) ->
	{match, Result}.

	
% returns either empty list or list containing matched part
match_part(Segment, #route_part_static{text = Text}) ->
	if
		Segment == Text -> [#route_part_static{text = Text}];
		true -> {nomatch}
	end;
	
match_part(Segment, #route_part_var{name = Name, regex = Regex}) ->
	% for now we ignore regex
	[#route_part_var{name = Name, regex = Regex, value = Segment}].

parse_var_name(PathPart) ->
	erlang:list_to_atom(string:substr(PathPart, 2, string:len(PathPart) - 2)).

parse_route(Route) ->
	PathParts = string:tokens(Route#route.pattern, "/"),
	RouteParts = lists:map( 
		fun (PathPart) ->
			VarStart = lists:prefix("{", PathPart),
			VarEnd = lists:suffix("}", PathPart),
			IsVar = VarStart and VarEnd,
			if 
				IsVar -> #route_part_var{name=parse_var_name(PathPart)};
				true -> #route_part_static{text=PathPart}
			end
		end,
		PathParts
	),
	RouteParts.


