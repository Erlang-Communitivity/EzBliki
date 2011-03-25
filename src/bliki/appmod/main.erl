-module(bliki.appmod.main).
-include("yaws_api.hrl").
-include("routes.hrl").
-export([out/1,render_view/2,routes/0]).

-import(bliki.templates).
-import(bliki.routes).
-import(error_logger).
-import(io).
-import(lists).

out(YawsArg) ->
	Req = YawsArg#arg.req,
	{_PathType, PathAndQueryString} = Req#http_request.path,
	%NOTE Think this is bug in Yaws, but returned path incldues querystring
	%	so we need to strip it out or dispatch gets confused.
	Path = eat_path_wo_qs(PathAndQueryString),
	%TODO We should also be merging {vars, PropListFromQs} with Options
	Method = Req#http_request.method,
	Routes = routes(),
	Options = [{request, Req}, {yaws_arg, YawsArg}],
	Renderer = ?MODULE,
	case routes:dispatch(Path, Method, Routes, Renderer, Options) of
		{html, Val} -> {html, Val};
		{redirect, URL}  -> {redirect, URL};
		{content, ContentType, Content} -> {content, ContentType, Content};
		{passthru, YawsResponse} -> YawsResponse;
		{error, Reason} ->
				error_logger:info_msg("Dispatch error: ~s", [lists:flatten(Reason)]),
				{status, 404};
		Other ->
				error_logger:info_msg("Unknown dispatch result: ~p", [Other]),
				{status, 500}
	end.

eat_path_wo_qs(InputPath) -> 			eat_path_wo_qs([], InputPath).

eat_path_wo_qs(Acc, [$? | _Rest]) -> 	lists:reverse(Acc);
eat_path_wo_qs(Acc, [H | Rest]) -> 		eat_path_wo_qs([H | Acc], Rest);
eat_path_wo_qs(Acc, []) -> 				lists:reverse(Acc).


%TODO: Have mapping of view to template
render_view(ViewName, VarBindings) ->
	case templates:compile("resources/templates/" ++ ViewName ++ ".html") of
		{ok, Template} ->
			case templates:render(Template, VarBindings) of 
				{html, Val} -> {html, Val};
				{error, Reason} -> 
					error_logger:info_msg("~p:render_view - Error during render: ~p~n",[?MODULE, Reason]),
					{error, Reason}
			end;
		{error, Reason} -> 
			error_logger:info_msg("~p:render_view - Error during compile: ~p~n",[?MODULE, Reason]),
			{error, Reason}
	end.
	
resource_folder(RealPath) ->
	static_folder("/" ++ RealPath, RealPath).
	
static_folder(UrlPath, RealPath) ->
	#route{
		pattern = UrlPath ++ "/{file}",
		options = [{type, RealPath}],
		method = 'GET', 
		action = view_static, 
		no_render = true, 
		model_factory = bliki.model.static 
	}.
	
routes() ->
[
% Any route with an action of "redirect" causes all a redirect to the value of
% url param in options when the path matches. All params besides action, options
% and path are ignored.

% Options are processed to do variable subsitituion. Variables are in the form of
% {xxx}. The values may come from vars extracted from path using pattern
% or may be built in vars, incl:
%  		this_year
%		this_month
%		this_day

	resource_folder("html"),
	resource_folder("image"),
	resource_folder("js"),
	resource_folder("css"),
#route{
	method = 'GET',
	action = view_page,
	pattern = "/about/{entry_id}/",
	model_factory = bliki.model.wikipage,
	view_name = "showwikipage",
	options = [{collection_id, "main"}]
	},
#route{
	method = 'GET',
	action = edit_page,
	model_factory = bliki.model.wikipage,
	pattern = "/about/{entry_id}/edit/",
	view_name = "editwikipage",
	options = [{collection_id, "main"}]
	},
#route{
	method = 'POST',
	action = update_pagedata,
	model_factory = bliki.model.wikipage,
	pattern = "/about/{entry_id}/edit",
	view_name = "editwikipage",
	options = [{collection_id, "main"}]
	},
#route{
	method = 'GET',
	action = about_this,
	pattern = "/about/",
	model_factory = bliki.model.about,
	view_name = "about_index",
	options = []
	},
#route{
	method = 'GET',
	action = login,
	model_factory = bliki.model.openid,
	pattern = "/login/",
	view_name = "login"
},
#route{
	method = 'POST',
	action = authenticate,
	model_factory = bliki.model.openid,
	pattern = "/login/",
	no_render = true
},
#route{
	method = 'POST',
	action = logout,
	model_factory = bliki.model.openid,
	pattern = "/logout/",
	no_render = true
},
#route{
	method = 'GET',
	action = verify,
	model_factory = bliki.model.openid,
	pattern = "/verify/",
	no_render = true
},
#route{
	method = 'GET',
	action = redirect,
	pattern = "/",
	options = [{url, "/{this_year}/{this_month}/"}]
	},
#route{
	method = 'GET',
	action = view_month,
	pattern = "/{year}/{month}/",
	model_factory = bliki.model.feed,
	view_name = "feed",
	options = [{collection_id, "main"}]
	},		
#route{
	method = 'GET',
	action = view_year,
	pattern = "/{year}/",
	model_factory = bliki.model.feed,
	view_name = "feed",
	options = [{collection_id, "main"}]
	},		
#route{
	method = 'GET',
	action = view_day,
	pattern = "/{year}/{month}/{day}/",
	model_factory = bliki.model.feed,
	view_name = "feed",
	options = [{collection_id, "main"}]
	},
#route{
	method = 'GET',
	action = view_tag,
	pattern = "/tag/{tag}/",
	model_factory = bliki.model.feed,
	view_name = "tag_index",
	options = [{collection_id, "tag_{tag}"}]
	}
].

