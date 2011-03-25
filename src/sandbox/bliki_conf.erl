-module(bliki_conf).
-export([routes/0]).

-author("=Bill.Barnhill").
-created("2008-Feb-25").
-copyright("Communitivity, Inc.").
-license("Mozilla Public License 1.1").

-include("routes.hrl").

%routeTemplate() ->
%	#route{
%		method = http_get,
%		view_name = "index",
%		renderer = xxx,
%		options = []
%	}.

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
	
	% For now we need to define everything on every route, ideally we want to
	% make use of RouteTemplate above
	#route{
		method = http_get,
		action = "redirect",
		pattern = "/index",
		options = [{url, "/{this_year}/{this_month}/{this_day}/index"}]
		},
	#route{
		method = http_get,
		action = "view_day",
		pattern = "/{year}/{month}/{day}/index",
		model_factory = feed_model,
		view_name = "feed_index",
		options = [{collection_id, "main"}]
		},		
	#route{
		method = http_get,
		action = "view_entry",
		pattern = "/{year}/{month}/{day}/{entry_id}/index",
		model_factory = entry_model,
		view_name = "entry_index",
		options = [{collection_id, "main"}]
		},		
	#route{
		method = http_get,
		action = "view_tag",
		pattern = "/tag/{tag}/index",
		model_factory = feed_model,
		view_name = "tag_index",
		options = [{collection_id, "tag_{tag}"}]
		},
	#route{
		method = http_get,
		action = "about_this",
		pattern = "/about/index",
		view_name = "about_index",
		options = []
		},
	#route{
		method = http_get,
		action = "add_entry_form",
		pattern = "/admin/entries/add",
		view_name = "add_entry_form",
		options = []
		},
	#route{
		method = http_post,
		action = "submit_entry_form",
		pattern = "/admin/entries/add",
		view_name = "added_entry_form",
		options = []
		},
	#route{
		method = http_get,
		action = "edit_entry_form",
		pattern = "/admin/entries/add",
		view_name = "edit_entry_form",
		options = []
		},
	#route{
		method = http_post,
		action = "submit_edit_entry_form",
		pattern = "/admin/entries/{entry_id}/edit",
		view_name = "edited_entry_form",
		options = []
		},
	#route{
		method = http_get,
		action = "manage_entries",
		pattern = "/admin/entries/index",
		view_name = "manage_entries",
		options = []
		}
	].
