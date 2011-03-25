
-module(bliki.model.wikipage).

-export([edit_page/1,update_pagedata/1, view_page/1, defaultModel/1]).

-import(bliki.app.atom.atom).
-import(proplists).
-import(error_logger).
-import(lists).
-import(yaws_api).
-import(timefuncs).
-import(bliki.tools).

-include("routes.hrl").

view_page({ParsedRoute, Options}) ->
	error_logger:error_msg("wikipage:view_page: Options=~p~n", [Options]),
	CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	EntryIdOption = proplists:get_value(entry_id, Options),
	
	if 
		is_list(EntryIdOption) -> EntryId = EntryIdOption;
		is_atom(EntryIdOption) -> EntryId = atom_to_list(EntryIdOption)
	end,
	error_logger:info_msg("Got to before get_entry call",[]),
	case atom:get_entry(EntryId) of
		{ok, Entry} -> 
			error_logger:info_msg("Got to before edit_existing_page call",[]),
			Model = edit_existing_page(CollectionId, Entry),
			{ParsedRoute, lists:merge(Model, Options)};
		{error, not_found} -> 
			error_logger:info_msg("Got to before redirect return",[]),
			Route = ParsedRoute#parsed_route.route,
			RouteFinal = Route#route{no_render = true},
			ParsedRouteFinal = ParsedRoute#parsed_route{route = RouteFinal},
			{ParsedRouteFinal, {redirect, "/about/"++EntryId++"/edit/"}}
	end.


edit_page({ParsedRoute, Options}) ->
	CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	EntryId = proplists:get_value(entry_id, Options),
	case atom:get_entry(EntryId) of
		{ok, Entry} -> 
			Model = edit_existing_page(CollectionId, Entry),
			{ParsedRoute, lists:merge(Model, Options)};
		{error, not_found} -> 
			Model = edit_new_page(CollectionId, EntryId),
			{ParsedRoute, lists:merge(Model, Options)}
	end.

edit_existing_page(_CollectionId, {entry, EntryProps} ) ->
	%%error_logger:info_msg("**** START OF ENTRY~n~p~n****** END OF ENTRY~n", [EntryProps]),
	MassagedEntryProps =  bliki.tools:massage_props(EntryProps),
	error_logger:info_msg("**** START OF MASSAGED~n~p~n****** END OF MASSAGED~n", [EntryProps]),
	Result = [{entry, MassagedEntryProps}],
	%%error_logger:info_msg("~p:edit_existing_page -> ~p~n", [?MODULE, Result]),
	Result.
	
edit_new_page(_CollectionId, EntryId) ->
	[tools:new_entry(EntryId)].

update_pagedata({ParsedRoute, Options}) ->
	%TODO CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	EntryId = proplists:get_value(entry_id, Options),
	%TODO Need a function for tesing page existence, so we dont fetch whole entry below
	case atom:get_entry(EntryId) of
		{ok, _Entry} -> 
			update_existing_pagedata({ParsedRoute, Options});
		{error, not_found} -> 
			update_new_pagedata({ParsedRoute, Options})
	end.

update_new_pagedata({ParsedRoute, Options}) ->
	CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	EntryId = proplists:get_value(entry_id, Options),
	%% Start update
	Vars = yaws_api:parse_post(proplists:get_value(yaws_arg, Options)),
	Title = proplists:get_value("title", Vars),
	Summary = proplists:get_value("summary", Vars),
	Content = proplists:get_value("Text", Vars),
	error_logger:info_msg("Parsed post vars: ~p~n", [ Vars]),
	UpdatedProps = [
		{title, Title}, 
		{summary, Summary},
		{content, [{text, [{wikicreole, [{en_us, Content}]}]}]},  
		{published, timefuncs:timestamp() },
		{link, 
		[
			{rel,"alternate"},
			{type, "text/html"},
			{href, "/about/"++EntryId}
		]}
		],
	atom:new_entry('gog@Signal-2', main, EntryId, UpdatedProps),
	%% End of update
	case atom:get_entry(EntryId) of
		{ok, Entry} -> 
			error_logger:info_msg("~p:update_page: Updated entry:~n~p~n", [?MODULE, Entry]),
			Model = edit_existing_page(CollectionId, Entry),
			{ParsedRoute, lists:merge(Model, Options)};
		_Other -> {ParsedRoute, {error, not_found}}
	end.


update_existing_pagedata({ParsedRoute, Options}) ->
	CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	EntryId = proplists:get_value(entry_id, Options),
	%% Start update
	Vars = yaws_api:parse_post(proplists:get_value(yaws_arg, Options)),
	Title = proplists:get_value("title", Vars),
	Summary = proplists:get_value("summary", Vars),
	Content = proplists:get_value("Text", Vars),
	error_logger:info_msg("Parsed post vars: ~p~n", [ Vars]),
	{ok, {entry, OldProps}} = atom:get_entry(EntryId),
	OldPropsWithKeysGone = 	proplists:delete(content, 
								proplists:delete(summary, 
										proplists:delete(title, OldProps))),
	UpdatedProps = [
		{title, Title}, 
		{summary, Summary},
		{content, [{text, [{wikicreole, [{en_us, Content}]}]}]}   ] ++ OldPropsWithKeysGone,
	atom:new_entry('gog@Signal-2', main, EntryId, UpdatedProps),
	%% End of update
	case atom:get_entry(EntryId) of
		{ok, Entry} -> 
			error_logger:info_msg("~p:update_page: Updated entry:~n~p~n", [?MODULE, Entry]),
			Model = edit_existing_page(CollectionId, Entry),
			{ParsedRoute, lists:merge(Model, Options)};
		_Other -> {ParsedRoute, {error, not_found}}
	end.

defaultModel({ParsedRoute, Options}) ->
	{ParsedRoute, Options}.

