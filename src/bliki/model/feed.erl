
-module(bliki.model.feed).

-export([view_day/1,view_month/1,view_year/1,defaultModel/1]).

-import(bliki.app.atom.atom).
-import(proplists).
-import(error_logger).
-import(lists).

%% view_entries takes care of all filtering based on options,
%% but we might still want specific-day, -month, or -year model items, so
%% kept time unit funcs.

view_day({ParsedRoute, Options}) ->
	%%error_logger:info_msg("+~p:view_day(_, ~p)~n", [?MODULE, Options]),
	{ParsedRouteFinal, BaseModel} = view_entries({ParsedRoute, Options}),
	Model = BaseModel,
	%%error_logger:info_msg("-~p:view_day -> ~p~n", [?MODULE, Model]),
	{ParsedRouteFinal, lists:merge(Model, Options)}.
	
view_month({ParsedRoute, Options}) ->
	%%error_logger:info_msg("+~p:view_month(_, ~p)~n", [?MODULE, Options]),
	{ParsedRouteFinal, BaseModel} = view_entries({ParsedRoute, Options}),
	Model = BaseModel,
	%%error_logger:info_msg("-~p:view_month -> ~p~n", [?MODULE, Model]),
	{ParsedRouteFinal, lists:merge(Model, Options)}.

view_year({ParsedRoute, Options}) ->
	%%error_logger:info_msg("+~p:view_month(_, ~p)~n", [?MODULE, Options]),
	{ParsedRouteFinal, BaseModel} = view_entries({ParsedRoute, Options}),
	Model = BaseModel,
	%%error_logger:info_msg("-~p:view_month -> ~p~n", [?MODULE, Model]),
	{ParsedRouteFinal, lists:merge(Model, Options)}.

view_entries({ParsedRoute, Options}) ->
	CollectionId = list_to_atom(proplists:get_value(collection_id, Options)),
	{ok, {entry_collection, FeedPropList}} = atom:get_collection(CollectionId),
	UnfilteredEntries = proplists:get_all_values(entry, FeedPropList),
	Entries = filter_entries(time, Options, UnfilteredEntries),
	BaseModel = [
		{feed, FeedPropList},
		{entries, Entries }
		],
	{ParsedRoute, lists:merge(BaseModel, Options)}.
	
defaultModel({ParsedRoute, Options}) ->
	{ParsedRoute, Options}.
		
filter_entries(time, Options, Entries) ->
	lists:filter(fun(Entry) -> filter_by_time(Options, Entry) end, Entries).

%% Really should be doing filtering at data side, bu for now
%% {day,"03"},
%% {month,"07"},
%% {year,"2008"}])
%% TimeProps is a proplist restricted to one of the following combinations
%%	[{year,"YYYY"},{month, "MM"}, {day, "DD"}]
%%	[{year,"YYYY"},{month, "MM"}]
%%	[{year,"YYYY"}]
%% Other combinations such as ranges may be supported at a later time.

%% entry.published "2008-01-14T14:12:10-06:00"
%% Returns true or false, true if it passes filter
filter_by_time(TimeProps, EntryProps) ->
	EntryTime = proplists:get_value(published, EntryProps),
	<<	EntryYearBin:4/binary, "-", 
		EntryMonthBin:2/binary, "-", 
		EntryDayBin:2/binary, _Rest/binary>> = list_to_binary(EntryTime),
	EntryYear = binary_to_list(EntryYearBin),
	EntryMonth = binary_to_list(EntryMonthBin),
	EntryDay = binary_to_list(EntryDayBin),	
	Year = proplists:get_value(year, TimeProps),
	Month = proplists:get_value(month, TimeProps),
	Day = proplists:get_value(day, TimeProps),
	HasDay = not (Day == undefined),
	HasMonth = not (Month == undefined),
	HasYear = not (Year == undefined),
	if
		HasDay ->
		 	%%error_logger:info_msg("***HasDay~n",[]),
			Result = (Year == EntryYear) and (Month == EntryMonth) and (Day == EntryDay);
		HasMonth -> 
			%%error_logger:info_msg("***HasMonth~n",[]),
	 		Result = (Year == EntryYear) and (Month == EntryMonth);
		HasYear -> 
				%%error_logger:info_msg("***HasYear~n",[]),
				Result = (Year == EntryYear);
		true -> 
				%%error_logger:info_msg("***Has no date props~n",[]),
				Result = true
	end,
	%%error_logger:info_msg("*** ~p-~p-~p == ~p-~p-~p -> ~p~n", [EntryYear, EntryMonth, EntryDay, Year, Month, Day, Result]),
	Result.
	
