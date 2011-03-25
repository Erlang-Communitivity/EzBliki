-module(bliki.app.atom.atom).
-behavior(.gen_server).
-author("=Bill.Barnhill").
-debug_level(all).

%% API exports
-export([
	new_entry_collection/4,	
	get_collection/3,
	new_entry/5,	
	get_entries/3,
	get_entry/3,	
	get_entries_in/3,	
	new_entry_collection/3,	
	get_collection/2,
	new_entry/4,	
	get_entries/2,
	get_entry/2,	
	get_entries_in/2,	
	new_entry_collection/2,	
	get_collection/1,
	new_entry/3,	
	get_entries/1,
	get_entry/1,	
	get_entries_in/1	
	]).
	
%% gen_server exports
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

%% convenience exports
-export([start_link/0]).
	
-import(gen_server).
-import(ets).
-import(lists).
-import(io).
-import(error_logger).
-import(bliki.tools).

%%%%%%%%%% Convenience funcs
start_link() ->
	LocalName = atom,
    gen_server:start_link({local, LocalName}, bliki.app.atom.atom, [], []).

%%%%%%%%% funcs interfacing with backing store %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_atom_tables() ->
	ets:new(atom_collections, [public, set, named_table]),
	ets:new(atom_entries, [public, set, named_table]).

entry_by_id(EntryId) ->
	case ets:lookup(atom_entries, EntryId) of
		[{_, Entry}] ->	{ok, Entry};
		[] -> {error, not_found}
	end.

collection_by_id(CollectionId) ->
	%%error_logger:info_msg("collection_by_id(~p)~n",[CollectionId]),
	[{_, Collection}] = ets:lookup(atom_collections, CollectionId),
	{ok, Collection}.

replace_entry_refs_with_entries({entry_collection, CollectionProps}) ->
	ExpandedProps = lists:map(
						fun ({entry, {id, Id}}) -> 
								{ok, Entry} = entry_by_id(Id),
								Entry;
							({entry, EntryProps}) -> {entry, EntryProps};
							({Key, Value}) ->  {Key, Value}
						end,
						CollectionProps),
	{ok, {entry_collection, ExpandedProps}}.					
	
expanded_collection_by_id(CollectionId) ->
	{ok, Collection} = collection_by_id(CollectionId),
	replace_entry_refs_with_entries(Collection).

entries_from_collection(CollectionId) ->
	{ok, {entry_collection, Props}} = expanded_collection_by_id(CollectionId),
	Entries = lists:filter(
					fun ({entry, _}) -> true;
							(_X) -> false
					end, 
					Props
					),
	{ok, Entries}.

%% The following are the start of multi-prop entry spec support
%
%entries_matching([], EntryAcc) ->
%	{ok, EntryAcc};
%	
%entries_matching([{entry_collection, CollectionId} | Left], EntryAcc) ->
%	{ok, Entries} = entries_from_collection(CollectionId),
%	entries_matching(Left, lists:append(EntryAcc, Entries)).
%


create_entry_collection(Id, Properties) ->
	HasId = lists:any(
				fun({Key, _Value}) ->
					case Key of
						id -> true;
						_ -> false
					end
				end,
				Properties
			),
	case HasId of 
		true -> Inserted = Properties;
		false -> Inserted = [{id, Id} | Properties]
	end,
	ets:insert(atom_collections, { Id, {entry_collection, Inserted}}),
	{ok}.

create_entry(Id, Properties) ->
	HasId = lists:any(
				fun({Key, _Value}) ->
					case Key of
						id -> true;
						_ -> false
					end
				end,
				Properties
			),
	case HasId of 
		true -> Inserted = Properties;
		false -> Inserted = [{id, Id} | Properties]
	end,
	ets:insert(atom_entries, { Id, {entry, Inserted} }),
	{ok}.
	
add_entry_to(EntryId, CollectionId) ->
	{ok, {entry_collection, Props}}  = collection_by_id(CollectionId),
	UpdatedProperties = lists:append(Props, [{entry, {id, EntryId}}]),
	ets:insert(atom_collections, { CollectionId, {entry_collection, UpdatedProperties}}),
	{ok}.

%%%%%%%%%%% gen_server  behavior %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
	create_atom_tables(),
	{ok, []}.
		
%% Really should put handle_discovery stuff in separate behaviour discoverable_gen_server
handle_discovery(_From, State) ->
	%%error_logger:info_msg( "==== Received discovery request ===", []),
    {reply, {discovery, response, ok, []}, State}.

handle_call({discovery, request}, _From, _State) ->
	handle_discovery(_From, _State);
	
handle_call({new_entry_collection, {Id, Properties}}, _From, _State) ->
	create_entry_collection(Id, Properties),
	%%error_logger:info_msg( "==== Created new entry collection with id '~p' ===", [Id]),
	{reply, ok, [] };

handle_call({get_collection,{CollectionId}}, _From, _State) ->
	{ok, Collection} = expanded_collection_by_id(CollectionId),
	{reply, {ok, Collection}, [] };
		
handle_call({new_entry,{CollectionId, EntryId, Properties}}, _From, _State) ->
	create_entry(EntryId, Properties),
	%%error_logger:info_msg("==== Created new entry with id '~p' ===", [EntryId]),
	add_entry_to(EntryId, CollectionId),
	%%error_logger:info_msg( "==== Added entry '~p' to collection with id '~p' ===", [EntryId, CollectionId]),
	{reply, ok, [] };
		
%% multi-specs not yet supported
handle_call({get_entries,{EntrySpec}}, _From, _State) ->
	[{Key, Value}] = EntrySpec,
	case Key of
		id -> 	case entry_by_id(Value) of
					{ok, Entry} -> {reply, {ok, [Entry]}, [] };
					{error, Reason} -> {reply, {error, Reason}, []}
				end;
		collection ->	{reply, entries_from_collection(Value),  [] }
	end.
		
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_terminated, Reason}, State};

handle_info(_Msg, State) ->
	%%error_logger:info_msg("==== Received unknown msg ===~n~p", [Msg]),
    {noreply, State}.
	
terminate({port_terminated, _Reason}, _State) ->
    ok.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

     		
%%%%%%%%%%%%% Client API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_entry_collection(Node, Name, Id, Properties) ->
	gen_server:call({Name, Node}, {new_entry_collection, {Id, Properties}}, 5000).

get_collection(Node, Name, CollectionId) ->
	gen_server:call({Name, Node}, {get_collection, {CollectionId}}, 5000).
	
new_entry(Node, Name, CollectionId, EntryId, Properties) ->
	gen_server:call({Name, Node}, {new_entry, {CollectionId, EntryId, Properties}}, 5000).
	
get_entries(Node, Name, EntrySpec) ->
	gen_server:call({Name, Node}, {get_entries, {EntrySpec}}, 5000).

get_entry(Node, Name, EntryId) ->
	{ok, [Entry]}  = get_entries(Node, Name, [{id, EntryId}]),
	{ok, Entry}.

get_entries_in(Node, Name, CollectionId) ->
	get_entries(Node, Name, [{collection, CollectionId}]).

%% API for using default name of atom on specific node
new_entry_collection(Node, Id, Properties) ->
	gen_server:call({atom, Node}, {new_entry_collection, {Id, Properties}}, 5000).

get_collection(Node, CollectionId) ->
	gen_server:call({atom, Node}, {get_collection, {CollectionId}}, 5000).
	
new_entry(Node, CollectionId, EntryId, Properties) ->
	gen_server:call({atom, Node}, {new_entry, {CollectionId, EntryId, Properties}}, 5000).
	
get_entries(Node, EntrySpec) ->
	gen_server:call({atom, Node}, {get_entries, {EntrySpec}}, 5000).

get_entry(Node, EntryId) ->
	{ok, [Entry]}  = get_entries(Node, atom, [{id, EntryId}]),
	{ok, Entry}.

get_entries_in(Node, CollectionId) ->
	get_entries(Node, [{collection, CollectionId}]).

%% API for using current node and default name of atom
new_entry_collection(Id, Properties) ->
	gen_server:call({atom, node()}, {new_entry_collection, {Id, Properties}}, 5000).

get_collection(CollectionId) ->
	gen_server:call({atom, node()}, {get_collection, {CollectionId}}, 5000).
	
new_entry(CollectionId, EntryId, Properties) ->
	gen_server:call({atom, node()}, {new_entry, {CollectionId, EntryId, Properties}}, 5000).
	
get_entries(EntrySpec) ->
	gen_server:call({atom, node()}, {get_entries, {EntrySpec}}, 5000).

get_entry(EntryId) ->
	case get_entries(node(), atom, [{id, EntryId}]) of
		{ok, [Entry]}  -> {ok, Entry};
		{error, Reason} -> {error, Reason}
	end.

get_entries_in(CollectionId) ->
	get_entries(node(), atom, [{collection, CollectionId}]).
	