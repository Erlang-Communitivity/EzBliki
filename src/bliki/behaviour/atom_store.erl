-module(bliki.behaviour.atom_store).

-export([behaviour_info/1]).

-author("=Bill.Barnhill").
-created("2008-Feb-25").
-copyright("Communitivity,Inc.").
-license("Mozilla Public License 1.1").

behaviour_info(callbacks) ->
	[
	{init,1}, 

	{entry_by_id,1},
		%% takes entry id
		%% returns entry tuple
	
	{entry_properties,2},
		%% takes entry id and list of property id's 
		%% returns proplist with entries matching prop ids, if their in entry props
		
	{collection_by_id,1},
		%% takes collection id
		%% returns collection tuple
		
	{update_collection,3},
		%% takes collection_id, proplist to remove, proplist to add
		%% returns {ok} or {error, Reason}
		
	{update_entry,3} 
		%% takes entry_id, proplist to remove, proplist to add 
		%% returns {ok} or {error, Reason}
	];
	
behaviour_info(_Other) ->
	    undefined.