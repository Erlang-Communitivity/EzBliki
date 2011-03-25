
-module(bliki.tools).
-export([populate_server/1, massage_props/1, start/0, info_msg/3, new_entry/1]).

-import(application).
-import(lists).
-import(bliki.app.atom.atom).
-import(proplists).
-import(error_logger).
-import(beam_lib).
-import(code).
-import(erlang).
-import(string).
-import(timefuncs).

-debug_level(error).

start() ->
	application:start(atom_app),
	populate_server('gog@Signal-2'),
	{ok, {entry, EntryProps}} = atom:get_entry("QCon_Interview_Online"),
	massage_props(EntryProps).
	
info_msg(Module, Format, Args) ->
	Level = get_attribute(Module, debug_level), %% Must be one of none, error, info, trace, all, 
	 											%% undefined defaults to none
	LevelMap = [{none,0}, {error,1}, {info,2}, {trace,3}, {all, 4}],
	CutoffLevel = proplists:get_value(info ,LevelMap) - 1,
	ModLevel = proplists:get_value(Level, LevelMap),
	error_logger:info_msg("~p , ~p ~n",[ModLevel, CutoffLevel]), 
	if
		ModLevel > CutoffLevel -> error_logger:info_msg("~p: " ++ Format ++ "~n", [Module | Args]);
		true -> false
	end.

get_attribute(Module, Attr) ->
	{ok,{Mod,[{attributes, Attributes}]}} = beam_lib:chunks(code:which(Module), [attributes]),
	info_msg(?MODULE,"Module attribs for ~p :~n ~p ~n",[Mod, Attributes]), 	
	lists:nth(1,proplists:get_value(Attr, Attributes)).

%% PITA # ATM - ErlyDTL doesn't allow us to get multiple values mapped to a single proplist key
%% Because of this we need to massage the content so we take all {link, [{a,1}]},{link, [{b,1}]},
%% and make them one property, {links, [[{a,1}], [{b,1}]]}
massage_props(Props) ->
	DistinctKeys = proplists:get_keys(Props),
	ReversedProcessedProps = lists:foldl( 
		fun(Key, PropAcc) ->
			Values = proplists:get_all_values(Key, Props),
			case length(Values) of
				1 -> [{Key, lists:nth(1, Values)} | PropAcc];
				_Other -> [{Key, Values} | PropAcc]
			end
		end,
		[],
		DistinctKeys
	),
	lists:reverse(ReversedProcessedProps).

populate_server(Node) ->
	{store, StoreProps} = get_store_data(Node),
	{value, {collections, Collections}} = lists:keysearch(collections, 1, StoreProps),
	lists:foreach( 
		fun (CollectionProps) ->
			{value, {id, CollectionId}} = lists:keysearch(id, 1, CollectionProps),
			atom:new_entry_collection(Node,	atom, CollectionId, CollectionProps),
			Entries = get_entries(Node, CollectionId),
			lists:foreach(
				fun(EntryProps) ->
					{value, {id, EntryId}} = lists:keysearch(id, 1, EntryProps),
					atom:new_entry(Node, atom, CollectionId, EntryId, EntryProps)
				end,
				Entries
			)			
		end,
		Collections
	).

% Test store data garnered from http://pragdave.blogs.pragprog.com  on 2/19/2008
get_store_data(_Node) ->
		{store, [
			{collections, 
			[
				[
						{title, "Blogging Communitivity"},
						{link, 
							[
								{rel, "self"},
								{type, "application/atom+xml"},
								{href, "http://pragdave.blogs.pragprog.com/pragdave/atom.xml"}
							]
						},
						{link, 
							[
								{rel, "alternate"},
								{type, "text/html"},
								{href, "http://localhost/bliki"}
							]
						},
						{id,main},
						{updated,"2008-01-28T10:07:00-06:00"},
						{subtitle,"Dave ThomasñPragmatic Programmer"},
						{generator,
							[
								{uri, "http://www.typepad.com/"},
								{value,"TypePad"}
							]
						}
				]
			]}
		]}.


% Test store data garnered from http://pragdave.blogs.pragprog.com on 2/19/2008
% and was generated from the first 4 entries.
get_entries(_Node, CollectionId) ->
		% ****************************************************************************
		% ** Entries must have an id that is capable of being a segment in a URL!!
		% * NOT an atom requirement, but requires with Bliki's current route handling
		% *****************************************************************************
		case CollectionId of
			main ->
				[
					[
							{title, "QCon Interview Online"},
							{link, 
								[
									{rel,"alternate"},
									{type, "text/html"},
									{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/qcon-interview.html"}
								]
							},
							{link, 
								[
									{rel, "replies"},
									{type, "text/html"},
									{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/qcon-interview.html"},
									{thr_count,"0"}
								]
							},
							{id,"QCon_Interview_Online"},
							{published,"2008-02-28T10:07:00-06:00"},
							{updated,"2008-03-28T10:07:09-06:00"},
							{summary,"Jim Coplien chatted with me on video during last year's QCon in London. They've just put the result online."},
							{author, 
								[
									{name,"Dave Thomas"}
								]
							},
							{content,
								[{text,
									[{wikicreole,
										[{en_us, "
	Jim Coplien chatted with me on video during last year's QCon in London. They've just put the result 
	 [[http://www.infoq.com/interviews/dave-thomas-agile-passion|online]].
									"	}]
									}]
								}]
							}
					 ],
					 [
							{title, "The Canary Benefit"},
							{link, 
								[
									{rel,"alternate"},
									{type, "text/html"},
									{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/the-canary-bene.html"}
								]
							},
							{link, 
								[
									{rel, "replies"},
									{type, "text/html"},
									{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/the-canary-bene.html"},
									{thr_count,"0"}
								]
							},
							{id,"The_Canary_Benefit"},
							{published,"2008-02-14T14:12:10-06:00"},
							{updated,"2008-02-14T14:13:10-06:00"},
							{summary,"I haven't done production work on a Windows machine for a long, long time. My desktops have been Linux since 0.99pl11 (was that &#39;93?). My laptops were Windows until Linux started working on them, and then I switched. It was..."},
							{author, 
								[
									{name,"Dave Thomas"}
								]
							},
							{category,
								[
									{scheme,"http://www.sixapart.com/ns/types#category"},
									{term,"Random Tech"}
								]
							},
							{content,
								[{text,
									[{wikicreole,
										[{en_us, "
	I haven't done production work on a Windows machine for a long, long time. My desktops have been Linux since 0.99pl11 
	(was that '93?). My laptops were Windows until Linux started working on them, and then I switched.

	It was good. I put up with the hassles: the upgrades, the incompatibilities, the laptops that would talk to some video 
	projectors but not others. I got behind in my patching, and had a server root-kitted once (well before we had an online 
	store, in case you're concerned).

	As the business grew, I found myself spending more and more time admining boxes. So, somewhat late, I made the switch 
	maybe 3 or 4 years ago, first with an Apple laptop, then with a Mac Pro. As I grew more and more confident in the 
	decision, I switched more and more of what I did to OSX. A bunch of our externally facing code runs on Linux and BSD, 
	but it is all administered by third partiesófolks whose job is to keep up with all the stuff that needs doing. 
	Everything else runs on Macs.

	And I've never really regretted the switch. I still don't.

	But, like miners keeping an eye on the [[http://www.wisegeek.com/what-does-it-mean-to-be-a-canary-in-a-coal-mine.htm|canary]], 
	I monitor the one real thing that makes my switching viable. The key benefit of switching for me is the lack of hassle. 
	I spend a bit extra for stuff that just works. .Mac syncing lets me move from desktop to laptop without a thought, 
	but now I have a syncing loop where each machine tries to override information that is identical in the other. Things just 
	aren't as smooth as they were.

	Am I regretting the switch? No. OSX works well for what I do, and it gives me access to tools I need (such as InDesign 
	for covers, Sibelius for scoring, and so on). But I'm also not such a acolyte that I'll never move off the Mac if I start 
	seeing the kind of hassle I used to experience with Linux reentering my life. Modern Linux distros have come a long way 
	since I last used one on a laptop, and I know that I could probably switch if I needed to with little regret.

	So, what do I want from Apple, both tomorrow at MacWorld and then over the coming months? Easy. I want to see fewer 
	cool featuresófeatures which seem to add problemsóand a refocusing on what made Apple the machine of choice for a 
	certain kind of developers. I want my Mac to be as hassle free, secure, and reliable as it was when I first started 
	using OSX.

	Right now, the hassle-free canary seems to be somewhat distressed. I'm monitoring its health closely.
									"	}]
									}]
								}]
							}
					 ]
			];
			_Other -> []
		end.

str_replace(Str, Replaced, Replacement) ->
	string:join(string:tokens(Str, Replaced), Replacement).

new_entry(EntryId) ->
	if
		is_atom(EntryId) -> 
			EntryIdStr = erlang:atom_to_list(EntryId),
			Id = EntryId,
			Title = str_replace(EntryIdStr, "_", " ");
		is_list(EntryId) -> 
			EntryIdStr = EntryId,
			Id = erlang:list_to_atom(EntryId),
			Title = str_replace(EntryId, "_", " ")
	end,
	{entry,
		[
		{title, Title},
		{link, 
			[
				{rel,"alternate"},
				{type, "text/html"},
				{href, "/about/"++EntryIdStr}
			]},
		{id, Id},
		{published,timefuncs:timestamp() },
		{summary,"Enter a 1-4 sentence summary here"},
		{author, 
				[
					{name,"Dave Thomas"}
				]},
		{content, [{text, [{wikicreole, [{en_us, "Enter entry content here in WikiCreole"}]	}] }]}
	]}.
	

%past_time(Timestamp) ->
%	case string:words(Timestamp, "-T:Z") of 
%		6 -> [Year, Month, Day, Hour, Min, Sec] = string:tokens(Timestamp, "-T:Z");
%		8 -> [Year, Month, Day, Hour, Min, Sec, TZHour, TZMin] = string:tokens(Timestamp, "-T:Z")
%	end,
%	{{CY, CM, CD}, {CHour, CMin, CSec}} = calendar:local_time(),
%	DYear = CY - Year,
%	if
%		DYear > 1
%	
	
