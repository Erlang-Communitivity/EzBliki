-module(bliki.test.test_atom).

-export([build_feeds/0,start/0]).

build_feeds() ->
	Feeds =	ets:new('urn:uuid:60a76c80-d399-11d9-ffff-0003939e0af6', [public, set]),
	Entries = ets:new('urn:uuid:1225c695-cfb8-4ebb-cccc-80da344efa6a', [public, set]),
	ets:insert(Feeds, { 'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6',
		{feed, [
			{id, 'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6'},
			{title, "Example Feed"},
			{link, "http://example.org/"},
			{updated, "2003-12-13T18:30:02Z"},
			{author, [{name,"John Doe"}]},
			{entry, {id, 'urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a'}},
			{entry, {id, 'urn:uuid:1225c695-cfb8-4ebb-bbbb-80da344efa6a'}}
	]}}),
	ets:insert(Entries, {'urn:uuid:1225c695-cfb8-4ebb-bbbb-80da344efa6a',
		{entry, [
			{title, "Atom-Powered Robots Run Amok"},
			{link, "http://example.org/2003/12/13/atom03" },
			{id, 'urn:uuid:1225c695-cfb8-4ebb-bbbb-80da344efa6a'},
			{updated, "2003-12-13T18:30:02Z"},
			{summary, "Some text"},
			{content, [{type, "text/wikicreole"},{text, "**A bold example**"}
	]}]}}),
	ets:insert(Entries, {'urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a',
		{entry, [
			{title, "Atom-Powered Robots Run Amok"},
			{link, "http://example.org/2003/12/13/atom03" },
			{id, 'urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a'},
			{updated, "2003-12-13T18:30:02Z"},
			{summary, "Some text"},
			{content, [{type, "text/wikicreole"},{text,
"//italics// \\
 **bold** \\
"}
	]}]}}),
	{Feeds, Entries}.


start() ->
	{FeedTab, EntryTab} = build_feeds(),
	%io:format("Feeds table:\n"),
	%io:write(ets:tab2list(FeedTab)),	
	%io:format("\nEntries table:\n"),
	%io:write(ets:tab2list(EntryTab)),	
	%io:format("\n \n"),
	[{_, {feed,Props}}] = ets:lookup(FeedTab, ets:first(FeedTab)),
	%io:write(Props),
	OnlyEntries = fun ({entry, _}) -> true;
			    (_X) -> false
			end,
	EntryProps = lists:filter(OnlyEntries, Props),
	%io:write(EntryProps),
	ExpandEntry = fun ({entry, {id, Id}}) -> [{_EntryId, {entry, Fields}}] = ets:lookup(EntryTab, Id), Fields ;
			  (_X) -> nomatch 
			end,

	Entries = lists:map(ExpandEntry, EntryProps),
	io:write(Entries),
	io:format("\ndone\n"),
	halt(0).

