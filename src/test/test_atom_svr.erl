-module(bliki.test.test_atom_svr).

-export([start/0,build_feeds/0]).

build_feeds() ->
	Node = 'gog@SIGNAL',
	atom:new_entry_collection(
		Node,
		atom,
		'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6',
		[
			{title, "Example Feed"},
			{link, "http://example.org/"},
			{author, [{name,"John Doe"}]}
		]
	),
	atom:new_entry(
		Node,
		atom,
		'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6',
		'urn:uuid:1225c695-cfb8-4ebb-aaa-80da344efa6a',
		[
			{title, "RSS Powered Robots Run Amok"},
			{link, "http://example.org/2003/12/13/atom03" },
			{updated, "2003-12-13T18:30:02Z"},
			{summary, "Some text"},
			{content, [{type, "text/wikicreole"},{text,
"//italics// \\
 **bold** \\
"}
		]}]
	),
	atom:new_entry(
		Node,
		atom,
		'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6',
		'urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a',
		[
			{title, "Atom-Powered Robots Run"},
			{link, "http://example.org/2003/12/13/atom03" },
			{updated, "2003-12-13T18:30:02Z"},
			{summary, "Some text"},
			{content, [{type, "text/wikicreole"},{text, "**A bold example**"}
		]}]
	),
	{ok}.


start() ->
	{ok} = build_feeds(),
	Node = 'gog@SIGNAL',
	Feed = atom:get_collection(Node,atom,'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6'),
	Entries = atom:get_entries_in(Node, atom, 'urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6'),
	io:format("\t--- Feed --- \n~p\n\n\t--- Entries ---\n~p\n\n", [Feed, Entries]),
	io:format("\ndone\n"),
	halt(0).

