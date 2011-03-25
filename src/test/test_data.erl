-module(bliki.test.test_data).

-export([push/0,populate_server/1, get_store_data/1, get_entries/2, check_server/1]).


push() ->
	Node = 'gog@Signal',
	populate_server(Node),
	check_server(Node).
	
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
	
check_server(Node) ->
	{store, StoreProps} = get_store_data(Node),
	{value, {collections, Collections}} = lists:keysearch(collections, 1, StoreProps),
	lists:foreach( 
		fun (CollectionProps) ->
			{value, {id, CollectionId}} = lists:keysearch(id, 1, CollectionProps),
			Feed = atom:get_collection(Node,atom,CollectionId),
			io:format("\n\t--- Feed --- \n~p\n\n", [Feed])
		end,
		Collections
	),
	io:format("\nCheck done\n").

% Test store data garnered from http://pragdave.blogs.pragprog.com  on 2/19/2008
get_store_data(_Node) ->
	{store, [
		{collections, 
		[
			[
					{title, "Prag Dave"},
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
					{subtitle,"Dave Thomas–Pragmatic Programmer"},
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
						{id,"tag:typepad.com,2003:post-44727824"},
						{published,"2008-01-28T10:07:00-06:00"},
						{updated,"2008-01-28T10:07:09-06:00"},
						{summary,"Jim Coplien chatted with me on video during last year's QCon in London. They've just put the result online."},
						{author, 
							[
								{name,"Dave Thomas"}
							]
						},
						{content,
							[
								{type, "text/wikicreole"},
								{xml_lang, "en-US"},
								{value, "
Jim Coplien chatted with me on video during last year's QCon in London. They've just put the result 
 [[http://www.infoq.com/interviews/dave-thomas-agile-passion|online]].
								"}
							]
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
						{id,"tag:typepad.com,2003:post-44133260"},
						{published,"2008-01-14T14:12:10-06:00"},
						{updated,"2008-01-14T14:12:10-06:00"},
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
							[
								{type, "text/wikicreole"},
								{xml_lang, "en-US"},
								{value, "
I haven't done production work on a Windows machine for a long, long time. My desktops have been Linux since 0.99pl11 
(was that '93?). My laptops were Windows until Linux started working on them, and then I switched.

It was good. I put up with the hassles: the upgrades, the incompatibilities, the laptops that would talk to some video 
projectors but not others. I got behind in my patching, and had a server root-kitted once (well before we had an online 
store, in case you're concerned).

As the business grew, I found myself spending more and more time admining boxes. So, somewhat late, I made the switch 
maybe 3 or 4 years ago, first with an Apple laptop, then with a Mac Pro. As I grew more and more confident in the 
decision, I switched more and more of what I did to OSX. A bunch of our externally facing code runs on Linux and BSD, 
but it is all administered by third parties—folks whose job is to keep up with all the stuff that needs doing. 
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
cool features—features which seem to add problems—and a refocusing on what made Apple the machine of choice for a 
certain kind of developers. I want my Mac to be as hassle free, secure, and reliable as it was when I first started 
using OSX.

Right now, the hassle-free canary seems to be somewhat distressed. I'm monitoring its health closely.
								"}
							]
						}
				],
				[
						{title, "A loud 'Huzzah! was heard throughout the land"},
						{link, 
							[
								{rel,"alternate"},
								{type, "text/html"},
								{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/a-loud-huzzah-w.html"}
							]
						},
						{link, 
							[
								{rel, "replies"},
								{type, "text/html"},
								{href, "http://pragdave.blogs.pragprog.com/pragdave/2008/01/a-loud-huzzah-w.html"},
								{thr_count,"0"}
							]
						},
						{id,"tag:typepad.com,2003:post-43841772"},
						{published,"2008-01-08T09:12:35-06:00"},
						{updated,"2008-01-08T09:12:35-06:00"},
						{summary,"Eric Hodel is giving RDoc some love. You can't imagine how happy that makes me. When I first wrote RDoc, I was trying to find a way of solving two problems: 1. Adding comments to the largely uncommented C source..."},
						{author, 
							[
								{name,"Dave Thomas"}
							]
						},
						{category,
							[
								{scheme,"http://www.sixapart.com/ns/types#category"},
								{term,"Rails"}
							]
						},
						{content,
							[
								{type, "text/wikicreole"},
								{xml_lang, "en-US"},
								{value, "
Eric Hodel is giving RDoc [[http://blog.segment7.net/articles/2008/01/07/rdocs-templatepage-removed-from-ruby|some love]]. 
You can't imagine how happy that makes me.

When I first wrote RDoc, I was trying to find a way of solving two problems:
# Adding comments to the largely uncommented C source of Ruby, and<br />
# Providing a means for library writers easily to document their creations.</p>

I'd just finished the PickAxe, and I wanted to take the work Andy and I had done reverse engineering the 
Ruby API and add it back into the interpreter source code. 

I set myself constraints with RDoc and ri:
* it should produce at least some documentation even on totally uncommented source files
* it should extract tacit information from the program source (for example guessing good names for block parameters by looking for yield statements inside methods)
* the markup in the source files should be unobtrusive. In the typical case, someone reading the source should not even notice that the comments follow markup conventions
* it should only use libraries that come pre-installed with Ruby
* the documentation it produced should be portable across machines and architectures
* it should allow incremental documentation. Libraries that you install over time can add methods to existing classes. As you add these libraries, the method lists in the classes you extend should grow to reflect the changes
* it should be secure. People pushed many times to add the ability to execute code during the documentation process. I didn't want to have code run on an end user's machine during a process that ostensibly was simply installing documentation (particularly as these installations often ran as root)
* it should be throw-away

The last one might be a surprise, but the real objective of RDoc wasn't the tool. The real objective was to set a standard 
that meant that future libraries would get documented in a consistent and usable way. And so RDoc and ri compromised like crazy. 
Rather than a database or some complex binary format, they used a set of directory trees in the user's filesystem to store 
documentation. This documentation, which is basically a set of Ruby objects, was stored using YAML, rather than marshaled 
objects or Ruby source. Even though YAML is slow, it is more portable than marshaled objects, and more secure than Ruby source. 
The parser in RDoc was a wild hack on the parser in irb. This means it performs a static, not dynamic, analysis and that it is 
sometimes confused by edge cases in Ruby syntax. So be it. 

But the very worst part of RDoc/ri is the output side. I wanted to be able to produce output in a variety of formats: HTML, plain 
text, XML, chm, LaTeX, and so on. So the analysis side of RDoc produces a data structure, and passes it to the output side. 
Here I made a stupid design decision. What RDoc generates internally is basically nested hashes. This has a couple of major 
advantages. In particular, there's a kind of fractal property when traversing it: it doesn't matter how deep you are in 
the structure—all you pass to the next routine down is a hash. But it has a major downside—it's a bitch to work with. If I 
were doing it again, I'd use Structs.

Finally, there's the generation of the output itself. I needed a templating system and, for what seemed like good reasons 
at the time, I wrote my own. It was only a handful of lines of code initially. It's still only a couple of hundred. It 
did a few things well, but ultimately it was ugly as sin. But now, as Erb has become something of a standard, it is 
definitely the right time to replace it.

RDoc and ri are, in a way, the ultimate [[http://en.wikipedia.org/wiki/Stone_soup|stone soup]]. The code itself is 
not the output of the project. The real output is the thousands of libraries that are now self-documenting. Eric and the crew 
are busy on the stew, replacing the stones with real and tasty ingredients. When they are finished, we'll be able to use all 
that library documentation in remarkable new ways. So, a big thank you to Eric and Seattle.rb, and to all the Ruby coders who've 
created such a great base of documentation for us all.

Here's to RDoc 2.0.
								"}
							]
						}
				]
			];
		_Other -> []
	end.
	