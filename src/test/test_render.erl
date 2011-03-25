-module(bliki.test.test_render).

-export([start/0]).

data() ->
	%Node = 'gog@SIGNAL',
	%test_data:populate_server(Node),
	%{ok, Feed} = atom:get_collection(Node,atom, 	main),
	%Feed.
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
	].

	
	
compile(File) ->
        CompileVars = [],
	Name = filename:rootname(filename:basename(File)),
   	Module = "template_" ++ Name,
	Options = [
               {vars, CompileVars}, 
               {force_recompile, true}
	],
	case erlydtl_compiler:compile(File, Module, Options) of
		ok -> {ok, list_to_atom(Module)};
		Other -> Other
	end.	
  
start() ->
	io:format("Starting test_render...~n"),
	io:format("Compiling template...~n"),
	{ok, Template} = compile("resources/templates/feed.html"),
	io:format("Finished compiling template '~p'...~n",[Template]),
	io:format("Rendering ...~n"),
	RenderVars = [{feed, data()}],
	{ok, Val} = Template:render(RenderVars),
	Rendering = {html, Val},
	io:format("Finished rendering with result:~n ~p ...~n", [Rendering]),
	io:format("End of test_render...~n"),
	halt(0).

