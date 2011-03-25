-module(tmp).
-export([start/0]).

start() ->
	yecc:yecc("erlydtl_parser.yrl", "erlydtl_parser.erl"),
	compile:file(erlydtl_parser),
	compile:file(erlydtl_scanner),
	io:format("~p ~n", [erlydtl_scanner:scan("{{ foo.bar }}")]),
	erlydtl_parser:parse(erlydtl_scanner:scan("{{ foo.bar }}")),
	halt(0).

