
-module(bliki.test.test).
-export([start/0]).

%starts() ->
%	{ok, Binary} = file:read_file("resources/test.data"),
%     	Buff = erlang:binary_to_list(Binary),
%        Buff.
	
start() ->
	%%{ok, Terms} = file:consult("resources/test.data"),
	io:write(file:consult("resources/test.data")),
	halt(0).	
 	
