-module(test_dets).

-export([start/0]).

build_dets(Name, Size) ->
	Ets = build_ets(Size),
	{ok, Dets} = dets:open_file(Name, []),
	ok = dets:from_ets(Dets, Ets),
	ok = dets:sync(Dets),
	ok = dets:close(Dets),
	ets:delete(Ets).

build_ets(Size) ->
	Ets = ets:new(t_ets, [public, set]),
	build_ets(Ets, Size).

build_ets(Ets, N) when N < 0 ->
	Ets;
build_ets(Ets, N) ->
	ets:insert(Ets, {N, N+1}),
	build_ets(Ets, N-1). 

start() ->
	build_dets("resources/wikidata", 1000),
	{ok, Dets} = dets:open_file("resources/wikidata", [{access, read}]),
	io:write(dets:lookup(Dets, 1)),
	halt(0).


