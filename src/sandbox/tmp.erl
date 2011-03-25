-module(tmp).
-import(lists,[filter/2, map/2,reverse/1]).
-import(io).
-export([main/0]).

main() ->
  Files =  ["A","b","C"],
  lists:map(fun(File) ->
          io:format("Found file: ~w\n", [File]) 
	end, Files).


