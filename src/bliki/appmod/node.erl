-module(bliki.appmod.node).
-include("yaws_api.hrl").
-export([out/1]).


out(A) ->
    %%
    %% Compile the template
    %%
    DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"../../..", "3rdparty", "erlydtl", "demo", "templates"]),
    ModuleName = "test_" ++ A#arg.appmoddata,
    Ext = ".html",
    File = filename:join([DocRoot, ModuleName ++ Ext]),
    CVars = [],
    case erlydtl_compiler:compile(File, ModuleName, DocRoot, CVars) of
        ok ->
            io:format("compile success: ~p~n",[ModuleName]);
        _ ->
            io:format("compile failure: ~p~n",[ModuleName])
    end,
    %%
    %% Then render it
    %%
    OutDir = filename:join([filename:dirname(code:which(?MODULE)),"../../..", "3rdparty", "erlydtl", "demo", "out"]),
    Module = list_to_atom(ModuleName),
    Args = [],
    case catch Module:render(Args) of
        {ok, Val} ->
            case file:open(filename:join([OutDir, filename:basename(Module:source())]), [write]) of
                {ok, IoDev} ->
                    file:write(IoDev, Val),
                    file:close(IoDev),
                    io:format("render success: ~p~n",[Module]),
		    {html, Val};
                _ ->
                    io:format("file writing failure: ~p~n",[Module])
            end;
        {error, Err} ->
            io:format("render failure: ~p ~p~n",[Module, Err])
    end.
