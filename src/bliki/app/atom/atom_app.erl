-module(bliki.app.atom.atom_app).
-behavior(application).

-include("yaws.hrl").

%% application callbacks
-export([start/2, 
         stop/1]).

-import(error_logger).
-import(application).
-import(yaws).
-import(yaws_config).
-import(yaws_api).

start(_Type, Args) ->
	start_atom_yaws(Args),
	start_ibrowse(Args),
	error_logger:info_msg("Starting atom_app~n",[]),
    atom_sup:start_link([]).
	


%% This does not work, hangs and requires a control C to terminate
stop(_State) ->
	stop_atom_yaws(),
    ok.


start_ibrowse(_Args) ->
	application:start(ibrowse).
	
start_atom_yaws(_Args) ->
	error_logger:info_msg("Starting embedded yaws~n",[]),
	Id = "yawstest",
	Debug = true,
	ok = application:load(yaws),
	ok = application:set_env(yaws, embedded, true),
	ok = application:set_env(yaws, id, Id),
	application:start(yaws),
    DefaultGC = yaws_config:make_default_gconf(Debug, Id),
    GC = DefaultGC#gconf{tmpdir="./temp/yaws",logdir="./temp/logs"},
    yaws:mkdir(GC#gconf.tmpdir),
    yaws:mkdir(GC#gconf.logdir),
    SC = #sconf{port = 8888,
		servername = "Signal-2",
		listen = {0,0,0,0},
		docroot = "www",
		appmods = [{"/", bliki.appmod.main}]
	},
	Result = yaws_api:setconf(GC, [[SC]]),
	error_logger:info_msg("Finished starting embedded yaws~n",[]),
	Result.

stop_atom_yaws() ->
	application:stop(yaws).
