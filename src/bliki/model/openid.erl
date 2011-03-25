
-module(bliki.model.openid).

-include("yaws_api.hrl").

-export([login/1,authenticate/1,verify/1,get_identity/2,logout/1,defaultModel/1]).

-import(bliki.app.atom.atom).
-import(bliki.openid.discovery).
-import(bliki.openid.auth).
-import(proplists).
-import(error_logger).
-import(yaws_api).
-import(lists).


login({ParsedRoute, Options}) ->
	error_logger:info_msg("+~p:login(_, ~p)~n", [?MODULE, Options]),
	Model = [],
	error_logger:info_msg("-~p:login -> ~p~n", [?MODULE, Model]),
	{ParsedRoute, lists:merge(Model, Options)}.

authenticate({ParsedRoute, Options}) ->
	error_logger:info_msg("+~p:verify(_, ~p)~n", [?MODULE, Options]),
	Arg = proplists:get_value(yaws_arg, Options),
	error_logger:info_msg("~n****~n** Arg: ~p~n****~n", [Arg]),
	Vars = yaws_api:parse_post(Arg),
	error_logger:info_msg("Parsed post vars: ~p~n", [ Vars]),
	Headers = Arg#arg.headers,
	VerificationUrl = "http://" ++ Headers#headers.host ++ "/verify",
	Identifier = proplists:get_value("openid_identifier", Vars),
	FinalUrl = proplists:get_value("return_to", Vars),
	ReturnToUrl = auth:encode_query_params(VerificationUrl, [{final_url, FinalUrl}]),
	error_logger:info_msg("****~n** Return To Url: ~p~n****~n", [ReturnToUrl]),
	case discovery:perform_on(Identifier) of 
			{openid_disco, _Version,  OpenId, OpenIdServer} -> %TODO Support v2_0 vs v1_0 here
				Model = auth:checkid_setup(OpenIdServer, OpenId, ReturnToUrl),
				{ParsedRoute, Model};
			Other -> erlang:error(openid_disco_problem, [Other])
	end.
	


session(State) ->
    receive
        {From, get} ->
            From ! {self(), State},
            session(State);
        {_From, stop} -> nil
    after 1800000 ->    %% keep state for 30 min only
            nil
    end.

%TODO We need to be doing checkid_authenticate via POST to verify signatures, otherwise
%	not true OpenId.	
verify({ParsedRoute, Options}) ->
	Arg = proplists:get_value(yaws_arg, Options),
	{ok, Identity} = yaws_api:queryvar(Arg, "openid.identity"),
	error_logger:info_msg("****~n** Identity: ~p~n****~n", [Identity]),
	%TODO Should be doing checkid_authenticate here
	{ok, FinalUrl} = yaws_api:queryvar(Arg, "final_url"),
	error_logger:info_msg("****~n** Final Url: ~p~n****~n", [FinalUrl]),
	SessionState = [{identity, Identity}],
	Pid = spawn(fun() -> session(SessionState) end),
	{ParsedRoute, {passthru, [
								{status, 303},
 								{allheaders,
     								[
										{header, ["Location: ", FinalUrl]},
      									{header, ["Set-Cookie: ","sessionid=" ++ erlang:pid_to_list(Pid) ++ ";"]}
									]
								 },
 								 {html,"<html><body>Authenticated, redirecting back to your page</body></html>"}
							 ]
     				}
	}.
	
logout({ParsedRoute, Options}) ->
	Arg = proplists:get_value(yaws_arg, Options),
	Vars = yaws_api:parse_post(Arg),
	FinalUrl = proplists:get_value("return_to", Vars),
	Headers = Arg#arg.headers,
	case yaws_api:find_cookie_val("sessionid", Headers#headers.cookie) of
        [] ->
            [];
        SessionIdStr ->
			list_to_pid(SessionIdStr) ! {self(), stop},
			{ParsedRoute, {passthru, [
								{status, 303},
 								{allheaders,
     								[
										{header, ["Location: ", FinalUrl]},
      									{header, ["Set-Cookie: ","sessionid=" ++ SessionIdStr ++";expires=Tuesday, 01-Jan-2008 01:01:01 GMT"]}
									]
								 },
 								 {html,"<html><body>Logged out, redirecting back to your page</body></html>"}
							 ]
     				}
			}
    end.
	

session_var(SessionIdStr, Key) ->
	SessionPid = list_to_pid(SessionIdStr),
	SessionPid ! {self(), get},
	State = receive
				{SessionPid, ReturnedState} -> ReturnedState
			end,
	proplists:get_value(Key, State).
	
get_identity(_ParsedRoute, Options) ->
	Arg = proplists:get_value(yaws_arg, Options),
	Headers = Arg#arg.headers,
	case yaws_api:find_cookie_val("sessionid", Headers#headers.cookie) of
        [] ->
            [];
        SessionIdStr ->
			case erlang:is_process_alive(erlang:list_to_pid(SessionIdStr)) of
				true -> [{openid_identifier, session_var(SessionIdStr, identity)}];
				false -> [] % Need to send redirect and cookie expire here
			end
    end.
	 
% For now we are making the following assumptions:
%	identifier starts with http://
%	identifier is already normalized in other ways
% but see comment below...


	
defaultModel({ParsedRoute, Options}) ->
	{ParsedRoute, Options}.