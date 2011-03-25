
-module(bliki.openid.discovery).

-import(erlang).
-import(ibrowse).
-import(error_logger).
-import(proplists).
-import(io_lib).
-import(xmerl_scan).
-import(xmerl_xpath).
-import(lists).


-export([
perform_on/1,
perform_xri_discovery/1,
perform_yadis_discovery/1,
perform_html_discovery/1
]).

% Does it start with an xri://, if so do xri discovery on portion after xri://
% Does it start with https://? If so raise exception, we dont support that yet
% Is it missing an http:// at the beginning. if so add it
% Perform Yadis discovery
% Did that return a {openid, disco_success, OpenId, OpenIdServer}?, if so return that
% Otherwise it returned {openid, disco_failure, OpenId} and we do HTML based discovery
% Did that return a {openid, disco_success, OpenId, OpenIdServer}, if so return that
% Did that return a {openid, disco_failure, OpenId}, if so return that
	
perform_on(OpenId) when erlang:is_list(OpenId) ->
 	perform_on(list_to_binary(OpenId));

perform_on(<< "xri://" , Rest/binary >>) ->
	OpenIdAsString = erlang:binary_to_list(Rest),
	case lists:nth(1, OpenIdAsString) of
		$$ -> erlang:error(xri_disco_not_possible_on_dollar_word, [OpenIdAsString]);
		$+ -> erlang:error(xri_disco_not_possible_on_plus_word, [OpenIdAsString]);
		$= -> perform_xri_discovery(OpenIdAsString);
		$@ -> perform_xri_discovery(OpenIdAsString);
		_Other ->  erlang:error(xri_disco_not_possible_malformed_xri, [OpenIdAsString])
	end;
	
% Returned version can currently be v1_0 or v2_0
perform_on(<< "http://", Rest/binary >>) ->
	YadisUrl = "http://" ++ erlang:binary_to_list(Rest),
	case perform_yadis_discovery(YadisUrl) of
		{openid_disco, undefined, OpenId} -> perform_html_discovery(OpenId);
		{openid_disco, Version, OpenId, OpenIdServer} -> {openid_disco, Version,  OpenId, OpenIdServer}
	end;
	
perform_on(<< "https://", Rest/binary >>) ->
	OpenIdAsString = erlang:binary_to_list(Rest),
	erlang:error(https_not_supported, [{openid, "https://" ++ OpenIdAsString}]);
	 
% Insert support for other schemes here

% we get to this point we assume no scheme, and so prepend http://
perform_on(<< OpenId/binary >>) ->
	OpenIdAsString = erlang:binary_to_list(OpenId),
	case lists:nth(1, OpenIdAsString) of
		$$ -> erlang:error(xri_disco_not_possible_on_plus_word, [OpenIdAsString]);
		$+ -> erlang:error(xri_disco_not_possible_on_plus_word, [OpenIdAsString]);
		$= -> perform_xri_discovery(OpenIdAsString);
		$@ -> perform_xri_discovery(OpenIdAsString);
		_Other -> 	
			NormalizedOpenId = erlang:list_to_binary("http://" ++ OpenIdAsString),
			perform_on(NormalizedOpenId)
	end.
		
		
% Curiously the text() predicate in xpath does not seem to be working with
% xmerl_xpath, so we just get all the services and then filter based on
% type.		
	
perform_xri_discovery(XriStr) ->
	HXri = "http://xri.net/" ++ XriStr ++ "?_xrd_r=application/xrds+xml",
	{ok, "200", _HeaderList, Body} = ibrowse:send_req(HXri, [], get),
	{ Xrds, _Rest } = xmerl_scan:string(Body),
	case extract_openid_server_from_xrds(Xrds) of
		{success, Version, Address} -> {openid_disco, Version, XriStr, Address};
		_Other -> {openid_disco, undefined, XriStr}
	end.

perform_yadis_discovery(YadisUrlStr) ->
	%{ok, "200", HeaderList, Body} = ibrowse:send_req(YadisUrlStr, [], get),
	erlang:error(openid_disco_yadis_not_supported_yet, [YadisUrlStr]).

perform_html_discovery(UrlStr) ->
	erlang:error(openid_disco_html_not_supported_yet, [UrlStr]).
	
	
highest_priority_endpoint([Uri | Rest]) ->
	[{xmlText, _Parents, _X, _Y, Address, text}] = xmerl_xpath:string("text()", Uri, [], Uri, []),
    [{xmlAttribute,priority,_X1,_X2,_X3,_X4,_X5,_X6,PriorityAsStr,_X7}] = xmerl_xpath:string("@priority", Uri, [], Uri, []),
	{ok, [Priority], []} = io_lib:fread("~d", PriorityAsStr),
	highest_priority_endpoint({Priority, Address}, Rest).
	
highest_priority_endpoint({HighestPriority, HighestAddress}, [Uri | Rest]) ->
	[{xmlText, _Parents, _X, _Y, Address, text}] = xmerl_xpath:string("text()", Uri, [], Uri, []),
    [{xmlAttribute,priority,_X1,_X2,_X3,_X4,_X5,_X6,PriorityAsStr,_X7}] = xmerl_xpath:string("@priority", Uri, [], Uri, []),
	{ok, [Priority], []} = io_lib:fread("~d", PriorityAsStr),
	if
		Priority > HighestPriority -> highest_priority_endpoint({Priority, Address}, Rest);
		true -> highest_priority_endpoint({HighestPriority, HighestAddress}, Rest)
 	end;

highest_priority_endpoint({_HighestPriority, HighestAddress}, []) ->
	HighestAddress.
	
	
% Also assuming one OpenId service
% We also assume append attr on URI is always QXRI
extract_openid_server_from_xrds(Xrds) ->
	Services = xmerl_xpath:string("//Service", Xrds, [], Xrds, []),
	V2_Services = filter_by_type("http://specs.openid.net/auth/2.0/signon", [], Services),
	XrdParseResult = case length(V2_Services) of
						0 ->
							V1_Services = filter_by_type("http://openid.net/signon/1.0", [], Services),
							case length(V1_Services) of
								0 -> no_openid_services;
								_Other -> {v1_0, lists:nth(1, V1_Services)}
							end;
						_Other ->
							{v2_0, lists:nth(1, V2_Services)}
					end,
	case XrdParseResult of
		{Version, Svc} ->
			Uris = xmerl_xpath:string("//URI", Svc, [], Svc, []),
			OpenIdEndpoint = highest_priority_endpoint(Uris),
			{success, Version, OpenIdEndpoint};
		no_openid_services ->
			{failure, no_openid_services}
	end.
	
% For convenience atm we're assuming 1 typ el on Openid services
% Here we're filtering all services so we have to handle multiple types
filter_by_type(Type, Acc, [Svc | Rest]) ->
	TypesInService = get_types_in_service(Svc),
	case lists:member(Type, TypesInService) of
		true  -> filter_by_type(Type, [Svc | Acc], Rest);
		false -> filter_by_type(Type, Acc, Rest)
	end;

filter_by_type(_Type, Acc, []) ->
	Acc.
	

get_types_in_service(Svc) ->
	get_types_in_service(Svc, [], xmerl_xpath:string("//Type/text()", Svc, [], Svc, [])).
	
get_types_in_service(Svc, Acc, [{xmlText,_Parents, _X1,_X2,TypeStr,text} | Rest]) ->
	get_types_in_service(Svc, [ TypeStr | Acc], Rest);
	
get_types_in_service(_Svc, Acc, []) ->
	Acc.
	
	