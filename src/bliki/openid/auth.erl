
-module(bliki.openid.auth).


-import(erlang).
-import(ibrowse).
-import(error_logger).
-import(proplists).
-import(io_lib).
-import(xmerl_scan).
-import(xmerl_xpath).
-import(lists).
-import(ibrowse_lib).

-export([
checkid_immediate/3,
checkid_setup/3,
check_authentication/4,
encode_query_params/2
]).
		
%%%%%%%
%% Private API
%%%%%%%

% Extra lists:revers around proplist is to ensure same param order
encode_query_params(BaseUrl, ParamPropList) ->
	lists:flatten(lists:reverse(encode_query_params_rec(BaseUrl, lists:reverse(ParamPropList)))).

encode_query_params_rec(BaseUrl, [{ParamName, ParamValue}]) when is_atom(ParamName) ->
	error_logger:info_msg(
		"***~nencode_query_params_rec(~p, [{~p, ~p}])~n**~n", 
		[BaseUrl, ParamName, ParamValue]),
	ParamNameAsStr = erlang:atom_to_list(ParamName),
	encode_query_params_rec(BaseUrl, [{ParamNameAsStr, ParamValue}]);

encode_query_params_rec(BaseUrl, [{ParamName, ParamValue} | Rest]) when is_atom(ParamName) ->
	error_logger:info_msg(
		"***~nencode_query_params_rec(~p, [{~p, ~p} | ~p])~n**~n", 
		[BaseUrl, ParamName, ParamValue, Rest]),
	ParamNameAsStr = erlang:atom_to_list(ParamName),
	encode_query_params_rec(BaseUrl, [{ParamNameAsStr, ParamValue} | Rest]);

encode_query_params_rec(BaseUrl, [{ParamName, ParamValue}]) ->
	error_logger:info_msg(
		"***~nencode_query_params_rec(~p, [{~p, ~p}])~n**~n", 
		[BaseUrl, ParamName, ParamValue]),
	Param = [ParamName, "=", ibrowse_lib:url_encode(ParamValue)],
	[Param | encode_query_params_rec(BaseUrl, [])];

encode_query_params_rec(BaseUrl, [{ParamName, ParamValue} | Rest]) ->
	error_logger:info_msg(
		"***~nencode_query_params_rec(~p, [{~p, ~p} | ~p])~n**~n", 
		[BaseUrl, ParamName, ParamValue, Rest]),
	Param = ["&", ParamName, "=", ibrowse_lib:url_encode(ParamValue)],
	[Param | encode_query_params_rec(BaseUrl, Rest)];

encode_query_params_rec(BaseUrl, []) ->
	error_logger:info_msg(
		"***~nencode_query_params_rec(~p, [])~n**~n", 
		[BaseUrl]),
	case erlang:length(BaseUrl) of
		o -> [];
		_Other -> ["?", BaseUrl]
	end. 

%%%%%%%
%% Public API
%%%%%%%

		
checkid_immediate(OpenIdServer, Identity, ReturnToUrl) ->
	Url = encode_query_params(OpenIdServer, [
		{openid.mode, "checkid_immediate"},
		{openid.identity, Identity},
		{openid.return_to, ReturnToUrl}
		]),
	{redirect, Url}.

checkid_setup(OpenIdServer, Identity, ReturnToUrl) ->
	Url = encode_query_params(OpenIdServer, [
		{openid.mode, "checkid_setup"},
		{openid.identity, Identity},
		{openid.return_to, ReturnToUrl}
		]),
	{redirect, Url}.

% SignedOpenIdFields should have them though
% SignedFieldsNames should be comma-separated list of field names w/o openid. prefix
% key of SignedOpenIdFields should be an atom or a String

check_authentication(OpenIdServer, OpenIdSignature, SignedFieldNames, SignedOpenIdFields) ->
	ParamPropList =	[
						{openid.mode, "check_authentication"}, 
						{openid.sig, OpenIdSignature},
						{openid.signed, SignedFieldNames}
					 ] ++ SignedOpenIdFields,
	Params = encode_query_params("", ParamPropList), % "" allows us to just get querystring
	Result = ibrowse:send_req(
      			OpenIdServer,
      			[{"Content-Type","application/x-www-form-urlencoded"}],
      			post,
      			Params
      		),
 	case Result of 
		{ok, "200", _Headers, Body} -> io:format("POST result =~n~p~n======~n", [Body]);
		_Other -> erlang:error(http_post_problem)
	end.
	
