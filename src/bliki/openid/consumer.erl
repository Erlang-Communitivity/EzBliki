-module(bliki.openid.consumer).


-export([verify/1,test/0]).

-import(lists).
-import(ibrowse).
-import(error_logger).
-import(proplists).

% Given the spec in RFC2631 of DH says
% the shared secret ZZ is generated as follows:
%
%     ZZ = g ^ (xb * xa) mod p
%
%   Note that the individual parties actually perform the computations:
%
%     ZZ = (yb ^ xa)  mod p  = (ya ^ xb)  mod p
%
%   where ^ denotes exponentiation
%
%         ya is party a's public key; ya = g ^ xa mod p
%         yb is party b's public key; yb = g ^ xb mod p
%         xa is party a's private key
%         xb is party b's private key
%         p is a large prime
%         q is a large prime
%         g = h^{(p-1)/q} mod p, where
%         h is any integer with 1 < h < p-1 such that h{(p-1)/q} mod p > 1
%           (g has order q mod p; i.e. g^q mod p = 1 if g!=1)
%         j a large integer such that p=qj + 1
%         (See Section 2.2 for criteria for keys and parameters)

% In openid terms, according to section 8.1.1 and 8.1.2 of http://openid.net/specs/openid-authentication-2_0.html
% and assuming the RP is party a, then
%  openid.dh_modulus = p
%  openid.dh_gen = g 
%  openid.dh_consumer_public = ya = g ^ xa mod p
%
%  We're given default values for p and g in the openid, so for now we'll use them
% g = 2, base64(btwoc(2)) = 
% p = .., base64(btwoc(p)) = ANz5OguIOXLsDhmYmsWizjEOHTdxfo2Vcbt2I3MYZuYe91ouJ4mLBX+YkcLiemOc
%	Pym2CBRYHNOyyjmG0mg3BVd9RcLn5S3IHHoXGHblzqdLFEi/368Ygo79JRnxTkXj
%	gmY0rxlJ5bU1zIKaSDuKdiI+XUkKJX8Fvf8W8vsixYOr

% For now we're only operating in dumb consumer mode
% That means
%yaws_api:url_encode
%http://www.myopenid.com/server?openid.mode=checkid_immediate&openid.identity
%=http://prawf.myopenid.com/&openid.return_to=http://example.org

test() ->
	verify("http://localhost:8000/id/bob").
	
% The verify below does not handle XRI OpenIDs
verify(OpenId) ->
	{ok, "200", _HeaderList, Body} = ibrowse:send_req(OpenId, [], get),
	OpenIdProps = parse_html(erlang:list_to_binary(Body)),
	error_logger:info_msg("OpenIdProps: ~p~n", [OpenIdProps]),
	HasXrdsLoc = proplists:is_defined("x-xrds-location", OpenIdProps),
	if 
		HasXrdsLoc -> 
			XrdsLocation = proplists:get_value("x-xrds-location", OpenIdProps, []),
			{ok, "200", _HeaderList2, Xrds} = ibrowse:send_req(
										XrdsLocation, 
										[{"Accept", "application/xrds+xml"}], 
										get);
		true -> Xrds = "Not Available"
	end,
	Xrds.
	
parse_html(<< "<link rel=\"" , Rest/binary >>) ->
	parse_rel(Rest,[]);

parse_html(<< "<meta http-equiv=\"" , Rest/binary >>) ->
	parse_meta(Rest,[]);
	
parse_html(<< _Ch:8, Rest/binary >>) ->
	parse_html(Rest);

parse_html(<<>>) ->
	[].

%% For link parsing
%%
parse_rel(<< "\"" , Rest/binary >>, Acc) ->
	Rel = lists:reverse(Acc),
	parse_html_after_rel(Rest, Rel);
	
parse_rel(<< Ch:8, Rest/binary >>, Acc) ->
	parse_rel(Rest, [Ch | Acc]).

parse_html_after_rel(<< "href=\"" , Rest/binary >>, Rel) ->
	parse_href(Rest, Rel, []);
	
parse_html_after_rel(<< _Ch:8, Rest/binary >>, Rel) ->
	parse_html_after_rel(Rest, Rel).	
	
parse_href(<< "\"" , Rest/binary >>, Rel, Acc) ->
	Href = lists:reverse(Acc),
	lists:append([{Rel, Href}], parse_html(Rest));
	
parse_href(<< Ch:8, Rest/binary >>, Rel, Acc) ->
	parse_href(Rest, Rel, [Ch | Acc]).

%% For meta parsing
%%
parse_meta(<< "\"" , Rest/binary >>, Acc) ->
	Equiv = lists:reverse(Acc),
	parse_html_after_equiv(Rest, Equiv);
	
parse_meta(<< Ch:8, Rest/binary >>, Acc) ->
	parse_meta(Rest, [Ch | Acc]).

parse_html_after_equiv(<< "content=\"" , Rest/binary >>, Equiv) ->
	parse_content(Rest, Equiv, []);
	
parse_html_after_equiv(<< _Ch:8, Rest/binary >>, Equiv) ->
	parse_html_after_equiv(Rest, Equiv).	
	
parse_content(<< "\"" , Rest/binary >>, Equiv, Acc) ->
	Content = lists:reverse(Acc),
	lists:append([{Equiv, Content}], parse_html(Rest));
	
parse_content(<< Ch:8, Rest/binary >>, Rel, Acc) ->
	parse_content(Rest, Rel, [Ch | Acc]).
	
