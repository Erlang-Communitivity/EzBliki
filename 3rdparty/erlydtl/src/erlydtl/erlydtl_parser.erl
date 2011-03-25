-module(erlydtl_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("e:/erl/lib/parsetools-1.4.2/include/yeccpre.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, 
              Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/erlydtl/erlydtl_parser.erl", 148).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccpars2(137, Cat, [5 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccpars2(128, Cat, [7 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2(117, Cat, [9 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2(112, Cat, [11 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccpars2(107, Cat, [15 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2(102, Cat, [19 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(97, Cat, [21 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Elements\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, close_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccpars2('yeccgoto_\'Variable\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ValueBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Value\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_36(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2('yeccgoto_\'Filter\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Filter\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Variable\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccpars2(71, Cat, [46 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_47(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, not_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, number_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_48: see yeccpars2_24

%% yeccpars2_49: see yeccpars2_24

yeccpars2_50(S, string_literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2('yeccgoto_\'LoadNames\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'LoadTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'LoadNames\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IncludeTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfNotEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_24

yeccpars2_60(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfEqualExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_24

yeccpars2_64(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_66(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_68: see yeccpars2_47

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'IfExpression\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'IfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CustomTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(S, equal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_24

yeccpars2_75(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_76(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, in_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_80(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_81: see yeccpars2_24

yeccpars2_82(_S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_close_tag(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForExpression\''(hd(Nss)), close_tag, Nss, NewStack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Value\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForGroup\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'ExtendsTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, with_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'CallTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_24

yeccpars2_91(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, pipe, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2('yeccgoto_\'CallWithTag\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'BlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AutoEscapeBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, endautoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndAutoEscapeBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'BlockBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, endblock_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndBlockBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CommentBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, endcomment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_110(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndCommentBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ForBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, endfor_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndForBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccpars2(125, Cat, [119 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_120(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ElseBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccpars2(134, Cat, [130 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_131(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_134(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_136(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, endifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccpars2(143, Cat, [139 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_140(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(S, close_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EndIfNotEqualBraced\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_143(S, open_tag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, open_var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(S, text, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2('yeccgoto_\'IfNotEqualBlock\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_145(S, autoescape_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, block_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, call_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, comment_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, endifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, extends_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, for_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, if_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ifequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ifnotequal_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, load_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Args\''(46) -> 71.

'yeccgoto_\'AutoEscapeBlock\''(1) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(97) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(102) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(107) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(112) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(117) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(125) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(128) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(134) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(137) -> 22;
'yeccgoto_\'AutoEscapeBlock\''(143) -> 22.

'yeccgoto_\'AutoEscapeBraced\''(1) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(97) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(102) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(107) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(112) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(117) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(125) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(128) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(134) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(137) -> 21;
'yeccgoto_\'AutoEscapeBraced\''(143) -> 21.

'yeccgoto_\'BlockBlock\''(1) -> 20;
'yeccgoto_\'BlockBlock\''(97) -> 20;
'yeccgoto_\'BlockBlock\''(102) -> 20;
'yeccgoto_\'BlockBlock\''(107) -> 20;
'yeccgoto_\'BlockBlock\''(112) -> 20;
'yeccgoto_\'BlockBlock\''(117) -> 20;
'yeccgoto_\'BlockBlock\''(125) -> 20;
'yeccgoto_\'BlockBlock\''(128) -> 20;
'yeccgoto_\'BlockBlock\''(134) -> 20;
'yeccgoto_\'BlockBlock\''(137) -> 20;
'yeccgoto_\'BlockBlock\''(143) -> 20.

'yeccgoto_\'BlockBraced\''(1) -> 19;
'yeccgoto_\'BlockBraced\''(97) -> 19;
'yeccgoto_\'BlockBraced\''(102) -> 19;
'yeccgoto_\'BlockBraced\''(107) -> 19;
'yeccgoto_\'BlockBraced\''(112) -> 19;
'yeccgoto_\'BlockBraced\''(117) -> 19;
'yeccgoto_\'BlockBraced\''(125) -> 19;
'yeccgoto_\'BlockBraced\''(128) -> 19;
'yeccgoto_\'BlockBraced\''(134) -> 19;
'yeccgoto_\'BlockBraced\''(137) -> 19;
'yeccgoto_\'BlockBraced\''(143) -> 19.

'yeccgoto_\'CallTag\''(1) -> 18;
'yeccgoto_\'CallTag\''(97) -> 18;
'yeccgoto_\'CallTag\''(102) -> 18;
'yeccgoto_\'CallTag\''(107) -> 18;
'yeccgoto_\'CallTag\''(112) -> 18;
'yeccgoto_\'CallTag\''(117) -> 18;
'yeccgoto_\'CallTag\''(125) -> 18;
'yeccgoto_\'CallTag\''(128) -> 18;
'yeccgoto_\'CallTag\''(134) -> 18;
'yeccgoto_\'CallTag\''(137) -> 18;
'yeccgoto_\'CallTag\''(143) -> 18.

'yeccgoto_\'CallWithTag\''(1) -> 17;
'yeccgoto_\'CallWithTag\''(97) -> 17;
'yeccgoto_\'CallWithTag\''(102) -> 17;
'yeccgoto_\'CallWithTag\''(107) -> 17;
'yeccgoto_\'CallWithTag\''(112) -> 17;
'yeccgoto_\'CallWithTag\''(117) -> 17;
'yeccgoto_\'CallWithTag\''(125) -> 17;
'yeccgoto_\'CallWithTag\''(128) -> 17;
'yeccgoto_\'CallWithTag\''(134) -> 17;
'yeccgoto_\'CallWithTag\''(137) -> 17;
'yeccgoto_\'CallWithTag\''(143) -> 17.

'yeccgoto_\'CommentBlock\''(1) -> 16;
'yeccgoto_\'CommentBlock\''(97) -> 16;
'yeccgoto_\'CommentBlock\''(102) -> 16;
'yeccgoto_\'CommentBlock\''(107) -> 16;
'yeccgoto_\'CommentBlock\''(112) -> 16;
'yeccgoto_\'CommentBlock\''(117) -> 16;
'yeccgoto_\'CommentBlock\''(125) -> 16;
'yeccgoto_\'CommentBlock\''(128) -> 16;
'yeccgoto_\'CommentBlock\''(134) -> 16;
'yeccgoto_\'CommentBlock\''(137) -> 16;
'yeccgoto_\'CommentBlock\''(143) -> 16.

'yeccgoto_\'CommentBraced\''(1) -> 15;
'yeccgoto_\'CommentBraced\''(97) -> 15;
'yeccgoto_\'CommentBraced\''(102) -> 15;
'yeccgoto_\'CommentBraced\''(107) -> 15;
'yeccgoto_\'CommentBraced\''(112) -> 15;
'yeccgoto_\'CommentBraced\''(117) -> 15;
'yeccgoto_\'CommentBraced\''(125) -> 15;
'yeccgoto_\'CommentBraced\''(128) -> 15;
'yeccgoto_\'CommentBraced\''(134) -> 15;
'yeccgoto_\'CommentBraced\''(137) -> 15;
'yeccgoto_\'CommentBraced\''(143) -> 15.

'yeccgoto_\'CustomTag\''(1) -> 14;
'yeccgoto_\'CustomTag\''(97) -> 14;
'yeccgoto_\'CustomTag\''(102) -> 14;
'yeccgoto_\'CustomTag\''(107) -> 14;
'yeccgoto_\'CustomTag\''(112) -> 14;
'yeccgoto_\'CustomTag\''(117) -> 14;
'yeccgoto_\'CustomTag\''(125) -> 14;
'yeccgoto_\'CustomTag\''(128) -> 14;
'yeccgoto_\'CustomTag\''(134) -> 14;
'yeccgoto_\'CustomTag\''(137) -> 14;
'yeccgoto_\'CustomTag\''(143) -> 14.

'yeccgoto_\'Elements\''(0) -> 1;
'yeccgoto_\'Elements\''(5) -> 137;
'yeccgoto_\'Elements\''(7) -> 128;
'yeccgoto_\'Elements\''(9) -> 117;
'yeccgoto_\'Elements\''(11) -> 112;
'yeccgoto_\'Elements\''(15) -> 107;
'yeccgoto_\'Elements\''(19) -> 102;
'yeccgoto_\'Elements\''(21) -> 97;
'yeccgoto_\'Elements\''(119) -> 125;
'yeccgoto_\'Elements\''(130) -> 134;
'yeccgoto_\'Elements\''(139) -> 143.

'yeccgoto_\'ElseBraced\''(117) -> 119;
'yeccgoto_\'ElseBraced\''(128) -> 130;
'yeccgoto_\'ElseBraced\''(137) -> 139.

'yeccgoto_\'EndAutoEscapeBraced\''(97) -> 98.

'yeccgoto_\'EndBlockBraced\''(102) -> 103.

'yeccgoto_\'EndCommentBraced\''(107) -> 108.

'yeccgoto_\'EndForBraced\''(112) -> 113.

'yeccgoto_\'EndIfBraced\''(117) -> 118;
'yeccgoto_\'EndIfBraced\''(125) -> 126.

'yeccgoto_\'EndIfEqualBraced\''(128) -> 129;
'yeccgoto_\'EndIfEqualBraced\''(134) -> 135.

'yeccgoto_\'EndIfNotEqualBraced\''(137) -> 138;
'yeccgoto_\'EndIfNotEqualBraced\''(143) -> 144.

'yeccgoto_\'ExtendsTag\''(1) -> 13;
'yeccgoto_\'ExtendsTag\''(97) -> 13;
'yeccgoto_\'ExtendsTag\''(102) -> 13;
'yeccgoto_\'ExtendsTag\''(107) -> 13;
'yeccgoto_\'ExtendsTag\''(112) -> 13;
'yeccgoto_\'ExtendsTag\''(117) -> 13;
'yeccgoto_\'ExtendsTag\''(125) -> 13;
'yeccgoto_\'ExtendsTag\''(128) -> 13;
'yeccgoto_\'ExtendsTag\''(134) -> 13;
'yeccgoto_\'ExtendsTag\''(137) -> 13;
'yeccgoto_\'ExtendsTag\''(143) -> 13.

'yeccgoto_\'Filter\''(34) -> 35.

'yeccgoto_\'ForBlock\''(1) -> 12;
'yeccgoto_\'ForBlock\''(97) -> 12;
'yeccgoto_\'ForBlock\''(102) -> 12;
'yeccgoto_\'ForBlock\''(107) -> 12;
'yeccgoto_\'ForBlock\''(112) -> 12;
'yeccgoto_\'ForBlock\''(117) -> 12;
'yeccgoto_\'ForBlock\''(125) -> 12;
'yeccgoto_\'ForBlock\''(128) -> 12;
'yeccgoto_\'ForBlock\''(134) -> 12;
'yeccgoto_\'ForBlock\''(137) -> 12;
'yeccgoto_\'ForBlock\''(143) -> 12.

'yeccgoto_\'ForBraced\''(1) -> 11;
'yeccgoto_\'ForBraced\''(97) -> 11;
'yeccgoto_\'ForBraced\''(102) -> 11;
'yeccgoto_\'ForBraced\''(107) -> 11;
'yeccgoto_\'ForBraced\''(112) -> 11;
'yeccgoto_\'ForBraced\''(117) -> 11;
'yeccgoto_\'ForBraced\''(125) -> 11;
'yeccgoto_\'ForBraced\''(128) -> 11;
'yeccgoto_\'ForBraced\''(134) -> 11;
'yeccgoto_\'ForBraced\''(137) -> 11;
'yeccgoto_\'ForBraced\''(143) -> 11.

'yeccgoto_\'ForExpression\''(45) -> 77.

'yeccgoto_\'ForGroup\''(45) -> 76.

'yeccgoto_\'IfBlock\''(1) -> 10;
'yeccgoto_\'IfBlock\''(97) -> 10;
'yeccgoto_\'IfBlock\''(102) -> 10;
'yeccgoto_\'IfBlock\''(107) -> 10;
'yeccgoto_\'IfBlock\''(112) -> 10;
'yeccgoto_\'IfBlock\''(117) -> 10;
'yeccgoto_\'IfBlock\''(125) -> 10;
'yeccgoto_\'IfBlock\''(128) -> 10;
'yeccgoto_\'IfBlock\''(134) -> 10;
'yeccgoto_\'IfBlock\''(137) -> 10;
'yeccgoto_\'IfBlock\''(143) -> 10.

'yeccgoto_\'IfBraced\''(1) -> 9;
'yeccgoto_\'IfBraced\''(97) -> 9;
'yeccgoto_\'IfBraced\''(102) -> 9;
'yeccgoto_\'IfBraced\''(107) -> 9;
'yeccgoto_\'IfBraced\''(112) -> 9;
'yeccgoto_\'IfBraced\''(117) -> 9;
'yeccgoto_\'IfBraced\''(125) -> 9;
'yeccgoto_\'IfBraced\''(128) -> 9;
'yeccgoto_\'IfBraced\''(134) -> 9;
'yeccgoto_\'IfBraced\''(137) -> 9;
'yeccgoto_\'IfBraced\''(143) -> 9.

'yeccgoto_\'IfEqualBlock\''(1) -> 8;
'yeccgoto_\'IfEqualBlock\''(97) -> 8;
'yeccgoto_\'IfEqualBlock\''(102) -> 8;
'yeccgoto_\'IfEqualBlock\''(107) -> 8;
'yeccgoto_\'IfEqualBlock\''(112) -> 8;
'yeccgoto_\'IfEqualBlock\''(117) -> 8;
'yeccgoto_\'IfEqualBlock\''(125) -> 8;
'yeccgoto_\'IfEqualBlock\''(128) -> 8;
'yeccgoto_\'IfEqualBlock\''(134) -> 8;
'yeccgoto_\'IfEqualBlock\''(137) -> 8;
'yeccgoto_\'IfEqualBlock\''(143) -> 8.

'yeccgoto_\'IfEqualBraced\''(1) -> 7;
'yeccgoto_\'IfEqualBraced\''(97) -> 7;
'yeccgoto_\'IfEqualBraced\''(102) -> 7;
'yeccgoto_\'IfEqualBraced\''(107) -> 7;
'yeccgoto_\'IfEqualBraced\''(112) -> 7;
'yeccgoto_\'IfEqualBraced\''(117) -> 7;
'yeccgoto_\'IfEqualBraced\''(125) -> 7;
'yeccgoto_\'IfEqualBraced\''(128) -> 7;
'yeccgoto_\'IfEqualBraced\''(134) -> 7;
'yeccgoto_\'IfEqualBraced\''(137) -> 7;
'yeccgoto_\'IfEqualBraced\''(143) -> 7.

'yeccgoto_\'IfEqualExpression\''(48) -> 63.

'yeccgoto_\'IfExpression\''(47) -> 67;
'yeccgoto_\'IfExpression\''(68) -> 69.

'yeccgoto_\'IfNotEqualBlock\''(1) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(97) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(102) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(107) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(112) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(117) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(125) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(128) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(134) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(137) -> 6;
'yeccgoto_\'IfNotEqualBlock\''(143) -> 6.

'yeccgoto_\'IfNotEqualBraced\''(1) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(97) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(102) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(107) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(112) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(117) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(125) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(128) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(134) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(137) -> 5;
'yeccgoto_\'IfNotEqualBraced\''(143) -> 5.

'yeccgoto_\'IfNotEqualExpression\''(49) -> 59.

'yeccgoto_\'IncludeTag\''(1) -> 4;
'yeccgoto_\'IncludeTag\''(97) -> 4;
'yeccgoto_\'IncludeTag\''(102) -> 4;
'yeccgoto_\'IncludeTag\''(107) -> 4;
'yeccgoto_\'IncludeTag\''(112) -> 4;
'yeccgoto_\'IncludeTag\''(117) -> 4;
'yeccgoto_\'IncludeTag\''(125) -> 4;
'yeccgoto_\'IncludeTag\''(128) -> 4;
'yeccgoto_\'IncludeTag\''(134) -> 4;
'yeccgoto_\'IncludeTag\''(137) -> 4;
'yeccgoto_\'IncludeTag\''(143) -> 4.

'yeccgoto_\'Literal\''(24) -> 28;
'yeccgoto_\'Literal\''(37) -> 38;
'yeccgoto_\'Literal\''(47) -> 28;
'yeccgoto_\'Literal\''(48) -> 28;
'yeccgoto_\'Literal\''(49) -> 28;
'yeccgoto_\'Literal\''(59) -> 28;
'yeccgoto_\'Literal\''(63) -> 28;
'yeccgoto_\'Literal\''(68) -> 28;
'yeccgoto_\'Literal\''(74) -> 28;
'yeccgoto_\'Literal\''(81) -> 28;
'yeccgoto_\'Literal\''(90) -> 28.

'yeccgoto_\'LoadNames\''(51) -> 52.

'yeccgoto_\'LoadTag\''(1) -> 3;
'yeccgoto_\'LoadTag\''(97) -> 3;
'yeccgoto_\'LoadTag\''(102) -> 3;
'yeccgoto_\'LoadTag\''(107) -> 3;
'yeccgoto_\'LoadTag\''(112) -> 3;
'yeccgoto_\'LoadTag\''(117) -> 3;
'yeccgoto_\'LoadTag\''(125) -> 3;
'yeccgoto_\'LoadTag\''(128) -> 3;
'yeccgoto_\'LoadTag\''(134) -> 3;
'yeccgoto_\'LoadTag\''(137) -> 3;
'yeccgoto_\'LoadTag\''(143) -> 3.

'yeccgoto_\'Value\''(24) -> 27;
'yeccgoto_\'Value\''(47) -> 66;
'yeccgoto_\'Value\''(48) -> 62;
'yeccgoto_\'Value\''(49) -> 58;
'yeccgoto_\'Value\''(59) -> 60;
'yeccgoto_\'Value\''(63) -> 64;
'yeccgoto_\'Value\''(68) -> 66;
'yeccgoto_\'Value\''(74) -> 75;
'yeccgoto_\'Value\''(81) -> 83;
'yeccgoto_\'Value\''(90) -> 91.

'yeccgoto_\'ValueBraced\''(1) -> 2;
'yeccgoto_\'ValueBraced\''(97) -> 2;
'yeccgoto_\'ValueBraced\''(102) -> 2;
'yeccgoto_\'ValueBraced\''(107) -> 2;
'yeccgoto_\'ValueBraced\''(112) -> 2;
'yeccgoto_\'ValueBraced\''(117) -> 2;
'yeccgoto_\'ValueBraced\''(125) -> 2;
'yeccgoto_\'ValueBraced\''(128) -> 2;
'yeccgoto_\'ValueBraced\''(134) -> 2;
'yeccgoto_\'ValueBraced\''(137) -> 2;
'yeccgoto_\'ValueBraced\''(143) -> 2.

'yeccgoto_\'Variable\''(24) -> 26;
'yeccgoto_\'Variable\''(47) -> 26;
'yeccgoto_\'Variable\''(48) -> 26;
'yeccgoto_\'Variable\''(49) -> 26;
'yeccgoto_\'Variable\''(59) -> 26;
'yeccgoto_\'Variable\''(63) -> 26;
'yeccgoto_\'Variable\''(68) -> 26;
'yeccgoto_\'Variable\''(74) -> 26;
'yeccgoto_\'Variable\''(81) -> 82;
'yeccgoto_\'Variable\''(90) -> 26.

-compile({inline,{yeccpars2_0_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_0_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 130).
yeccpars2_2_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 133).
yeccpars2_3_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 132).
yeccpars2_4_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_5_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 138).
yeccpars2_6_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_7_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 137).
yeccpars2_8_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_9_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 136).
yeccpars2_10_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_11_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 135).
yeccpars2_12_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 131).
yeccpars2_13_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 141).
yeccpars2_14_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_15_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 140).
yeccpars2_16_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 143).
yeccpars2_17_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 142).
yeccpars2_18_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_19_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 134).
yeccpars2_20_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_21_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 139).
yeccpars2_22_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 129).
yeccpars2_25_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 151).
yeccpars2_29_([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 145).
yeccpars2_32_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 147).
yeccpars2_35_([__3,__2,__1 | Stack]) ->
 [begin
   { apply_filter , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 201).
yeccpars2_36_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 202).
yeccpars2_38_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 152).
yeccpars2_39_([__3,__2,__1 | Stack]) ->
 [begin
   { attribute , { __3 , __1 } }
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 209).
yeccpars2_46_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 158).
yeccpars2_53_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 157).
yeccpars2_54_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { load , __3 }
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 159).
yeccpars2_55_([__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __2 ]
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 155).
yeccpars2_57_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { include , __3 }
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 193).
yeccpars2_61_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 187).
yeccpars2_65_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 179).
yeccpars2_69_([__2,__1 | Stack]) ->
 [begin
   { 'not' , __2 }
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 178).
yeccpars2_70_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 207).
yeccpars2_72_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { tag , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 210).
yeccpars2_75_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ { __2 , __4 } ]
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 173).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 170).
yeccpars2_79_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_82_close_tag,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 172).
yeccpars2_82_close_tag([__3,__2,__1 | Stack]) ->
 [begin
   { in , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 174).
yeccpars2_84_([__3,__2,__1 | Stack]) ->
 [begin
   __1 ++ [ __3 ]
  end | Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 154).
yeccpars2_86_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { extends , __3 }
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_87_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 212).
yeccpars2_89_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { call , __3 }
  end | Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 213).
yeccpars2_92_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { call , __3 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 162).
yeccpars2_94_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 198).
yeccpars2_96_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 197).
yeccpars2_98_([__3,__2,__1 | Stack]) ->
 [begin
   { autoescape , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_101_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 161).
yeccpars2_103_([__3,__2,__1 | Stack]) ->
 [begin
   { block , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_106_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 165).
yeccpars2_108_([__3,__2,__1 | Stack]) ->
 [begin
   { comment , __2 }
  end | Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_111_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 169).
yeccpars2_113_([__3,__2,__1 | Stack]) ->
 [begin
   { for , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_116_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 177).
yeccpars2_118_([__3,__2,__1 | Stack]) ->
 [begin
   { 'if' , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_119_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_123_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_124_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 176).
yeccpars2_126_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 186).
yeccpars2_129_([__3,__2,__1 | Stack]) ->
 [begin
   { ifequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_130_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_133_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_133_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 185).
yeccpars2_135_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifequalelse , __1 , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_138_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 192).
yeccpars2_138_([__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequal , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 128).
yeccpars2_139_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 0).
yeccpars2_142_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("src/erlydtl/erlydtl_parser.yrl", 191).
yeccpars2_144_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { ifnotequalelse , __1 , __2 , __4 }
  end | Stack].


