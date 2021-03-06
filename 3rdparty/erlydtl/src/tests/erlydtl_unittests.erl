-module(erlydtl_unittests).

-export([run_tests/0]).

tests() ->
    [
        {"comment", [
                {"Comment block is excised",
                    <<"Bob {% comment %}(moron){% endcomment %} Loblaw">>,
                    [], <<"Bob  Loblaw">>},
                {"Inline comment is excised",
                    <<"You're {# not #} a very nice person">>,
                    [], <<"You're  a very nice person">>}
            ]},
        {"autoescape", [
                {"Autoescape works",
                    <<"{% autoescape on %}{{ var1 }}{% endautoescape %}">>,
                    [{var1, "<b>bold</b>"}], <<"&lt;b&gt;bold&lt;/b&gt;">>},
                {"Nested autoescape",
                    <<"{% autoescape on %}{{ var1 }}{% autoescape off %}{{ var1 }}{% endautoescape %}{% endautoescape %}">>,
                    [{var1, "<b>"}], <<"&lt;b&gt;<b>">>}
            ]},
        {"string literal", [
                {"Render literal",
                    <<"{{ \"foo\" }} is my name">>, [], <<"foo is my name">>},
                {"Newlines are escaped",
                    <<"{{ \"foo\\n\" }}">>, [], <<"foo\n">>}
            ]},
        {"number literal", [
                {"Render integer",
                    <<"{{ 5 }}">>, [], <<"5">>}
            ]},
        {"variable", [
                {"Render variable",
                    <<"{{ var1 }} is my game">>, [{var1, "bar"}], <<"bar is my game">>},
                {"Render variable with attribute",
                    <<"I enjoy {{ var1.game }}">>, [{var1, [{game, "Othello"}]}], <<"I enjoy Othello">>},
                {"Render variable in dict",
                    <<"{{ var1 }}">>, dict:store(var1, "bar", dict:new()), <<"bar">>},
                {"Render variable in gb_tree",
                    <<"{{ var1 }}">>, gb_trees:insert(var1, "bar", gb_trees:empty()), <<"bar">>},
                {"Render variable with attribute in dict",
                    <<"{{ var1.attr }}">>, [{var1, dict:store(attr, "Othello", dict:new())}], <<"Othello">>},
                {"Render variable with attribute in gb_tree",
                    <<"{{ var1.attr }}">>, [{var1, gb_trees:insert(attr, "Othello", gb_trees:empty())}], <<"Othello">>},
                {"Render variable in parameterized module",
                    <<"{{ var1.some_var }}">>, [{var1, erlydtl_example_variable_storage:new("foo")}], <<"foo">>},
                {"Nested attributes",
                    <<"{{ person.city.state.country }}">>, [{person, [{city, [{state, [{country, "Italy"}]}]}]}],
                    <<"Italy">>}
            ]},
        {"if", [
                {"If/else",
                    <<"{% if var1 %}boo{% else %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
                {"If",
                    <<"{% if var1 %}boo{% endif %}">>, [{var1, ""}], <<>>},
                {"If not",
                    <<"{% if not var1 %}yay{% endif %}">>, [{var1, ""}], <<"yay">>},
                {"If \"0\"",
                    <<"{% if var1 %}boo{% endif %}">>, [{var1, "0"}], <<>>},
                {"If false",
                    <<"{% if var1 %}boo{% endif %}">>, [{var1, false}], <<>>},
                {"If undefined",
                    <<"{% if var1 %}boo{% endif %}">>, [{var1, undefined}], <<>>},
                {"If other atom",
                    <<"{% if var1 %}yay{% endif %}">>, [{var1, foobar}], <<"yay">>},
                {"If non-empty string",
                    <<"{% if var1 %}yay{% endif %}">>, [{var1, "hello"}], <<"yay">>},
                {"If proplist",
                    <<"{% if var1 %}yay{% endif %}">>, [{var1, [{foo, "bar"}]}], <<"yay">>}
            ]},
        {"for", [
                {"Simple loop",
                    <<"{% for x in list %}{{ x }}{% endfor %}">>, [{'list', ["1", "2", "3"]}],
                    <<"123">>},
                {"Expand list",
                    <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [["X", "1"], ["X", "2"]]}],
                    <<"X,1\nX,2\n">>},
                {"Expand tuple",
                    <<"{% for x, y in list %}{{ x }},{{ y }}\n{% endfor %}">>, [{'list', [{"X", "1"}, {"X", "2"}]}],
                    <<"X,1\nX,2\n">>},
                {"Resolve variable attribute",
                    <<"{% for number in person.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{numbers, ["411", "911"]}]}],
                    <<"411\n911\n">>},
                {"Resolve nested variable attribute",
                    <<"{% for number in person.home.numbers %}{{ number }}\n{% endfor %}">>, [{person, [{home, [{numbers, ["411", "911"]}]}]}],
                    <<"411\n911\n">>},
                {"Counter0",
                    <<"{% for number in numbers %}{{ forloop.counter0 }}. {{ number }}\n{% endfor %}">>, 
                    [{numbers, ["Zero", "One", "Two"]}], <<"0. Zero\n1. One\n2. Two\n">>},
                {"Counter",
                    <<"{% for number in numbers %}{{ forloop.counter }}. {{ number }}\n{% endfor %}">>, 
                    [{numbers, ["One", "Two", "Three"]}], <<"1. One\n2. Two\n3. Three\n">>},
                {"Reverse Counter0",
                    <<"{% for number in numbers %}{{ forloop.revcounter0 }}. {{ number }}\n{% endfor %}">>, 
                    [{numbers, ["Two", "One", "Zero"]}], <<"2. Two\n1. One\n0. Zero\n">>},
                {"Reverse Counter",
                    <<"{% for number in numbers %}{{ forloop.revcounter }}. {{ number }}\n{% endfor %}">>, 
                    [{numbers, ["Three", "Two", "One"]}], <<"3. Three\n2. Two\n1. One\n">>},
                {"Counter \"first\"",
                    <<"{% for number in numbers %}{% if forloop.first %}{{ number }}{% endif %}{% endfor %}">>,
                    [{numbers, ["One", "Two", "Three"]}], <<"One">>},
                {"Counter \"last\"",
                    <<"{% for number in numbers %}{% if forloop.last %}{{ number }}{% endif %}{% endfor %}">>,
                    [{numbers, ["One", "Two", "Three"]}], <<"Three">>},
                {"Nested for loop",
                    <<"{% for outer in list %}{% for inner in outer %}{{ inner }}\n{% endfor %}{% endfor %}">>,
                    [{'list', [["Al", "Albert"], ["Jo", "Joseph"]]}],
                    <<"Al\nAlbert\nJo\nJoseph\n">>},
                {"Access parent loop counters",
                    <<"{% for outer in list %}{% for inner in outer %}({{ forloop.parentloop.counter0 }}, {{ forloop.counter0 }})\n{% endfor %}{% endfor %}">>,
                    [{'list', [["One", "two"], ["One", "two"]]}], <<"(0, 0)\n(0, 1)\n(1, 0)\n(1, 1)\n">>}
            ]},
        {"ifequal", [
                {"Compare variable to literal",
                    <<"{% ifequal var1 \"foo\" %}yay{% endifequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare variable to unequal literal",
                    <<"{% ifequal var1 \"foo\" %}boo{% endifequal %}">>,
                    [{var1, "bar"}], <<>>},
                {"Compare literal to variable",
                    <<"{% ifequal \"foo\" var1 %}yay{% endifequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare literal to unequal variable",
                    <<"{% ifequal \"foo\" var1 %}boo{% endifequal %}">>,
                    [{var1, "bar"}], <<>>}
            ]},
        {"ifequal/else", [
                {"Compare variable to literal",
                    <<"{% ifequal var1 \"foo\" %}yay{% else %}boo{% endifequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare variable to unequal literal",
                    <<"{% ifequal var1 \"foo\" %}boo{% else %}yay{% endifequal %}">>,
                    [{var1, "bar"}], <<"yay">>},
                {"Compare literal to variable",
                    <<"{% ifequal \"foo\" var1 %}yay{% else %}boo{% endifequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare literal to unequal variable",
                    <<"{% ifequal \"foo\" var1 %}boo{% else %}yay{% endifequal %}">>,
                    [{var1, "bar"}], <<"yay">>}
            ]},
        {"ifnotequal", [
                {"Compare variable to literal",
                    <<"{% ifnotequal var1 \"foo\" %}boo{% endifnotequal %}">>,
                    [{var1, "foo"}], <<>>},
                {"Compare variable to unequal literal",
                    <<"{% ifnotequal var1 \"foo\" %}yay{% endifnotequal %}">>,
                    [{var1, "bar"}], <<"yay">>},
                {"Compare literal to variable",
                    <<"{% ifnotequal \"foo\" var1 %}boo{% endifnotequal %}">>,
                    [{var1, "foo"}], <<>>},
                {"Compare literal to unequal variable",
                    <<"{% ifnotequal \"foo\" var1 %}yay{% endifnotequal %}">>,
                    [{var1, "bar"}], <<"yay">>}
            ]},
        {"ifnotequal/else", [
                {"Compare variable to literal",
                    <<"{% ifnotequal var1 \"foo\" %}boo{% else %}yay{% endifnotequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare variable to unequal literal",
                    <<"{% ifnotequal var1 \"foo\" %}yay{% else %}boo{% endifnotequal %}">>,
                    [{var1, "bar"}], <<"yay">>},
                {"Compare literal to variable",
                    <<"{% ifnotequal \"foo\" var1 %}boo{% else %}yay{% endifnotequal %}">>,
                    [{var1, "foo"}], <<"yay">>},
                {"Compare literal to unequal variable",
                    <<"{% ifnotequal \"foo\" var1 %}yay{% else %}boo{% endifnotequal %}">>,
                    [{var1, "bar"}], <<"yay">>}
            ]},
        {"filters", [
                {"Filter a literal",
                    <<"{{ \"pop\"|capfirst }}">>, [],
                    <<"Pop">>},
                {"Filters applied in order",
                    <<"{{ var1|force_escape|length }}">>, [{var1, <<"&">>}],
                    <<"5">>},
                {"Escape is applied last",
                    <<"{{ var1|escape|linebreaksbr }}">>, [{var1, <<"\n">>}],
                    <<"&lt;br /&gt;">>},
                {"|capfirst",
                    <<"{{ var1|capfirst }}">>, [{var1, "dana boyd"}], 
                    <<"Dana boyd">>},
                {"|center:10",
                    <<"{{ var1|center:10 }}">>, [{var1, "MB"}], 
                    <<"    MB    ">>},
                {"|center:1",
                    <<"{{ var1|center:1 }}">>, [{var1, "KBR"}], 
                    <<"B">>},
                {"|escapejs",
                    <<"{{ var1|escapejs }}">>, [{var1, "Skip's \"Old-Timey\" Diner"}], 
                    <<"Skip\\'s \\\"Old-Timey\\\" Diner">>},
                {"|first",
                    <<"{{ var1|first }}">>, [{var1, "James"}], 
                    <<"J">>},
                {"|fix_ampersands",
                    <<"{{ var1|fix_ampersands }}">>, [{var1, "Ben & Jerry's"}], 
                    <<"Ben &amp; Jerry's">>},
                {"|force_escape",
                    <<"{{ var1|force_escape }}">>, [{var1, "Ben & Jerry's <=> \"The World's Best Ice Cream\""}],
                    <<"Ben &amp; Jerry&#039;s &lt;=&gt; &quot;The World&#039;s Best Ice Cream&quot;">>},
                {"|format_integer",
                    <<"{{ var1|format_integer }}">>, [{var1, 28}], <<"28">>},
                {"|join:\", \"",
                    <<"{{ var1|join:\", \" }}">>, [{var1, ["Liberte", "Egalite", "Fraternite"]}],
                    <<"Liberte, Egalite, Fraternite">>},
                {"|last",
                    <<"{{ var1|last }}">>, [{var1, "XYZ"}],
                    <<"Z">>},
                {"|length",
                    <<"{{ var1|length }}">>, [{var1, "antidisestablishmentarianism"}],
                    <<"28">>},
                {"|linebreaksbr",
                    <<"{{ var1|linebreaksbr }}">>, [{var1, "One\nTwo\n\nThree\n\n\n"}],
                    <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
                {"|linebreaksbr",
                    <<"{{ \"One\\nTwo\\n\\nThree\\n\\n\\n\"|linebreaksbr }}">>, [],
                    <<"One<br />Two<br /><br />Three<br /><br /><br />">>},
                {"|ljust:10",
                    <<"{{ var1|ljust:10 }}">>, [{var1, "Gore"}],
                    <<"Gore      ">>},
                {"|lower",
                    <<"{{ var1|lower }}">>, [{var1, "E. E. Cummings"}],
                    <<"e. e. cummings">>},
                {"|rjust:10",
                    <<"{{ var1|rjust:10 }}">>, [{var1, "Bush"}],
                    <<"      Bush">>},
                {"|plus:4",
                    <<"{{ one|plus:4 }}">>, [{one, "1"}],
                    <<"5">>},
                {"|upper",
                    <<"{{ message|upper }}">>, [{message, "That man has a gun."}],
                    <<"THAT MAN HAS A GUN.">>},
                {"|urlencode",
                    <<"{{ url|urlencode }}">>, [{url, "You #$*@!!"}],
                    <<"You+%23%24%2A%40%21%21">>}
            ]}
    ].

run_tests() ->
    Failures = lists:foldl(
        fun({Group, Assertions}, GroupAcc) ->
                io:format("Running test group ~p...~n", [Group]),
                lists:foldl(fun({Name, DTL, Vars, Output}, Acc) ->
                            case erlydtl_compiler:compile(DTL, erlydtl_running_test, []) of
                                {ok, _} ->
                                    {ok, IOList} = erlydtl_running_test:render(Vars),
                                    {ok, IOListBin} = erlydtl_running_test:render(vars_to_binary(Vars)),
                                    case {iolist_to_binary(IOList), iolist_to_binary(IOListBin)} of
                                        {Output, Output} ->
                                            Acc;
                                        {Output, Unexpected} ->
                                            [{Group, Name, 'binary', Unexpected, Output} | Acc];
                                        {Unexpected, Output} ->
                                            [{Group, Name, 'list', Unexpected, Output} | Acc];
                                        {Unexpected1, Unexpected2} ->
                                            [{Group, Name, 'list', Unexpected1, Output}, 
                                                {Group, Name, 'binary', Unexpected2, Output} | Acc]
                                    end;
                                Err ->
                                    [{Group, Name, Err} | Acc]
                            end
                    end, GroupAcc, Assertions)
        end, [], tests()),
    
    io:format("Failures: ~p~n", [Failures]),
    erlang:halt().

vars_to_binary(Vars) when is_list(Vars) ->
    lists:map(fun
            ({Key, [H|_] = Value}) when is_tuple(H) ->
                {Key, vars_to_binary(Value)};
            ({Key, [H|_] = Value}) when is_integer(H) ->
                {Key, list_to_binary(Value)};
            ({Key, Value}) ->
                {Key, Value}
        end, Vars);
vars_to_binary(Vars) ->
    Vars.
