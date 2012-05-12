-module(test_helpers).

-export([read_examples/1, run_examples/1]).

read_examples(Path) ->
    {ok, Content} = file:read_file(Path),
    {ok, {List}} = json:decode(Content),
    Tests = proplists:get_value(<<"tests">>, List),
    json_to_proplist(Tests).

run_examples(Examples) ->
    lists:foreach(fun ({proplist, Example}) ->
        Desc = proplists:get_value(desc, Example),
        Data = proplists:get_value(data, Example),
        Expected = proplists:get_value(expected, Example),
        Template = proplists:get_value(template, Example),
        
        io:format("~s~n", [Desc]),
        WalrusTemplate = walrus:compile(Template),
        Got = walrus:render(WalrusTemplate, Data),
        if Got =/= Expected ->
            io:format("  Got ~w~n", [Got]),
            io:format("  Expected: ~w~n", [Expected]);
           true             -> io:format("pass~n", [])
        end
    end, Examples).

json_to_proplist(List) when is_list(List) ->
    lists:map(fun json_to_proplist/1, List);
json_to_proplist({PropList}) when is_list(PropList) ->
    Result = lists:map(fun ({Key, Value}) ->
        {list_to_atom(binary_to_list(Key)), json_to_proplist(Value)}
    end, PropList),
    {proplist, Result};
json_to_proplist(Binary) when is_binary(Binary) ->
    Binary.