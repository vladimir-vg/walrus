-module(walrus).
-author("Devin Torres <devin@devintorres.com>").
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, render/2, render/3]).

-define(DEFAULT_VALUE, "").
-define(is_falsy(V),
    (V =:= false orelse V =:= ?DEFAULT_VALUE orelse V =:= undefined orelse V =:= null)).

compile(Template) when is_binary(Template) ->
    compile(binary_to_list(Template));
compile(Template) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    {walrus_template, ParseTree}.


render({walrus_template, ParseTree}, Context) ->
    render({walrus_template, ParseTree}, Context, {}).
render({walrus_template, ParseTree}, Context, PartialsContext) ->
    IOList = render_iolist(ParseTree, Context, [Context], PartialsContext, []),
    iolist_to_binary(IOList).


render_iolist([{text, Text} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Text | Acc]);
render_iolist([{var, Key} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    Value = get(Key, ContextList),
    Text = stringify(Value, TopContext, true),
    render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Text | Acc]);
render_iolist([{var_unescaped, Key} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    Value = get(Key, ContextList),
    Text = stringify(Value, TopContext, false),
    render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Text | Acc]);
render_iolist([{block, Key, SubParseTree} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    Value = get(Key, ContextList),
    case Value of
        Val when ?is_falsy(Val) -> %% just skip
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, Acc);
        List when is_list(List) ->
            Tmpl = [render_iolist(SubParseTree, TopContext, [SubContext | ContextList], PartialsContext, [])
                    || SubContext <- List],
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Tmpl | Acc]);
        {bson, Document} ->
            Tmpl = render_iolist(SubParseTree, TopContext, [{bson, Document} | ContextList], PartialsContext, []),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Tmpl | Acc]);
        {proplist, List} ->
            Tmpl = render_iolist(SubParseTree, TopContext, [{proplist, List} | ContextList], PartialsContext, []),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Tmpl | Acc]);
        _ ->
            Tmpl = render_iolist(SubParseTree, TopContext, ContextList, PartialsContext, []),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Tmpl | Acc])
    end;
render_iolist([{inverse, Key, SubParseTree} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    Value = get(Key, ContextList),
    case Value of
        Val when ?is_falsy(Val) ->
            Tmpl = render_iolist(SubParseTree, TopContext, ContextList, PartialsContext, []),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Tmpl | Acc]);
        _ ->
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, Acc)
    end;
render_iolist([{partial, Key} | ParseTree], TopContext, ContextList, PartialsContext, Acc) ->
    Value = get(Key, [PartialsContext]),
    case Value of
        Fun when is_function(Fun, 1) ->
            Text = Fun(TopContext),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Text | Acc]);
        Fun when is_function(Fun, 2) ->
            Text = Fun(TopContext, PartialsContext),
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, [Text | Acc]);
        ?DEFAULT_VALUE ->
            render_iolist(ParseTree, TopContext, ContextList, PartialsContext, Acc)
    end;
render_iolist([], _TopContext, _ContextList, _PartialsContext, Acc) ->
    lists:reverse(Acc).

get(Key, [{bson, Document} | ContextList]) ->
    case bson:lookup(Key, Document) of
        {Value} -> Value;
        {}      -> get(Key, ContextList)
    end;
get(Key, [{proplist, List} | ContextList]) ->
    case proplists:get_value(Key, List) of
        undefined -> get(Key, ContextList);
        Value     -> Value
    end;
get(_Key, []) ->
    ?DEFAULT_VALUE.

stringify(Value, _Context, false) when is_list(Value) ->
    Value;
stringify(Value, _Context, true) when is_list(Value) ->
    escape(Value);
stringify(Value, _Context, false) when is_binary(Value) ->
    binary_to_list(Value);
stringify(Value, _Context, true) when is_binary(Value) ->
    escape(binary_to_list(Value));
stringify(Value, _Context, _Escape) when is_integer(Value) ->
    integer_to_list(Value);
stringify(Value, _Context, _Escape) when is_float(Value) ->
    walrus_mochinum:digits(Value);
stringify(Value, _Context, false) when is_atom(Value) ->
    atom_to_list(Value);
stringify(Value, _Context, true) when is_atom(Value) ->
    escape(atom_to_list(Value));
stringify(Value, Context, Escape) when is_function(Value) ->
    stringify(Value(Context), Context, Escape).

escape(Value) ->
    escape(Value, []).

escape([$< | Tail], Acc) ->
    escape(Tail, [<<"&lt;">> | Acc]);
escape([$> | Tail], Acc) ->
    escape(Tail, [<<"&gt;">> | Acc]);
escape([$& | Tail], Acc) ->
    escape(Tail, [<<"&amp;">> | Acc]);
escape([C | Tail], Acc) ->
    escape(Tail, [C | Acc]);
escape([], Acc) ->
    lists:reverse(Acc).
