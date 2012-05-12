-module(walrus).
-author("Devin Torres <devin@devintorres.com>").
-author("Gordeev Vladimir <gordeev.vladimir.v@gmail.com>").

-export([compile/1, render/2, render/3]).

-define(DEFAULT_VALUE, "").
-define(is_falsy(V),
    (V =:= false orelse V =:= ?DEFAULT_VALUE orelse V =:= undefined orelse V =:= null)).

-record(state, {top, contexts, partials}).

compile(Template) when is_binary(Template) ->
    compile(binary_to_list(Template));
compile(Template) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    {walrus_template, ParseTree}.

render({walrus_template, ParseTree}, Context) ->
    render({walrus_template, ParseTree}, Context, {}).
render({walrus_template, ParseTree}, Context, PartialsContext) ->
    State = #state{top=Context, contexts=[Context], partials=PartialsContext},
    IOList = render_iolist(ParseTree, State, []),
    iolist_to_binary(IOList).

render_iolist([{text, Text} | ParseTree], State, Acc) ->
    render_iolist(ParseTree, State, [Text | Acc]);
render_iolist([{var, Key} | ParseTree], State, Acc) ->
    Value = get(Key, State#state.contexts),
    Text = stringify(Value, State#state.top, true),
    render_iolist(ParseTree, State, [Text | Acc]);
render_iolist([{var_unescaped, Key} | ParseTree], State, Acc) ->
    Value = get(Key, State#state.contexts),
    Text = stringify(Value, State#state.top, false),
    render_iolist(ParseTree, State, [Text | Acc]);
render_iolist([{block, Key, SubParseTree} | ParseTree], State, Acc) ->
    Value = get(Key, State#state.contexts),
    case Value of
        Val when ?is_falsy(Val) -> %% just skip
            render_iolist(ParseTree, State, Acc);
        List when is_list(List) ->
            Contexts = State#state.contexts,
            Text = [render_iolist(SubParseTree, State#state{contexts=[SubContext | Contexts]}, [])
                    || SubContext <- List],
            render_iolist(ParseTree, State, [Text | Acc]);
        {bson, Document} ->
            Contexts = State#state.contexts,
            Text = render_iolist(SubParseTree, State#state{contexts=[{bson, Document} | Contexts]}, []),
            render_iolist(ParseTree, State, [Text | Acc]);
        {proplist, List} ->
            Contexts = State#state.contexts,
            Text = render_iolist(SubParseTree, State#state{contexts=[{proplist, List} | Contexts]}, []),
            render_iolist(ParseTree, State, [Text | Acc]);
        _ ->
            Text = render_iolist(SubParseTree, State, []),
            render_iolist(ParseTree, State, [Text | Acc])
    end;
render_iolist([{inverse, Key, SubParseTree} | ParseTree], State, Acc) ->
    Value = get(Key, State#state.contexts),
    case Value of
        Val when ?is_falsy(Val) ->
            Text = render_iolist(SubParseTree, State, []),
            render_iolist(ParseTree, State, [Text | Acc]);
        _ ->
            render_iolist(ParseTree, State, Acc)
    end;
render_iolist([{partial, Key} | ParseTree], State, Acc) ->
    Value = get(Key, [State#state.partials]),
    case Value of
        Fun when is_function(Fun, 1) ->
            Text = Fun(State#state.top),
            render_iolist(ParseTree, State, [Text | Acc]);
        Fun when is_function(Fun, 2) ->
            Text = Fun(State#state.top, State#state.partials),
            render_iolist(ParseTree, State, [Text | Acc]);
        ?DEFAULT_VALUE ->
            render_iolist(ParseTree, State, Acc)
    end;
render_iolist([], _State, Acc) ->
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
