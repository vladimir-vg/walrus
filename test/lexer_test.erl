-module(lexer_test).

-compile([export_all]).

text_test() ->
    {ok, [{text, _, <<"text">>}], _} = walrus_lexer:string("text").

newline_test() ->
    {ok, [{lf, _}, {text, _, <<"    ">>}], _} = walrus_lexer:string("\n    "),
    {ok, [{cr, _}, {lf, _}, {text, _, <<"  ">>}], _} = walrus_lexer:string("\r\n  ").

nl_text_nl_test() ->
    {ok, [{text, _, <<"text">>}, {cr, _}, {lf, _}, {text, _, <<"   again">>}], _}
        = walrus_lexer:string("text\r\n   again"),
    {ok, [{text, _, <<"text">>}, {cr, _}, {text, _, <<"  again">>}], _}
        = walrus_lexer:string("text\r  again").

text_var_text_test() ->
    {ok, [{text, _, <<"  left ">>},
          {'{{', _, <<>>}, {key, _, var}, {'}}', _, <<" ">>}, {text, _, <<"right">>}], _}
        = walrus_lexer:string("  left {{ var }} right"),
    {ok, [{text, _, <<"  left  ">>}, {cr, _}, {lf, _},
          {'{{', _, <<"  ">>}, {key, _, var}, {'}}', _, <<"  ">>}, {lf, _},
          {text, _, <<"  right">>}], _}
        = walrus_lexer:string("  left  \r\n  {{ var }}  \n  right").

standalone_var_test() ->
    {ok, [{text, _, <<"text: ">>}, {cr, _}, {lf, _},
          {'{{>', _, <<"\t">>}, {key, _, partial}, {'}}', _, <<>>}, {cr, _}], _}
        = walrus_lexer:string("text: \r\n\t{{> partial }}\r").

basic_test() ->
    Tmpl = "Hello {{name}}. Would you like a\n"
           "{{#over18}}beer{{/over18}}\n"
           "{{^over18}}juice box{{/over18}}?",
    Expected = {ok,[{text,1,<<"Hello ">>},
                    {'{{',1},
                    {key,1,name},
                    {'}}',1},
                    {text,1,<<". Would you like a\n">>},
                    {'{{#',2},
                    {key,2,over18},
                    {'}}',2},
                    {text,2,<<"beer">>},
                    {'{{/',2},
                    {key,2,over18},
                    {'}}',2},
                    {text,2,<<"\n">>},
                    {'{{^',3},
                    {key,3,over18},
                    {'}}',3},
                    {text,3,<<"juice box">>},
                    {'{{/',3},
                    {key,3,over18},
                    {'}}',3},
                    {text,3,<<"?">>}],3},
    Expected = walrus_lexer:string(Tmpl).
