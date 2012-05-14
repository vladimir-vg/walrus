Definitions.

Key = [a-zA-Z0-9_]+
WS = [\s\t]

Rules.

\r            : {token, {cr, TokenLine}}.
\n            : {token, {lf, TokenLine}}.
{{!([^{}]|(}[^}]))+}} : {token, {comment, TokenLine}}.
(([^{}\r\n])|({[^{])|(}[^}]))+ : {token,{text,TokenLine,?ltb(TokenChars)}}.
{WS}*{{       : {token, {'{{' ,TokenLine, ?left(TokenChars, TokenLen, 2)}}.
{WS}*{{#      : {token, {'{{#',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
{WS}*{{/      : {token, {'{{/',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
{WS}*{{\^     : {token, {'{{^',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
{WS}*{{{      : {token, {'{{{',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
{WS}*{{>      : {token, {'{{>',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
{WS}*{{&      : {token, {'{{&',TokenLine, ?left(TokenChars, TokenLen, 3)}}.
\s*{Key}\s*}} : {token, {key,TokenLine,?key(TokenChars,TokenLen)},"}}"}.
}}{WS}*       : {token, {'}}', TokenLine, ?right(TokenChars, TokenLen, 2)}}.
}}}{WS}*      : {token, {'}}}',TokenLine, ?right(TokenChars, TokenLen, 3)}}.

Erlang code.

-define(left(Chars, Len, N),
  list_to_binary(string:left(Chars, Len-N))).

-define(right(Chars, Len, N),
  list_to_binary(string:right(Chars, Len-N))).

-define(ltb(List), list_to_binary(List)).

-define(key(Chars, Len),
    list_to_atom(string:strip(string:left(Chars, Len-2)))).
