structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z];
alphanum=[A-Za-z0-9];
digits=[0-9];
ws = [\ \t];
integ = {digits}+;
rational = {digits}*"."{digits}*"("{digits}+")";
id = {alpha}({alphanum}*);
%%
\n            => (pos := (!pos)+1;lex());
{ws}+         => (lex());

"tt"          => (Tokens.TRUE (!pos,!pos));
"ff"          => (Tokens.FALSE (!pos,!pos));

"make_rat"    => (Tokens.MAKERAT (!pos,!pos));
"fromDecimal" => (Tokens.FROMDEC (!pos,!pos));
"rat"         => (Tokens.RAT (!pos,!pos));
"inverse"     => (Tokens.INV (!pos,!pos));
"~"           => (Tokens.NEG (!pos,!pos));
"!"           => (Tokens.BOOLNEG (!pos,!pos));

{integ}       => (Tokens.INTEG (yytext,!pos,!pos));
{rational}    => (Tokens.DECIM (yytext,!pos,!pos));

".+."         => (Tokens.RPLUS (!pos,!pos));
".-."         => (Tokens.RSUB (!pos,!pos));
".*."         => (Tokens.RTIMES (!pos,!pos));
"./."         => (Tokens.RDIV (!pos,!pos));

"+"           => (Tokens.PLUS (!pos,!pos));
"-"           => (Tokens.SUB (!pos,!pos));
"*"           => (Tokens.TIMES (!pos,!pos));
"/"           => (Tokens.DIV (!pos,!pos));
"%"           => (Tokens.MOD (!pos,!pos));

"&&"          => (Tokens.AND (!pos,!pos));
"||"          => (Tokens.OR (!pos,!pos));

"<="          => (Tokens.LESSOREQ (!pos,!pos));
">="          => (Tokens.GREATOREQ (!pos,!pos));
"<"           => (Tokens.LESS (!pos,!pos));
">"           => (Tokens.GREAT (!pos,!pos));
"="           => (Tokens.EQUAL (!pos,!pos));
"<>"          => (Tokens.NOTEQUAL (!pos,!pos));

"("           => (Tokens.LPAR (!pos,!pos));
")"           => (Tokens.RPAR (!pos,!pos));

","           => (Tokens.COMMA (!pos,!pos));