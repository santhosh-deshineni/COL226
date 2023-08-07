structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => ((pos:=0);Tokens.EOF(!pos,!pos))
%%
%header (functor PL0LexFun(structure Tokens: PL0_TOKENS));
alpha=[A-Za-z];
alphanum=[A-Za-z0-9];
digits=[0-9];
ws = [\ \t];
any = ([^*] | "*"+ [^")"]);
integ = {digits}+;
rational = {digits}*"."{digits}*"("{digits}+")";
id = {alpha}({alphanum}*);
%%
"(*"{any}*"*)"=> (lex());
\n            => (pos := (!pos) + 1; lex());
{ws}+         => (lex());

"rational"    => (Tokens.RATDEC (!pos,!pos));
"integer"     => (Tokens.INTDEC (!pos,!pos));
"boolean"     => (Tokens.BOOLDEC (!pos,!pos));
"tt"          => (Tokens.TRUE (!pos,!pos));
"ff"          => (Tokens.FALSE (!pos,!pos));
"if"          => (Tokens.IF (!pos,!pos));
"then"        => (Tokens.THEN (!pos,!pos));
"else"        => (Tokens.ELSE (!pos,!pos));
"fi"          => (Tokens.FI (!pos,!pos));
"while"       => (Tokens.WHILE (!pos,!pos));
"do"          => (Tokens.DO (!pos,!pos));
"od"          => (Tokens.OD (!pos,!pos));
"procedure"   => (Tokens.PROC (!pos,!pos));
"print"       => (Tokens.PRINT (!pos,!pos));
"read"        => (Tokens.READ (!pos,!pos));
"call"        => (Tokens.CALL (!pos,!pos));

"make_rat"    => (Tokens.MAKERAT (!pos,!pos));
"rat"         => (Tokens.RAT (!pos,!pos));
"showRat"     => (Tokens.SHOWRAT (!pos,!pos));
"showDecimal" => (Tokens.SHOWDEC (!pos,!pos));
"fromDecimal" => (Tokens.FROMDEC (!pos,!pos));
"toDecimal"   => (Tokens.TODEC (!pos,!pos));

"inverse"     => (Tokens.INV (!pos,!pos));
"~"           => (Tokens.NEG (!pos,!pos));
"!"           => (Tokens.BOOLNEG (!pos,!pos));

{integ}       => (Tokens.INTEG (yytext,!pos,!pos));
{rational}    => (Tokens.DECIM (yytext,!pos,!pos));
{id}          => (Tokens.ID (yytext,!pos,!pos));

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

":="          => (Tokens.ASSIGN (!pos,!pos));
"<="          => (Tokens.LESSOREQ (!pos,!pos));
">="          => (Tokens.GREATOREQ (!pos,!pos));
"<"           => (Tokens.LESS (!pos,!pos));
">"           => (Tokens.GREAT (!pos,!pos));
"="           => (Tokens.EQUAL (!pos,!pos));
"<>"          => (Tokens.NOTEQUAL (!pos,!pos));

"("           => (Tokens.LPAR (!pos,!pos));
")"           => (Tokens.RPAR (!pos,!pos));
"{"           => (Tokens.LCURL (!pos,!pos));
"}"           => (Tokens.RCURL (!pos,!pos));

";"           => (Tokens.SEMI (!pos,!pos));
","           => (Tokens.COMMA (!pos,!pos));