open Rational;

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")
%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z];
digits=[0-9];
ws = [\ \t];
integ = [~]?{digits}+;
decim = [~]?{digits}*"."{digits}*"("{digits}+")";
fract = [~]?{digits}"/"{digits};
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{integ} => (Tokens.INTEG (yytext,!pos,!pos));
{decim} => (Tokens.DECIM(yytext,!pos,!pos));
{fract} => (Tokens.FRACT(yytext,!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"="      => (Tokens.EQUAL(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"("      => (Tokens.LPAR(!pos,!pos));
")"      => (Tokens.RPAR(!pos,!pos));
_      => (error ("ignoring bad character "^yytext,!pos,!pos);lex());