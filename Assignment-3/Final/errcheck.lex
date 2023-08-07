datatype lexresult= T1 | T2 | F | EOF

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => (EOF)
%%
%structure StringLex
digit=[0-9];
sign = [~+];
%%
{sign}?{digit}+"\n" => (T1);
{sign}?{digit}*"."{digit}*"("{digit}+")\n" => (T2);
.        => (F);
