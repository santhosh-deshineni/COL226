structure PL0LrVals = PL0LrValsFun(
structure Token = LrParser.Token);
structure PL0Lex = PL0LexFun(
structure Tokens = PL0LrVals.Tokens);
structure PL0Parser = Join(
structure ParserData = PL0LrVals.ParserData
structure Lex=PL0Lex
structure LrParser=LrParser);

structure CalcLrVals = CalcLrValsFun(
structure Token = LrParser.Token);
structure CalcLex = CalcLexFun(
structure Tokens = CalcLrVals.Tokens);
structure CalcParser = Join(
structure ParserData = CalcLrVals.ParserData
structure Lex=CalcLex
structure LrParser=LrParser);