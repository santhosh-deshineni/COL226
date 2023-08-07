fun makeCounter() =
  let
    val countRef = ref 0
    fun counter(n: int) =
      if !countRef = 0 then
        (countRef := 1; valOf(TextIO.inputLine TextIO.stdIn))
      else
        ""
  in
    counter
  end;

structure CalcLrVals =
  CalcLrValsFun(structure Token = LrParser.Token)

structure CalcLex =
  CalcLexFun(structure Tokens = CalcLrVals.Tokens);

structure CalcParser =
  Join(structure LrParser = LrParser
       structure ParserData = CalcLrVals.ParserData
       structure Lex = CalcLex)
fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in CalcParser.parse(0,lexstream,print_error,())
    end

fun parse () = 
    let val lexer = CalcParser.makeLexer
                      (makeCounter())
        val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
        val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = CalcParser.Stream.get lexer
             in case result
                  of SOME r =>
                      TextIO.output(TextIO.stdOut,
                             "output: " ^ r ^ "\n")
                   | NONE => ();
                if CalcParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end