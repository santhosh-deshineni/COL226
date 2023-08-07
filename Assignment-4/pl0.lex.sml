functor PL0LexFun(structure Tokens: PL0_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => ((pos:=0);Tokens.EOF(!pos,!pos))


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\t",#"\t",1),
(#" ",#" ",1),
(#"\n",#"\n",2),
(#"!",#"!",3),
(#"%",#"%",4),
(#"&",#"&",5),
(#"(",#"(",6),
(#")",#")",7),
(#"*",#"*",8),
(#"+",#"+",9),
(#",",#",",10),
(#"-",#"-",11),
(#".",#".",12),
(#"/",#"/",13),
(#"0",#"9",14),
(#":",#":",15),
(#";",#";",16),
(#"<",#"<",17),
(#"=",#"=",18),
(#">",#">",19),
(#"A",#"Z",20),
(#"a",#"a",20),
(#"g",#"h",20),
(#"j",#"l",20),
(#"n",#"n",20),
(#"q",#"q",20),
(#"u",#"v",20),
(#"x",#"z",20),
(#"b",#"b",21),
(#"c",#"c",22),
(#"d",#"d",23),
(#"e",#"e",24),
(#"f",#"f",25),
(#"i",#"i",26),
(#"m",#"m",27),
(#"o",#"o",28),
(#"p",#"p",29),
(#"r",#"r",30),
(#"s",#"s",31),
(#"t",#"t",32),
(#"w",#"w",33),
(#"{",#"{",34),
(#"|",#"|",35),
(#"}",#"}",36),
(#"~",#"~",37)], []), ([(#"\t",#"\t",1),
(#" ",#" ",1)], [2]), ([], [1]), ([], [27]), ([], [39]), ([(#"&",#"&",156)], []), ([(#"*",#"*",150)], [49]), ([], [50]), ([], [37]), ([], [35]), ([], [54]), ([], [36]), ([(#"(",#"(",139),
(#"*",#"*",142),
(#"+",#"+",143),
(#"-",#"-",144),
(#"/",#"/",145),
(#"0",#"9",138)], []), ([], [38]), ([(#".",#".",138),
(#"0",#"9",14)], [28]), ([(#"=",#"=",137)], []), ([], [53]), ([(#"=",#"=",135),
(#">",#">",136)], [45]), ([], [47]), ([(#"=",#"=",134)], [46]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",128)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",125)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",124)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",121)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"e",20),
(#"g",#"h",20),
(#"j",#"q",20),
(#"s",#"z",20),
(#"f",#"f",109),
(#"i",#"i",110),
(#"r",#"r",111)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"e",20),
(#"g",#"m",20),
(#"o",#"z",20),
(#"f",#"f",97),
(#"n",#"n",98)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",90)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",89)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",78)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"d",20),
(#"f",#"z",20),
(#"a",#"a",68),
(#"e",#"e",69)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"z",20),
(#"h",#"h",55)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"n",20),
(#"p",#"s",20),
(#"u",#"z",20),
(#"h",#"h",43),
(#"o",#"o",44),
(#"t",#"t",45)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"g",20),
(#"i",#"z",20),
(#"h",#"h",39)], [30]), ([], [51]), ([(#"|",#"|",38)], []), ([], [52]), ([], [26]), ([], [41]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",40)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",41)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",42)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [12, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",53)], [30]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",46)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [6, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",47)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",48)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",49)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",50)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",51)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",52)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [24, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",54)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [9, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",56)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"v",20),
(#"x",#"z",20),
(#"w",#"w",57)], [30]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Q",20),
(#"S",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",58),
(#"R",#"R",59)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",62)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",60)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",61)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [21, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",63)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",64)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",65)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",66)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",67)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [22, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",72)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",70)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",71)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [17, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",73)], [20, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",74)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",75)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",76)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",77)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [3, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"n",20),
(#"p",#"z",20),
(#"i",#"i",79),
(#"o",#"o",80)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",87)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",81)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",82)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"c",20),
(#"e",#"z",20),
(#"d",#"d",83)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"t",20),
(#"v",#"z",20),
(#"u",#"u",84)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",85)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",86)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [15, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"z",20),
(#"t",#"t",88)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [16, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [14, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"j",20),
(#"l",#"z",20),
(#"k",#"k",91)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",92)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20),
(#"_",#"_",93)], [30]), ([(#"r",#"r",94)], []), ([(#"a",#"a",95)], []), ([(#"t",#"t",96)], []), ([], [19]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [8, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"s",20),
(#"u",#"u",20),
(#"w",#"z",20),
(#"t",#"t",99),
(#"v",#"v",100)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",105)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",101)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",102)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"r",20),
(#"t",#"z",20),
(#"s",#"s",103)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",104)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [25, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"f",20),
(#"h",#"z",20),
(#"g",#"g",106)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",107)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"q",20),
(#"s",#"z",20),
(#"r",#"r",108)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [4, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [7, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [11, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",112)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",113)], [30]), ([(#"0",#"9",20),
(#"A",#"C",20),
(#"E",#"Z",20),
(#"a",#"z",20),
(#"D",#"D",114)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",115)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"b",20),
(#"d",#"z",20),
(#"c",#"c",116)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"h",20),
(#"j",#"z",20),
(#"i",#"i",117)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"l",20),
(#"n",#"z",20),
(#"m",#"m",118)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",119)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",120)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [23, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"r",20),
(#"t",#"z",20),
(#"s",#"s",122)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",123)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [10, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [13, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",126)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",127)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [18, 30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"n",20),
(#"p",#"z",20),
(#"o",#"o",129)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"k",20),
(#"m",#"z",20),
(#"l",#"l",130)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"d",20),
(#"f",#"z",20),
(#"e",#"e",131)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"b",#"z",20),
(#"a",#"a",132)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"m",20),
(#"o",#"z",20),
(#"n",#"n",133)], [30]), ([(#"0",#"9",20),
(#"A",#"Z",20),
(#"a",#"z",20)], [5, 30]), ([], [44]), ([], [43]), ([], [48]), ([], [42]), ([(#"(",#"(",139),
(#"0",#"9",138)], []), ([(#"0",#"9",140)], []), ([(#")",#")",141),
(#"0",#"9",140)], []), ([], [29]), ([(#".",#".",149)], []), ([(#".",#".",148)], []), ([(#".",#".",147)], []), ([(#".",#".",146)], []), ([], [34]), ([], [32]), ([], [31]), ([], [33]), ([(#"\^@",#")",150),
(#"+",#"\255",150),
(#"*",#"*",151)], []), ([(#"\^@",#"!",150),
(#"#",#"(",150),
(#"+",#"\255",150),
(#")",#")",152),
(#"*",#"*",153)], []), ([], [0]), ([(#"\^@",#")",150),
(#"+",#"\255",150),
(#"*",#"*",154)], []), ([(#"\^@",#"(",150),
(#"+",#"\255",150),
(#")",#")",155),
(#"*",#"*",154)], []), ([(#"\^@",#")",150),
(#"+",#"\255",150),
(#"*",#"*",151)], [0]), ([], [40])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos) + 1; lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATDEC (!pos,!pos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTDEC (!pos,!pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLDEC (!pos,!pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TRUE (!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FALSE (!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF (!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN (!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE (!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FI (!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE (!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO (!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OD (!pos,!pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PROC (!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PRINT (!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.READ (!pos,!pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CALL (!pos,!pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MAKERAT (!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RAT (!pos,!pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWRAT (!pos,!pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWDEC (!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FROMDEC (!pos,!pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TODEC (!pos,!pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INV (!pos,!pos)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEG (!pos,!pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLNEG (!pos,!pos)))
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INTEG (yytext,!pos,!pos))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DECIM (yytext,!pos,!pos))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID (yytext,!pos,!pos))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPLUS (!pos,!pos)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSUB (!pos,!pos)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RTIMES (!pos,!pos)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RDIV (!pos,!pos)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS (!pos,!pos)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUB (!pos,!pos)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES (!pos,!pos)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV (!pos,!pos)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD (!pos,!pos)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND (!pos,!pos)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR (!pos,!pos)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN (!pos,!pos)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESSOREQ (!pos,!pos)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATOREQ (!pos,!pos)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESS (!pos,!pos)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREAT (!pos,!pos)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQUAL (!pos,!pos)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOTEQUAL (!pos,!pos)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAR (!pos,!pos)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAR (!pos,!pos)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LCURL (!pos,!pos)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RCURL (!pos,!pos)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI (!pos,!pos)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA (!pos,!pos)))
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
