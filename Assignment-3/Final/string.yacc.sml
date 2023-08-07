functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Sample interactive calculator for ML-Yacc *)
open Rational;
val globvarlist:string list ref = ref [];
val globratlist:rational list ref = ref [];

fun lookup2(x,chl1,chl2) =
  if (x = (hd chl1)) then
    (hd chl2)
  else
    lookup2(x,tl chl1,tl chl2)

fun lookup(x) =
  let
    val list1 = !globvarlist
    val list2 = !globratlist
  in
    lookup2(x,list1,list2)
  end

fun addvar(id, ration) =
  (globvarlist := id :: !globvarlist;globratlist := ration :: !globratlist;id ^ " assignment")


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\004\000\008\000\005\000\028\000\006\000\028\000\
\\009\000\007\000\010\000\006\000\011\000\005\000\012\000\004\000\000\000\
\\001\000\001\000\015\000\009\000\007\000\010\000\006\000\011\000\005\000\
\\012\000\004\000\000\000\
\\001\000\002\000\029\000\003\000\029\000\005\000\029\000\006\000\029\000\
\\007\000\029\000\008\000\029\000\013\000\029\000\000\000\
\\001\000\002\000\030\000\003\000\030\000\005\000\030\000\006\000\030\000\
\\007\000\030\000\008\000\030\000\013\000\030\000\000\000\
\\001\000\002\000\030\000\003\000\030\000\005\000\030\000\006\000\030\000\
\\007\000\030\000\008\000\030\000\014\000\017\000\000\000\
\\001\000\002\000\031\000\003\000\031\000\005\000\031\000\006\000\031\000\
\\007\000\031\000\008\000\031\000\013\000\031\000\000\000\
\\001\000\002\000\032\000\003\000\032\000\005\000\032\000\006\000\032\000\
\\007\000\032\000\008\000\032\000\013\000\032\000\000\000\
\\001\000\002\000\033\000\003\000\012\000\005\000\033\000\006\000\033\000\
\\007\000\011\000\008\000\033\000\013\000\033\000\000\000\
\\001\000\002\000\034\000\003\000\034\000\005\000\034\000\006\000\034\000\
\\007\000\034\000\008\000\034\000\013\000\034\000\000\000\
\\001\000\002\000\035\000\003\000\035\000\005\000\035\000\006\000\035\000\
\\007\000\035\000\008\000\035\000\013\000\035\000\000\000\
\\001\000\002\000\036\000\003\000\012\000\005\000\036\000\006\000\036\000\
\\007\000\011\000\008\000\036\000\013\000\036\000\000\000\
\\001\000\002\000\037\000\003\000\037\000\005\000\037\000\006\000\037\000\
\\007\000\037\000\008\000\037\000\013\000\037\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\005\000\025\000\006\000\025\000\
\\007\000\011\000\008\000\010\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\005\000\026\000\006\000\026\000\
\\007\000\011\000\008\000\010\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\005\000\027\000\006\000\027\000\
\\007\000\011\000\008\000\010\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\007\000\011\000\008\000\010\000\
\\013\000\022\000\000\000\
\\001\000\005\000\000\000\006\000\000\000\000\000\
\"
val actionRowNumbers =
"\000\000\013\000\001\000\002\000\
\\005\000\006\000\001\000\004\000\
\\001\000\001\000\001\000\001\000\
\\015\000\003\000\012\000\001\000\
\\010\000\009\000\008\000\007\000\
\\011\000\014\000\016\000"
val gotoT =
"\
\\001\000\001\000\002\000\022\000\000\000\
\\000\000\
\\001\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\000\000\
\\000\000\
\\001\000\016\000\000\000\
\\001\000\017\000\000\000\
\\001\000\018\000\000\000\
\\001\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 23
val numrules = 13
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INTEG of unit ->  (string) | DECIM of unit ->  (string)
 | FRACT of unit ->  (string) | ID of unit ->  (string)
 | START of unit ->  (string option) | EXP of unit ->  (rational)
end
type svalue = MlyValue.svalue
type result = string option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 3) => true | (T 4) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 0),nil
 $$ (T 3))::
(nil
,nil
 $$ (T 1))::
(nil
,nil
 $$ (T 2))::
(nil
,nil
 $$ (T 6))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "PLUS"
  | (T 2) => "TIMES"
  | (T 3) => "PRINT"
  | (T 4) => "SEMI"
  | (T 5) => "EOF"
  | (T 6) => "DIV"
  | (T 7) => "SUB"
  | (T 8) => "FRACT"
  | (T 9) => "DECIM"
  | (T 10) => "INTEG"
  | (T 11) => "LPAR"
  | (T 12) => "RPAR"
  | (T 13) => "EQUAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (SOME (showRat(EXP)))
end)
 in ( LrTable.NT 1, ( result, PRINT1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 in (SOME (showRat(EXP)))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.START (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (SOME (addvar(ID,EXP)))
end)
 in ( LrTable.NT 1, ( result, ID1left, EXP1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.START (fn _ => (NONE
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INTEG INTEG1, INTEG1left, INTEG1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (INTEG
 as INTEG1) = INTEG1 ()
 in (valOf(rat(BigInt.fromString(INTEG))))
end)
 in ( LrTable.NT 0, ( result, INTEG1left, INTEG1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (lookup(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.DECIM DECIM1, DECIM1left, DECIM1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECIM
 as DECIM1) = DECIM1 ()
 in (fromDecimal(DECIM))
end)
 in ( LrTable.NT 0, ( result, DECIM1left, DECIM1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.FRACT FRACT1, FRACT1left, FRACT1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (FRACT
 as FRACT1) = FRACT1 ()
 in (
let
                          fun count_till_slash(x,num) =
                          if String.substring(x,num,1) = "/" then
                            num
                          else
                            count_till_slash(x,num+1)
                        in
                            let
                              val slash_index = count_till_slash(FRACT,0)
                            in
                              valOf(make_rat(BigInt.fromString(String.substring(FRACT,0,slash_index)),BigInt.fromString(String.substring(FRACT,slash_index+1,String.size(FRACT)-slash_index-1))))
                            end
                        end
)
end)
 in ( LrTable.NT 0, ( result, FRACT1left, FRACT1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (add(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (multiply(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
let
                          val x = divide(EXP1,EXP2)
                        in
                          if (x = NONE) then
                            raise(rat_error)
                          else
                            valOf(x)
                        end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (subtract(EXP1,EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FRACT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.FRACT (fn () => i),p1,p2))
fun DECIM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.DECIM (fn () => i),p1,p2))
fun INTEG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.INTEG (fn () => i),p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
end
end
