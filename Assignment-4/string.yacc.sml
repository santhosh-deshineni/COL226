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
open DataTypes;
exception type_mismatch_error;

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\002\000\013\000\003\000\012\000\004\000\011\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\009\000\006\000\010\000\005\000\028\000\004\000\000\000\
\\001\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\029\000\056\000\000\000\
\\001\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\029\000\060\000\000\000\
\\001\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\029\000\061\000\000\000\
\\001\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\029\000\064\000\000\000\
\\001\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\030\000\062\000\000\000\
\\001\000\028\000\036\000\000\000\
\\001\000\028\000\037\000\000\000\
\\001\000\028\000\038\000\000\000\
\\001\000\031\000\000\000\000\000\
\\066\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\078\000\013\000\029\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\079\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\000\000\
\\080\000\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\000\000\
\\081\000\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\082\000\017\000\025\000\018\000\024\000\019\000\023\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\000\000\
\\087\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\000\000\
\\088\000\000\000\
\\089\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\090\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\091\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\092\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\093\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\\094\000\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\000\000\
\"
val actionRowNumbers =
"\000\000\010\000\000\000\011\000\
\\012\000\000\000\000\000\000\000\
\\006\000\007\000\008\000\014\000\
\\013\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\001\000\020\000\
\\019\000\018\000\000\000\000\000\
\\000\000\038\000\037\000\033\000\
\\035\000\034\000\036\000\031\000\
\\030\000\029\000\028\000\027\000\
\\026\000\025\000\024\000\023\000\
\\022\000\021\000\032\000\002\000\
\\003\000\005\000\016\000\017\000\
\\000\000\004\000\015\000\009\000"
val gotoT =
"\
\\001\000\001\000\002\000\063\000\000\000\
\\000\000\
\\001\000\030\000\000\000\
\\000\000\
\\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\001\000\042\000\000\000\
\\001\000\043\000\000\000\
\\001\000\044\000\000\000\
\\001\000\045\000\000\000\
\\001\000\046\000\000\000\
\\001\000\047\000\000\000\
\\001\000\048\000\000\000\
\\001\000\049\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\001\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\055\000\000\000\
\\001\000\056\000\000\000\
\\001\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 64
val numrules = 29
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
 | DECIM of unit ->  (string) | INTEG of unit ->  (string)
 | START of unit ->  (Exp) | Expression of unit ->  (string*Exp)
end
type svalue = MlyValue.svalue
type result = Exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 30) => true | _ => false
val showTerminal =
fn (T 0) => "TRUE"
  | (T 1) => "FALSE"
  | (T 2) => "MAKERAT"
  | (T 3) => "FROMDEC"
  | (T 4) => "RAT"
  | (T 5) => "INV"
  | (T 6) => "NEG"
  | (T 7) => "BOOLNEG"
  | (T 8) => "INTEG"
  | (T 9) => "DECIM"
  | (T 10) => "RPLUS"
  | (T 11) => "RSUB"
  | (T 12) => "RTIMES"
  | (T 13) => "RDIV"
  | (T 14) => "PLUS"
  | (T 15) => "SUB"
  | (T 16) => "TIMES"
  | (T 17) => "DIV"
  | (T 18) => "MOD"
  | (T 19) => "AND"
  | (T 20) => "OR"
  | (T 21) => "LESSOREQ"
  | (T 22) => "GREATOREQ"
  | (T 23) => "LESS"
  | (T 24) => "GREAT"
  | (T 25) => "EQUAL"
  | (T 26) => "NOTEQUAL"
  | (T 27) => "LPAR"
  | (T 28) => "RPAR"
  | (T 29) => "COMMA"
  | (T 30) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expression Expression1, Expression1left, 
Expression1right)) :: rest671)) => let val  result = MlyValue.START
 (fn _ => let val  (Expression as Expression1) = Expression1 ()
 in (#2 Expression)
end)
 in ( LrTable.NT 1, ( result, Expression1left, Expression1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECIM DECIM1, DECIM1left, DECIM1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
DECIM as DECIM1) = DECIM1 ()
 in (("rat",UNIONCON(Ratum(Rational.fromDecimal(DECIM)))))
end)
 in ( LrTable.NT 0, ( result, DECIM1left, DECIM1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INTEG INTEG1, INTEG1left, INTEG1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
INTEG as INTEG1) = INTEG1 ()
 in (("int",UNIONCON(Intum(BigInt.fromString(INTEG)))))
end)
 in ( LrTable.NT 0, ( result, INTEG1left, INTEG1right), rest671)
end
|  ( 3, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => (("bool",UNIONCON(Boolum(true)))
))
 in ( LrTable.NT 0, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 4, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val 
 result = MlyValue.Expression (fn _ => (
("bool",UNIONCON(Boolum(false)))))
 in ( LrTable.NT 0, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression2, _, _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _,
 _)) :: _ :: ( _, ( _, MAKERAT1left, _)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => let val  Expression1 = 
Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("rat",make_ratcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, MAKERAT1left, RPAR1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, RAT1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (fn _ => let val  (Expression
 as Expression1) = Expression1 ()
 in (
if (#1 Expression) = "int" then () else raise type_mismatch_error;("rat",ratcon(#2 Expression))
)
end)
 in ( LrTable.NT 0, ( result, RAT1left, RPAR1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, FROMDEC1left, _)) :: rest671))
 => let val  result = MlyValue.Expression (fn _ => let val  (
Expression as Expression1) = Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",from_deccon(#2 Expression))
)
end)
 in ( LrTable.NT 0, ( result, FROMDEC1left, RPAR1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right)
) :: ( _, ( _, INV1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",invcon(#2 Expression))
)
end)
 in ( LrTable.NT 0, ( result, INV1left, Expression1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right)
) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (not((#1 Expression) = "bool")) then () else raise type_mismatch_error;if((#1 Expression) = "rat") then (#1 Expression,rat_negcon(#2 Expression)) else (#1 Expression,int_negcon(#2 Expression))
)
end)
 in ( LrTable.NT 0, ( result, NEG1left, Expression1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, BOOLNEG1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "bool" then () else raise type_mismatch_error; ("bool",bool_negcon(#2 Expression))
)
end)
 in ( LrTable.NT 0, ( result, BOOLNEG1left, Expression1right), rest671
)
end
|  ( 11, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_plus(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_sub(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_times(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_div(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",plus(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",sub(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",times(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",divi(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",modu(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",andcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",orcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 22, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let
 val  result = MlyValue.Expression (fn _ => let val  (Expression as 
Expression1) = Expression1 ()
 in (Expression)
end)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_greatcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_greatcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_greatoreqcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_greatoreqcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_lesscon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_lesscon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_lessoreqcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_lessoreqcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_equalcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_equalcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              if (not((#1 Expression1) = "bool")) then
                                                ()
                                              else
                                                raise type_mismatch_error
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_notequalcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool", rat_notequalcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 0, ( result, Expression1left, Expression2right), 
rest671)
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
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun INV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLNEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.INTEG (fn () => i),p1,p2))
fun DECIM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.DECIM (fn () => i),p1,p2))
fun RPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSOREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATOREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GREAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
end
end
