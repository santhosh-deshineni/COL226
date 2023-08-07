functor PL0LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PL0_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes;
exception type_mismatch_error;
exception var_not_found_error;
exception proc_not_found_error;

val IDlist: (string*string*int) list ref = ref [];
val funclist: (string*int) list ref = ref [];
val Scoper: int ref = ref 0;
val Stacker: int list ref = ref []; 
val Header: int ref = ref 0;

fun get_type_help(id,[]:(string*string*int) list)=NONE
    |get_type_help(id,h::T) = if(id=(#1 h)) then SOME(#2 h) else get_type_help(id,T)

fun get_type(id)=get_type_help(id,!IDlist)

fun add_var(id,typ,scp) =
  (IDlist := ((id,typ,scp)::(!IDlist)))

fun add_proc(id,scp) =
  (funclist := ((id,scp)::(!funclist)))

fun remove_all_of_scope(scope:int,checklist:(string*string*int) list) = 
  if (checklist = []) then
    []
  else if (not((#3 (hd checklist)) = scope)) then
    checklist
  else
    remove_all_of_scope(scope,tl checklist)

fun remove_procs_from_scope(scope:int,checklist:(string*int) list) =
  if (checklist = []) then
    []
  else if (not((#2 (hd checklist)) = scope)) then
    checklist
  else
    remove_procs_from_scope(scope,tl checklist)

fun func_search_help(name:string,[]: (string*int) list) = ((raise proc_not_found_error);false)
  | func_search_help(name,h::T) = if (name = (#1 h)) then true else func_search_help(name,T)

fun func_search(procname) = func_search_help(procname,(!funclist))


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\004\000\065\000\005\000\064\000\017\000\063\000\018\000\062\000\
\\021\000\061\000\023\000\060\000\024\000\059\000\025\000\058\000\
\\026\000\057\000\027\000\056\000\028\000\055\000\033\000\054\000\
\\048\000\053\000\000\000\
\\001\000\007\000\107\000\029\000\095\000\030\000\094\000\031\000\093\000\
\\032\000\092\000\033\000\091\000\034\000\090\000\035\000\089\000\
\\036\000\088\000\037\000\087\000\038\000\086\000\039\000\085\000\
\\041\000\084\000\042\000\083\000\043\000\082\000\044\000\081\000\
\\045\000\080\000\046\000\079\000\000\000\
\\001\000\008\000\138\000\000\000\
\\001\000\009\000\142\000\000\000\
\\001\000\011\000\106\000\029\000\095\000\030\000\094\000\031\000\093\000\
\\032\000\092\000\033\000\091\000\034\000\090\000\035\000\089\000\
\\036\000\088\000\037\000\087\000\038\000\086\000\039\000\085\000\
\\041\000\084\000\042\000\083\000\043\000\082\000\044\000\081\000\
\\045\000\080\000\046\000\079\000\000\000\
\\001\000\012\000\137\000\000\000\
\\001\000\028\000\016\000\000\000\
\\001\000\028\000\020\000\000\000\
\\001\000\028\000\023\000\000\000\
\\001\000\028\000\044\000\000\000\
\\001\000\028\000\068\000\000\000\
\\001\000\028\000\104\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\049\000\126\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\049\000\131\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\049\000\134\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\049\000\135\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\049\000\141\000\000\000\
\\001\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\052\000\136\000\000\000\
\\001\000\040\000\051\000\000\000\
\\001\000\047\000\021\000\000\000\
\\001\000\047\000\041\000\000\000\
\\001\000\047\000\045\000\000\000\
\\001\000\047\000\049\000\000\000\
\\001\000\047\000\074\000\000\000\
\\001\000\048\000\069\000\000\000\
\\001\000\048\000\070\000\000\000\
\\001\000\048\000\101\000\000\000\
\\001\000\048\000\102\000\000\000\
\\001\000\048\000\103\000\000\000\
\\001\000\049\000\130\000\000\000\
\\001\000\050\000\014\000\000\000\
\\001\000\051\000\050\000\000\000\
\\001\000\053\000\000\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\001\000\007\000\000\000\
\\150\000\000\000\
\\151\000\052\000\042\000\000\000\
\\152\000\000\000\
\\153\000\002\000\009\000\000\000\
\\154\000\000\000\
\\155\000\052\000\046\000\000\000\
\\156\000\000\000\
\\157\000\003\000\018\000\000\000\
\\158\000\000\000\
\\159\000\052\000\075\000\000\000\
\\160\000\000\000\
\\161\000\013\000\012\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\006\000\040\000\010\000\039\000\014\000\038\000\015\000\037\000\
\\016\000\036\000\019\000\035\000\020\000\034\000\022\000\033\000\
\\028\000\032\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\000\000\
\\174\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\000\000\
\\175\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\000\000\
\\176\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\041\000\084\000\
\\042\000\083\000\043\000\082\000\044\000\081\000\045\000\080\000\
\\046\000\079\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\031\000\093\000\032\000\092\000\033\000\091\000\034\000\090\000\
\\035\000\089\000\036\000\088\000\037\000\087\000\000\000\
\\194\000\031\000\093\000\032\000\092\000\033\000\091\000\034\000\090\000\
\\035\000\089\000\036\000\088\000\037\000\087\000\000\000\
\\195\000\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\000\000\
\\196\000\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\000\000\
\\197\000\035\000\089\000\036\000\088\000\037\000\087\000\000\000\
\\198\000\035\000\089\000\036\000\088\000\037\000\087\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\000\000\
\\203\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\000\000\
\\204\000\000\000\
\\205\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\206\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\207\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\208\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\209\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\210\000\029\000\095\000\030\000\094\000\031\000\093\000\032\000\092\000\
\\033\000\091\000\034\000\090\000\035\000\089\000\036\000\088\000\
\\037\000\087\000\038\000\086\000\039\000\085\000\000\000\
\\211\000\035\000\089\000\036\000\088\000\037\000\087\000\000\000\
\"
val actionRowNumbers =
"\038\000\042\000\050\000\030\000\
\\033\000\006\000\046\000\007\000\
\\019\000\035\000\008\000\034\000\
\\055\000\020\000\040\000\036\000\
\\009\000\021\000\044\000\050\000\
\\038\000\052\000\061\000\060\000\
\\058\000\059\000\057\000\056\000\
\\022\000\031\000\018\000\000\000\
\\000\000\000\000\010\000\024\000\
\\025\000\000\000\000\000\037\000\
\\006\000\023\000\048\000\041\000\
\\007\000\049\000\051\000\055\000\
\\053\000\000\000\064\000\000\000\
\\000\000\071\000\072\000\073\000\
\\000\000\000\000\000\000\026\000\
\\027\000\028\000\075\000\074\000\
\\063\000\062\000\066\000\011\000\
\\000\000\004\000\001\000\039\000\
\\045\000\009\000\043\000\054\000\
\\065\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\012\000\100\000\
\\081\000\080\000\079\000\000\000\
\\000\000\000\000\029\000\013\000\
\\030\000\030\000\047\000\099\000\
\\098\000\094\000\096\000\095\000\
\\097\000\092\000\091\000\090\000\
\\089\000\088\000\087\000\086\000\
\\085\000\084\000\083\000\082\000\
\\093\000\014\000\015\000\017\000\
\\067\000\068\000\005\000\002\000\
\\078\000\077\000\000\000\070\000\
\\030\000\016\000\003\000\076\000\
\\069\000\032\000"
val gotoT =
"\
\\001\000\141\000\002\000\004\000\003\000\003\000\005\000\002\000\
\\007\000\001\000\000\000\
\\008\000\006\000\000\000\
\\006\000\009\000\013\000\008\000\000\000\
\\004\000\011\000\000\000\
\\000\000\
\\010\000\013\000\000\000\
\\009\000\015\000\000\000\
\\011\000\017\000\000\000\
\\000\000\
\\000\000\
\\023\000\020\000\000\000\
\\000\000\
\\014\000\029\000\015\000\028\000\016\000\027\000\017\000\026\000\
\\018\000\025\000\019\000\024\000\020\000\023\000\021\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\041\000\000\000\
\\000\000\
\\000\000\
\\006\000\045\000\013\000\008\000\000\000\
\\002\000\046\000\003\000\003\000\005\000\002\000\007\000\001\000\000\000\
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
\\022\000\050\000\000\000\
\\022\000\064\000\000\000\
\\022\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\069\000\000\000\
\\022\000\070\000\000\000\
\\000\000\
\\010\000\071\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\074\000\000\000\
\\000\000\
\\000\000\
\\014\000\075\000\015\000\028\000\016\000\027\000\017\000\026\000\
\\018\000\025\000\019\000\024\000\020\000\023\000\021\000\022\000\000\000\
\\000\000\
\\022\000\076\000\000\000\
\\000\000\
\\022\000\094\000\000\000\
\\022\000\095\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\096\000\000\000\
\\022\000\097\000\000\000\
\\022\000\098\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\107\000\000\000\
\\022\000\108\000\000\000\
\\022\000\109\000\000\000\
\\022\000\110\000\000\000\
\\022\000\111\000\000\000\
\\022\000\112\000\000\000\
\\022\000\113\000\000\000\
\\022\000\114\000\000\000\
\\022\000\115\000\000\000\
\\022\000\116\000\000\000\
\\022\000\117\000\000\000\
\\022\000\118\000\000\000\
\\022\000\119\000\000\000\
\\022\000\120\000\000\000\
\\022\000\121\000\000\000\
\\022\000\122\000\000\000\
\\022\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\125\000\000\000\
\\022\000\126\000\000\000\
\\022\000\127\000\000\000\
\\000\000\
\\000\000\
\\004\000\130\000\000\000\
\\004\000\131\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\137\000\000\000\
\\000\000\
\\004\000\138\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 142
val numrules = 68
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
 | ID of unit ->  (string) | DECIM of unit ->  (string)
 | INTEG of unit ->  (string) | PROCID of unit ->  (string)
 | Expression of unit ->  ( ( string*Exp ) )
 | WhileCmd of unit ->  (COM) | ConditionalCmd of unit ->  (COM)
 | ReadCmd of unit ->  (COM) | PrintCmd of unit ->  (COM)
 | CallCmd of unit ->  (COM) | AssignmentCmd of unit ->  (COM)
 | Command of unit ->  (COM) | Commands of unit ->  (COM list)
 | ProcDef of unit ->  (PRO) | newboollist of unit ->  (string list)
 | newintlist of unit ->  (string list)
 | newratlist of unit ->  (string list)
 | BoolVarDecls of unit ->  (string list)
 | IntVarDecls of unit ->  (string list)
 | RatVarDecls of unit ->  (string list)
 | ProcDecls of unit ->  (PRO list) | VarDecls of unit ->  (VARDEC)
 | CommandSeq of unit ->  (COM list)
 | DeclarationSeq of unit ->  (DEC) | Block of unit ->  (BLK)
 | Program of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
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
fn (T 52) => true | _ => false
val showTerminal =
fn (T 0) => "RATDEC"
  | (T 1) => "INTDEC"
  | (T 2) => "BOOLDEC"
  | (T 3) => "TRUE"
  | (T 4) => "FALSE"
  | (T 5) => "IF"
  | (T 6) => "THEN"
  | (T 7) => "ELSE"
  | (T 8) => "FI"
  | (T 9) => "WHILE"
  | (T 10) => "DO"
  | (T 11) => "OD"
  | (T 12) => "PROC"
  | (T 13) => "PRINT"
  | (T 14) => "READ"
  | (T 15) => "CALL"
  | (T 16) => "MAKERAT"
  | (T 17) => "RAT"
  | (T 18) => "SHOWRAT"
  | (T 19) => "SHOWDEC"
  | (T 20) => "FROMDEC"
  | (T 21) => "TODEC"
  | (T 22) => "INV"
  | (T 23) => "NEG"
  | (T 24) => "BOOLNEG"
  | (T 25) => "INTEG"
  | (T 26) => "DECIM"
  | (T 27) => "ID"
  | (T 28) => "RPLUS"
  | (T 29) => "RSUB"
  | (T 30) => "RTIMES"
  | (T 31) => "RDIV"
  | (T 32) => "PLUS"
  | (T 33) => "SUB"
  | (T 34) => "TIMES"
  | (T 35) => "DIV"
  | (T 36) => "MOD"
  | (T 37) => "AND"
  | (T 38) => "OR"
  | (T 39) => "ASSIGN"
  | (T 40) => "LESSOREQ"
  | (T 41) => "GREATOREQ"
  | (T 42) => "LESS"
  | (T 43) => "GREAT"
  | (T 44) => "EQUAL"
  | (T 45) => "NOTEQUAL"
  | (T 46) => "SEMI"
  | (T 47) => "LPAR"
  | (T 48) => "RPAR"
  | (T 49) => "LCURL"
  | (T 50) => "RCURL"
  | (T 51) => "COMMA"
  | (T 52) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46)
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
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
of  ( 0, ( ( _, ( MlyValue.Block Block1, Block1left, Block1right)) :: 
rest671)) => let val  result = MlyValue.Program (fn _ => let val  (
Block as Block1) = Block1 ()
 in (Scoper:=0;IDlist:=[];Stacker:=[];Header:=0;PROG("cool",Block))

end)
 in ( LrTable.NT 0, ( result, Block1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (BLK(DeclarationSeq,CommandSeq))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.ProcDecls ProcDecls1, _, ProcDecls1right))
 :: ( _, ( MlyValue.VarDecls VarDecls1, VarDecls1left, _)) :: rest671)
) => let val  result = MlyValue.DeclarationSeq (fn _ => let val  (
VarDecls as VarDecls1) = VarDecls1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 in (DEC(VarDecls,ProcDecls))
end)
 in ( LrTable.NT 2, ( result, VarDecls1left, ProcDecls1right), rest671
)
end
|  ( 3, ( ( _, ( MlyValue.BoolVarDecls BoolVarDecls1, _, 
BoolVarDecls1right)) :: ( _, ( MlyValue.IntVarDecls IntVarDecls1, _, _
)) :: ( _, ( MlyValue.RatVarDecls RatVarDecls1, RatVarDecls1left, _))
 :: rest671)) => let val  result = MlyValue.VarDecls (fn _ => let val 
 (RatVarDecls as RatVarDecls1) = RatVarDecls1 ()
 val  (IntVarDecls as IntVarDecls1) = IntVarDecls1 ()
 val  (BoolVarDecls as BoolVarDecls1) = BoolVarDecls1 ()
 in (
Scoper:=(!Scoper)+1;Stacker:=(!Scoper)::(!Stacker);VARDEC(RatVarDecls,IntVarDecls,BoolVarDecls)
)
end)
 in ( LrTable.NT 4, ( result, RatVarDecls1left, BoolVarDecls1right), 
rest671)
end
|  ( 4, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.newratlist 
newratlist1, _, _)) :: ( _, ( _, RATDEC1left, _)) :: rest671)) => let
 val  result = MlyValue.RatVarDecls (fn _ => let val  (newratlist as 
newratlist1) = newratlist1 ()
 in (newratlist)
end)
 in ( LrTable.NT 6, ( result, RATDEC1left, SEMI1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.RatVarDecls (fn _ =>
 ([]:string list))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.newratlist newratlist1, _, newratlist1right)
) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.newratlist (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (newratlist as newratlist1) = newratlist1 ()
 in (add_var(ID,"rat",(!Scoper)+1);ID::newratlist)
end)
 in ( LrTable.NT 9, ( result, ID1left, newratlist1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.newratlist (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (add_var(ID,"rat",(!Scoper)+1);[ID])
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 8, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.newintlist 
newintlist1, _, _)) :: ( _, ( _, INTDEC1left, _)) :: rest671)) => let
 val  result = MlyValue.IntVarDecls (fn _ => let val  (newintlist as 
newintlist1) = newintlist1 ()
 in (newintlist)
end)
 in ( LrTable.NT 7, ( result, INTDEC1left, SEMI1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.IntVarDecls (fn _ =>
 ([]:string list))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.newintlist newintlist1, _, newintlist1right
)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.newintlist (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (newintlist as newintlist1) = newintlist1 ()
 in (add_var(ID,"int",(!Scoper)+1);ID::newintlist)
end)
 in ( LrTable.NT 10, ( result, ID1left, newintlist1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.newintlist (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (add_var(ID,"int",(!Scoper)+1);[ID])
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.newboollist 
newboollist1, _, _)) :: ( _, ( _, BOOLDEC1left, _)) :: rest671)) =>
 let val  result = MlyValue.BoolVarDecls (fn _ => let val  (
newboollist as newboollist1) = newboollist1 ()
 in (newboollist)
end)
 in ( LrTable.NT 8, ( result, BOOLDEC1left, SEMI1right), rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.BoolVarDecls (fn _
 => ([]:string list))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.newboollist newboollist1, _, 
newboollist1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.newboollist (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (newboollist as newboollist1) = newboollist1 ()
 in (add_var(ID,"bool",(!Scoper)+1);ID::newboollist)
end)
 in ( LrTable.NT 11, ( result, ID1left, newboollist1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.newboollist (fn _ => let val  (ID as ID1)
 = ID1 ()
 in (add_var(ID,"bool",(!Scoper)+1);[ID])
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ProcDecls ProcDecls1, _, ProcDecls1right))
 :: _ :: ( _, ( MlyValue.ProcDef ProcDef1, ProcDef1left, _)) :: 
rest671)) => let val  result = MlyValue.ProcDecls (fn _ => let val  (
ProcDef as ProcDef1) = ProcDef1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 in (ProcDef::ProcDecls)
end)
 in ( LrTable.NT 5, ( result, ProcDef1left, ProcDecls1right), rest671)

end
|  ( 17, ( rest671)) => let val  result = MlyValue.ProcDecls (fn _ =>
 ([]:PRO list))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: ( _, ( 
MlyValue.PROCID PROCID1, _, _)) :: ( _, ( _, PROC1left, _)) :: rest671
)) => let val  result = MlyValue.ProcDef (fn _ => let val  (PROCID as 
PROCID1) = PROCID1 ()
 val  (Block as Block1) = Block1 ()
 in (
Header:=(hd (!Stacker));IDlist:=remove_all_of_scope((!Header),(!IDlist));funclist:=remove_procs_from_scope((!Header),(!funclist));Stacker:=(tl (!Stacker));PRO(PROCID,Block,(!Header)::(!Stacker))
)
end)
 in ( LrTable.NT 12, ( result, PROC1left, Block1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.PROCID (fn _ => let val  (ID as ID1) = ID1
 ()
 in (add_proc(ID,(hd (!Stacker)));(ID))
end)
 in ( LrTable.NT 22, ( result, ID1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RCURL1right)) :: ( _, ( MlyValue.Commands 
Commands1, _, _)) :: ( _, ( _, LCURL1left, _)) :: rest671)) => let
 val  result = MlyValue.CommandSeq (fn _ => let val  (Commands as 
Commands1) = Commands1 ()
 in (Commands)
end)
 in ( LrTable.NT 3, ( result, LCURL1left, RCURL1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Commands Commands1, _, Commands1right)) ::
 _ :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: rest671))
 => let val  result = MlyValue.Commands (fn _ => let val  (Command as 
Command1) = Command1 ()
 val  (Commands as Commands1) = Commands1 ()
 in (Command::Commands)
end)
 in ( LrTable.NT 13, ( result, Command1left, Commands1right), rest671)

end
|  ( 22, ( rest671)) => let val  result = MlyValue.Commands (fn _ => (
[]:COM list))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.AssignmentCmd AssignmentCmd1, 
AssignmentCmd1left, AssignmentCmd1right)) :: rest671)) => let val  
result = MlyValue.Command (fn _ => let val  (AssignmentCmd as 
AssignmentCmd1) = AssignmentCmd1 ()
 in (AssignmentCmd)
end)
 in ( LrTable.NT 14, ( result, AssignmentCmd1left, AssignmentCmd1right
), rest671)
end
|  ( 24, ( ( _, ( MlyValue.CallCmd CallCmd1, CallCmd1left, 
CallCmd1right)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (CallCmd as CallCmd1) = CallCmd1 ()
 in (CallCmd)
end)
 in ( LrTable.NT 14, ( result, CallCmd1left, CallCmd1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.ReadCmd ReadCmd1, ReadCmd1left, 
ReadCmd1right)) :: rest671)) => let val  result = MlyValue.Command (fn
 _ => let val  (ReadCmd as ReadCmd1) = ReadCmd1 ()
 in (ReadCmd)
end)
 in ( LrTable.NT 14, ( result, ReadCmd1left, ReadCmd1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.PrintCmd PrintCmd1, PrintCmd1left, 
PrintCmd1right)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (PrintCmd as PrintCmd1) = PrintCmd1 ()
 in (PrintCmd)
end)
 in ( LrTable.NT 14, ( result, PrintCmd1left, PrintCmd1right), rest671
)
end
|  ( 27, ( ( _, ( MlyValue.ConditionalCmd ConditionalCmd1, 
ConditionalCmd1left, ConditionalCmd1right)) :: rest671)) => let val  
result = MlyValue.Command (fn _ => let val  (ConditionalCmd as 
ConditionalCmd1) = ConditionalCmd1 ()
 in (ConditionalCmd)
end)
 in ( LrTable.NT 14, ( result, ConditionalCmd1left, 
ConditionalCmd1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.WhileCmd WhileCmd1, WhileCmd1left, 
WhileCmd1right)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (WhileCmd as WhileCmd1) = WhileCmd1 ()
 in (WhileCmd)
end)
 in ( LrTable.NT 14, ( result, WhileCmd1left, WhileCmd1right), rest671
)
end
|  ( 29, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, SHOWRAT1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;show_ratcon(#2 Expression)
)
end)
 in ( LrTable.NT 14, ( result, SHOWRAT1left, Expression1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, SHOWDEC1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;show_deccon(#2 Expression)
)
end)
 in ( LrTable.NT 14, ( result, SHOWDEC1left, Expression1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, TODEC1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;to_deccon(#2 Expression)
)
end)
 in ( LrTable.NT 14, ( result, TODEC1left, Expression1right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.AssignmentCmd (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (Expression as Expression1) = Expression1 ()
 in (

                                      let
                                        val checking = get_type(ID)
                                      in
                                      (if (checking = NONE) then
                                        raise var_not_found_error
                                      else
                                        ());
                                      (if (valOf(checking) = (#1 Expression)) then
                                        ()
                                      else
                                      raise type_mismatch_error);
                                      ASSG(ID,#2 Expression)
                                      end
                                      
)
end)
 in ( LrTable.NT 15, ( result, ID1left, Expression1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
CALL1left, _)) :: rest671)) => let val  result = MlyValue.CallCmd (fn
 _ => let val  (ID as ID1) = ID1 ()
 in (func_search(ID);CAL(ID))
end)
 in ( LrTable.NT 16, ( result, CALL1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.ID ID1, _, _))
 :: _ :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.ReadCmd (fn _ => let val  (ID as ID1) = ID1 ()
 in (RE(ID))
end)
 in ( LrTable.NT 18, ( result, READ1left, RPAR1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.PrintCmd (fn _ => let val  (Expression as 
Expression1) = Expression1 ()
 in (PRI(#2 Expression))
end)
 in ( LrTable.NT 17, ( result, PRINT1left, RPAR1right), rest671)
end
|  ( 36, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ConditionalCmd (fn _ => let val  (Expression as Expression1)
 = Expression1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (
if (#1 Expression) = "bool" then () else raise type_mismatch_error;COND(#2 Expression,CommandSeq1,CommandSeq2)
)
end)
 in ( LrTable.NT 19, ( result, IF1left, FI1right), rest671)
end
|  ( 37, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.WhileCmd (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (
if (#1 Expression) = "bool" then () else raise type_mismatch_error;WH(#2 Expression,CommandSeq)
)
end)
 in ( LrTable.NT 20, ( result, WHILE1left, OD1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Expression (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (
let val typeofid = get_type(ID) in ((if (typeofid = NONE) then raise var_not_found_error else ());(valOf(typeofid),Ident(ID))) end
)
end)
 in ( LrTable.NT 21, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.DECIM DECIM1, DECIM1left, DECIM1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
DECIM as DECIM1) = DECIM1 ()
 in (("rat",UNIONCON(Ratum(Rational.fromDecimal(DECIM)))))
end)
 in ( LrTable.NT 21, ( result, DECIM1left, DECIM1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.INTEG INTEG1, INTEG1left, INTEG1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
INTEG as INTEG1) = INTEG1 ()
 in (("int",UNIONCON(Intum(BigInt.fromString(INTEG)))))
end)
 in ( LrTable.NT 21, ( result, INTEG1left, INTEG1right), rest671)
end
|  ( 41, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => (("bool",UNIONCON(Boolum(true)))
))
 in ( LrTable.NT 21, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 42, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Expression (fn _ => (
("bool",UNIONCON(Boolum(false)))))
 in ( LrTable.NT 21, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression2, _, _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _,
 _)) :: _ :: ( _, ( _, MAKERAT1left, _)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => let val  Expression1 = 
Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("rat",make_ratcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, MAKERAT1left, RPAR1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, RAT1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (fn _ => let val  (Expression
 as Expression1) = Expression1 ()
 in (
if (#1 Expression) = "int" then () else raise type_mismatch_error;("rat",ratcon(#2 Expression))
)
end)
 in ( LrTable.NT 21, ( result, RAT1left, RPAR1right), rest671)
end
|  ( 45, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: _ :: ( _, ( _, FROMDEC1left, _)) :: rest671))
 => let val  result = MlyValue.Expression (fn _ => let val  (
Expression as Expression1) = Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",from_deccon(#2 Expression))
)
end)
 in ( LrTable.NT 21, ( result, FROMDEC1left, RPAR1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, INV1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",invcon(#2 Expression))
)
end)
 in ( LrTable.NT 21, ( result, INV1left, Expression1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (not((#1 Expression) = "bool")) then () else raise type_mismatch_error;if((#1 Expression) = "rat") then (#1 Expression,rat_negcon(#2 Expression)) else (#1 Expression,int_negcon(#2 Expression))
)
end)
 in ( LrTable.NT 21, ( result, NEG1left, Expression1right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, BOOLNEG1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if (#1 Expression) = "bool" then () else raise type_mismatch_error; ("bool",bool_negcon(#2 Expression))
)
end)
 in ( LrTable.NT 21, ( result, BOOLNEG1left, Expression1right), 
rest671)
end
|  ( 49, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_plus(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_sub(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_times(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 52, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_div(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",plus(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 54, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",sub(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 55, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",times(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 56, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",divi(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 57, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",modu(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 58, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",andcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 59, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (
if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",orcon(#2 Expression1,#2 Expression2))
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 60, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expression 
Expression1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let
 val  result = MlyValue.Expression (fn _ => let val  (Expression as 
Expression1) = Expression1 ()
 in (Expression)
end)
 in ( LrTable.NT 21, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
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
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 62, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
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
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 63, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
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
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 64, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
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
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 65, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              ()
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_equalcon(#2 Expression1,#2 Expression2))
                                            else if (#1 Expression1) = "rat" then
                                              ("bool", rat_equalcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool",bool_equalcon(#2 Expression1,#2 Expression2))
                                            
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 66, ( ( _, ( MlyValue.Expression Expression2, _, Expression2right
)) :: _ :: ( _, ( MlyValue.Expression Expression1, Expression1left, _)
) :: rest671)) => let val  result = MlyValue.Expression (fn _ => let
 val  Expression1 = Expression1 ()
 val  Expression2 = Expression2 ()
 in (

                                            (if ((#1 Expression1) = (#1 Expression2)) then
                                              ()
                                            else
                                              raise type_mismatch_error);
                                            if (#1 Expression1) = "int" then
                                              ("bool", int_notequalcon(#2 Expression1,#2 Expression2))
                                            else if (#1 Expression1) = "rat" then
                                              ("bool", rat_notequalcon(#2 Expression1,#2 Expression2))
                                            else
                                              ("bool",bool_notequalcon(#2 Expression1, #2 Expression2))
                                            
)
end)
 in ( LrTable.NT 21, ( result, Expression1left, Expression2right), 
rest671)
end
|  ( 67, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.Expression (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (
if ((#1 Expression) = "bool") then raise type_mismatch_error  else ();(Expression)
)
end)
 in ( LrTable.NT 21, ( result, PLUS1left, Expression1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PL0_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RATDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INTDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PROC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TODEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun INV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLNEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.INTEG (fn () => i),p1,p2))
fun DECIM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.DECIM (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun RPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSOREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATOREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun GREAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
end
end
