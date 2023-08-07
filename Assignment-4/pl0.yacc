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

%%

%name PL0

%term RATDEC | INTDEC | BOOLDEC | TRUE | FALSE | IF | THEN | ELSE | FI | WHILE |
DO | OD | PROC | PRINT | READ | CALL | MAKERAT | RAT | SHOWRAT | SHOWDEC | FROMDEC |
TODEC | INV | NEG | BOOLNEG | INTEG of string | DECIM of string | ID of string |
RPLUS | RSUB | RTIMES | RDIV | PLUS | SUB | TIMES | DIV | MOD | AND | OR | ASSIGN |
LESSOREQ | GREATOREQ | LESS | GREAT | EQUAL | NOTEQUAL | SEMI | LPAR | RPAR | LCURL |
RCURL | COMMA | EOF
%nonterm Program of AST | Block of BLK | DeclarationSeq of DEC | CommandSeq of COM list | VarDecls of VARDEC | ProcDecls of PRO list | RatVarDecls of string list |
IntVarDecls of string list | BoolVarDecls of string list | newratlist of string list | newintlist of string list | newboollist of string list | ProcDef of PRO | Commands of COM list | Command of COM |
AssignmentCmd of COM | CallCmd of COM | PrintCmd of COM | ReadCmd of COM | ConditionalCmd of COM | WhileCmd of COM | Expression of (string*Exp) | PROCID of string

%left ASSIGN
%left EQUAL NOTEQUAL LESS GREAT LESSOREQ GREATOREQ 
%left AND OR
%left RPLUS RSUB
%left RTIMES RDIV
%left PLUS SUB
%left TIMES DIV MOD
%left BOOLNEG INV NEG

%eop EOF
%noshift EOF
%pos int
%verbose

%%

Program : Block (Scoper:=0;IDlist:=[];Stacker:=[];Header:=0;PROG("cool",Block))

Block : DeclarationSeq CommandSeq (BLK(DeclarationSeq,CommandSeq))

DeclarationSeq : VarDecls ProcDecls (DEC(VarDecls,ProcDecls))

VarDecls : RatVarDecls IntVarDecls BoolVarDecls (Scoper:=(!Scoper)+1;Stacker:=(!Scoper)::(!Stacker);VARDEC(RatVarDecls,IntVarDecls,BoolVarDecls))

RatVarDecls : RATDEC newratlist SEMI (newratlist)
            | ([]:string list)

newratlist : ID COMMA newratlist (add_var(ID,"rat",(!Scoper)+1);ID::newratlist)
           | ID (add_var(ID,"rat",(!Scoper)+1);[ID])

IntVarDecls : INTDEC newintlist SEMI (newintlist)
            | ([]:string list)

newintlist : ID COMMA newintlist (add_var(ID,"int",(!Scoper)+1);ID::newintlist)
           | ID (add_var(ID,"int",(!Scoper)+1);[ID])

BoolVarDecls : BOOLDEC newboollist SEMI (newboollist)
             | ([]:string list)

newboollist : ID COMMA newboollist (add_var(ID,"bool",(!Scoper)+1);ID::newboollist)
            | ID (add_var(ID,"bool",(!Scoper)+1);[ID])

ProcDecls : ProcDef SEMI ProcDecls (ProcDef::ProcDecls)
          |  ([]:PRO list)

ProcDef : PROC PROCID Block (Header:=(hd (!Stacker));IDlist:=remove_all_of_scope((!Header),(!IDlist));funclist:=remove_procs_from_scope((!Header),(!funclist));Stacker:=(tl (!Stacker));PRO(PROCID,Block,(!Header)::(!Stacker)))

PROCID : ID (add_proc(ID,(hd (!Stacker)));(ID))

CommandSeq : LCURL Commands RCURL (Commands)

Commands : Command SEMI Commands (Command::Commands)
         | ([]:COM list)

Command : AssignmentCmd (AssignmentCmd)
        | CallCmd (CallCmd)
        | ReadCmd (ReadCmd)
        | PrintCmd (PrintCmd)
        | ConditionalCmd (ConditionalCmd)
        | WhileCmd (WhileCmd)
        | SHOWRAT Expression (if (#1 Expression) = "rat" then () else raise type_mismatch_error;show_ratcon(#2 Expression))
        | SHOWDEC Expression (if (#1 Expression) = "rat" then () else raise type_mismatch_error;show_deccon(#2 Expression))
        | TODEC Expression (if (#1 Expression) = "rat" then () else raise type_mismatch_error;to_deccon(#2 Expression))

AssignmentCmd : ID ASSIGN Expression (
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
CallCmd : CALL ID (func_search(ID);CAL(ID))
ReadCmd : READ LPAR ID RPAR (RE(ID))
PrintCmd : PRINT LPAR Expression RPAR (PRI(#2 Expression))
ConditionalCmd : IF Expression THEN CommandSeq ELSE CommandSeq FI (if (#1 Expression) = "bool" then () else raise type_mismatch_error;COND(#2 Expression,CommandSeq1,CommandSeq2))
WhileCmd : WHILE Expression DO CommandSeq OD (if (#1 Expression) = "bool" then () else raise type_mismatch_error;WH(#2 Expression,CommandSeq))

Expression :    ID (let val typeofid = get_type(ID) in ((if (typeofid = NONE) then raise var_not_found_error else ());(valOf(typeofid),Ident(ID))) end)
              | DECIM (("rat",UNIONCON(Ratum(Rational.fromDecimal(DECIM)))))
              | INTEG (("int",UNIONCON(Intum(BigInt.fromString(INTEG)))))
              | TRUE (("bool",UNIONCON(Boolum(true))))
              | FALSE (("bool",UNIONCON(Boolum(false))))

              | MAKERAT LPAR Expression COMMA Expression RPAR (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("rat",make_ratcon(#2 Expression1,#2 Expression2)))
              | RAT LPAR Expression RPAR (if (#1 Expression) = "int" then () else raise type_mismatch_error;("rat",ratcon(#2 Expression)))
              | FROMDEC LPAR Expression RPAR (if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",from_deccon(#2 Expression)))
              | INV Expression (if (#1 Expression) = "rat" then () else raise type_mismatch_error;("rat",invcon(#2 Expression)))
              | NEG Expression (if (not((#1 Expression) = "bool")) then () else raise type_mismatch_error;if((#1 Expression) = "rat") then (#1 Expression,rat_negcon(#2 Expression)) else (#1 Expression,int_negcon(#2 Expression)))
              | BOOLNEG Expression (if (#1 Expression) = "bool" then () else raise type_mismatch_error; ("bool",bool_negcon(#2 Expression)))

              | Expression RPLUS Expression (if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_plus(#2 Expression1,#2 Expression2)))
              | Expression RSUB Expression (if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_sub(#2 Expression1,#2 Expression2)))
              | Expression RTIMES Expression (if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_times(#2 Expression1,#2 Expression2)))
              | Expression RDIV Expression (if ((#1 Expression1) = "rat") andalso ((#1 Expression2) = "rat") then () else raise type_mismatch_error;("rat",rat_div(#2 Expression1,#2 Expression2)))

              | Expression PLUS Expression (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",plus(#2 Expression1,#2 Expression2)))
              | Expression SUB Expression (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",sub(#2 Expression1,#2 Expression2)))
              | Expression TIMES Expression (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",times(#2 Expression1,#2 Expression2)))
              | Expression DIV Expression (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",divi(#2 Expression1,#2 Expression2)))
              | Expression MOD Expression (if ((#1 Expression1) = "int") andalso ((#1 Expression2) = "int") then () else raise type_mismatch_error;("int",modu(#2 Expression1,#2 Expression2)))

              | Expression AND Expression (if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",andcon(#2 Expression1,#2 Expression2)))
              | Expression OR Expression (if ((#1 Expression1) = "bool") andalso ((#1 Expression2) = "bool") then () else raise type_mismatch_error;("bool",orcon(#2 Expression1,#2 Expression2)))
              | LPAR Expression RPAR (Expression)

              | Expression GREAT Expression (
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
              | Expression GREATOREQ Expression (
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
              | Expression LESS Expression (
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
              | Expression LESSOREQ Expression (
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
              | Expression EQUAL Expression (
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
              | Expression NOTEQUAL Expression (
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
              | PLUS Expression (if ((#1 Expression) = "bool") then raise type_mismatch_error  else ();(Expression))