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

%%

%eop EOF SEMI

(* %pos declares the type of positions for terminals.
   Each symbol has an associated left and right position. *)

%pos int

%left SUB PLUS
%left TIMES DIV

%term ID of string | PLUS | TIMES | PRINT | SEMI | EOF | DIV | SUB | FRACT of string | DECIM of string | INTEG of string | LPAR | RPAR | EQUAL
%nonterm EXP of rational | START of string option

%name Calc

%subst PRINT for ID
%prefer PLUS TIMES DIV SUB
%keyword PRINT SEMI

%noshift EOF
%nodefault
%verbose
%%

(* the parser returns the value associated with the expression *)

  START : PRINT EXP (SOME (showRat(EXP)))
        | EXP (SOME (showRat(EXP)))
        | ID EQUAL EXP (SOME (addvar(ID,EXP)))
        | (NONE)
  EXP : INTEG             (valOf(rat(BigInt.fromString(INTEG))))
      | ID              (lookup(ID))
      | DECIM           (fromDecimal(DECIM))
      | FRACT           (let
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
                        end)
      | EXP PLUS EXP    (add(EXP1,EXP2))
      | EXP TIMES EXP   (multiply(EXP1,EXP2))
      | EXP DIV EXP     (let
                          val x = divide(EXP1,EXP2)
                        in
                          if (x = NONE) then
                            raise(rat_error)
                          else
                            valOf(x)
                        end)
      | EXP SUB EXP     (subtract(EXP1,EXP2))
      | LPAR EXP RPAR   (EXP)