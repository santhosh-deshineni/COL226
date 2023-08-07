(* Sample interactive calculator for ML-Yacc *)
open DataTypes;
exception type_mismatch_error;
%%

%eop EOF

(* %pos declares the type of positions for terminals.
   Each symbol has an associated left and right position. *)

%term TRUE | FALSE | MAKERAT | FROMDEC | RAT | INV | NEG | BOOLNEG | INTEG of string | DECIM of string | RPLUS | RSUB | RTIMES | RDIV | PLUS | SUB | TIMES | DIV | MOD | AND | OR | LESSOREQ | GREATOREQ | LESS | GREAT | EQUAL | NOTEQUAL | LPAR | RPAR | COMMA | EOF
%nonterm Expression of string*Exp | START of Exp

%left EQUAL NOTEQUAL LESS GREAT LESSOREQ GREATOREQ 
%left AND OR
%left RPLUS RSUB
%left RTIMES RDIV
%left PLUS SUB
%left TIMES DIV MOD
%left BOOLNEG INV NEG

%name Calc
%pos int
%noshift EOF
%verbose
%%

(* the parser returns the value associated with the expression *)
START : Expression (#2 Expression)

Expression :   DECIM (("rat",UNIONCON(Ratum(Rational.fromDecimal(DECIM)))))
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
              | Expression NOTEQUAL Expression (
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