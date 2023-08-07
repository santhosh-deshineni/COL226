structure DataTypes = 
struct
datatype AST  = PROG of string * BLK 
and BLK = BLK of (DEC) * (COM list) 
and DEC = DEC of ( VARDEC * (PRO list))
and VARDEC = VARDEC of ((string list)*(string list)*(string list))
and PRO = PRO of (string*BLK*int list)
and COM = ASSG of (string*Exp) | CAL of (string) | RE of (string) | PRI of (Exp) | COND of (Exp * (COM list) * (COM list)) | WH of (Exp * (COM list)) | show_ratcon of Exp | show_deccon of Exp | to_deccon of Exp
and Exp = make_ratcon of Exp*Exp
          | ratcon of Exp
          | from_deccon of Exp
          | invcon of Exp
          | rat_negcon of Exp
          | int_negcon of Exp
          | bool_negcon of Exp
          | rat_plus of Exp*Exp
          | rat_sub of Exp*Exp
          | rat_times of Exp*Exp
          | rat_div of Exp*Exp
          | plus of Exp*Exp
          | sub of Exp*Exp
          | times of Exp*Exp
          | divi of Exp*Exp
          | modu of Exp*Exp
          | andcon of Exp*Exp
          | orcon of Exp*Exp
          | int_greatcon of Exp*Exp
          | rat_greatcon of Exp*Exp
          | int_greatoreqcon of Exp*Exp
          | rat_greatoreqcon of Exp*Exp
          | int_lesscon of Exp*Exp
          | rat_lesscon of Exp*Exp
          | int_lessoreqcon of Exp*Exp
          | rat_lessoreqcon of Exp*Exp
          | int_equalcon of Exp*Exp
          | rat_equalcon of Exp*Exp
          | bool_equalcon of Exp*Exp
          | int_notequalcon of Exp*Exp
          | rat_notequalcon of Exp*Exp
          | bool_notequalcon of Exp*Exp
          | Ident of string
          | UNIONCON of unionofibr
and unionofibr = Intum of BigInt.bigint | Ratum of Rational.rational | Boolum of bool
end ; 