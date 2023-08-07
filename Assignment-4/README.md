## README for COL226 Assignment-4

### Grammar for Rational Numbers in decimal or fractional form

Let us consider the grammar G1 = `<N1,T1,P1,S>`
- `N1` = `{S,R,D,F,Z,W,N}`
- `T1` = `{+,~,.,(,),/,[0-9]}`

`P1` consists of the following production rules

- `S` -> `+R | ~R | R`
- `R` -> `D | F`
- `D` -> `Z.Z(WZ)`
- `F` -> `WZ/NZ`
- `Z` -> `WZ | eps`
- `W` -> `0 | N`
- `N` -> `1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9`

The above grammar produces rational numbers in either `"p/q"` fractional form or in decimal form(with the recurring part brackets mandatory as mentioned in assignment pdf).

### Grammar for Rational Number Expressions

Let us consider the grammar G2 = `<N2,T2,P2,A>`
- `N2` = `N1 U {A,B,C,id}`
- `T2` = `T1 U {+,-,*,/}`

`P2` consists of the union of following production rules with P1

- `A` -> `A+B | A-B | B`
- `B` -> `B*C | B/C | C`
- `C` -> `S | id | (A) | WZ`

(S, W and Z are non-terminals of the previous grammar G1)
The above grammar gives us rational number expressions with associativity and precedence.

### Grammar for Parsing the Language

- `Program` -> `Block`
- `Block` -> `DeclarationSeq CommandSeq`
- `DeclarationSeq` -> `VarDecls ProcDecls`
- `VarDecls` : `RatVarDecls IntVarDecls BoolVarDecls`

- `RatVarDecls` : `RATDEC newratlist SEMI` | `eps`
- `newratlist` : `ID COMMA newratlist` | `ID`
- `IntVarDecls` : `INTDEC newintlist SEMI` | `eps`
- `newintlist` : `ID COMMA newintlist` | `ID`
- `BoolVarDecls` : `BOOLDEC newboollist SEMI` | `eps`
- `newboollist` : `ID COMMA newboollist` | `ID`

- `ProcDecls` : `ProcDef SEMI ProcDecls` | `eps`
- `ProcDef` : `PROC PROCID Block`
- `PROCID` : `ID`

- `CommandSeq` : `LCURL Commands RCURL`
- `Commands` : `Command SEMI Commands` | `eps`

- `Command` : `AssignmentCmd` | `CallCmd`| `PrintCmd` | `ConditionalCmd` | `WhileCmd` | `SHOWRAT Expression`
            | `SHOWDEC Expression` | `TODEC Expression`
- `AssignmentCmd` : `ID ASSIGN Expression`
- `CallCmd` : `CALL ID`
- `ReadCmd` : `READ LPAR ID RPAR`
- `PrintCmd` : `PRINT LPAR Expression RPAR`
- `ConditionalCmd` : `IF Expression THEN CommandSeq ELSE CommandSeq FI`
- `WhileCmd` : `WHILE Expression DO CommandSeq OD`

- `Expression` : `ID` | `DECIM` | `INTEG` | `TRUE` | `FALSE` | `MAKERAT LPAR Expression COMMA Expression RPAR`
            | `RAT LPAR Expression RPAR` | `FROMDEC LPAR Expression RPAR` | `INV Expression` | `NEG Expression`
            | `BOOLNEG Expression` | `Expression RPLUS Expression` | `Expression RSUB Expression`
            | `Expression RTIMES Expression` | `Expression RDIV Expression` | `Expression PLUS Expression`
            | `Expression SUB Expression` | `Expression TIMES Expression` | `Expression DIV Expression`
            | `Expression MOD Expression` | `Expression AND Expression` | `Expression OR Expression`
            | `LPAR Expression RPAR` | `Expression GREAT Expression` | `Expression GREATOREQ Expression`
            | `Expression LESS Expression` | `Expression LESSOREQ Expression` | `Expression EQUAL Expression`
            | `Expression NOTEQUAL Expression` | `PLUS Expression`

### Design Decisions and Implementation Details

1. The 'interpret' function can be called with the input and output file after running the command 'sml' on the file 'compile.sml'.

2. The language 'PL0' defined by the grammar given is statically scoped. Further, the interpreter implemented ensures that all types are correct and that declarations are present in the input file. It also throws an appropriate error otherwise. Further, it ensures that a procedure can only call the procedures in its static scope order.

3. The unary conversions show_rat, show_decimal and to_decimal have been considered to be commands for printing in the appropriate rational format.

4. It has been assumed that expressions present in standard decimal format represent rationals on their own as well.

5. Further, read not only works on integer,rational and booleans but also their expressions.

6. The precedence order is assignment, relational operators, (AND,OR), all arithmetic operations in usual (plus,sub and times,div,mod) order finally followed by unary operators in ascending order. All operators are left associative.

### Acknowledgements
- [ML-Lex Docs](https://www.smlnj.org/doc/ML-Lex/manual.html) for ml-lex documentation
- [ML-Yacc Example](https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html) for ml-yacc basic syntax of running lexer and parser
- [CM Make SML](https://www.smlnj.org/doc/CM/Old/index.html)