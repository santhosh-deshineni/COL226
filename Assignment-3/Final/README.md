## README for COL226 Assignment-3

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

### Design Decisions and Implementation Details

1. The structures can be obtained by using the command `sml rational.sml` which opens up an SML terminal with the `BigInt` structure of `BIGINT` signature and a functor which takes `BigInt` to give `Rational` structure implemented in it. The `Rational` structure has already been created by calling the functor named `MakeRational` in `rational.sml`.

2. Running the command `sml calculator.sml` opens up the input stream for the rational number expressions calculator. The program can be run again to input another expression. `ml-lex` and `ml-yacc` has been used here to scan and parse the expression which is then provided to functions of `Rational` structure.

3. The function `BigInt.fromString` can be utilized to create a bigint given the integer in string form. This can then be used in `Rational.make_rat` to create a rational. This allows us to completely make use of the "Rational" structure to carry out arbitrary length rational number operations without the need for understanding of the implementation of the structure.

4. `ml-lex` has been used in the functions of the `BigInt` and `Rational` structure which take string input to allow scanning of input and reporting of errors in case of wrong format. In case the format is wrong, the corresponding exception is raised.

### Acknowledgements
- [ML-Lex Docs](https://www.smlnj.org/doc/ML-Lex/manual.html) for ml-lex documentation
- [ML-Yacc Example](https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html) for ml-yacc basic syntax of running lexer and parser