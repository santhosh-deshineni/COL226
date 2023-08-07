(* The below file is the output of ml-lex which is being utilized to check for the correctness of format
in the input to functions taking a string namely BigInt.fromString and Rational.fromDecimal. Their regular
expression was provided to ml-lex and a corresponding token is outputted as a string which can be checked
for in the functions below. *)

(* A signature for implementing integers of arbitrary length containing some basic functions of conversion and
a few binary integer operations. *)
signature BIGINT =
sig
    type bigint
    exception bigint_error
    val fromString: string -> bigint
    val toString: bigint -> string
    val getonesdigit: bigint -> int
    val neg: bigint -> bigint
    val check_pos: bigint -> bool
    val equality: bigint*bigint -> bool
    val less: bigint*bigint -> bool
    val add: bigint*bigint -> bigint
    val subtract: bigint*bigint -> bigint
    val multiply: bigint*bigint -> bigint
    val divandrem: bigint*bigint -> bigint*bigint
    val gcd: bigint*bigint -> bigint
    val abs_value: bigint -> bigint
end;

(* A structure which is an implementation of the above signature utilizing integer lists with
the very first or head element being used for sign. It is 1 for +ve and 0 for -ve integers. *)
structure BigInt: BIGINT =
struct
  type bigint = int list;

  exception bigint_error;

  (* returns the digit present at ones place of integer *)
  fun getonesdigit(x) =
    hd(tl x)

  (* A helper function for zero_rem *)
  fun zero_rem_helper(x) =
    if null(x) then
      [0]
    else
      if (hd x = 0) then
        zero_rem_helper(tl x)
      else
        x ;

  (* removes the trailing zeroes of output occurring after certain operations *)
  fun zero_rem(a) =
      (hd a) :: rev(zero_rem_helper(rev(tl a))) ;

  (* converts a string into a bigint type. The lexer is used to check if input format is correct. *)
  fun fromString(a) =
    let
      val boolcheck = StringLex.makeLexer(fn _ => (a ^ "\n"))()
    in
      if (boolcheck = "T1") then
        let
            val (sign,digits) =
            case String.sub(a, 0) of
            #"+" => (1, String.extract(a, 1, NONE))
            | #"~" => (0, String.extract(a, 1, NONE))
            | _ => (1, a)
        in
            zero_rem([sign] @ List.map (fn x => ord x - ord #"0") (rev(explode(digits))))
        end
      else
        raise bigint_error
    end ;
  
  (* checks if a given bigint is +ve *)
  fun check_pos(x) =
    if (hd x = 1) then
      true
    else
      false

  (* converts bigint back into string representation *)
  fun toString(a) =
    if (check_pos(a)) then
      String.implode(List.map (fn(x) => Char.chr(x+48)) (rev(tl a)))
    else
      "~" ^ String.implode(List.map (fn(x) => Char.chr(x+48)) (rev(tl a)))
  
  (* helper function for equality *)
  fun equality_helper(x: int list,y: int list) =
    if (null(x) = false andalso null(y) = false) then
      if (hd x = hd y) then
        equality_helper(tl x,tl y)
      else
        false
    else if (null(x) andalso null(y)) then
      true
    else
      false ;

  (* checks if two bigints are equal *)
  fun equality(a: bigint,b: bigint): bool =
    if (hd a = hd b) then
      equality_helper(tl a,tl b)
    else
      false ;

  (* returns negative of a bigint *)
  fun neg(x) =
    if equality(x,fromString("0")) then
      [1,0]
    else if (hd x = 1) then
      (0 :: tl x)
    else
      (1 :: tl x) ;

  (* Helper function for less. Takes a and b in not reversed form without sign given length is same
  and returns true if a < b *)
  fun less_helper(a,b) =
    if (null(a) = true) then
      false
    else
      if (hd a < hd b) then
        true
      else if (hd a = hd b) then
        less_helper(tl a, tl b)
      else
        false;

  (* checks if a is less than b *)
  fun less(a,b) =
    let
      val heada = hd a
      val headb = hd b
      val lenga = length(a)
      val lengb = length(b)
    in
      if (heada = 0 andalso headb = 0) then
        if (lenga < lengb) then
          false
        else if (lenga = lengb) then
          not(less_helper(rev(tl a),rev(tl b)))
        else
          true
      else if (heada = 1 andalso headb = 1) then
        if (lenga < lengb) then
          true
        else if (lenga = lengb) then
          less_helper(rev(tl a),rev(tl b))
        else
          false
      else if (heada = 0 andalso headb = 1) then
        true
      else
        false
    end ;

  (* Helper function for adding two +ve numbers.takes x and y in reverse form without signand returns ans in reverse form
  without sign *)
  fun add_helper(x,y,carry,ans) =
    if (not(null(x)) andalso not(null(y))) then
      let
        val sum = (hd x + hd y + carry) mod 10
        val newcarry = (hd x + hd y + carry) div 10
      in
        add_helper(tl x,tl y,newcarry,(sum) :: ans)
      end
    else if (null(x) andalso null(y)) then
      if (carry = 1) then
        rev(1 :: ans)
      else
        rev(ans)
    else if (null(x)) then
      if (carry = 1) then
        if (hd y = 9) then
          add_helper(x,tl y,1,(0)::ans)
        else
          rev(ans) @ ((hd y +1) :: (tl y))
      else
        rev(ans) @ y
    else
      if (carry = 1) then
        if (hd x = 9) then
          add_helper(tl x,y,1,(0)::ans)
        else
          rev(ans) @ ((hd x +1) :: (tl x))
      else
        rev(ans) @ x ;

  (* Helper function for subtraction of two +ve numbers.does x - y for x and y in reverse form without sign
  given x > y and returns ans in reverse form without sign *)
  fun sub_helper(x,y,borrow,ans) =
    if (not(null(y))) then
      let
        val top = hd x
        val bottom = hd y
      in
        if (top - borrow >= bottom) then
          sub_helper(tl x,tl y,0,(top-borrow-bottom) :: ans)
        else
          sub_helper(tl x, tl y,1,(top-borrow-bottom+10) :: ans)
      end
    else
      if null(x) then
        rev(ans)
      else
        if (borrow = 1) then
          if (hd x = 0) then
            sub_helper(tl x, y, 1, 9 :: ans)
          else
            rev(ans) @ ((hd x - 1) :: tl x)
        else
          rev(ans) @ x ;
  
  (* adds two bigints and returns the sum as a bigint *)
  fun add(a,b) =
    let
      val heada = hd a
      val headb = hd b
    in
      if (heada = 1 andalso headb = 1) then
        1 :: add_helper(tl a,tl b,0,[])
      else if (heada = 0 andalso headb = 0) then
        0 :: add_helper(tl a,tl b,0,[])
      else if (heada = 0 andalso headb = 1) then
        if less(1 :: tl a, 1 :: tl b) then
          1 :: sub_helper(tl b, tl a,0,[])
        else
          0 :: sub_helper(tl a, tl b,0,[])
      else
        if less(1 :: tl a,1 :: tl b) then
          0 :: sub_helper(tl b, tl a,0,[])
        else
          1 :: sub_helper(tl a, tl b,0,[])
    end ;
  
  (* subtracts two bigints and returns the difference as a bigint *)
  fun subtract(a,b) =
    let
      val heada = hd a
      val headb = hd b
    in
      if (heada = 1 andalso headb = 1) then
        if less(1 :: tl a, 1 :: tl b) then
          zero_rem(0 :: sub_helper(tl b, tl a,0,[]))
        else
          zero_rem(1 :: sub_helper(tl a, tl b,0,[]))
      else if (heada = 0 andalso headb = 0) then
        if less(1 :: tl a, 1 :: tl b) then
          zero_rem(1 :: sub_helper(tl b, tl a,0,[]))
        else
          zero_rem(0 :: sub_helper(tl a, tl b,0,[]))
      else if (heada = 0 andalso headb = 1) then
        zero_rem(0 :: add_helper(tl a,tl b,0,[]))
      else
        zero_rem(1 :: add_helper(tl a,tl b,0,[]))
    end ;
  
  (* Helper function for multiplying a large int with a single digit. Multiply int list in reversed form
  without sign with a single digit and return reversed form without sign *)
  fun mult_with_digit(a,digit,carry,ans) =
    if (not(null(a))) then
      let
        val prod = (((hd a)*digit)+carry) mod 10
        val newcarry = (((hd a)*digit)+carry) div 10
      in
        mult_with_digit(tl a, digit, newcarry,prod :: ans)
      end
    else
      if (carry = 0) then
        rev(ans)
      else
        rev(carry :: ans) ;
  
  (* Helper function for multiply. *)
  fun mult_helper(x,y) =
    if (null(y)) then
      []
    else
      add_helper((mult_with_digit(x,hd y,0,[])),(0 :: mult_helper(x,tl y)),0,[]) ;
  
  (* multiplies two bigints and returns the product as a bigint *)
  fun multiply(a,b) =
    if (b = [1,0] orelse a = [1,0]) then
      [1,0]
    else if (hd a = hd b) then
      (1) :: (mult_helper(tl a,tl b))
    else
      (0) :: (mult_helper(tl a,tl b)) ;
  
  (* Helper function to find the digit the divisor must be multiplied with during division. 
  y and div in reverse form with sign *)
  fun find_multiplier(y,curdiv,num) =
    if (num = 9) then
      (9,tl (subtract(curdiv,multiply(y,[1,num]))))
    else
      if ((less(multiply(y,[1,num]),curdiv) orelse equality(multiply(y,[1,num]),curdiv)) andalso not(less(multiply(y,[1,num+1]),curdiv) orelse equality(multiply(y,[1,num+1]),curdiv)))  then
        (num,tl(subtract(curdiv,multiply(y,[1,num]))))
      else
        find_multiplier(y,curdiv,num+1) ;

  (* Helper function for divide. x in normal and y in reverse form without sign *)
  fun divide_helper(x,y,curdiv,curquot) =
    if (null(x)) then
      (1 :: curquot, 1::curdiv)
    else
      let
        val (multi,newdiv) = find_multiplier(1::y,zero_rem(1::(hd x)::(curdiv)),0)
      in
        divide_helper(tl x,y,newdiv,(multi)::(curquot))
      end ;
  
  (* returns a tuple of quotient and remainder obtained when dividing two bigints *)
  fun divandrem(a,b) =
    let
      val (quot,rem) = (divide_helper(rev(tl a),tl b,[],[]))
    in
      if (equality(b,[1,0])) then
        raise bigint_error
      else
        if (hd a = 1 andalso hd b = 1) then
          (zero_rem(quot),zero_rem(rem))
        else if (hd a = 1 andalso hd b = 0) then
          (neg(zero_rem(quot)),zero_rem(rem))
        else if (hd a = 0 andalso hd b = 1) then
          (neg(zero_rem(quot)),neg(zero_rem(rem)))
        else
          (zero_rem(quot),neg(zero_rem(rem)))
    end
  
  (* returns absolute value of bigint *)
  fun abs_value(a) =
    (1 :: tl a) ;
  
  (* Helper function for GCD *)
  fun gcd_helper(a,b) =
    if b = fromString("0") then
      a
    else
      gcd_helper(b, #2(divandrem(a,b))) ;

  (* returns the GCD of absolute values of any two bigints *)
  fun gcd(a, b) = gcd_helper(abs_value(a),abs_value(b)) ;

end ;

(* the below functor is given the above BigInt structure as input to create the Rational structure *)
functor MakeRational (SInt : BIGINT):
sig
  type rational
  exception rat_error
  val make_rat: SInt.bigint * SInt.bigint -> rational option
  val rat: SInt.bigint -> rational option
  val reci: SInt.bigint -> rational option
  val neg: rational -> rational
  val inverse : rational -> rational option
  val equal : rational * rational -> bool (* equality *)
  val less : rational * rational -> bool (* less than *)
  val add : rational * rational -> rational (* addition *)
  val subtract : rational * rational -> rational (* subtraction *)
  val multiply : rational * rational -> rational (* multiplication *)
  val divide : rational * rational -> rational option (* division *)
  val showRat : rational -> string
  val showDecimal : rational -> string
  val fromDecimal : string -> rational
  val toDecimal : rational -> string
end
=
struct
  type rational = SInt.bigint * SInt.bigint;

  exception rat_error;

  (* gives a rational a/b from the bigints *)
  fun make_rat(a,b): rational option =
    let
      val gcd = SInt.gcd(a,b)
    in
      if (SInt.equality(b,SInt.fromString("0"))) then
        NONE
      else if (SInt.check_pos(b) = false) then
        SOME (#1(SInt.divandrem(SInt.neg(a),gcd)),#1(SInt.divandrem(SInt.neg(b),gcd)))
      else
        SOME (#1(SInt.divandrem(a,gcd)),#1(SInt.divandrem(b,gcd)))
    end ;
  
  (* gives the rational corresponding to an integer *)
  fun rat(i) = SOME (i,SInt.fromString("1"));

  (* gives the reciprocal of an integer as a rational *)
  fun reci(i) =
  if (SInt.equality(i,SInt.fromString("0"))) then
    NONE
  else
    if (SInt.check_pos(i)) then
      SOME (SInt.fromString("1"),i)
    else
      (make_rat(SInt.fromString("~1"),SInt.neg(i))) ;
  
  (* gives the negative of a rational *)
  fun neg(ration: rational): rational =
    let
      val (num,den) = (ration)
    in
      valOf(make_rat(SInt.neg(num),den))
    end ;
  
  (* gives the invers of a rational that is for a rational a/b, it inverts this*)
  fun inverse(ration) =
    let
      val (num,den) = (ration)
    in
      if (SInt.equality(num,SInt.fromString("0"))) then
        NONE
      else if (SInt.check_pos(num)) then
        SOME (den,num)
      else
        SOME (SInt.neg(den),SInt.neg(num))
    end ;

  (* checks it two rationals a and b are equal *)
  fun equal(a,b) =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      if (SInt.equality(num1,num2) andalso SInt.equality(den1,den2)) then
        true
      else
        false
    end ;
  (* checks if a is less than b *)
  fun less(a,b) =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      if (SInt.check_pos(num1)) then
        if (SInt.check_pos(num2)) then
          SInt.less(SInt.multiply(num1,den2),SInt.multiply(num2,den1))
        else
          false
      else
        if (SInt.check_pos(num2)) then
          true
        else
          SInt.less(SInt.multiply(num1,den2),SInt.multiply(num2,den1))
    end ;
  
  (* adds two rationals and gives sum *)
  fun add(a:rational,b:rational): rational =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      valOf(make_rat(SInt.add(SInt.multiply(num1,den2),SInt.multiply(num2,den1)),SInt.multiply(den1,den2)))
    end ;
  (* subtracts two rationals and gives difference *)
  fun subtract(a:rational,b:rational): rational =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      valOf(make_rat(SInt.subtract(SInt.multiply(num1,den2),SInt.multiply(num2,den1)),SInt.multiply(den1,den2)))
    end ;
  
  (* gives product of two rationals *)
  fun multiply(a,b) =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      valOf(make_rat(SInt.multiply(num1,num2),SInt.multiply(den1,den2)))
    end ;
  
  (* divides two rationals a and b *)
  fun divide(a,b) =
    let
      val (num1,den1) = (a)
      val (num2,den2) = (b)
    in
      make_rat(SInt.multiply(num1,den2),SInt.multiply(den1,num2))
    end ;
  
  (* shows or returns a string to show the rational in p/q form *)
  fun showRat(a) =
    let
      val (num,den) = a
    in
      SInt.toString(num) ^ "/" ^ SInt.toString(den)
    end ;
  
  (* helper function to check if item is present in a list *)
  fun member_of (item, list) = List.exists (fn (x) => SInt.equality(x,item)) list;
  
  (* helper function to give out the string after the decimal point if the number has a recurring part *)
  fun getstring(rem,storedrems,quot,ans) =
    if (SInt.equality(rem,hd storedrems)) then
      ans ^ "(" ^ quot ^ ")"
    else
      getstring(rem,tl storedrems,String.extract(quot, 1, NONE),ans ^ String.extract(quot, 0, SOME 1)) ;
  
  (* helper function for showDecimal to give part of string after decimal point*)
  fun show_dec_helper(rem,divisor,quot,remstore) =
    if (SInt.equality(rem,SInt.fromString("0"))) then
      quot ^ "(0)"
    else if member_of(rem,remstore) then
      getstring(rem,rev(remstore),(quot),"")
    else
      let
        val (a,b) = SInt.divandrem(SInt.multiply(rem,SInt.fromString("10")),divisor)
      in
        show_dec_helper(b,divisor,quot ^ SInt.toString(a),rem :: remstore)
      end ;

  (* returns the rational number in decimal-normal form *)
  fun showDecimal(x) =
    let
      val (top,bottom) = SInt.divandrem(#1(x),#2(x))
      val (num,den) = x
    in
      SInt.toString(top) ^ "." ^ show_dec_helper(SInt.abs_value(bottom),den,"",[])
    end ;
  
  (* same as showDecimal *)
  fun toDecimal(x) = showDecimal(x) ;

  fun repntimes(n) =
    String.implode(List.tabulate(n, fn _ => #"0"));
  
  fun special_fromString(a) =
    if (a = "+" orelse a = "" orelse a = "~") then
      SInt.fromString("0")
    else
      SInt.fromString(a)

  fun from_decimal_helper(x,y,z) =
    let
      val bigsize = size(y)+size(z)
      val smallsize = size(y)
    in
      valOf(make_rat(SInt.subtract(SInt.fromString(x ^ y ^ z),special_fromString(x ^ y)),SInt.subtract(SInt.fromString("1" ^ repntimes(bigsize)),SInt.fromString("1" ^ repntimes(smallsize)))))
    end ;

  fun count_till_dot(x,num) =
    if String.substring(x,num,1) = "." then
      num
    else
      count_till_dot(x,num+1) ;

  fun count_till_brack(x,num) =
    if String.substring(x,num,1) = "(" then
      num
    else
      count_till_brack(x,num+1) ;

  (* converts a string in decimal form to rational *)
  fun fromDecimal(a) =
    let
      val boolcheck = StringLex.makeLexer(fn _ => (a ^ "\n"))()
    in
      if (boolcheck = "T2") then
        let
          val dot_in = count_till_dot(a,0)
          val br_in = count_till_brack(a,0)
        in
          from_decimal_helper(String.substring(a,0,dot_in),String.substring(a,dot_in+1,br_in-dot_in-1),String.substring(a,br_in+1,String.size(a)-br_in-2))
        end
      else
        raise (rat_error)
    end ;

end ;

(* creating the Rational structure from BigInt using above functor *)
structure Rational = MakeRational(BigInt);