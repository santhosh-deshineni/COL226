(* This function corrects an integer present in the form of an integer list by removing trailing zeros created
either during multiplication or subtraction *)
fun correction(a: int list) =
    let
        val lenga = length(a)
    in
        if (lenga > 1) then
            if (hd a = 0) then
                correction(tl a)
            else
                a
        else
            a
    end

(* This function multiplies an integer 'a' present as an integer list with a single digit 'b'. The argument 'c' 
represents the carry at each level of recursion and 'lenga' is the length of the part of int list a currently
considered *)
fun multiply(a: int list, b: int,c: int,lenga: int) =
    if (lenga = 1) then
        let
            val prod = ((hd a)*b)+c
        in
            if (prod > 9) then
                let
                    val output = [(prod-(prod mod 10)) div 10,(prod mod 10)]
                in
                    correction(output)
                end
            else
                let
                    val output = [prod]
                in
                    correction(output)
                end
        end
    else if lenga = 0 then
        [0]
    else
        let
            val prod = ((List.nth(a,lenga-1))*b)+c
        in
            let
                val output = multiply(a,b,(prod-(prod mod 10)) div 10,lenga-1) @ [(prod mod 10)]
            in
                correction(output)
            end
        end

(* This function is a helper function for the 'subtract' function defined below. It helps ensure that a zero is
placed in the lower digit when the 'b' in 'subtract' has lesser digits than 'a' in 'subtract'. *)
fun lencheck(b: int list,len: int) =
    if (len <= 0) then
        0
    else
        List.nth(b,len-1)

(* This function is used to subtract 'b' from 'a'. The argument 'c' represents the borrow which can either
be 0 or 1. 'lenga' and 'lengb' represent the length of the part of the int lists in consideration at the
current level of recursion. *)
fun subtract(a: int list,b: int list,c: int,lenga: int,lengb: int) =
    let
        val top = List.nth(a,lenga-1)
        val bottom = lencheck(b,lengb)
    in
            if (lenga = 1) then
                [top-c-bottom]
            else
                if (top-c >= bottom) then
                    subtract(a,b,0,lenga-1,lengb-1) @ [top-c-bottom]
                else
                    subtract(a,b,1,lenga-1,lengb-1) @ [top-c-bottom+10]
    end

(* This function returns true if 'a' is greater than 'b' and false otherwise *)
fun compare(a: int list,b: int list) =
    let
        val lenga = length(a)
        val lengb = length(b)
    in
        if (lenga > lengb) then
            true
        else if (lenga < lengb) then
            false
        else
            if (hd a > hd b) then
                true
            else if (hd a < hd b) then
                false
            else if (lenga = 1) then
                false
            else
                compare(tl a, tl b)
    end

(* This is a helper function which recurses from 1 to 9 to search for a match which when multiplied with
the divisor on the left gives a number just slightly smaller than the current remainder *)
fun checkerhelp(curdiv: int list, rem: int list,curcheck: int) =
    if (curcheck = 10) then
        let
            val alpha = multiply(curdiv @ [9],9,0,length(curdiv)+1)
        in
            ([9],correction(subtract(rem,alpha,0,length(rem),length(alpha))))
        end
    else
        let
            val selfmul = multiply(curdiv @ [curcheck],curcheck,0,length(curdiv)+1)
        in
            if compare(selfmul,rem) then
                let
                    val beta = multiply(curdiv @ [curcheck-1],curcheck-1,0,length(curdiv)+1)
                in 
                    ([curcheck-1],correction(subtract(rem,beta,0,length(rem),length(beta))))
                end
            else
                checkerhelp(curdiv,rem,curcheck+1)
        end

(* This function calculates the current divisor by doubling the quotient *)
fun divcalc(curq: int list) =
    multiply(curq,2,0,length(curq))

(* This function adds 2 digits to the current remainder *)
fun addpair(currem: int list, diglist: int list, leng: int) =
    currem @ [List.nth(diglist,leng-2)] @ [List.nth(diglist,leng-1)]

(* This function takes current values of quotient and remainder then calls 'checkerhelp' above to give
the needed digit and new remainder *)
fun checker(curq: int list, currem: int list, diglist: int list,leng: int) =
    checkerhelp(divcalc(curq), correction(addpair(currem,diglist,leng)), 1)

(* This is the main helper function of isqrtld which recurses through the number removing 2 digits at a time
then returns the current square root and current remainder at each level of recursion to finally
give the desired output *)
fun sqrthelper(diglist: int list,leng: int) =
    if (leng > 1) then              
        let
            val (curq,currem) = sqrthelper(diglist, leng-2)  
        in                
            let
                val (digneed,newrem) = checker(curq,currem,diglist,leng)
            in
                (curq @ digneed,newrem)
            end
        end
    else if (leng = 1) then
        let
            val (digneed,newrem) = checkerhelp([]: int list, [hd diglist], 1)
        in
            ([] @ digneed,newrem)
        end
    else
        ([],[])

(* This function converts the int lists obtained as output of above function back into strings *)
fun sqrtconvert(tup:int list*int list) =
    let
        val first = String.concat (List.map Int.toString (#1 tup));
        val second = String.concat (List.map Int.toString (#2 tup));
    in
        (first,second)
    end

(* The required function which takes a number 'a' in the form of a string and returns the ordered pair
(b,r) where b is the square root and r is the remainder *)
fun isqrtld(a: string) =
    let 
        val b = explode(a)
    in
        let
            val int_list = List.map (fn c => (Char.ord c)-48) b ;
        in
            sqrtconvert(sqrthelper(correction(int_list),length(correction(int_list))))
        end
    end