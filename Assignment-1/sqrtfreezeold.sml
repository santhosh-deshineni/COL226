fun correction(a: int list) =
    let
        val lenga = List.length(a)
    in
        if (lenga > 1) then
            if (hd a = 0) then
                correction(tl a)
            else
                a
        else
            a
    end

fun multiply(a: int list, b: int,c: int) =
    if (List.length(a) = 1) then
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
    else if List.length(a) = 0 then
        [0]
    else
        let
            val prod = ((List.nth(a,List.length(a)-1))*b)+c
        in
            let
                val output = multiply(List.take(a,List.length(a)-1),b,(prod-(prod mod 10)) div 10) @ [(prod mod 10)]
            in
                correction(output)
            end
        end

fun lencheck(b: int list,len: int) =
    if (len = 0) then
        0
    else
        List.nth(b,len-1)

fun subtract(a: int list,b: int list,c: int) =
    let
        val lenga = List.length(a)
        val lengb = List.length(b)
    in
        let
            val top = List.nth(a,lenga-1)
            val bottom = lencheck(b,lengb)
        in
                if (lenga = 1) then
                    [top-c-bottom]
                else
                    if (top-c >= bottom) then
                        subtract(List.take(a,lenga-1),List.take(b,lengb-1),0) @ [top-c-bottom]
                    else
                        subtract(List.take(a,lenga-1),List.take(b,lengb-1),1) @ [top-c-bottom+10]
        end
    end

fun compare(a: int list,b: int list) =
    let
        val lenga = List.length(a)
        val lengb = List.length(b)
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


fun checker(curdiv: int list, rem: int list,curcheck: int) =
    if (curcheck = 10) then
        ([9],correction(subtract(rem,multiply(curdiv @ [9],9,0),0)))
    else
        let
            val selfmul = multiply(curdiv @ [curcheck],curcheck,0)
        in
            if compare(selfmul,rem) then
                ([curcheck-1],correction(subtract(rem,multiply(curdiv @ [curcheck-1],curcheck-1,0),0)))
            else
                checker(curdiv,rem,curcheck+1)
        end


    

fun sqrthelp(diglist: int list) =
    let
        val leng = List.length(diglist)
    in
        if (leng > 1) then
            let
                val newList = List.take(diglist, leng - 2)
            in
                let
                    val currenttup = sqrthelp(newList)
                in
                    let
                        val curq = #1 currenttup
                        val currem = correction((#2 currenttup) @ (List.drop(diglist, leng - 2)))
                    in
                        let
                            val checkedtup = checker(multiply(curq,2,0), currem, 1)
                        in
                            let
                                val digneed = #1 checkedtup
                                val newrem = #2 checkedtup
                            in
                                let
                                    val newq = curq @ digneed
                                in
                                    (newq,newrem)
                                end
                            end
                        end
                    end
                end
            end
        else
            if (leng = 1) then
                        let
                            val checkedtup = checker([]: int list, [hd diglist], 1)
                        in
                            let
                                val digneed = #1 checkedtup
                                val newrem = #2 checkedtup
                            in
                                let
                                    val newq = [] @ digneed
                                in
                                    (newq,newrem)
                                end
                            end
                        end
            else
                ([],[])
    end


fun sqrthelper(tup:int list*int list) =
    let
        val first = String.concat (List.map Int.toString (#1 tup));
        val second = String.concat (List.map Int.toString (#2 tup));
    in
        (first,second)
    end

fun isqrtld(a: string) =
    let 
        val a = explode(a)
    in
        let
            val int_list = List.map (fn c => (Char.ord c)-48) a ;
        in
            sqrthelper(sqrthelp(int_list))
        end
    end