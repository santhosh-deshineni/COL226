structure PL0 :
sig val compiler : string*string -> unit
end =
    struct
    exception PL0Error;
    exception div_by_zero_error;
    exception proc_not_found_error;
    exception var_not_found_error;
    exception incorrect_input_error;

    val IDvalscopelist: (string*(DataTypes.unionofibr)*int) list ref = ref []
    val procscopelist: (DataTypes.PRO * int) list ref = ref []
    val outputstrref: TextIO.outstream option ref = ref NONE 

    fun makeCounter() =
    let
        val countRef = ref 0
        fun counter(n: int) =
        if !countRef = 0 then
            (countRef := 1; valOf(TextIO.inputLine TextIO.stdIn))
        else
            ""
    in
        counter
    end

    fun uniontostr(x) = 
        case x of DataTypes.Intum(A) => BigInt.toString(A)
        | DataTypes.Ratum(A) =>Rational.toDecimal(A)
        | DataTypes.Boolum(A) => if (A) then "tt" else "ff"
    fun typereturn(x) = 
        case x of DataTypes.Intum(A) => "int"
        | DataTypes.Ratum(A) => "rat"
        | DataTypes.Boolum(A) => if (A) then "true" else "false"
    fun prog_to_block(DataTypes.PROG(A,B)) = B
    fun block_to_dec(DataTypes.BLK(A,B)) = A
    fun block_to_comlist(DataTypes.BLK(A,B)) = B
    fun dec_to_vardec(DataTypes.DEC(A,B)) = A
    fun dec_to_prolist(DataTypes.DEC(A,B)) = B
    fun vardec_to_ratdec(DataTypes.VARDEC(A,B,C)) = A
    fun vardec_to_intdec(DataTypes.VARDEC(A,B,C)) = B
    fun vardec_to_booldec(DataTypes.VARDEC(A,B,C)) = C
    fun proc_to_name(DataTypes.PRO(A,B,C)) = A
    fun proc_to_block(DataTypes.PRO(A,B,C)) = B
    fun proc_to_scopestack(DataTypes.PRO(A,B,C)) = C
    fun get_bool(DataTypes.Boolum(A)) = A
        | get_bool(x) = false
    fun get_int(DataTypes.Intum(A)) = A
        | get_int(x) = BigInt.fromString("0")
    fun get_rat(DataTypes.Ratum(A)) = A
        | get_rat(x) = Rational.fromDecimal("0.(0)")

    fun ExptoTree () =
    let
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print ("stdin"^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (exptree,rem) = CalcParser.parse
        (15,
        (CalcParser.makeLexer (makeCounter())),
        printError,
        ()) 
        handle CalcParser.ParseError => raise PL0Error ;
    in 
        exptree
    end

    fun assign_help_found(x, valu, scope, checklist, storelist) =
        (IDvalscopelist:=rev(storelist)@((x,valu,scope)::(tl checklist)))

    fun assign_help(x:string, valu:DataTypes.unionofibr, scope:int, checklist:(string*(DataTypes.unionofibr)*int) list,storelist) =
        if (null(checklist)) then
            false
        else if ((x = (#1 (hd checklist))) andalso (scope = (#3 (hd checklist)))) then
            (assign_help_found(x,valu,scope,checklist,storelist);true)
        else
            assign_help(x, valu, scope, tl checklist, hd checklist::storelist)

    fun assign(x,valu,scopestack) =
        if (scopestack = []) then
            raise var_not_found_error
        else if assign_help(x,valu, hd scopestack,(!IDvalscopelist),[]) then
            ()
        else
            assign(x,valu,tl scopestack)

    fun dynamic_type_check_help_found(x, valu, scope, checklist, storelist) =
        if (not(typereturn(valu) = typereturn(#2 (hd checklist)))) then
            raise incorrect_input_error
        else
            (IDvalscopelist:=rev(storelist)@((x,valu,scope)::(tl checklist)))

    fun dynamic_type_check_help(x:string, valu:DataTypes.unionofibr, scope:int, checklist:(string*(DataTypes.unionofibr)*int) list,storelist) =
        if (null(checklist)) then
            false
        else if ((x = (#1 (hd checklist))) andalso (scope = (#3 (hd checklist)))) then
            (dynamic_type_check_help_found(x,valu,scope,checklist,storelist);true)
        else
            dynamic_type_check_help(x, valu, scope, tl checklist, hd checklist::storelist)

    fun dynamic_type_check(x,valu,scopestack) =
        if (scopestack = []) then
            raise var_not_found_error
        else if dynamic_type_check_help(x,valu, hd scopestack,(!IDvalscopelist),[]) then
            ()
        else
            dynamic_type_check(x,valu,tl scopestack)

    fun search_help(x:string,scope,checklist: (string*(DataTypes.unionofibr)*int) list) =
        if (null(checklist)) then
            (false,DataTypes.Boolum(true))
        else if (((#1 (hd checklist)) = x) andalso ((#3 (hd checklist)) = scope)) then
            (true,(#2 (hd checklist)))
        else
            search_help(x,scope, tl checklist)
    
    fun search(x,scopestack) =
        let 
            val tup = search_help(x, hd scopestack, (!IDvalscopelist))
        in
            if (#1 tup) then
                (#2 tup)
            else
                search(x, tl scopestack)
        end

    fun storerats(ratlist,scoper) =
        if (ratlist = ([]:string list)) then
            ()
        else
            (IDvalscopelist:=((hd ratlist),DataTypes.Ratum(Rational.fromDecimal("0.(0)")),scoper)::(!IDvalscopelist);storerats(tl ratlist,scoper))

    fun storeints(intlist,scoper) =
        if (intlist = ([]:string list)) then
            ()
        else
            (IDvalscopelist:=((hd intlist),DataTypes.Intum(BigInt.fromString("0")),scoper)::(!IDvalscopelist);storeints(tl intlist,scoper))

    fun storebools(boollist,scoper) =
        if (boollist = ([]:string list)) then
            ()
        else
            (IDvalscopelist:=((hd boollist),DataTypes.Boolum(false),scoper)::(!IDvalscopelist);storebools(tl boollist,scoper))

    fun storeprocs(inplist,scoper) =
        if (null(inplist)) then
            ()
        else
            (procscopelist:=((hd inplist,scoper))::(!procscopelist);storeprocs(tl inplist,scoper))

    fun evalExp(exp,scopestack) =
        case exp of DataTypes.make_ratcon(A,B) => DataTypes.Ratum(valOf(Rational.make_rat(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.ratcon(A) => DataTypes.Ratum(valOf(Rational.rat(get_int(evalExp(A,scopestack)))))
        | DataTypes.from_deccon(A) => evalExp(A,scopestack)
        | DataTypes.invcon(A) => let val inver = Rational.inverse(get_rat(evalExp(A,scopestack))) in DataTypes.Ratum(valOf(inver)) end
        | DataTypes.rat_negcon(A) => DataTypes.Ratum(Rational.neg(get_rat(evalExp(A,scopestack))))
        | DataTypes.int_negcon(A) => DataTypes.Intum(BigInt.neg(get_int(evalExp(A,scopestack))))
        | DataTypes.bool_negcon(A) => DataTypes.Boolum(not(get_bool(evalExp(A,scopestack))))
        | DataTypes.rat_plus(A,B) => DataTypes.Ratum(Rational.add(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))))
        | DataTypes.rat_sub(A,B) => DataTypes.Ratum(Rational.subtract(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))))
        | DataTypes.rat_times(A,B) => DataTypes.Ratum(Rational.multiply(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))))
        | DataTypes.rat_div(A,B) => let val quot = Rational.divide(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))) in DataTypes.Ratum(valOf(quot)) end
        | DataTypes.plus(A,B) => DataTypes.Intum(BigInt.add(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack))))
        | DataTypes.sub(A,B) => DataTypes.Intum(BigInt.subtract(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack))))
        | DataTypes.times(A,B) => DataTypes.Intum(BigInt.multiply(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack))))
        | DataTypes.divi(A,B) => DataTypes.Intum(#1 (BigInt.divandrem(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.modu(A,B) => DataTypes.Intum(#2 (BigInt.divandrem(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.andcon(A,B) => DataTypes.Boolum(get_bool(evalExp(A,scopestack)) andalso get_bool(evalExp(B,scopestack)))
        | DataTypes.orcon(A,B) => DataTypes.Boolum(get_bool(evalExp(A,scopestack)) orelse get_bool(evalExp(B,scopestack)))
        | DataTypes.int_greatcon(A,B) => DataTypes.Boolum((BigInt.less(get_int(evalExp(B,scopestack)),get_int(evalExp(A,scopestack)))))
        | DataTypes.rat_greatcon(A,B) => DataTypes.Boolum((Rational.less(get_rat(evalExp(B,scopestack)),get_rat(evalExp(A,scopestack)))))
        | DataTypes.int_greatoreqcon(A,B) => DataTypes.Boolum((BigInt.less(get_int(evalExp(B,scopestack)),get_int(evalExp(A,scopestack)))) orelse (BigInt.equality(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.rat_greatoreqcon(A,B) => DataTypes.Boolum((Rational.less(get_rat(evalExp(B,scopestack)),get_rat(evalExp(A,scopestack)))) orelse (Rational.equal(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack)))))
        | DataTypes.int_lesscon(A,B) => DataTypes.Boolum((BigInt.less(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.rat_lesscon(A,B) => DataTypes.Boolum((Rational.less(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack)))))
        | DataTypes.int_lessoreqcon(A,B) => DataTypes.Boolum((BigInt.less(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))) orelse (BigInt.equality(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.rat_lessoreqcon(A,B) => DataTypes.Boolum((Rational.less(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack)))) orelse (Rational.equal(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack)))))
        | DataTypes.int_equalcon(A,B) => DataTypes.Boolum(BigInt.equality(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack))))
        | DataTypes.rat_equalcon(A,B) => DataTypes.Boolum(Rational.equal(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))))
        | DataTypes.bool_equalcon(A,B) => DataTypes.Boolum((get_bool(evalExp(A,scopestack)) = get_bool(evalExp(B,scopestack))))
        | DataTypes.int_notequalcon(A,B) => DataTypes.Boolum(not(BigInt.equality(get_int(evalExp(A,scopestack)),get_int(evalExp(B,scopestack)))))
        | DataTypes.rat_notequalcon(A,B) => DataTypes.Boolum(not((Rational.equal(get_rat(evalExp(A,scopestack)),get_rat(evalExp(B,scopestack))))))
        | DataTypes.bool_notequalcon(A,B) => DataTypes.Boolum((not(get_bool(evalExp(A,scopestack)) = get_bool(evalExp(B,scopestack)))))
        | DataTypes.Ident(A) => search(A,scopestack)
        | DataTypes.UNIONCON(A) => A

    fun evalCommands(comlist,block,scopestack) =
        let
            fun proc_search(name,scopestack) =
                let
                    fun procedure_call(block,scopestack) =
                        let
                            val rattab = vardec_to_ratdec(dec_to_vardec(block_to_dec(block)))
                            val inttab = vardec_to_intdec(dec_to_vardec(block_to_dec(block)))
                            val booltab = vardec_to_booldec(dec_to_vardec(block_to_dec(block)))
                            val proctab = dec_to_prolist(block_to_dec(block))
                            fun pop_vars(size) =
                                if (size = 0) then
                                    ()
                                else
                                    ((IDvalscopelist := (tl (!IDvalscopelist)));pop_vars(size-1))
                            fun pop_procs(size) =
                                if (size = 0) then
                                    ()
                                else
                                    (((procscopelist) := (tl (!procscopelist)));pop_procs(size-1))
                        in
                            (storerats(rattab,hd scopestack);
                            storeints(inttab,hd scopestack);
                            storebools(booltab,hd scopestack);
                            storeprocs(proctab,hd scopestack);
                            evalCommands(block_to_comlist(block),block,scopestack);pop_vars(length(rattab)+length(inttab)+length(booltab));pop_procs(length(proctab)))
                        end
                    fun proc_search_help(name:string,scope,prlist: ((DataTypes.PRO)*int) list) =
                        if (null(prlist)) then
                            (false)
                        else if ((proc_to_name(#1 (hd prlist)) = name) andalso ((#2 (hd prlist)) = scope)) then
                            (procedure_call(proc_to_block(#1 (hd prlist)),proc_to_scopestack(#1 (hd prlist)));true)
                        else
                            proc_search_help(name,scope,tl prlist)
                in
                    if (null(!procscopelist)) then
                        raise proc_not_found_error
                    else if (proc_search_help(name,hd scopestack,(!procscopelist))) then
                        ()
                    else
                        proc_search(name,tl scopestack)
                end
        in
            if (null(comlist)) then
                ()
            else
                let
                    val Command = hd comlist
                in
                    (case Command of DataTypes.ASSG(A,B) => ((assign(A,evalExp(B,scopestack),scopestack);evalCommands(tl comlist,block,scopestack)))
                    | DataTypes.COND(A,B,C) =>  if (get_bool(evalExp(A,scopestack))) then
                                                    (evalCommands(B,block,scopestack);evalCommands(tl comlist,block,scopestack))
                                                else
                                                    (evalCommands(C,block,scopestack);evalCommands(tl comlist,block,scopestack))
                    | DataTypes.WH(A,B) =>  if (get_bool(evalExp(A,scopestack))) then
                                                (evalCommands(B,block,scopestack);evalCommands(comlist,block,scopestack))
                                            else
                                                (evalCommands(tl comlist,block,scopestack))
                    | DataTypes.CAL(A) => (proc_search(A,scopestack);evalCommands(tl comlist,block,scopestack))
                    | DataTypes.RE(A) => (dynamic_type_check(A,evalExp(ExptoTree(),[]),scopestack);evalCommands(tl comlist,block,scopestack))
                    | DataTypes.PRI(A) => (TextIO.output(valOf(!outputstrref),uniontostr(evalExp(A,scopestack)) ^ "\n");evalCommands(tl comlist,block,scopestack))
                    | DataTypes.to_deccon(A) => (TextIO.output(valOf(!outputstrref),uniontostr(evalExp(A,scopestack)) ^ "\n");evalCommands(tl comlist,block,scopestack))
                    | DataTypes.show_ratcon(A) => (TextIO.output(valOf(!outputstrref),uniontostr(evalExp(A,scopestack)) ^ "\n");evalCommands(tl comlist,block,scopestack))
                    | DataTypes.show_deccon(A) => (TextIO.output(valOf(!outputstrref),uniontostr(evalExp(A,scopestack)) ^ "\n");evalCommands(tl comlist,block,scopestack)))
                end
        end

    fun compiler (fileName,outputFile) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = PL0Parser.parse
        (15,
        (PL0Parser.makeLexer grab ),
        printError,
        ()) 
        handle PL0Parser.ParseError => raise PL0Error ; 
        val _ = TextIO.closeIn inStream;

    in 
        (outputstrref:=(SOME (TextIO.openOut outputFile));storerats(vardec_to_ratdec(dec_to_vardec(block_to_dec(prog_to_block(tree)))),1);
        storeints(vardec_to_intdec(dec_to_vardec(block_to_dec(prog_to_block(tree)))),1);
        storebools(vardec_to_booldec(dec_to_vardec(block_to_dec(prog_to_block(tree)))),1);
        storeprocs(dec_to_prolist(block_to_dec(prog_to_block(tree))),1);
        evalCommands(block_to_comlist(prog_to_block(tree)),prog_to_block(tree),[1]);TextIO.closeOut (valOf(!outputstrref));IDvalscopelist:=[];procscopelist:=[])
    end
    
end;
