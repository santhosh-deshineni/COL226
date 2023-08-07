fun mdt2html(s: string) = 
    let
        val infile = TextIO.openIn(s^".mdt")
        val outfile = TextIO.openOut(s^".html")

        fun headingcount(num) =
            let
                val nextch = TextIO.inputN(infile,1)
            in
                if (nextch = "#") then
                    headingcount(num+1)
                else if (nextch = " ") then
                    (num,TextIO.inputN(infile,1))
                else
                    (num,nextch)
            end

        fun fileconvert(ch, hashtags, inpara, linestart, inital, inbold, underline, inol, indent) =
            let
                fun lbcount(num,ws) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (nextch = "\n") then
                            lbcount(num+1,0)
                        else if (nextch = " ") then
                            lbcount(num,ws+1)
                        else
                            ((if (ws = 4) then
                                (TextIO.output(outfile,"</p>\n\n<pre><code>");()) else ());
                            (if (num = 1 andalso inpara = 1) then
                                if (nextch = "#") then
                                    (TextIO.output(outfile,"</p>\n\n");fileconvert(nextch,0,0,1,inital,inbold,0,inol,indent))
                                else
                                    (TextIO.output(outfile,"\n");fileconvert(nextch,0,1,0,inital,inbold,0,inol,indent))
                            else if ((num = 1 orelse num >= 2) andalso inpara = 0) then
                                (TextIO.output(outfile,"</h"^(Int.toString(hashtags))^">\n\n");fileconvert(nextch,0,0,1,inital,inbold,0,inol,indent))
                            else if (num >= 2 andalso inpara = 1) then
                                (TextIO.output(outfile,"</p>\n\n");fileconvert(nextch,0,1,1,inital,inbold,0,inol,indent))
                            else
                                fileconvert(nextch,0,0,1,inital,inbold,0,inol,indent)))
                    end
                
                fun olorul(ch) = 
                    if (ch = "-") then
                        ""
                    else
                        TextIO.inputN(infile, 1)

                fun olchecker(ch, check, shift, wsnum, hypcount) = 
                    let
                        val nextch = olorul(ch)
                    in 
                        if (check = 0) then
                            if (ch = "-") then
                                olchecker(ch,2,shift,wsnum,1)
                            else if (Int.fromString(nextch) <> NONE) then
                                olchecker(ch^nextch,0,shift,wsnum,0)
                            else if (nextch = ".") then
                                olchecker(ch^nextch,2,shift,wsnum,0)
                            else if (nextch = "\\" ) then
                                (TextIO.inputN(infile,1);fileconvert("</p></li></ol><p>"^ch^".",hashtags,1,0,inital,inbold,underline,inol,indent))
                            else
                                (TextIO.output(outfile,ch^nextch);fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,underline,inol,indent))

                        else if (check = 1) then
                            (TextIO.output(outfile,"<p>"^ch^nextch);fileconvert(TextIO.inputN(infile,1),hashtags,1,0,inital,inbold,underline,inol,indent))
                        else
                            let
                                val nextch = TextIO.inputN(infile,1)
                            in
                                if (hypcount > 2 andalso nextch = "\n") then
                                    (TextIO.output(outfile,"<hr>");fileconvert(nextch,hashtags,inpara,0,inital,inbold,underline,inol,indent))
                                else if (nextch = "-") then
                                    olchecker(ch,2,shift,wsnum,hypcount+1)
                                else if not (null(inol)) then
                                    if (shift = 0) then
                                        (TextIO.output(outfile,"</p>\n</li>\n<li>\n<p>");
                                        fileconvert(nextch,hashtags,1,0,inital,inbold,underline,inol,indent))
                                    else if (shift = ~1) then
                                        (TextIO.output(outfile,"</p>\n</li>\n<li>\n<p>");
                                        fileconvert(nextch,hashtags,1,0,inital,inbold,underline,tl inol,wsnum))
                                    else
                                        if (ch <> "-") then
                                            (TextIO.output(outfile,"</p>\n<ol>\n<li>\n<p>");
                                            fileconvert(nextch,hashtags,1,0,inital,inbold,underline,1 :: inol,wsnum))
                                        else
                                            (TextIO.output(outfile,"</p>\n<ul>\n<li>\n<p>");fileconvert(nextch,hashtags,1,0,inital,inbold,underline,2 :: inol,wsnum))
                                else
                                    if (ch <> "-") then
                                        (TextIO.output(outfile,"\n<ol>\n<li>\n<p>");fileconvert(nextch,hashtags,1,0,inital,inbold,underline,[1],indent))
                                    else
                                        (TextIO.output(outfile,"\n<ul>\n<li>\n<p>");fileconvert(nextch,hashtags,1,0,inital,inbold,underline,[2],indent))
                            end
                    end
                
                fun ollbcount(lbnum,wsnum,incode) = 
                    let
                        val nextch = TextIO.inputN(infile,1)
                        fun codeblock(ch) =
                            if (ch = "\n") then
                                ollbcount(1,0,1)
                            else if (ch = "<") then
                                (TextIO.output(outfile,"&lt;");codeblock(TextIO.inputN(infile,1)))
                            else if (ch = ">") then
                                (TextIO.output(outfile,"&gt;");codeblock(TextIO.inputN(infile,1)))
                            else
                                (TextIO.output(outfile,ch);codeblock(TextIO.inputN(infile,1)))
                        fun wsindentchecks() = 
                            if ((wsnum-indent) < 0) then
                                if ((Int.fromString(nextch) <> NONE) orelse (nextch = "-")) then
                                    if (hd inol = 1) then
                                        (TextIO.output(outfile,"</p>\n</li>\n</ol>\n");olchecker(nextch,0,~1,wsnum,0))
                                    else
                                        (TextIO.output(outfile,"</p>\n</li>\n</ul>\n");olchecker(nextch,0,~1,wsnum,0))
                                else
                                    if (hd inol = 1) then
                                        (TextIO.output(outfile,"</p>\n</li>\n</ol>\n");fileconvert(nextch,0,0,1,inital,inbold,underline,tl inol,wsnum))    
                                    else
                                        (TextIO.output(outfile,"\n</li>\n</ul>\n");fileconvert(nextch,0,1,0,inital,inbold,underline,tl inol,wsnum))
                            else if ((wsnum-indent) = 0) then
                                if ((Int.fromString(nextch) <> NONE) orelse (nextch = "-")) then
                                    olchecker(nextch,0,0,0,0)
                                else
                                    if (hd inol = 1) then
                                        (TextIO.output(outfile,"</p>\n</li>\n</ol>\n");fileconvert(nextch,0,0,1,inital,inbold,underline,[],indent))
                                    else
                                        (TextIO.output(outfile,"\n</li>\n</ul>\n");fileconvert(nextch,0,1,0,inital,inbold,underline,[],indent))
                            else if ((wsnum-indent) <= 4) then
                                if ((Int.fromString(nextch) <> NONE) orelse (nextch = "-")) then
                                    olchecker(nextch,0,1,wsnum,0)
                                else
                                    (TextIO.output(outfile," ");fileconvert(nextch,0,1,0,inital,inbold,underline,inol,indent))
                            else
                                (TextIO.output(outfile,"</p>\n\n<pre><code>");codeblock(nextch))
                    in
                        if (nextch = "\n") then
                            ollbcount(lbnum+1,0,incode)
                        else if (nextch = " ") then
                            ollbcount(lbnum,wsnum+1,incode)
                        else if (nextch = "\t") then
                            ollbcount(lbnum,wsnum+4,incode)
                        else
                             if (lbnum = 1) then
                                if (incode = 1) then
                                    (TextIO.output(outfile,"\n");codeblock(nextch))
                                else if ((Int.fromString(nextch) <> NONE) orelse (nextch = "-")) then
                                    olchecker(nextch,0,0,0,0)
                                else
                                    (TextIO.output(outfile," ");fileconvert(nextch,0,1,0,inital,inbold,underline,inol,indent))
                            else
                                if (incode = 1) then
                                    ((TextIO.output(outfile,"\n</code></pre>\n<p>"));wsindentchecks())
                                else
                                    wsindentchecks()

                    end

                fun asterchecker(num) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (nextch = "*") then
                            asterchecker(num+1)
                        else
                            if (num = 1 andalso inital = 0) then
                                (TextIO.output(outfile,"<em>");fileconvert(nextch,hashtags,inpara,0,1,inbold,underline,inol,indent))
                            else if (num = 1 andalso inital = 1) then
                                (TextIO.output(outfile,"</em>");fileconvert(nextch,hashtags,inpara,0,0,inbold,underline,inol,indent))
                            else if (num = 2 andalso inbold = 0) then
                                (TextIO.output(outfile,"<strong>");fileconvert(nextch,hashtags,inpara,0,inital,1,underline,inol,indent))
                            else if (num = 2 andalso inbold = 1) then
                                (TextIO.output(outfile,"</strong>");fileconvert(nextch,hashtags,inpara,0,inital,0,underline,inol,indent))
                            else if (num = 3 andalso inbold = 0 andalso inital = 0) then
                                (TextIO.output(outfile,"<strong><em>");fileconvert(nextch,hashtags,inpara,0,1,1,underline,inol,indent)) 
                            else
                                (TextIO.output(outfile,"</em><strong>");fileconvert(nextch,hashtags,inpara,0,0,0,underline,inol,indent))                              

                    end
                
                fun linkconsume(ch,name,link,mode) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (mode = 0) then
                            if (ch = "]" andalso (nextch = "[" orelse nextch = "(")) then
                                linkconsume(TextIO.inputN(infile,1),name,"",1)
                            else if (ch = "]" andalso nextch = " ") then
                                linkconsume("]",name,"",0) 
                            else if (ch = "]") then
                                (TextIO.output(outfile,"["^name^ch);fileconvert(nextch,hashtags,inpara,0,inital,inbold,underline,inol,indent))
                            else
                                linkconsume(nextch,name^ch,"",0)
                        else
                            if (ch = "]" orelse ch = ")") then
                                (TextIO.output(outfile,"<a href=\""^link^"\">"^name^"</a>");fileconvert(nextch,hashtags,inpara,0,inital,inbold,underline,inol,indent))
                            else
                                linkconsume(nextch,name,link^ch,mode)
                    end
                
                fun stronghelper(ch) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (nextch="*") then 
                            (TextIO.inputN(infile,1);ch^"</strong>")
                        else
                            stronghelper(ch^nextch)
                    end

                fun emhelper(ch,str) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (nextch="*") then 
                            (ch^"</em>")
                        else
                            emhelper(ch^str^nextch,"")
                    end

                fun tableeater(ch) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (ch = "*" andalso nextch = "*") then
                            tableeater(stronghelper("<strong>"))
                        else if (ch = "*" andalso nextch <> "*") then
                            tableeater(emhelper("<em>",nextch))
                        else if (ch = ">" andalso nextch = ">") then
                            (TextIO.output(outfile,"</TABLE></CENTER>");fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,underline,inol,indent))
                        else if (ch = "\n") then
                            if (nextch <> ">") then (TextIO.output(outfile,"<TR><TD>");tableeater(nextch))
                            else tableeater(nextch)
                        else if (nextch = "\n") then
                            (TextIO.output(outfile,ch^"</TD></TR>\n");tableeater(nextch))
                        else if (nextch = "|") then
                            (TextIO.output(outfile,ch^"</TD><TD>");tableeater(nextch))
                        else if (ch = "|") then
                            tableeater(nextch)
                        else
                            tableeater(ch^nextch)
                    end
                
                fun splinkconsume(ch) =
                    let
                        val nextch = TextIO.inputN(infile,1)
                    in
                        if (nextch = ">") then
                            (TextIO.output(outfile,"<a href=\""^ch^"\">"^ch^"</a>");fileconvert(TextIO.inputN(infile,1),hashtags,1,0,inital,inbold,underline,inol,indent))
                        else
                            splinkconsume(ch^nextch)
                    end
                
                fun tablinktag(ch) =
                    if (ch = "<") then
                        (TextIO.output(outfile,"<CENTER><TABLE border=\"1\">\n");
                        tableeater(TextIO.inputN(infile,1)))
                    else if (ch = "h") then
                        let
                            val ttp = TextIO.inputN(infile,3)
                        in
                            if (ttp = "ttp") then
                                splinkconsume("http")
                            else
                                (TextIO.output(outfile,"<h"^ttp);fileconvert(TextIO.inputN(infile,1),hashtags,1,0,inital,inbold,underline,inol,indent))
                        end
                    else
                        (TextIO.output(outfile,"<"^ch);
                        fileconvert(TextIO.inputN(infile,1),hashtags,1,0,inital,inbold,underline,inol,indent))
                    
            in
                if ch = "" then
                    (TextIO.closeIn(infile);
                    TextIO.closeOut(outfile))
                else
                    case ch of
                    "\n" => 
                        if (null(inol)) then
                            lbcount(1,0)
                        else
                            ollbcount(1,0,0)
                    | "\\" =>
                        let
                            val nextch = TextIO.inputN(infile,1)
                        in
                            (TextIO.output(outfile,nextch);fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,underline,inol,indent))
                        end
                    | "#" => 
                        (TextIO.output(outfile,"<h");
                        let
                            val newhash = (headingcount(hashtags+1));
                            val newhashstr = Int.toString(#1 newhash)
                        in
                            (TextIO.output(outfile,newhashstr^">");
                            fileconvert(#2 newhash,#1 newhash,0,0,inital,inbold,0,inol,indent))
                        end)
                    | "*" =>
                        if (linestart = 1) then (TextIO.output(outfile,"<p>");asterchecker(1))
                        else asterchecker(1)
                    | "_" =>
                        if (underline = 1) then
                            (TextIO.output(outfile," ");fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,1,inol,indent))
                        else
                            (TextIO.output(outfile," <u>");
                            fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,1,inol,indent))
                    | "[" =>
                        linkconsume(TextIO.inputN(infile,1),"","",0)   
                    | " " =>
                        if (underline = 1) then
                            (TextIO.output(outfile,"</u>&nbsp; ");
                            fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,0,inol,indent))
                        else
                            (TextIO.output(outfile," ");
                            fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,0,inol,indent))
                    | _ =>
                        if (linestart = 1) then
                            if (Int.fromString(ch) <> NONE orelse ch = "-") then
                                olchecker(ch,0,0,0,0)
                            else if (ch = "<") then
                                (TextIO.output(outfile,"<p>");
                                tablinktag(TextIO.inputN(infile,1)))
                            else
                                (TextIO.output(outfile,"<p>"^ch);
                                fileconvert(TextIO.inputN(infile,1),0,1,0,inital,inbold,0,inol,indent))
                        else
                            if (ch = "<") then
                                tablinktag(TextIO.inputN(infile,1))
                            else
                                (TextIO.output(outfile,ch);
                                fileconvert(TextIO.inputN(infile,1),hashtags,inpara,0,inital,inbold,underline,inol,indent))
            end

    in
        fileconvert(TextIO.inputN(infile,1),0,0,1,0,0,0,[],0)
    end