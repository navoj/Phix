--
-- src/rein.ew  -- Source code beautifier / re-indent
-- ===========
--
--DEV: does not handle format directives
--with trace
constant use2 = 1   --DEV delete and rename sometime after March 2017

-- Invoked via Tools/Re-Indent source.
-- Processes the current source, inserting and removing whitespace
--  (Phix/Eu source files only), concentrating mainly on the indent
--  of if, while, and for statements, but also optionally updating
--  whitespace in expressions.
-- Any changes it makes can be undone via Ctrl Z.
--
-- Ideally this program should only be run on programs that compile cleanly.
--  This is not a full-featured compiler, and hence there will always be 
--  some things that it does not cope particularly well with, eg:
--
--  * Externally defined types. This program does not scan included files, or
--     for that matter the file(s) that include it. Hence it must sometimes
--     assume an external type and display a warning. You can specify eg:
--          --#withtype boolean
--     (strictly for the benefit of this & scan) to eliminate the warning.
--
--  * Overloaded types. While "integer integer integer = 1" is technically
--     legal, don't expect this to cope. Likewise procedure p(flag boolean)
--     where boolean is also a type, is quite likely to trip this up. (The
--     latter, being quite legal according to RDS Eu/Phix language rules, 
--     declares a /variable/ named boolean of type flag, and is nothing 
--     whatsoever to do with some previously declared boolean type.)
--
--  * Non-standard ifdefs. This program does not recognise with define, and 
--     most certainly cannot retrieve "-D xxx" from the command_line of the 
--     last time it was run. Use --#withdef to suppress warnings.
--
--  * Partial-hll-construct ifdefs. For example consider:
--
--      ifdef WINDOWS then
--          if <condw> then
--      elsdef
--          if <condx> then
--      end ifdef
--              ...
--          end if
--
--    RDS Eu does not cope at all with such, btw. There are some examples of
--    this in src\indent.exw labelled "ifdef/if nesting". This program tries
--    valiantly to cope, but there will always be limits...
--
-- ifdef indentation
-- =================
--
--  There is a choice between:
--
--          if <condtion> then
--              ...
--      ifdef WINDOWS the
--              msg = "windows"
--      elsedef
--              msg = "other"
--      end ifdef
--              ...
--          end if
--
--  which, fwiw, is my preference since it clearly separates the preprocessing
--  from the actual program logic flow, or alternatively:
--
--          if <condtion> then
--              ...
--              ifdef WINDOWS the
--                  msg = "windows"
--              elsedef
--                  msg = "other"
--              end ifdef
--              ...
--          end if
--      
--  If fact, you get a 3-way choice:
--
--  When processing ifdef,
--      [] Align in column 1 (recommended)
--      [] Align as part of code
--      [] Do nothing
--
--  As always, there is a catch. Consider: [DEV is this fixed?]
--
--      ifdef WINDOWS then
--          return 2
--      elsedef
--          return 0
--      end ifdef
--
--  The problem is that "elsedef" and "end ifdef" are discovered deep inside
--  the Expression() call of DoReturn(), at which point any indent information
--  is all wrong.

--      DEV
--   can manage a 
--    passable job as long as the total number of "if .. then" match "end if", 
--    but in general it will baulk at such things. There is an example in the
--    indent.exw test file, where it does one thing the first time through and
--    needs a second bash to correct itself, which I was going to debug until
--    I realised it would all just be a rather pointless waste of my time.
--
--    Note that if isAlignIfdef=0 then partial hll construct indenting goes
--    (even more) completely to pot. See the tests in src\indent.exw.
--

-- Particular sections of code can be wrapped in the comment-style directives
-- (beginning in column 1), to prevent any reformatting between those points:
--
--#without reformat
--   ...
--#with reformat
--
--  There are a couple of examples of this being used in edix.exw.
--

--
-- There are four main parts to this program:
--
-- Part 1. Tokeniser and error routines.
-- Part 2. The main re-indent routine.
-- Part 3. The (highly simplified) parser.
-- Part 4. The main control routine.
--

--with trace

--DEV/SUG: leave this be (if0/if1, in column 1):
----    if sometest then
----if 0 then -- new code
----        if someothertest then
----            ?1
----        end if
----else -- old code
----        if someothertest then
----            ?2
----        end if
----end if
----    end if

include reinh.e as reinh

--default changed 23/1/17 (it may be that DoReturn was wrong)
--DEV these should be in Edix.cfg:
integer isAlignIfdef        --  0 = ifdef indented as part of the code,
--      isAlignIfdef = -1   --  1 = ifdef indented independently of the code.
        isAlignIfdef = 1    --  1 = ifdef indented independently of the code.
                            -- -1 = ifdef indentation left as-is.
integer isStripSpaces       -- 1 makes eg "a=length ( x [ 3 ] )" => "a = length(x[3])"
        isStripSpaces = 1

--with trace

integer nest    -- main indent control; 0 for top-level, 4 in routines,
                -- +4 for if/for/while. NB tempered by Indent() parameter.
                -- (actually, it is isTabWidth rather than a literal 4)

sequence also   -- secondary indent control. Allows line-up of constants etc.
                -- (Can allow several permitted indent levels simultaneously)

integer reformat    -- controlled via --#with[out] reformat directive (in col 1).
                    -- normal reformatting when this is 1, none when it is 0.

integer lexer_tokno     -- token no in line as lexer(aka tokeniser) sees things
integer parser_tokno    -- token no in line as parser sees things (main)
                        -- (the lexer is usually one step ahead of whatever it
                        --  is we are currently parsing)

integer r_CompleteIndents

integer --newRoutineBlock,
--      undoExtraNest,
        indentandor

--
-- Part 1. Tokeniser and error routines
--

integer textlen
integer CurrLine, col, tokline, tokstart, tokend, toktype
sequence token

sequence withdefs
sequence vartypes

integer stripleading
integer striptrailing
integer insertspaces

integer adjust_count

integer nTabs,
        nSpaces

-- (There is no particular need for these to be kept in step with ptok.e,
--  on which this was based, except that doing so may ease porting of new
--  features.)
constant SYMBOL  = 1,
         DQUOTE  = 2,
         BKTICK  = 3,   -- Back tick (string with no escape characters)
         SQUOTE  = 4,
         ELLIPSE = 5,
         HEXDEC  = 6,
         DIGIT   = 7,
         FLOAT   = 8,
--       USCORE  = 9,
         LETTER  = 10,
--       SLICE   = 11,
--       SUBSCR  = 12,
         ILLEGAL = 99

integer Ch

sequence errtext
sequence xlt    -- scratch var of xl() results

integer nWarns
--DEV these now need thorough checking for line numbers, trailing \n, etc:
procedure Message(sequence msg, sequence params)
sequence oneline
integer k
    if nWarns<10 then
        nWarns += 1
        if length(params) then
            msg = sprintf(msg,params)
        end if
        if tokline and tokline<=textlen then
            oneline = filetext[currfile][tokline]
            while 1 do
                k = find('\t',oneline)
                if not k then exit end if
                oneline[k] = ' '
            end while
            CursorY = tokline-1
            msg = sprintf("line %d:\n%s\n%s^ %s.\n",{tokline,oneline,repeat(' ',tokstart-1),msg})
        else
--          msg = sprintf("%s.\n",{msg})
            msg &= ".\n"
        end if
        errtext &= msg
    elsif nWarns=10 then
        nWarns += 1
--      msg &= xl("further warnings suppressed\n")
        msg &= "further warnings suppressed\n"
    end if
end procedure

procedure Warning(sequence msg, sequence params)
    Message(xl("Warning: ") & xl(msg), params)
end procedure

procedure WarnType()
    Warning("%s assumed to be external type",{token})
end procedure

integer oline, ocol
procedure missing_closing()
    tokline = oline
    tokstart = ocol
    Message(xl("missing closing block comment"),{})
    stripleading = 0
    Ch = -1
end procedure

--DEV:: PARSE ERROR ON LINE NNN; JUMP TO LINE!
procedure Abort(sequence msg)
-- msg must already be xl()'d, if rqd.
    Message(msg,{})
    Ch = -1
    stripleading = 0
    toktype = ILLEGAL
    token = "bogus"
end procedure

procedure SkipBlockComment()
-- Skip nested /* block comments */
-- Note that "*/" inside a string is interpreted as end of comment, 
--  (since it is technically text not code, and for example we must
--   treat '... and " here too */' as end comment), though a "/*" 
--  and "*/" pair (of strings/within a string) behave as expected.
-- The opening /* has already been processed; if we cannot find a 
-- matching */ before EOF then display an error.
integer cp1, ch2
sequence oneline
    ocol = col
    oline = CurrLine
    while 1 do -- until eof or the matching */ is found
        col += 1
        while 1 do -- until eof or unfinished/non-blank line
            oneline = filetext[currfile][CurrLine]
            if col<=length(oneline) then exit end if
            CurrLine += 1
            if CurrLine>length(filetext[currfile]) then
                missing_closing()
                return
            end if
            lexer_tokno = 1
            col = 1
        end while
        Ch = oneline[col]
        if Ch='*'           -- possibly the closing */ we seek, or
        or Ch='/' then      -- possibly a nested /* (dealt with recursively)
            cp1 = col+1
            if cp1<=length(oneline) then
                ch2 = oneline[cp1]
                if Ch='*' then
                    if ch2='/' then
                        col += 2
                        while col>length(oneline) do  --skip completed/blank lines
                            CurrLine += 1
                            if CurrLine>length(filetext[currfile]) then
                                Ch = -1
                                return
                            end if
--added 4/3/2011!
                            oneline = filetext[currfile][CurrLine]
                            lexer_tokno = 1
                            col = 1
                        end while
                        Ch = oneline[col]
                        return
                    end if
                else -- Ch='/'
                    if ch2='*' then
                        col = cp1
                        SkipBlockComment()
                        if CurrLine>length(filetext[currfile]) then
                            missing_closing()
                            return
                        end if
                        col -= 1
                    end if
                end if
            end if
        end if
    end while
    ?9/0 --(there is no exit, only returns in the above loop(s))
end procedure

constant TABWORK = 1

procedure Delete(sequence oneline, integer currline, integer first, integer last, integer ctrl)
-- Delete specified characters from the specified line (common code)
-- Save selection stuff (since it is long-term used by main indent code, eg
--  dragCommentsAlong() extends it and much later CompleteIndents uses it)
integer wasSelON, wasCursorY, wasCursorX, wasSelY, wasSelX
sequence tabwork
--trace(1)
--puts(1,"Delete\n")
--?9/0
    if not reformat then Abort("Delete() called when reformat==0") end if
    wasSelON = selON
    wasCursorY = CursorY
    wasCursorX = CursorX
    wasSelY = selX
    wasSelX = selY
    selON = 1
    CursorY = currline-1
    selY = CursorY
    selX = ExpLength(oneline[1..first-1])
    CursorX = ExpLength(oneline[1..last])
    if deleteSelection() then end if
    if ctrl=TABWORK then
        if nTabs+nSpaces then
            tabwork = {repeat('\t',nTabs)&
                       repeat(' ',nSpaces)}
            addAction(INSERTBLOCK,tabwork)
            InsertBlock(tabwork)
        end if
    end if
    selON = wasSelON
    CursorY = wasCursorY
    CursorX = wasCursorX
    selX = wasSelY
    selY = wasSelX
    adjust_count += 1
end procedure

procedure Insert(sequence oneline, integer k)
integer wasSelON, wasCursorY, wasCursorX, wasSelY, wasSelX
--trace(1)
--?9/0
    if not reformat then Abort("Insert() called when reformat==0") end if
    wasSelON = selON
    wasCursorY = CursorY
    wasCursorX = CursorX
    wasSelY = selX
    wasSelX = selY
    selON = 0
    CursorY = CurrLine-1
    CursorX = ExpLength(oneline[1..k-1])
    addAction(INSERTBLOCK,{" "})
    InsertBlock({" "})
    selON = wasSelON
    CursorY = wasCursorY
    CursorX = wasCursorX
    selX = wasSelY
    selY = wasSelX
    adjust_count += 1
end procedure


procedure SkipSpacesAndComments()
-- Note: To skip "include", we just increment CurrLine and set col to 1.
--       We also set col to 1:
--          * if we fall off the end of a line (see nextCh()), and 
--          * at the very start.
--       Thus if col=1 we *must* set Ch.
sequence oneline, wtoken
integer onelen, k, l
    while 1 do -- until eof or non-blank/comment char found
        while 1 do -- until eof or non-blank line (with Ch set)
            if CurrLine>textlen then
                Ch = -1
                return
            end if
            oneline = filetext[currfile][CurrLine]
            onelen = length(oneline)

            if col!=1 then exit end if

            if onelen>=1 then -- (equivalent to col<=onelen)
                Ch = oneline[1] -- (equivalent to Ch=oneline[col])
                exit
            end if
            CurrLine += 1
            lexer_tokno = 1
            col = 1
        end while
        if Ch=' ' or Ch='\t' then
            k = col
            while 1 do -- until Ch!=' '|'\t'
                col += 1
                if col>onelen then
                    -- delete k..col-1 (tabs/spaces at end of line)
                    Delete(oneline,CurrLine,k,onelen,0)
                    CurrLine += 1
                    lexer_tokno = 1
                    col = 1
                    exit
                end if
                Ch = oneline[col]
                if Ch!=' ' and Ch!='\t' then
                    exit
                end if
            end while
        else
            if Ch='-' then
                if col<onelen
                and oneline[col+1]='-' then
                    -- This line is a comment (-- just found), check for
                    -- --/* (block comment) and --# directives
                    if col+2<onelen
                    and oneline[col+2]='/'
                    and oneline[col+3]='*' then
--                      col += 3
                        col += 4
                        SkipBlockComment()
                    else
                        if col+6<=onelen
                        and equal(oneline[col+2..col+6],"#with") then
                            oneline = oneline[col..length(oneline)]
                            if match("--#without reformat",oneline)=1 then
                                reformat = 0
                            elsif match("--#with reformat",oneline)=1 then
                                reformat = 1
                            else
                                if match("--#withdef",oneline)=1 then
                                    k = 11
                                elsif match("--#withtype",oneline)=1 then
                                    k = 12
                                else
                                    k = 0
                                end if
                                wtoken = ""
                                if k and find(oneline[k]," \t") then
                                    while k<length(oneline) do
                                        k += 1
                                        if not find(oneline[k]," \t") then
                                            l = k+1
                                            while l<=length(oneline) do
                                                if find(oneline[l]," \t") then exit end if
                                                l += 1
                                            end while
                                            wtoken = oneline[k..l-1]
                                            exit
                                        end if
                                    end while
                                end if
                                if length(wtoken) then
                                    if oneline[8]='d' then -- withdef
                                        if not find(wtoken,withdefs) then
                                            withdefs = append(withdefs,wtoken)
                                        end if
                                    elsif oneline[8]='t' then -- withtype
                                        if not find(wtoken,vartypes) then
                                            vartypes = append(vartypes,wtoken)
                                        end if
                                    else
                                        ?9/0
                                    end if
                                elsif k then
                                    Warning("a name is expected here",{})
                                else
                                    Warning("unrecognised --#with directive",{})
                                end if
                            end if
                        end if
                        CurrLine += 1
                        lexer_tokno = 1
                        col = 1
                    end if
                else
                    exit
                end if
            elsif Ch='/' then
                if col<onelen
                and oneline[col+1]='*' then
--                  col += 1
                    col += 2
                    SkipBlockComment()
                else -- (we just found a divide symbol then)
                    exit
                end if
            else
                exit
            end if
        end if
    end while
end procedure

sequence charClass

charClass = repeat(ILLEGAL,255)
charClass['!'] = SYMBOL
charClass['\"'] = DQUOTE
charClass['`'] = BKTICK
charClass['\''] = SQUOTE
charClass['#'] = HEXDEC
charClass['&'] = SYMBOL
charClass['('..'/'] = SYMBOL    -- ()*+,-./
charClass['.'] = ELLIPSE
charClass['0'..'9'] = DIGIT
charClass[':'..'?'] = SYMBOL    -- :;<=>?
--charClass['_'] = USCORE
charClass['_'] = LETTER
--charClass['@'] = SYMBOL
charClass['A'..'Z'] = LETTER
charClass['a'..'z'] = LETTER
charClass['['] = SYMBOL
charClass[']'] = SYMBOL
charClass['{'] = SYMBOL
charClass['}'] = SYMBOL
charClass['~'] = SYMBOL


procedure nextCh()
sequence oneline
    tokend = col-1
    if CurrLine>textlen then
        Ch = -1
        return
    end if
    oneline = filetext[currfile][CurrLine]
    if col<=length(oneline) then
        Ch = oneline[col]
    else
        CurrLine += 1
        lexer_tokno = 1
        col = 1
    end if
    SkipSpacesAndComments()
end procedure

procedure getFloat()
-- finish off a float. The first few DIGITS (if any) have been processed;
-- continue from '.' or 'eE' (we have no use for the value)
integer lenTC
sequence oneline
    oneline = filetext[currfile][CurrLine]
    lenTC = length(oneline)
    if Ch='.' then
        -- make sure it is not an ellipse
        if col<lenTC
        and oneline[col+1]='.' then
--          col -= 1
        else
            toktype = FLOAT
            while 1 do
                col += 1
                if col>lenTC then exit end if
                Ch = oneline[col]
                if charClass[Ch]!=DIGIT then
                    if Ch!='_' then exit end if
                end if
            end while
        end if
    end if
    if find(Ch,"eE")
    and col<lenTC then
        toktype = FLOAT
        Ch = oneline[col+1]
        if Ch='-' or Ch='+' then
            col += 1
            Ch = oneline[col+1]
        end if
        while 1 do
            col += 1
            if col>lenTC then exit end if
            Ch = oneline[col]
            if charClass[Ch]!=DIGIT then
                if Ch!='_' then exit end if
            end if
        end while
    end if
    nextCh()
end procedure

integer mapEndToMinusOne
        mapEndToMinusOne = 0

constant bases = {8,16,2,10}    -- NB: oxbd order
integer base
        base = -1

procedure loadBase()
integer lenTC, chidx
sequence oneline
    oneline = filetext[currfile][CurrLine]
    lenTC = length(oneline)
    if base=5 then      -- 0(nn) case
        base = 0
        while 1 do
            col += 1
            if col>lenTC then exit end if
            Ch = oneline[col]
            if Ch<'0' or Ch>'9' then
                if Ch!='_' then exit end if
            else
                base = (base*10)+(Ch-'0')
            end if
        end while
--      if base<2 or base>16 then
        if base<2 or base>36 then
            Abort(xl("unsupported number base"))
        end if
        if Ch!=')' then
--          tokcol = col
            Abort("')'"&xl(" expected"))
        end if
    else
        base = bases[base]
    end if
    col += 1
    if col>lenTC then Abort(xl("number expected")) end if
    Ch = oneline[col]
    while 1 do
        chidx = find(Ch,"_0123456789ABCDEFabcdef")
        --                          ^12   ^18
        if chidx>=18 then chidx -= 6 end if
        if chidx=0 or chidx>base+1 then exit end if
        col += 1
        if col>lenTC then exit end if
        Ch = oneline[col]
    end while
    nextCh()
end procedure

procedure pp_replace(sequence s)
-- note this does not enter into main Edix undo/redo handling, 
--  but will(/should!) all get done/undone manually here
sequence prev
    prev = filetext[currfile][tokline][tokstart..tokend]
    if not equal(reverse(prev),s) then
        -- note that if you get say "pp_replace(dne):dne",
        --      then it means we expected "end" not "dne".
        Abort("pp_replace("&s&"):"&prev) -- (bug in this program)
    else
        filetext[currfile][tokline][tokstart..tokend] = s
    end if
end procedure

integer ifdefstackidx
sequence ifdefnests,    -- nest at ifdef
         ifdefnestz,    -- nest at elsifdef/elsedef
         ifdefstate,    -- 0=ifdef, 1=elsifdef, 2=elsedef
         ifdefflags     -- the xxx of "if [not] xxx then" 

integer pptokline, pptokstart, pptokend,    -- for recovery at end
        pplastline, pplaststart

integer r_preprocess
integer nPreprocess
        nPreprocess = 0

integer nestAtStatementStart
        nestAtStatementStart = 0

procedure IndentIfdef(integer line)
-- ensure line begins with correct whitespace
sequence oneline
integer k, w, ch
integer indentRqd
    if not reformat then return end if --#without reformat in force.
    if isAlignIfdef=-1 then return end if
    oneline = filetext[currfile][line]
    if isAlignIfdef then
        indentRqd = ifdefstackidx-1
    else
--      indentRqd = nest
        indentRqd = nestAtStatementStart
    end if
    k = 0
    w = 0
    for j=1 to length(oneline) do
        ch = oneline[j]
        if ch='\t' then
            k += isTabWidth - remainder(k, isTabWidth)
        elsif ch=' ' then
            k += 1
        else
            w = j-1
            exit
        end if
    end for
    -- Permit expressions such as:
    --      elsifdef xxx
    --            or xxx
    --           and xxx
    if  oneline[w+1]='o'
    and oneline[w+2]='r'
    and find(oneline[w+3]," \t") then
        k -= 5
    elsif oneline[w+1]='a'
      and oneline[w+2]='n'
      and oneline[w+3]='d'
      and find(oneline[w+4]," \t") then
        k -= 6
    end if

    if k!=indentRqd then
        nTabs = floor(indentRqd/isTabWidth)
        nSpaces = indentRqd-nTabs*isTabWidth
        while w and nSpaces and oneline[w]=' ' do
            nSpaces -= 1
            w -= 1
        end while
        if nSpaces=0 then
            while w and nTabs and oneline[w]='\t' do
                nTabs -= 1
                w -= 1
            end while
        end if

        Delete(oneline,line,1,w,TABWORK)
        w -= nTabs+nSpaces  -- (w:=+/-delta)
        if line=CurrLine then
            col -= w
        end if
        if line=pptokline then
            pptokstart -= w
            pptokend -= w
        end if

    end if
end procedure

sequence ifdefcheck -- see process_one_ifdef
constant Dif=2,  Delsif=3,  Delse=4,  Dend=5, Dinc=6, Dthen=7

--#without reformat
constant PPGETTOKENSET = {"ifdef","fedfi","elsfedfi","fedesle","dne"}
constant PPRECOVERSET  = {      0,"fedfi","elsfedfi","fedesle","dne","include","neht"}
constant PPRECOVERWITH = {      0,"ifdef","elsifdef","elsedef","end",        0,"then"}
constant PPONEIFDEFSET = {      0,"ifdef","elsifdef","elsedef"}
constant PPTWOIFDEFSET = {      0,"ifdef","elsifdef","elsedef","end","include",
                                  "fedfi","elsfedfi","fedesle","dne"} -- oops
--#with reformat

integer inexpression
        inexpression = 0

integer r_completeifdef

bool fromsubss = false

procedure getToken(bool float_valid=false)
integer nxtCh, lenTC, k, tmpch
sequence oneline
--if col>1 and equal(token,"try") then trace(1) end if
--trace(1)
    if stripleading then
        if isStripSpaces and reformat and parser_tokno!=1 then
--trace(1)
            oneline = filetext[currfile][tokline]
            --  oneline:        .....token...Ch(@[col])
            --                      ^stripleading operates here
--          stripspaces(tokstart)
            k = tokstart
            while k>1 and find(oneline[k-1]," \t") do
                k -= 1
            end while
            if k<tokstart and k>1 then
                -- kill oneline[k..tokstart-1]
                Delete(oneline,tokline,k,tokstart-1,0)
                k = (tokstart-k)
                tokstart -= k
                tokend -= k
                if CurrLine=tokline then
                    oneline = filetext[currfile][CurrLine]
                    col -= k
--                  if Ch!=oneline[col] then ?9/0 end if
                    if Ch!=-1 and Ch!=oneline[col] then ?9/0 end if
                end if
            end if
        end if
        stripleading = 0
    end if
    if Ch=-1 then return end if
    if striptrailing then
--DEV maybe this should still be (and still run back from col):
--      if CurrLine=tokline and isStripSpaces and reformat then
        if isStripSpaces and reformat then
            oneline = filetext[currfile][tokline]
            --  oneline:        .....token...Ch(@[col])
            --                           ^striptrailing operates at tokend
            --          stripspaces(col)
            if 1 then
                k = tokend
                while k<length(oneline) do
                    tmpch = oneline[k+1]
                    if not find(tmpch," \t") then
                        -- added 29/05/2011, to stop " a - -- comment\n b" 
                        -- getting wrongly mangled to " a--- comment\n b":
                        if find(tmpch,"-") then
                            k -= 1
                        end if
                        exit
                    end if
                    k += 1
                end while
                if k>tokend then
                    -- kill oneline[tokend+1..k]
                    Delete(oneline,tokline,tokend+1,k,0)
                    k = (k-tokend)
                    tokstart -= k
                    tokend -= k
                    if CurrLine=tokline then
                        oneline = filetext[currfile][CurrLine]
                        col -= k
                        if Ch!=oneline[col] then ?9/0 end if
                    end if
                end if
            else -- old code
                k = col
                while k>1 and find(oneline[k-1]," \t") do
                    k -= 1
                end while
                if k<col and k>1 then
                    -- kill oneline[k..col-1]
                    Delete(oneline,tokline,k,col-1,0)
                    oneline = filetext[currfile][CurrLine]
--              col = k
                    col -= (col-k)
                    if Ch!=oneline[col] then ?9/0 end if
                end if
            end if
        end if
        striptrailing = 0
    end if
    oneline = filetext[currfile][CurrLine]
    if insertspaces then
        if tokline=CurrLine and isStripSpaces and reformat then
            if tokstart>1 and not find(oneline[tokstart-1]," \t") then
                Insert(oneline,tokstart)
                oneline = filetext[currfile][CurrLine]
                col += 1
                if Ch!=oneline[col] then ?9/0 end if
            end if
            if col>1 and not find(oneline[col-1]," \t") then
                Insert(oneline,col)
                oneline = filetext[currfile][CurrLine]
                col += 1
                if Ch!=oneline[col] then ?9/0 end if
            end if
        end if
        insertspaces = 0
    end if
    tokline = CurrLine
    tokstart = col
    parser_tokno = lexer_tokno
    lexer_tokno += 1
    token = ""
    toktype = charClass[Ch]
    lenTC = length(oneline)
    if col<lenTC then
        nxtCh = oneline[col+1]
    else
        nxtCh = -1
    end if
    if toktype=LETTER then
        while 1 do
            col += 1
            if col>lenTC then exit end if
            nxtCh = charClass[oneline[col]]
            if nxtCh<DIGIT then exit end if
            if nxtCh>LETTER then exit end if
        end while
        tokend = col-1
        token = oneline[tokstart..tokend]
        if mapEndToMinusOne=1 and equal(token,"end") and not fromsubss then 
            token = "-1"
            toktype = DIGIT
        elsif not nPreprocess then
-- constant PPGETTOKENSET = {"ifdef","fedfi","elsfedfi","fedesle","dne"}
            nPreprocess = find(token,PPGETTOKENSET)
            if nPreprocess then
                nextCh()
                if nPreprocess=1 then           -- ifdef
                    nPreprocess = call_func(r_preprocess,{}) -- (nPreprocess:=0|2) 
                    -- should leave CurrLine/col/Ch on the (mangled) "ifdef"
                end if
                if nPreprocess then-- (if r_preprocess met no error)
--DEV:
                    if inexpression then
                        token = "end"
                        return
                    end if
--                  completeifdef()
                    call_proc(r_completeifdef,{})
                    return
                end if
                getToken(float_valid)
            end if
        end if
        nextCh()
    elsif toktype=SYMBOL then
        if find({Ch,nxtCh},{"<=",">=","==",":=","!=","+=","-=","*=","/=","&=","@="}) then
            col += 1
        elsif Ch='?' then
            toktype = LETTER
        end if
        col += 1
        token = oneline[tokstart..col-1]
        nextCh()
    elsif toktype=DQUOTE then
        if col+2<=lenTC
        and oneline[col+1]='\"'
        and oneline[col+2]='\"' then
            -- multiline (""") string. (no point doing trim handling)
--DEV any point in setting token??
            token = ""
            col += 3
            while 1 do -- until closing """ found
                while col>lenTC do
                    CurrLine += 1
                    if CurrLine>length(filetext[currfile]) then
                        Abort(xl("missing ")&"\"\"\"")
                        exit
                    end if
                    token &= '\n'
                    oneline = filetext[currfile][CurrLine]
                    lenTC = length(oneline)
--                  lexer_tokno = 1
                    col = 1
                end while
                token &= nxtCh
                nxtCh = oneline[col]
                if nxtCh='\"'
                and col+2<=lenTC
                and oneline[col+1]='\"'
                and oneline[col+2]='\"' then
                    col += 3
                    exit    -- (closing """ found)
                end if
                col += 1
            end while
--29/9/16:
            nextCh()
            return
        end if
        while 1 do
            col += 1
            if col>lenTC then
                Abort(xl("missing ")&"\"")
                exit
            end if
            nxtCh = oneline[col]
            if nxtCh='\\' then
                col += 1
            elsif nxtCh='\"' then
                col += 1
                exit
            end if
        end while
        token = oneline[tokstart..col-1]
        nextCh()
    elsif toktype=BKTICK then
        token = ""
        while 1 do
            col += 1
            while col>lenTC do  -- convert any blank lines to \n
                CurrLine += 1
                if CurrLine>length(filetext[currfile]) then
                    Abort(xl("missing ")&"`")
                    exit
                end if
                token &= '\n'
                oneline = filetext[currfile][CurrLine]
                lenTC = length(oneline)
                col = 1
            end while
            -- otherwise add chars unaltered, to closing tick
            nxtCh = oneline[col]
            if nxtCh='`' then
                col += 1
                exit        -- (closing ` found)
            end if
            token &= nxtCh
        end while
        toktype = DQUOTE
        nextCh()
    elsif toktype=SQUOTE then
        if nxtCh='\\' then
            col += 4
        else
            col += 3
        end if
        nextCh()
    elsif toktype=HEXDEC then
--DEV #456ilasm?
--      if Ch='i' then
        if col<lenTC and nxtCh='i' then
            col += 1
            nextCh()
            -- #ilasm{inline assembly},
            -- #istype{var_id,var_type}, or
            -- #isinit{var_id,0/1}.
--          toktype = ILASM
            toktype = LETTER
            token = "#i"
            return
        end if
        while 1 do
            col += 1
            if col>lenTC then exit end if
            nxtCh = oneline[col]
            if nxtCh<'0' then exit end if
            if nxtCh>'9' then
                if nxtCh<'A' then exit end if
                if nxtCh>'F' and nxtCh!='_' then
                    if nxtCh<'a' then exit end if
                    if nxtCh>'f' then exit end if
                end if
            end if
        end while
        nextCh()
    elsif toktype=ELLIPSE then
        if nxtCh='.' then
            col += 2
            nextCh()
        elsif not float_valid then
            toktype = SYMBOL
            token = "."
            col += 1
            nextCh()
        else
            getFloat()
        end if
    elsif toktype=DIGIT then
        if Ch='0' then  -- check for 0x/o/b/d formats
            if col<lenTC then
                Ch = oneline[col+1]
                base = find(Ch,"toxbd(")
                if base then
                    if base>1 then base -= 1 end if
                    col += 1
                    loadBase()
                    return
                end if
            end if
        end if
        while 1 do
            col += 1
            if col>lenTC then exit end if
            Ch = oneline[col]
            if charClass[Ch]!=DIGIT then
                if Ch!='_' then exit end if
            end if
        end while
--      if find(Ch,".eE") then
        if find(Ch,"eE")
        or (Ch='.' and float_valid) then
            getFloat()
        else
            nextCh()
        end if
    elsif toktype=ILLEGAL and mapEndToMinusOne then
        if Ch='$' then
            token = "-1"
            toktype = DIGIT
            col += 1
            nextCh()
            return
        elsif Ch='%'
          and mapEndToMinusOne=-3 then  -- from preprocess()
            -- (preprocess() scans the whole file, which may contain #ilasm
            --  constructs. Allows preprocess() to quietly skip eg %isVar)
            token = "-1"
            toktype = DIGIT
            col += 1
            nextCh()
            return
        else
            Abort(xl("unrecognised"))
        end if
    else
        Abort(xl("unrecognised"))
    end if
end procedure

procedure completeifdef()
integer wastokline, state, mywasAlignIfdef, k
--integer wastokstart
object onetoken
sequence flags

    ifdefcheck[nPreprocess] = ifdefcheck[nPreprocess]-1 -- (avoid that Eu 2.4 bug)
    if nPreprocess=2 then           -- ifdef/fedfi
        ifdefstackidx += 1
        if length(ifdefnests)<ifdefstackidx then
            ifdefnests &= nest
            ifdefnestz &= nest
            ifdefstate &= 0
            ifdefflags = append(ifdefflags,{})
        else
            ifdefnests[ifdefstackidx] = nest
            ifdefnestz[ifdefstackidx] = nest
            ifdefstate[ifdefstackidx] = 0
            ifdefflags[ifdefstackidx] = {}
        end if
    end if
--DEV create a "stack" of ifdefs and ensure indent levels etc are changed by the same
--      amount for each branch... and that be 0 for no elsifdef, though we might want
--      to keep quiet for (WIN32|WINDOWS)/(LINUX|UNIX), WIN32_GUI/WIN32_CONSOLE, and
--      any (simple) xxx/not xxx.
-- 1) identify "nest" vars, save/check at start/end of each block (DONE: just "nest")
-- 2) flag for first ifdef at this level/multiple elsifdef found (ifdefstate)
-- 3) let's test for elsedef followed by elsifdef why not.
-- 3) save of flags tested (iff 2))
--
-- If no change to nest, then it is OK
-- Otherwise, the change to nest must be the same on each branch, AND:
--      if there is an elsedef, then it is OK
--      otherwise the last must be a "seen" var, ie
--              ifdef xxx then -- (not necessarily first)
--              <any other elsifdefs you like>
--              elsifdef not xxx then
--              end ifdef
--      gets treated as an elsedef, since it effectively is.
--      We won't actually bother to ensure one has "not"|!,
--      and as soon as any and|or|parenthesis get involved,
--      pretty much cease all efforts to avoid the warning.
    wastokline = tokline
    state = ifdefstate[ifdefstackidx]
    if nPreprocess<=Delsif then         -- ifdef/fedfi/elsfedfi (ifdef/elsifdef)
--                      wastokstart = tokstart
        tokstart = tokend-4
        pp_replace("ifdef")
        getToken()
        if toktype='!'
        or (toktype=LETTER and equal(token,"not")) then
            getToken()
        end if
        if find(token,{"WIN32","LINUX","UNIX"}) then
            token = "WINDOWS"
        elsif equal(token,"WIN32_CONSOLE") then
            token = "WIN32_GUI"
        end if
        onetoken = token
        getToken()
        if state=0 then
            state = 1
        elsif and_bits(state,3)=1 then
            ifdefnestz[ifdefstackidx] = nest
            state += 1
        else
            if ifdefnestz[ifdefstackidx]!=nest
            and not and_bits(state,#80) then
-- 13/3/11. Removed because of the "return 0 end ifdef" issue.
--                              Warning("imbalanced nesting",{})
                state = or_bits(state,#80)
            end if
        end if
        if isAlignIfdef=1 then
            nest = ifdefnests[ifdefstackidx]
        end if
        if equal(token,"neht") then
            -- good, this is a simple case
            state = or_bits(state,#08)
        else
            while Ch!=-1 do
                getToken()
                if equal(token,"neht") then exit end if
            end while
            onetoken = 0
            state = and_bits(state,#F7)
        end if
        ifdefflags[ifdefstackidx] = append(ifdefflags[ifdefstackidx],onetoken)
        ifdefstate[ifdefstackidx] = state
        ifdefcheck[Dthen] = ifdefcheck[Dthen]-1 -- (avoid that Eu 2.4 bug)
        pp_replace("then")
    else
        if nPreprocess=Delse then       -- fedesle  (elsedef)
            pp_replace("elsedef")
            ifdefstate[ifdefstackidx] = or_bits(state,#10)
            if isAlignIfdef=1 then
                nest = ifdefnests[ifdefstackidx]
            end if
        elsif nPreprocess=Dend then     -- dne
--trace(1)
            pp_replace("end")
            getToken()
            pp_replace("ifdef")
            mywasAlignIfdef = isAlignIfdef -- (save before getToken/SkipSpacesAndComments)
--                          if ifdefnestz[ifdefstackidx]!=ifdefnests[ifdefstackidx]
            if isAlignIfdef=1
            and not and_bits(state,#90) -- no elsedef/already warned
--                          and nest!=ifdefnestz[ifdefstackidx] then
-- 03/04/2011:
--                          and nest-undoExtraNest!=ifdefnestz[ifdefstackidx] then
--          and nest-undoExtraNest!=ifdefnestz[ifdefstackidx]
            and nest!=ifdefnestz[ifdefstackidx]
--          and nestAtStatementStart-undoExtraNest!=ifdefnestz[ifdefstackidx] then
            and nestAtStatementStart!=ifdefnestz[ifdefstackidx] then
                flags = ifdefflags[ifdefstackidx]
--                              k = length(flags)
--if k=1 then trace(1) end if
--DEV DEV DEV!!
--                              if k>1 then
--                              if isAlignIfdef=1 then
                k = length(flags)

                onetoken = flags[k]
                if atom(onetoken) or find(onetoken,flags)=k then
--trace(1)
-- 21/8/11. Removed because of the "return 0 end ifdef" issue.
--                                      Warning("imbalanced nesting",{})
                end if
--                              end if
            end if
--?{CurrLine,nest,ifdefnests,ifdefnestz}
--trace(1)
--                          nest = ifdefnestz[ifdefstackidx]
        else
            ?9/0
        end if
    end if
    if nPreprocess=2 then       -- ifdef/fedfi
        if isAlignIfdef=0 then
            nest += isTabWidth
        end if
    end if
    for line=wastokline to tokline do
        IndentIfdef(line)
    end for
--                  if nPreprocess=2 then       -- ifdef/fedfi
--                      if isAlignIfdef=0 then
--                          nest += isTabWidth
--                      end if
--                  els
    if nPreprocess=Dend then -- dne fedfi
        ifdefstackidx -= 1
--                      if isAlignIfdef=0 then
        if mywasAlignIfdef=0 then
            if nest then
                nest -= isTabWidth
            else
                Warning("makes nest<0",{})
            end if
        end if
    end if
    nPreprocess = 0
    getToken()
    nextCh()
end procedure
r_completeifdef = routine_id("completeifdef")

procedure Check(sequence text)
    if not equal(token,text) then
        Abort(text&xl(" expected"))
    end if
end procedure

procedure Match(sequence text, bool float_valid=false)
    if not equal(token,text) then
        Abort(text&xl(" expected"))
    end if
    getToken(float_valid)
end procedure

procedure recover(integer ifdefline, integer ifdefstart)
-- Some error occurred in process_one_ifdef. Repair any and all ifdef->fedfi etc
integer wastokline, wastokstart, k
sequence oneline
    wastokline = CurrLine
    wastokstart = tokstart
    CurrLine = ifdefline
    col = ifdefstart
    oneline = filetext[currfile][CurrLine]
    Ch = oneline[col]
    nPreprocess = -1    -- prevent any further preprocessing in getToken()
    getToken()
    while Ch!=-1
      and (tokline<wastokline or (tokline=wastokline and tokstart<wastokstart)) do
-- constant PPRECOVERSET  = {       0,"fedfi","elsfedfi","fedesle","dne","include","neht"}
        k = find(token,PPRECOVERSET)

        if k then
            if k=Dinc then -- include
                -- (avoid any '\' illegal char errors)
                CurrLine += 1
                lexer_tokno = 1
                col = 1
                SkipSpacesAndComments()
            else
                ifdefcheck[k] = ifdefcheck[k]-1 -- (avoid that Eu 2.4 bug)
                pp_replace(PPRECOVERWITH[k])
                if k=Dend then
                    -- treat "dne fedfi" as one "end ifdef" segment.
                    getToken()
                    pp_replace("ifdef")
                end if
            end if
        end if
        getToken()
    end while
    nPreprocess = 0
end procedure

function process_one_ifdef()
--
-- returns 2 on success, 0 on failure
--
integer andor, k
integer wastokline, wastokstart, wastokend

    andor = 0
    while Ch>0 do
--constant PPONEIFDEFSET = {0,"ifdef","elsifdef","elsedef"}
        k = find(token,PPONEIFDEFSET)

        if k=Dif or k=Delsif then -- ifdef/elsifdef
            ifdefcheck[k] = ifdefcheck[k]+1 -- (avoid that Eu 2.4 bug)
            -- fedfi/elsfedfi
            tokstart = tokend-4
            pp_replace("fedfi")
            getToken()
            while Ch>0 do   -- process any and/or in the tests
--DEV 6/3/11 (spotted in passing)
                if toktype='!'
--              or (toktype=LETTER and equal(token,"end")) then
                or (toktype=LETTER and equal(token,"not")) then
                    getToken()
                end if
                if toktype!=LETTER then
                    Abort(xl("a name is expected here"))
                    return 0
                end if
                if not find(token,{"WIN32","WINDOWS","LINUX","FREEBSD",
                                   "SUNOS","OPENBSD","OSX","UNIX",
                                   "WIN32_GUI","WIN32_CONSOLE",
                                   "SAFE","DATA_EXECUTE","UCSTYPE_DEBUG",
                                   "CRASH","EU4_1"}) then
                    if not find(token,withdefs) then
--                      errtext &= "idfef "&token&xl(" unrecognised, and no \"--#withdef\" for it")
--trace(1)
                        Warning("unknown/--#withdef missing",{})
                    end if
                end if
                getToken()
                if toktype!=LETTER then exit end if
                if andor=0 then -- neither encountered yet
                    andor = find(token,{"and","or"})
                    if andor=0 then exit end if
                else
                    if andor=1 then -- we've had an and already
                        if not equal(token,"and") then exit end if
                    elsif andor=2 then  -- we've had an or already
                        if not equal(token,"or") then exit end if
                    else
                        ?9/0 --DEV
                    end if
                end if
                getToken() -- discard the and/or
            end while
            if not equal(token,"then") then
                Abort("\"then\""&xl(" expected"))
                return 0
            end if
            pp_replace("neht")
            ifdefcheck[Dthen] = ifdefcheck[Dthen]+1 -- (avoid that Eu 2.4 bug)
        elsif k=Delse then -- elsedef
            pp_replace("fedesle")
            ifdefcheck[Delse] = ifdefcheck[Delse]+1 -- (avoid that Eu 2.4 bug)
        else
            Abort("ifdef/elsifdef/elsedef"&xl(" expected"))
            return 0
        end if
        getToken()
        -- process one branch
        while Ch>0 do
            if toktype=LETTER then
-- constant PPTWOIFDEFSET = {0,"ifdef","elsifdef","elsedef","end","include",
--                             "fedfi","elsfedfi","fedesle","dne"} -- oops

                k = find(token,PPTWOIFDEFSET)

                if k then
                    if k=Dif then       -- ifdef
                        if not process_one_ifdef() then
                            return 0
                        end if
                    elsif k=Delsif      -- elsifdef
                       or k=Delse then  -- elsedef
                        exit -- (process remaining branches)
                    elsif k=Dend then   -- end
                        wastokline = tokline
                        wastokstart = tokstart
                        wastokend = tokend
                        getToken()
                        if equal(token,"ifdef") then
                            ifdefcheck[Dend] = ifdefcheck[Dend]+1 -- (avoid that Eu 2.4 bug)
                            filetext[currfile][wastokline][wastokstart..wastokend] = "dne"
                            pp_replace("fedfi")
                            return Dif      -- ALL DONE! (Dif==2)
                        end if
                    elsif k=Dinc then  -- include
                        -- (avoid any '\' illegal char errors)
                        CurrLine += 1
                        lexer_tokno = 1
                        col = 1
                        SkipSpacesAndComments()
--                      getToken()
                    else            -- oops!
                        Abort("oops!?")
                        return 0
                    end if
                end if
            end if
            getToken()
        end while
    end while
    Abort("\"end ifdef\""&xl(" expected"))
    return 0
end function

function preprocess()
--
-- Called for each top-level ifdef.
--          
-- Converts eg "ifdef WIN32 then s = "W32" elsifdef UNIX then s = "LNX" end ifdef"
--          to "fedfi WIN32 neht s = "W32" elsfedfi UNIX neht s = "LNX" dne fedfi"
--
--  (note it is elsfedfi for elsifdef instead of fedfisle to simplfy the undo)
--
-- These tokens are replaced as soon as they are found, by getToken(), and then
--  skipped so that the main program does not even see them.
-- Of course if we run into any fedfi/elsfedfi/fedesle/dne/neht then we have to
--  undo any we did manage to replace and report the error. (There is now a final
--  double-check/recover() call on ifdefcheck at the end of ReIndent().)
--
-- WHY? Because otherwise in eg:
--
--      while 1 do
--  ifdef WIN32 then
--          msg = "windows"
--  elsedef
--          msg = "linux"
--  end ifdef
--          puts(1,msg)
--      end while
--
--  the "end ifdef" prematurely terminates the while-block. Additionally, we want 
--  to process the "end" of "end ifdef" in getToken(), but the "end" of "end while"
--  in DoWhile(). You could probably achieve the same effect by looking ahead for
--  the next token (and putting it back), but this seems easier, given that we
--  want to process/indent these language constructs independently.
--
--  I also want something that will cope (however ill-advised) with eg:
--
--      ifdef WIN32 then
--          if c_func(xGetWindow,{}) then
--      elsedef LINUX then
--          if int80(getwin) then  -- or maybe while here!
--          end if
--      end ifdef
--      ifdef WIN32 then
--          end if
--      end ifdef
--
--  and/or
--
--      ifdef WIN32 then
--          if c_func(xGetWindow,{}) then
--      elsedef
--          if int80(getwin) then
--      end idfef
--              ...
--          end if
--
integer wasline, wascol, wasCh
integer wasreformat
integer wasmapEndToMinusOne, res

--trace(1)
    wasline = CurrLine
    wascol = col
    wasCh = Ch
    wasreformat = reformat
    pptokline = tokline
    pptokstart = tokstart
if wasCh!=-1 then
    if filetext[currfile][wasline][wascol]!=wasCh then ?9/0 end if  -- sanity check
end if
    pptokend = tokend
    wasmapEndToMinusOne = mapEndToMinusOne
    mapEndToMinusOne = -3   -- treat '$' and '%' as normal symbols
                            -- (since this must skip/ignore eg x[$], #ilasm{...%isVar} etc.)
    res = process_one_ifdef()   -- (res:=0|2)

    pplastline = CurrLine   -- \ if ReIndent() spots error,
    pplaststart = tokstart  -- / recover() up to this point

    if res=0 then
        recover(pptokline, pptokstart)
    end if

    mapEndToMinusOne = wasmapEndToMinusOne
    -- leave CurrLine/col/Ch/etc on the (mangled) "ifdef"
    CurrLine = wasline
    col = wascol
    Ch = wasCh
    reformat = wasreformat
    tokline = pptokline
    tokstart = pptokstart
    tokend = pptokend
    return res
end function
r_preprocess = routine_id("preprocess")



--
-- Part 2. The main re-indent routines
--

integer lastIline, lastIaction, lastIcount, lastRexpr
constant indent = 1,
         un_indent = -1

integer returnexpr          --   1 for function/type "return <expr>",
        returnexpr = -1     --   0 for procedure "return",
                            --  -1 for toplevel statements (return is illegal)

procedure CompleteIndents()
    if lastIline!=-1 then
        if reformat then    -- (check #without reformat not in force)
--trace(1)
            CursorY = lastIline-1
            selX = 0
            CursorX = 0
            for i=1 to lastIcount do
                if lastIaction=indent then
--                  addAction(INDENT,indentWith(VK_TAB))
                    addAction(INDENT,indentWith(K_TAB))
                else
--                  addAction(UNINDENT,unindent(VK_TAB))
                    addAction(UNINDENT,unindent(K_TAB))
                end if
            end for
--?9/0
            adjust_count += 1
            lastIaction = 0
        end if
        lastIline = -1
        selON = 0
    end if
end procedure
r_CompleteIndents = routine_id("CompleteIndents")

function inPend()
    if tokline!=lastIline then return 0 end if
    return lastIaction*lastIcount*isTabWidth
end function

procedure dragCommentsAlong(integer direction)
--
-- NB: this routine is only called for "pure" indent and unindent,
--     ie when inserting/removing leading tabs only, with no changes
--     to any leading spaces. The general idea is to help out a bit 
--     when surrounding constructs (if/for/while) are added/deleted.
--
sequence wsmatch -- whitespace + comment ("--"); required match string.
sequence wnmatch -- whitespace + blockcomment("--/**/"); skip string.
sequence checkline
integer lws,        -- length(wsmatch)
        ch,         -- a ' ' or '\t' to check
        sidx,       -- position of ch
        six,        -- effective column (until we pass "--\**\")
        thislen,    -- double blank lines now split up comment
        lastlen     -- blocks, except when inside a routine.

    if (direction=+1 and tokline>lastIline+1)
    or (direction=-1 and selY) then
        wsmatch = filetext[currfile][lastIline]
        lws = length(wsmatch)
        lastlen = lws
        for k=1 to lws do
            if not find(wsmatch[k]," \t") then
                wsmatch = wsmatch[1..k-1]
                exit
            end if
        end for
        wsmatch &= "--"
        sidx = 1
        six = 1
        lws = length(wsmatch)
        while sidx<=lws do
            ch = wsmatch[sidx]
            if ch=' ' then
                six += 1
            elsif ch='\t' then
                six += isTabWidth-remainder(six-1,isTabWidth)
            else
                exit
            end if
            if six>6 then exit end if
            sidx += 1
        end while
        wnmatch = "--/**/" & wsmatch[sidx..lws]
        if direction=1 then
            while tokline>lastIline+1 do
                checkline = filetext[currfile][lastIline+1]
                thislen = length(checkline)
                if match(wsmatch,checkline)!=1 and  -- not "		--"
--                 match(wnmatch,checkline)!=1 then -- not "--/**/	--"
                   match(wnmatch,checkline)!=1 and  -- not "--/**/	--"
                   (thislen!=0 or (lastlen=0 and lastRexpr=-1)) then
                    exit
                end if
                lastlen = thislen
                lastIline += 1
            end while
        else
            while selY do
                checkline = filetext[currfile][selY]
                thislen = length(checkline)
                if match(wsmatch,checkline)!=1 and  -- not "		--"
--                 match(wnmatch,checkline)!=1 then -- not "--/**/	--"
                   match(wnmatch,checkline)!=1 and  -- not "--/**/	--"
                   (thislen!=0 or (lastlen=0 and lastRexpr=-1)) then
                    exit
                end if
                lastlen = thislen
                selY -= 1
            end while
        end if
    end if
end procedure

procedure indentline()
--trace(1)
    if lastIline!=-1 then
        dragCommentsAlong(1)
        if tokline!=lastIline+1
        or lastIaction!=indent
        or lastIcount!=nTabs then
            CompleteIndents()
        end if
    end if
    lastIaction = indent
    lastIcount = nTabs
    lastIline = tokline
    lastRexpr = returnexpr
    if selON=0 then
        selY = tokline-1
        selON = 1
        dragCommentsAlong(-1)
    end if
end procedure

function unindentline(integer w, sequence line)
--trace(1)
    for i=1 to w do
        if line[i]!='\t' then return 0 end if
    end for
    if lastIline!=-1 then
        dragCommentsAlong(1)
        if tokline!=lastIline+1
        or lastIaction!=un_indent
        or lastIcount!=w then
            CompleteIndents()
        end if
    end if
    lastIaction = un_indent
    lastIcount = w
    lastIline = tokline
    lastRexpr = returnexpr
    if selON=0 then
        selY = tokline-1
        selON = 1
        dragCommentsAlong(-1)
    end if
    return 1
end function

--integer once = 1
procedure Indent(integer i)
-- check that line tokline begins with correct whitespace
sequence line
integer w, ch
--, wasNewRoutineBlock

    if not reformat then return end if --#without reformat in force.
    line = filetext[currfile][tokline]
--DEV once got set to {}
--if once then
--  once = 0
--  ?{line,also}
--end if
    i += nest
    --DEV possibly we could indent "--/**/", by first replacing
    --      that with spaces then undoubtedly several more tricks,
    --      but realistically there isn't or shouldn't be enough
    --      such code, wrongly indented, to bother about.
    if match("--/**/",line)=1
    or match("trace(1)",line)=1
    or match("trace(0)",line)=1 then
        return
    end if
--  wasNewRoutineBlock = newRoutineBlock
--  newRoutineBlock = 0
    w = 0
    for j=1 to length(line) do
        ch = line[j]
        if ch='\t' then
            w += isTabWidth - remainder(w, isTabWidth)
        elsif ch=' ' then
            w += 1
        else
            if j<=length(line)-4
            and indentandor then -- set/reset in DoWhile&DoIf (only)
                -- align as eg:
                --      elsif xxx           while xxx
                --        and xxx              or xxx
                -- rather than:
                --      elsif xxx           while xxx
                --      and xxx             or xxx
                if equal(line[j..j+1],"or")
                and find(line[j+2]," \t") then
                    i += 3
                elsif equal(line[j..j+2],"and")
                and find(line[j+3]," \t") then
                    i += 2
                end if
            end if
            exit
        end if
    end for
    w = i-w
    if length(also) then
        for j=1 to length(also) do
            if i=also[j]
            or i=also[j]+w then
                w = 0
                exit
            end if
        end for
    end if
    if w then
        for j=1 to length(line) do
            if not find(line[j]," \t") then
                nTabs = floor(i/isTabWidth)
                nSpaces = i-nTabs*isTabWidth
                w = j-1
                while w and nSpaces and line[w]=' ' do
                    nSpaces -= 1
                    w -= 1
                end while
                if nSpaces=0 then
                    while w and nTabs and line[w]='\t' do
                        nTabs -= 1
                        w -= 1
                    end while

                    if w=0 then -- pure indent
                        indentline()
                        return
                    else            -- pure un_indent?
--15/12/15:
--/*
                        if wasNewRoutineBlock then
                            -- first statement for this routine:
                            -- allow either:
                            --      procedure x()
                            --          a=0
                            --          b=0
                            --      end procedure
                            -- or:
                            --      procedure x()
                            --              a=0
                            --              b=0
                            --      end procedure
                            --
--                          if w=1 then
                            if w=1 and line[w]='\t' then
                                undoExtraNest = isTabWidth
                                nest += isTabWidth
                                return
                            end if
                        end if
--*/
                        if unindentline(w,line) then return end if
                    end if
                end if
                CompleteIndents()
                Delete(line,tokline,1,w,TABWORK)
                if tokline=CurrLine then
                    w -= nTabs+nSpaces
                    col -= w
                    tokstart -= w   -- for DoIf
                    tokend -= w
                end if
                exit
            end if
        end for
--trace(1)
--?9/0
        adjust_count += 1
    end if
end procedure


--
-- Part 3. The (highly simplified) parser.
--

--integer rExpression
forward procedure Expression()
forward procedure Factor()

procedure DoSequence(integer vch)
-- token=="{" on entry
integer wasNest -- see eg Expression()
integer wasMapEndToMinusOne
integer wasinexpression
--integer nIndent
sequence wasalso = also     -- (added 19/3/17)
    wasinexpression = inexpression
    inexpression = 1
    wasNest = nest
--DEV crash here 27/6/16:
    nest = ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
    striptrailing = 1
if 0 then --old code
    Match("{",vch!='@')
--  if parser_tokno=1 then Indent(0) end if
    while not equal(token,"}") do
--/*
--      if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then  
--          mapEndToMinusOne = 0
--          getToken()
--          exit
--      end if
--      mapEndToMinusOne = 0
----===
--
--*/
        if parser_tokno=1 then Indent(0) end if
--      call_proc(rExpression,{})
        Expression()
        if not equal(token,",") then exit end if
--1/2/17:
        also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
        Match(",",true)
--      if parser_tokno=1 then Indent(0) end if
    end while
else
    while Ch!=-1 do
        if Ch='$' then
            wasMapEndToMinusOne = mapEndToMinusOne
            mapEndToMinusOne = 1
            getToken()
            mapEndToMinusOne = wasMapEndToMinusOne
            getToken()
            exit
        end if
        getToken(vch!='@')
        if equal(token,"}") then exit end if
        if parser_tokno=1 then Indent(0) end if
--      call_proc(rExpression,{})
        Expression()
        if not equal(token,",") then exit end if
--1/2/17: (DEV/doc see trick with IupButton comma at end of plade.exw)
--[[ ie          {nr("IupButton")       ,'E',"[T]AFAD", -- comma is just before the 'E'... ]]
        also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
--?{tokline,also}
--/*
--      if parser_tokno!=1 then
        SkipSpacesAndComments()
        if CurrLine=tokline then
            nIndent = ExpLength(filetext[currfile][tokline][1..col])+inPend()
            if not find(nIndent,also) then also &= nIndent end if
?{tokline,nIndent,also}
        end if
--*/
    end while
end if

    -- if the closing '}' is the first token on its own 
    --  line, put it directly under the opening '{'
    if parser_tokno=1 then nest -= 1 Indent(0) end if

    nest = wasNest
    inexpression = wasinexpression
    if not inexpression then
        if nPreprocess then
            completeifdef()
        end if
    end if
    stripleading = 1
    if vch!=0 then
--      charClass['@'] = SYMBOL
        charClass[vch] = SYMBOL
    end if
    Match("}")
    also = wasalso
end procedure

procedure Params(integer asiff)
integer wasNest -- see eg Indent()
integer wasinexpression
integer namedparams = 0
    getToken()
    stripleading = 1
    striptrailing = 1
    Match("(",true)
    if not equal(token,")") then
        wasinexpression = inexpression
        inexpression = 1
        wasNest = nest
        nest = ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()
        while Ch!=-1 do
--          call_proc(rExpression,{})
            Expression()
            if asiff then
                if not find(token,{",","?",":"}) then exit end if
            else
--<25/1/17> (named and defaulted parameters)
                if find(token,{":=","="}) then
                    getToken(true)
--                  call_proc(rExpression,{})
                    Expression()
                    namedparams = 1
                elsif namedparams then
                    Match("=")  -- trigger error
                end if
--</25/1/17>
                if not equal(token,",") then exit end if
            end if
--1/2/17:
            also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
            getToken(true)
            if parser_tokno=1 then Indent(0) end if
        end while
        -- if the closing ')' is the first token on its own 
        --  line, put it directly under the opening '('
        if parser_tokno=1 then nest -= 1 Indent(0) end if
        nest = wasNest
        inexpression = wasinexpression
        if not inexpression then
            if nPreprocess then
                completeifdef()
            end if
        end if
    end if
    stripleading = 1
    Match(")")
end procedure

procedure DoSubScripts2()
integer wasMapEndToMinusOne = mapEndToMinusOne
bool wasdot = false,
     wasfromsubss = fromsubss

    mapEndToMinusOne = 1
    while 1 do
        stripleading = 1
        striptrailing = 1
        if token="." then
            Match(".",false)
            wasdot = true
            fromsubss = true
            Factor()
            fromsubss = wasfromsubss
        else
            Match("[",true)
--          call_proc(rExpression,{})
            Expression()
        end if
        if toktype=ELLIPSE
        or (toktype=LETTER and token="to") then
            if toktype=ELLIPSE then
                stripleading = 1
                striptrailing = 1
            end if
            getToken(true)
--          call_proc(rExpression,{})
            Expression()
            stripleading = 1
            exit
        end if
--/"*
        if token="," and not wasdot then    -- (don't allow x.i,j)
            token = "["
        elsif token!="." then
            if Ch!='[' then exit end if
            stripleading = 1
            Match("]")
        end if
--*"/
--wasdot>>
--      if Ch!='[' then exit end if
--      stripleading = 1
--      Match("]")
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
    if not wasdot then
        Match("]")
    end if
end procedure

procedure DoSubScripts()
integer wasMapEndToMinusOne
    wasMapEndToMinusOne = mapEndToMinusOne
    mapEndToMinusOne = 1
    while Ch='[' do
        getToken()
        stripleading = 1
        striptrailing = 1
        Match("[",true)
--      call_proc(rExpression,{})
        Expression()
        if toktype=ELLIPSE then
            stripleading = 1
            striptrailing = 1
            getToken(true)
--          call_proc(rExpression,{})
            Expression()
            stripleading = 1
            Check("]")
            exit
        end if
        stripleading = 1
        Check("]")
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
    getToken()
end procedure

procedure skip_namespace()
integer wasparser_tokno
    if CurrLine>textlen
    or col>=length(filetext[currfile][CurrLine])
    or filetext[currfile][CurrLine][col+1]!='=' then
        wasparser_tokno = parser_tokno
        getToken()
        Match(":")
        parser_tokno = wasparser_tokno
    end if
end procedure

integer treatColonAsThen

procedure Factor()
integer wasNest
integer wasinexpression
integer wasindentandor
    wasindentandor = indentandor
    indentandor = 0
--if token="~" then ?toktype end if
    if toktype=SYMBOL and find(token,{"+","-","~"}) then
--DEV (no help)
--if token="~" then
--      wasNest = nest
--      Match(token,true)
--      nest = wasNest
--else
        striptrailing = 1
        Match(token,true)
--end if
    elsif toktype=LETTER and equal(token,"not") then
        Match(token,true)
    end if
    if toktype=LETTER then
        if not treatColonAsThen then
            if Ch=':' then skip_namespace() end if
        end if
        if Ch='(' then  -- a function, we presume
            Params(find(token,{"iff","iif"}))
--17/12/15: (rescinded)
        else            -- a variable, we presume
if use2 then
            getToken()
--?{token,toktype,tokline,"rein.e line 2096"}
--if tokline=135 then trace(1) end if
else
            DoSubScripts()
end if
        end if
    elsif find(toktype,{DIGIT,FLOAT,HEXDEC,DQUOTE,SQUOTE}) then
        getToken()
    elsif toktype=SYMBOL and equal(token,"{") then
        DoSequence(0)
    elsif toktype=SYMBOL and equal(token,"(") then
        wasinexpression = inexpression
        inexpression = 1
        wasNest = nest

        nest = ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
--erm...
--      striptrailing = 1
        getToken(true)
--      call_proc(rExpression,{})
        Expression()
--      stripleading = 1
--?tokline
--if tokline=50 then ?1 trace(1) end if
--17/12/15: (rescinded, could not get this to work...)
        Match(")")
--      Check(")")
--      DoSubScripts()
        nest = wasNest
        inexpression = wasinexpression
        if not inexpression then
            if nPreprocess then
                completeifdef()
            end if
        end if
    else
        Abort(xl("unrecognised"))
    end if
--17/12/15: (never worked, there's a getToken at the end of DoSubScripts)
if use2 then
    if toktype=SYMBOL and (equal(token,"[") or (not fromsubss and token=".")) then
--  if Ch='[' then
--  if toktype=SYMBOL and Ch='[' then
--trace(1)
        DoSubScripts2()
    end if
end if
    indentandor = wasindentandor
end procedure

-- strip leading/trailing spaces around these operators...
constant eRelop = {"<","<=","=","==","!=",">",">="}
constant eMaths = {"*","/","+","-"}
constant eStrip = eRelop & eMaths

--  ... but not these
constant eNoStrip = {"and","or","xor","&"}

procedure Expression()
integer k
integer wasNest
integer wasinexpression
    wasinexpression = inexpression
    inexpression = 1
    wasNest = nest
    Factor()
    while 1 do
        k = find(token,eStrip)
        if k then
            stripleading = 1
            striptrailing = 1
            if k<=length(eRelop) then
                nest = ExpLength(filetext[currfile][tokline][1..tokend])+inPend()
            else
                nest = wasNest
            end if
        else
            nest = wasNest
            k = find(token,eNoStrip)
            if k=0 then exit end if
        end if
        if parser_tokno=1 then Indent(0) end if
--DEV [not]fromsubss?
        Match(token,true)
        if parser_tokno=1 then Indent(0) end if
        Factor()
    end while
    nest = wasNest
    inexpression = wasinexpression
    if not inexpression then
        if nPreprocess then
            completeifdef()
        end if
    end if
end procedure
--rExpression = routine_id("Expression")

forward procedure Block()

--with trace
procedure DoIf()    -- Process an if construct
--integer wasNest
sequence wasalso
--trace(1)
--  wasNest = nest
--DEV I think this is now always {}? (search for DoIf)
    wasalso = also
    if tokstart>1 or nest or inPend() then
        nest += isTabWidth
    end if
    Match("if",true)
    also = {ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()}
    Expression()
    also = {}
    Match("then")
--trace(1)
    Block()
    while equal(token,"elsif") do
        if parser_tokno=1 then Indent(0) end if
        Match("elsif")
        indentandor = 1
        Expression()
        indentandor = 0
        Match("then")
        Block()
    end while
    if equal(token,"else") then
        if parser_tokno=1 then Indent(0) end if
        Match("else")
        Block()
    end if
    if parser_tokno=1 then Indent(0) end if
--DEV +adjustments for ifdef? (lots of other places too)
--  nest = wasNest
--DEV better!!!
    nest -= isTabWidth
    Match("end")
--trace(1)
    Match("if")
    also = wasalso
end procedure

procedure DoWhile() -- Process a while statement
integer wasNest
    wasNest = nest
    if tokstart>1 or nest or inPend() then
        nest += isTabWidth
    end if
    Match("while",true)
    indentandor = 1
    Expression()
    indentandor = 0
    Match("do")
    Block()
    if parser_tokno=1 then Indent(0) end if
    nest = wasNest
    Match("end")
    Match("while")
end procedure

procedure DoSwitch()    -- Process a switch statement
integer wasNest
--trace(1)
    wasNest = nest
    if tokstart>1 or nest or inPend() then
        nest += isTabWidth
    end if
    Match("switch",true)
    Expression()
    if find(token,{"with","without"}) then
        while Ch!=-1 do
            getToken()
            if not find(token,{"fallthru","fallthrough","jump_table"}) then exit end if
            getToken()
            if not equal(token,",") then exit end if
        end while
    end if
    if token="do" then Match("do") end if
    nest += isTabWidth
    while Ch!=-1 do
        if find(token,{"else","default"}) then
            getToken()
        else
            if parser_tokno=1 then Indent(0) end if
            Match("case",true)
            if find(token,{"else","default"}) then
                getToken()
            else
                treatColonAsThen = 1
                while Ch!=-1 do
                    Expression()
                    if not equal(token,",") then exit end if
--1/2/17: (erm)
--                  also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
                    getToken(true)
                end while
                treatColonAsThen = 0
            end if
        end if
        if find(token,{":","then"}) then
            getToken(true)
        end if
--      nest += isTabWidth
        Block()
--      nest -= isTabWidth
        if find(token,{"fallthru","fallthrough","break"}) then
            getToken()
        end if

        -- (added 18/12/2015:)
        if token=";" then
            getToken()
        end if

        if equal(token,"end") then exit end if
    end while
    nest -= isTabWidth
    if parser_tokno=1 then Indent(0) end if
    nest = wasNest
    Match("end")
    Match("switch")
end procedure

procedure DoFor()       -- Process a for statement
integer wasNest
    wasNest = nest
    if tokstart>1 or nest or inPend() then
        nest += isTabWidth
    end if
--?{tokline,also}
    Match("for")
    getToken()
    stripleading = 1
    striptrailing = 1
    Match("=",true)
    Expression()
    Match("to",true)
    Expression()
    if equal(token,"by") then
        getToken(true)
        Expression()
    end if
    Match("do")
--DEV..
--  nest = wasNest
    Block()
--?{tokline,also}
    if parser_tokno=1 then Indent(0) end if
    nest = wasNest
    Match("end")
    Match("for")
end procedure

procedure DoExit()
    Match("exit")
end procedure

procedure DoBreak()
    Match("break")
end procedure

procedure DoContinue()
    Match("continue")
end procedure

procedure DoReturn()        -- Process a return statement
integer wasNest
integer wasinexpression
--(untried; isAlignIfdef=1 made it better)
--sequence wasalso
--  wasalso = also
    if returnexpr=-1 then Abort(xl("return must be inside a procedure or function")) end if
--  if returnexpr=-1 then ?{"oops",tokline} Abort(xl("return must be inside a procedure or function")) end if
    Match("return",true)
    if returnexpr then
--      also = {ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()}
        wasinexpression = inexpression
        inexpression = 1
        if isAlignIfdef=1 then
            wasNest = nest
            nest = ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()
            Expression()
            nest = wasNest
        else
            Expression()
        end if
        inexpression = wasinexpression
        if not inexpression then
            if nPreprocess then
                completeifdef()
            end if
        end if
--      also = wasalso
    end if
end procedure

procedure DoQu()    -- The '?' shorthand.
    getToken(true)
    Expression()
end procedure

--DEV this currently makes no attempt to indent anything:
procedure DoIlasmEtc()
    getToken()
    if not find(token,{"ilasm","ilASM","istype","isinit","isginfo"}) then
        Abort(xl("ilasm, istype, isinit, or isginfo expected"))
        return
    end if
    getToken()
    Match("{")
    while Ch!=-1 do
        while find(Ch,":%@") do
            col += 1
            nextCh()
        end while
        getToken()
        if equal(token,"}") then
            getToken()
            exit
        end if
    end while
end procedure

procedure Assignment2(bool allowsubscripts)
integer wasNest
integer wasinexpression
--bool wasdot = false,
--   wasfromsubss = fromsubss

    getToken()
    if toktype=SYMBOL and find(token,{"[","."}) then
        if not allowsubscripts then
            Abort("invalid")
        end if
        DoSubScripts2()
    end if
    if not find(token,{"=",":=","+=","-=","*=","/=","&="}) then
        Abort("assignment operator expected")
        return
    end if
    insertspaces = 1
    getToken(true)
    wasinexpression = inexpression
    inexpression = 1
    wasNest = nest
    nest = ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()
    Expression()
    nest = wasNest
    inexpression = wasinexpression
    if not inexpression then
        if nPreprocess then
            completeifdef()
        end if
    end if
end procedure

procedure Assignment()
integer wasNest
integer wasinexpression
--trace(1)
    DoSubScripts()
    if not find(token,{"=",":=","+=","-=","*=","/=","&="}) then
--?9/0
        Abort(xl("assignment operator expected"))
        return
    end if
    insertspaces = 1
    getToken(true)
    wasinexpression = inexpression
    inexpression = 1
    wasNest = nest
    nest = ExpLength(filetext[currfile][tokline][1..tokstart-1])+inPend()
    Expression()
--DEV also = wasalso?
    nest = wasNest
    inexpression = wasinexpression
    if not inexpression then
        if nPreprocess then
            completeifdef()
        end if
    end if
end procedure

procedure MultiAssignment()
    if parser_tokno=1 then Indent(isTabWidth) end if -- added 13/2/14
--  charClass['@'] = SYMBOL
    DoSequence('@')
    if not find(token,{"=",":=","@="}) then
        Abort(xl("assignment operator expected"))
        return
    end if
    charClass['@'] = ILLEGAL
    getToken(true)
    Expression()
    -- (added 18/12/2015:)
    if token=";" then
        getToken()
    end if
end procedure

--NB keep Statement() in step with this
constant ifforwhileetc = {"if","for","while","continue","exit","break","switch","return","end","?","#i","class"}

--with trace
procedure DoRoutineDef(integer rType, integer fwd)
sequence sType, tType

--trace(1)
    sType = token   -- procedure/function/type
    getToken()
--?{"DoRoutineDef",token}
    tType = token   -- type name
--?{"DoRoutineDef",token,nest}
    getToken()
    stripleading = 1
    striptrailing = 1
--DEV set nest here?
    Match("(")
--1/2/17: (erm, untried)
--  also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
    if not equal(token,")") then
        while Ch!=-1 do
            if Ch=':' then skip_namespace() end if
            if not find(token,vartypes) then
                if charClass[Ch]!=LETTER then
                    Abort(xl("a type is expected here"))
                    exit
                end if
                WarnType()
                vartypes = append(vartypes,token)
            end if
--if tType="resize_cb" then
--  trace(1)
--end if
            getToken()
--if tType="leftarrow" then
--  ?token
--end if
--15/2/16: (unnamed params)
if equal(token,",") then
            while 1 do
                getToken()
                if not equal(token,",") then exit end if
            end while
            if equal(token,")") then exit end if
elsif equal(token,")") then
            exit
else
            getToken()
--          if equal(token,"=") then
            if find(token,{"=",":="}) then
                getToken(true)
                Expression()
            end if
            if not equal(token,",") then exit end if
--1/2/17: (erm)
--          also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
            getToken()
end if
--DEV positive types..
--          while Ch=',' do
--              getToken()
--              getToken()
--          end while
        end while
    end if
    stripleading = 1
    Match(")")
    if fwd then return end if
--  also = {0,4}
    also = {}
    while toktype=LETTER do
        if not find(token,vartypes) then    -- see Note1
            if find(token,ifforwhileetc) then exit end if
            --
            if charClass[Ch]!=LETTER then exit end if
            -- ^explanation^: word word (not word =, or word[... etc) must be
            --                a variable definition (eg boolean t), not code.
            --
            WarnType()
            vartypes = append(vartypes,token)
        end if
        if parser_tokno=1 then Indent(0) end if
        while 1 do
            getToken()
--added 7/3/16:
            if toktype=SYMBOL and equal(token,"{") then
                MultiAssignment()
                if Ch!=',' then exit end if
            end if
            if Ch='=' then
if use2 then
                Assignment2(False)
else
                Assignment()
end if
                if token!="," then exit end if
            else
                if Ch!=',' then getToken() exit end if
                getToken()
            end if
        end while
        -- (added 18/12/2015:)
        if token=";" then
            getToken()
        end if
    end while
    also = {}
--  newRoutineBlock = 1     -- allows 1 or 2 tab indent
    returnexpr = (rType>1)
--?{2541,returnexpr,tokline}
--trace(1)
    Block()
--  if undoExtraNest then
--      undoExtraNest = 0
--      nest -= isTabWidth
--  end if
--  newRoutineBlock = 0
    also = {}
--  also = {0,4}
    if parser_tokno=1 then Indent(0) end if
    Match("end")
    Match(sType)
    if rType=3 then
        vartypes = append(vartypes,tType)
    end if
    returnexpr = -1
--?{2558,returnexpr,tokline}
end procedure

forward function TopLevel()

integer in_class = 0

--DEV/sug...
procedure DoClass() -- Process a class definition
integer wasNest = nest
    if in_class then
        Abort(xl("classes may not be nested"))
        return
    end if
    in_class = 1
--  if tokstart>1 or nest or inPend() then
--      nest += isTabWidth
--  end if
    Match("class")
--?{"class",token,nest}
    if Ch='<' then
        getToken()
        Match("<")
    end if
    getToken()
--  Block()
    while 1 do
--type?
--DEV handle property etc specially... prohibit "normal" code?? (NO! ...erm YEP!)
        if find(token,{"procedure","function","type"}) then
            nest += isTabWidth
        end if
        if not TopLevel() then exit end if
        nest = wasNest
        if token="end" then exit end if
    end while
    if parser_tokno=1 then Indent(0) end if
    nest = wasNest
    Match("end")
    Match("class")
    in_class = 0
end procedure

procedure Statement()
integer k
    if toktype = LETTER then
        nestAtStatementStart = nest
--constant ifforwhileetc = {"if","for","while","continue","exit","break","switch","return","end","?","#i","class"}
--                           1     2      3        4        5       6       7        8       9   10   11    12
        k = find(token,ifforwhileetc)
        if k then
--1/2/17:
--          also = {}
            if parser_tokno=1 then Indent(isTabWidth) end if
            if k=1 then DoIf()
            elsif k=2 then DoFor()
            elsif k=3 then DoWhile()
            elsif k=4 then DoContinue()
            elsif k=5 then DoExit()
            elsif k=6 then DoBreak()
            elsif k=7 then DoSwitch()
            elsif k=8 then DoReturn()
            elsif k=9 then Abort(xl("unexpected"))
            elsif k=10 then DoQu()
            elsif k=11 then DoIlasmEtc()
            elsif k=12 then DoClass()
            end if
        else
            if parser_tokno=1 then Indent(isTabWidth) end if
            if Ch=':' then skip_namespace() end if
            if Ch='(' then Params(0)
--          else Assignment()
            else 
if use2 then
                Assignment2(True)
else
                Assignment()
end if
            end if
        end if
        if equal(token,";") then
            getToken()
        end if
    elsif toktype=SYMBOL and equal(token,"{") then
        MultiAssignment()
    else
        Abort(xl("unrecognised"))
    end if
end procedure

--with trace
procedure Block()
    while 1 do
        if toktype=LETTER then
--DEV break should probably be a statement... (see exit)
--          if find(token,{"elsif","else","end","case","default","fallthru","fallthrough","break"}) then exit end if
            if find(token,{"elsif","else","end","case","default","fallthru","fallthrough"}) then exit end if
-- 13/2/14 ("declare anywhere")
            while toktype=LETTER do
                if not find(token,vartypes) then    -- see Note1
                    if find(token,ifforwhileetc) then exit end if
                    --
                    if charClass[Ch]!=LETTER then exit end if
                    -- ^explanation^: word word (not word =, or word[... etc) must be
                    --                a variable definition (eg boolean t), not code.
                    --
                    WarnType()
                    vartypes = append(vartypes,token)
                end if
                nest += isTabWidth
                if parser_tokno=1 then Indent(0) end if
                while 1 do
                    getToken()
-- 18/6/16:
                    if toktype=SYMBOL and equal(token,"{") then
                        MultiAssignment()
                        if Ch!=',' then exit end if
                    end if
--23/2/14:
--                  if Ch='=' then
                    if Ch='=' or Ch=':' then
if use2 then
                        Assignment2(False)
else
                        Assignment()
end if
                        if token!="," then exit end if
                    else
                        if Ch!=',' then getToken() exit end if
                        getToken()
                    end if
                end while

                -- (added 18/12/2015:)
                if token=";" then
                    getToken()
                end if

                nest -= isTabWidth
            end while
-- /13/2/14 ends
            Statement()
        elsif toktype=SYMBOL and equal(token,"{") then
            MultiAssignment()
        else
            exit
        end if
    end while
end procedure
--rBlock = routine_id("Block")

procedure TopDecls()
integer wasMapEndToMinusOne
    if parser_tokno=1 then Indent(0) end if
    while Ch!=-1 do
        getToken()
        if toktype=SYMBOL and equal(token,"{") then
            MultiAssignment()
            if Ch!=',' then exit end if
        end if
        if Ch='=' then
if use2 then
            Assignment2(False)
else
            Assignment()
end if
            if not equal(token,",") then exit end if
        else
            if Ch!=',' then getToken() exit end if
            getToken()
        end if
        if Ch='$' then
            wasMapEndToMinusOne = mapEndToMinusOne
            mapEndToMinusOne = 1
            getToken()
            mapEndToMinusOne = wasMapEndToMinusOne
            getToken()
            exit
        end if
    end while
-- (added 19/12/2015:)
    if token=";" then
        getToken()
    end if
end procedure

procedure DoConstant()
integer wasMapEndToMinusOne
    getToken()  -- discard "constant".
--if tokline=29 then trace(1) end if
    while Ch!=-1 do
        if parser_tokno=1 then Indent(0) end if
        if toktype=LETTER
        and find(token,vartypes)
        and (charClass[Ch]=LETTER or Ch='{') then
            -- cope with typed constant definitions, eg:
            --  constant string s = "string",
            --           integer {i,j,k} @= 0
            getToken()
        end if
        if toktype=SYMBOL and equal(token,"{") then
            MultiAssignment()
        else
            getToken()          -- constant name
            insertspaces = 1
            Match("=",true)
            if Ch=':' then skip_namespace() end if
            Expression()
        end if
        if not equal(token,",") then exit end if
        if Ch='$' then
            wasMapEndToMinusOne = mapEndToMinusOne
            mapEndToMinusOne = 1
            getToken()
            mapEndToMinusOne = wasMapEndToMinusOne
            getToken()
            exit
        end if
--1/2/17: (erm)
--      also &= ExpLength(filetext[currfile][tokline][1..tokstart])+inPend()
        getToken()
    end while
    -- (added 18/12/2015:)
    if token=";" then
        getToken()
    end if
end procedure

function TopLevel()
integer fwd = 0
    if toktype=LETTER then
        nestAtStatementStart = nest
        integer t = find(token,{"procedure","function","type"})
        if t then
            also = {}
            if parser_tokno=1 then Indent(0) end if
            DoRoutineDef(t,fwd)
            fwd = 0
            also = {0}
        elsif equal(token,"constant") then
            also = {ExpLength(filetext[currfile][CurrLine][1..col-1])}
            if parser_tokno=1 then Indent(0) end if
            DoConstant()
--1/2/17:
--          if not find(0,also) then also = prepend(also,0) end if
            also = {0}
        elsif find(token,{"global","public","export","override","forward"}) then
            if token="forward" then fwd = 1 end if
--1/2/17:
--          also = {}
            if parser_tokno=1 then Indent(0) end if
            getToken()  -- otherwise ignore it
        elsif find(token,{"include","with","without","namespace"}) then
            also = {}
            if parser_tokno=1 then Indent(0) end if
            CurrLine += 1
            lexer_tokno = 1
            col = 1
            SkipSpacesAndComments()
            getToken()
            also = {0}
        else
            if Ch=':' then skip_namespace() end if
            t = find(token,vartypes)
            if not t and not equal(token,"?") then
                if charClass[Ch]=LETTER
                and not find(token,ifforwhileetc) then
                    -- word word (not word =, or word[... etc) /must/ be
                    --  a variable definition (eg boolean t), not code.
                    WarnType()
                    vartypes = append(vartypes,token)
                    t = length(vartypes)
                end if
            end if
            if t then
                if parser_tokno=1 then Indent(0) end if
                also = {ExpLength(filetext[currfile][CurrLine][1..col-1])}
                TopDecls()
                if not find(0,also) then
                    also = prepend(also,0)
                end if
            else
                if ifdefstackidx=0
                and equal(token,"abort") and Ch='(' then
                    -- stop processing, and warn if remainder of file
                    -- is other than just blank lines and comments
                    for j=CurrLine+1 to length(filetext[currfile]) do
                        sequence aline = filetext[currfile][j]
                        while length(aline) and find(aline[1]," \t\r\n") do
                            aline = aline[2..length(aline)]
                        end while
                        if length(aline)!=0 and match("--",aline)!=1 then
                            Warning("no further lines processed",{})
                            exit
                        end if
                    end for
                    return false    -- (cease)
                end if
                Statement()
            end if
        end if
    elsif toktype=SYMBOL and equal(token,"{") then
        MultiAssignment()
    else
        Abort(xl("invalid"))
        return false    -- (cease)
    end if
    return true     -- (continue)
end function

--
-- Part 4. The main control routine.
--

procedure onclick_GO()
--integer t
--sequence aline
integer wasmapEndToMinusOne, wascursorY

    clearSelection()    -- added 28/8/09
    wascursorY = CursorY
    CurrLine = 1
    lexer_tokno = 1
    col = 1
--  undoExtraNest = 0
--  newRoutineBlock = 0
    inexpression = 0
    indentandor = 0
    adjust_count = 0
    reformat = 1
    lastIline = -1
    treatColonAsThen = 0
    nest = 0
    errtext = ""
    nWarns = 0
    also = {0}
    vartypes = {"atom","integer","int","bool","sequence","seq","string","object","Ihandle","Ihandln","cdCanvas","cdCanvan","nullable_string","imImage"}
    vartypes = append(vartypes,"enum") -- DEV...
    withdefs = {}
    textlen = length(filetext[currfile])
    if textlen
    and match("#!",filetext[currfile][1])=1 then    -- skip shebang
        CurrLine = 2
    end if
    ifdefstackidx = 0
    ifdefnests = {}
    ifdefnestz = {}
    ifdefstate = {}
    ifdefflags = {}
    ifdefcheck = repeat(0,7)
    pptokline = 0
    stripleading = 0
    striptrailing = 0
    insertspaces = 0
    SkipSpacesAndComments()
    getToken()
    while Ch!=-1 do
        if not TopLevel() then exit end if
    end while
    CompleteIndents()
    if not equal(ifdefcheck,repeat(0,7)) then

        -- 06/03/2011: use exactly the same settings for recover() as preprocess().
        wasmapEndToMinusOne = mapEndToMinusOne
        mapEndToMinusOne = -3   -- treat '$' and '%' as normal symbols
                                -- (since this must skip/ignore eg x[$], #ilasm{...%isVar} etc.)

        CurrLine = pplastline
        tokstart = pplaststart
        recover(pptokline, pptokstart)

        mapEndToMinusOne = wasmapEndToMinusOne

        if not equal(ifdefcheck,repeat(0,7)) then
            errtext &= "*** ooops, ifdef<==>fedfi not right ***\n"
            errtext &= sprint(ifdefcheck) & "\n"
        end if
    end if
    xlt = xl("Re-Indent complete: %d change%s made")
    errtext &= sprintf(xlt,{adjust_count,repeat('s',adjust_count!=1)})
--  void = proemh(xl("ReIndent"), errtext, 0)
    IupMessage(xl("ReIndent"), errtext)
    if CursorY!=wascursorY then
        forceCursorOnscreen()
        ensureVisible(CursorY,0)
        paintLines(wascursorY,wascursorY)
        paintCursorY()
    end if
end procedure


constant xlTITLE = xl("Reindent source file")
--constant xlTIP     = xl("Tip: To see the effect of these flags, run this on src\\indent.exw")
constant xlSPACE = xl("Remove spaces in expressions?")
constant xlALGN1 = xl("Align ifdef in column 1")
constant xlALGN0 = xl("Align ifdef as part of code")
constant xlALGNO = xl("Do not align ifdef")
constant xlGO    = xl("Go")
constant xlCANCL = xl("Cancel")
constant xlHELP  = xl("Help")

constant {fnames,fvalues} = columnize({{"xlSPACE",xlSPACE},
                                       {"xlALGN1",xlALGN1},
                                       {"xlALGN0",xlALGN0},
                                       {"xlALGNO",xlALGNO},
                                       {"xlGO",   xlGO   },
                                       {"xlCANCL",xlCANCL},
                                       {"xlHELP", xlHELP }})

constant fmt = substitute_all("""
_____________xlSPACE: %b[No,Yes]
             %o|xlALGN1|xlALGN0|xlALGNO|
             Bt %u[xlGO, xlCANCL, xlHELP]
""",fnames,fvalues)

integer space = isStripSpaces,
        align = iff(isAlignIfdef=-1?2:isAlignIfdef),
        status

--constant REIN = create(Window,     xlTITLE,   0, Main, 330, 165, 337, 194, 0)
--constant TIP  = create(Label,      xlTIP,     0, REIN,  11,  14, 341,  20, 0)
--constant SPACE    = create(CheckBox,   xlSPACE,   0, REIN,   8,  40, 175,  20, 0)
--constant ALIGN1 = create(RadioButton,xlALGN1, 0, REIN,   8,  65, 165,  20, 0)
--constant ALIGN0 = create(RadioButton,xlALGN0, 0, REIN,   8,  83, 165,  20, 0)
--constant ALIGNO = create(RadioButton,xlALGNO, 0, REIN,   8, 100, 165,  20, 0)
--constant GO   = create(Button,     xlGO,      0, REIN, 125, 126,  80,  25, 0)

--setHint(SPACE,xl("Converts eg \"a = length ( x [ 3 ] )\" to \"a = length(x[3])\""))
--setCheck(SPACE,isStripSpaces)
--setCheck(ALIGN1,isAlignIfdef=1)   -> 1
--setCheck(ALIGN0,isAlignIfdef=0)   -> 0
--setCheck(ALIGNO,isAlignIfdef=-1) -> 2

--/*
without trace
function reinHandler(integer id, integer msg, atom wParam, object lParam)
    if object(lParam) then end if
    if msg = WM_CHAR then
--      if wParam = VK_RETURN then
        if wParam = K_CR then
            msg = WM_COMMAND
            id = GO
        elsif wParam = VK_ESCAPE then
            msg = WM_CLOSE
        end if
    end if
    if msg = WM_COMMAND then
        if find(id,{SPACE,ALIGN1,ALIGN0,ALIGNO}) then
            if lParam=1 then    -- accelerator key
                setCheck(id,not isChecked(id))
            else                -- space bar or mouse click
                setCheck(id,isChecked(id))
            end if
        elsif id = GO then
            if isChecked(ALIGN1) then
                isAlignIfdef = 1
            elsif isChecked(ALIGN0) then
                isAlignIfdef = 0
            else -- isChecked(ALIGNO) then
                isAlignIfdef = -1
            end if
            isStripSpaces = isChecked(SPACE)
            onclick_GO()
            msg = WM_CLOSE
        end if
    end if
    if msg = WM_CLOSE then
        removeFocus(REIN)
        setVisible(REIN, False)
        setFocus(Main)
    end if
    return 0
end function
setHandler({REIN,TIP,SPACE,ALIGN1,ALIGN0,ALIGNO,GO},
            routine_id("reinHandler"))
--*/
global procedure ReIndent()
--/**/sequence ext
    if currfile then
--!/**/ ext = getFileExtension(filenames[currfile])
--/**/  ext = get_file_extension(filenames[currfile])
--/**/  if find(ext,{"htm","html","xml"}) then
--/**/      reinh:htmlreindent()
--/**/  else
--          addFocus(REIN)
--          openWindow(REIN, SW_NORMAL)
            {space, align, status} = IupGetParam(xlTITLE, NULL, 0, fmt, {space, align})
            if status=1 then
                isAlignIfdef = iff(align=2?-1:align)
                isStripSpaces = space
                onclick_GO()
            end if
--/**/  end if
    end if
end procedure
--global constant r_ReIndent = routine_id("ReIndent")


