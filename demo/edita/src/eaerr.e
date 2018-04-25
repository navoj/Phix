--
-- eaerr.e
--
-- Preprocesses a *.err file to make it more readable.
-- Must also recalculate linelengths.

-- Programming notes:
-- =================
--  Additional processing is willfully undertaken to avoid any overhead from
--   memory allocations. OK, speed is not a major worry as such, but if I can 
--   make it unnoticeable, I will. It seems fast enough, eg a 5241 line, 247K,
--   *.err is timed at 0.23 seconds on my 233MHz antique. It also results in 
--   just 3848 lines, a saving of 1393 lines, or 26.5% (not that I ever meant
--   to make the file shorter, nor do I think it is a good idea to attempt to
--   further improve on that). It can be a bit fiddly in places, however, so 
--   be warned.
--
--  That said, apart from the dealing of 16 or so cases long-hand, the only 
--   real complication is the handling of "it isn't really a string" after 
--   several lines have been processed (hence inch/newinch, the flag 
--   fromNextNonSpace, inln/newinln, etc).
--
--  Plain character comparisons are used in preference to converting numbers
--   such as 147756045 with n=n*10+ch-'0', and then checking the resulting value
--   lies between 32 and 126, which would be much slower.
--
--  I am not fully convinced about the check[s] for '}'... Test cases please ;-))
--
--  [Partial] replacement strings are built in the static newstring area,
--   and only copied once confirmed, else processing resumes at the last
--   line/col (where the '{' was found)
--
-- Debugging:
-- =========
--  All development work should be performed in eaerr.exw, which is a copy
--  of eaerr.e with some additional code to load a test file and invoke
--  preprocessErr(). Set this to 1 only in eaerr.exw:

constant DEBUG=0

--sequence linelengths  -- comment this out in eaerr.e.
--function ExpLength(sequence l) return length(l) end function  --""

sequence oneinline

sequence newstring
         newstring=repeat('\"',80)  -- newstring[1] is always a double quote;
integer newstringlength         -- the remainder is left full of garbage.

--with trace
procedure appendToNewString(object chs)
-- bolt additional character or escape pair onto the output.
--if equal(chs,92) then trace(1) end if
    if newstringlength>=length(newstring)-2 then
        newstring&=repeat(0,80)
    end if
    if integer(chs) then
        newstring[newstringlength]=chs
        newstringlength+=1
    else
        newstring[newstringlength..newstringlength+length(chs)-1]=chs
        newstringlength+=length(chs)
    end if
end procedure

integer inch, newinch       -- column (byte) numbers

integer fromNextNonSpace
        fromNextNonSpace=0

integer ch
function getQuoted(integer expected)
-- confirm it is eg 32' ', not say 32'X' (or even 32774569 etc)...
    newinch+=1
    ch=oneinline[newinch]
    if ch!='\'' then return 0 end if
    newinch+=1
    ch=oneinline[newinch]
    if ch!=expected then return 0 end if
    if ch='\\' then
        appendToNewString("\\\\")
--14/8/15:
    elsif ch='\"' then
        appendToNewString("\\\"")
    else
        appendToNewString(ch)
    end if
    newinch+=1
    ch=oneinline[newinch]
    if ch!='\'' then return 0 end if
    return 1
end function

function isString()
--
-- Scan from inln/inch, all the way to the closing '}', building a replacement 
-- string. Return 0 if it is not really a string.
-- The flag fromNextNonSpace is normally zero, if 1 it means we (as in the
--  routine inString() itself) have requested the next line to continue 
--  scanning (pseudo-re-entrant code).
--
integer expected
    if not fromNextNonSpace then
        newstringlength=2
        newinch=inch+1  -- skip the '{' on entry
    end if
    fromNextNonSpace=0
    if newinch>length(oneinline) then return 0 end if
    ch=oneinline[newinch]
    while 1 do
        --
        -- Every char in a string should be one of:
        --  9, (tab)  10, (newline)  13, (return) or
        --  32' ', thru 126'~', (in literal ascii terms, that is).
        --  127..255 (which Eu does not quote)
        -- The trailing comma might be a '}'.
        --      
        if ch<='0' or ch>'9' then return 0 end if
        if ch='1' then  -- special handling for linefeed, cr (and 'd' upwards)
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch='0' then
                newinch+=1
                if newinch>length(oneinline) then return 0 end if
                ch=oneinline[newinch]
                if ch = ',' then
                    appendToNewString("\\n")
                elsif ch='}' then
                    appendToNewString("\\n")
                    exit    -- success!
                else    -- 'd' thru 'm'
                    if ch<'0' or ch>'9' then return 0 end if
                    expected=100+ch-'0'
                    if not getQuoted(100+ch-'0') then return 0 end if
                end if
            elsif ch='3' then   -- cr ("13," or die)
                newinch+=1
                if newinch>length(oneinline) then return 0 end if
                ch=oneinline[newinch]
                if ch = ',' then
                    appendToNewString("\\r")
                elsif ch = '}' then
                    appendToNewString("\\r")
                    exit    -- success!
                else
                    return 0
                end if
            else
                if ch<'0' or ch>'9' then return 0 end if
                -- 110..126:
                if ch='1' then -- 'n' thru 'w'
                    newinch+=1
                    if newinch>length(oneinline) then return 0 end if
                    ch=oneinline[newinch]
                    if ch<'0' or ch>'9' then return 0 end if
                    if not getQuoted(110+ch-'0') then return 0 end if
                elsif ch='2' then -- 'x' thru '~', 127..129
                    newinch+=1
                    if newinch>length(oneinline) then return 0 end if
                    ch=oneinline[newinch]
                    if ch<'0' or ch>'9' then return 0 end if
                    if ch<='6' then
                        if not getQuoted(120+ch-'0') then return 0 end if
                    else
                        newinch+=1
                        if newinch>length(oneinline) then return 0 end if
                        ch=oneinline[newinch]
                        if ch=',' then
                            appendToNewString(120+ch-'0')
                        elsif ch = '}' then
                            appendToNewString(120+ch-'0')
                            exit    -- success!
                        else
                            return 0
                        end if
                    end if
                else    -- 130..199
                    expected=100+(ch-'0')*10
                    newinch+=1
                    if newinch>length(oneinline) then return 0 end if
                    ch=oneinline[newinch]
                    if ch<'0' or ch>'9' then return 0 end if
                    expected+=ch-'0'
                    newinch+=1
                    if newinch>length(oneinline) then return 0 end if
                    ch=oneinline[newinch]
                    if ch=',' then
                        appendToNewString(expected)
                    elsif ch = '}' then
                        appendToNewString(expected)
                        exit    -- success!
                    else
                        return 0
                    end if
                end if
            end if              
        elsif ch='3' then   -- special handling for 30,31 (and space..singlequote)
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch<'2' or ch>'9' then return 0 end if
            if not getQuoted(30+ch-'0') then return 0 end if
        elsif ch='9' then   -- special handling for tab (and 'Z'..'c')
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch=',' then
                appendToNewString("\\t")
            elsif ch='}' then
                appendToNewString("\\t")
                exit        -- success!
            else
                if ch<'0' or ch>'9' then return 0 end if
                if not getQuoted(90+ch-'0') then return 0 end if
            end if
        elsif ch='2' then   -- 200 thru 255
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch<'0' or ch>'5' then return 0 end if
            expected=200+(ch-'0')*10
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            if ch = '5' then    -- 250..255
                ch=oneinline[newinch]
                if ch<'0' or ch>'5' then return 0 end if
            else
                ch=oneinline[newinch]
                if ch<'0' or ch>'9' then return 0 end if
            end if
            expected+=ch-'0'
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch=',' then
                appendToNewString(expected)
            elsif ch = '}' then
                appendToNewString(expected)
                exit    -- success!
            else
                return 0
            end if
        else    -- 40..89 ('(' thru 'Y')
            expected=(ch-'0')*10
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch<'0' or ch>'9' then return 0 end if
            if not getQuoted(expected+ch-'0') then return 0 end if
        end if
        if ch!=',' then
            if ch='}' then exit end if  -- success! (this /is/ needed!)
            newinch+=1
            if newinch>length(oneinline) then return 0 end if
            ch=oneinline[newinch]
            if ch='}' then exit end if  -- success!
            if ch!=',' then return 0 end if
        end if
        if newinch = length(oneinline) then
            fromNextNonSpace=1
            return 1
        end if
        newinch+=1
        if newinch>length(oneinline) then return 0 end if
        ch=oneinline[newinch]
--DEV might need this:
--      if ch='}' then exit end if  -- success!
--if ch='}' then ?9/0 end if

        if ch='.' and newinch=length(oneinline)-2 and equal(oneinline[newinch..length(oneinline)],"...") then   
            newinch-=1
            exit
        end if
    end while
    appendToNewString('\"')
    return 1
end function

integer templinestall
    templinestall=0


global function preprocessErr(sequence file)
-- convert eg {100'd',111'o',116't'} to "dot",
-- return updated text, and set global linelengths.
integer itReallyIsAString
integer newinln
sequence oneoutline
integer inln, outln -- line numbers
integer outch   -- column (byte) number
integer linelength
integer k, k2
integer ch
integer inquote

    inln=1
    outln=1
    while inln<=length(file) do
        oneinline=file[inln]
        oneoutline=oneinline
        inch=1
        outch=0
        inquote=0
        while inch<length(oneinline) do
            ch = oneinline[inch]
            if ch='{' and not inquote then
                newinln=inln
                while 1 do
                    itReallyIsAString=isString()
                    if itReallyIsAString=0 then exit end if
                    if not fromNextNonSpace then exit end if
                    newinln+=1
                    if newinln>length(file) then exit end if
                    oneinline=file[newinln]
                    newinch=1
                    while newinch<=length(oneinline)
                      and find(oneinline[newinch]," \t") do
                        newinch+=1
                    end while
                end while
                if itReallyIsAString then
                    oneoutline=oneoutline[1..outch]&
                               newstring[1..newstringlength-1]&
                               oneinline[newinch+1..length(oneinline)]
                    inch=newinch
-- 16/5/2012:
--                  outch+=newstringlength  --DEV not fully tested
                    outch+=newstringlength-2
                    inln=newinln
                else
                    oneinline=file[inln]
                end if
            elsif ch='\"' then
                inquote = 1-inquote
            end if
            inch+=1
            outch+=1
        end while
        if DEBUG then
            printf(1,"#%s\n",{oneoutline})
            templinestall+=1
            if templinestall=20 then
                puts(1,"Next page?")
                if getc(0) then end if
                puts(1,'\n')
                templinestall=0
            end if
        end if
        -- split very long text lines at "\n" where possible.
        if length(oneoutline)>charsWide then
            k = match(" = \"",oneoutline)
            if k and k<floor(charsWide/2) then
                while length(oneoutline)>charsWide and outln<inln do
                    k2 = charsWide-5
                    for i=k2 to k+3 by -1 do
                        if oneoutline[i]='\\' and oneoutline[i+1]='n' then
                            k2 = i+1
                            exit
                        elsif i=k+3 then
                            -- in that case, spilt at nearest space where possible
                            for j=k2 to k+3 by -1 do
                                if oneoutline[j]=' ' then
                                    k2 = j+1
                                    exit
                                end if
                            end for
                            exit
                        end if
                    end for
                    file[outln]=oneoutline[1..k2]&"\"&"
                    outln+=1
                    oneoutline = repeat(' ',k+2)&'\"'&oneoutline[k2+1..length(oneoutline)]
                end while
            end if
        end if
        file[outln]=oneoutline
        outln+=1
        inln+=1
    end while
    --
    -- lastly, build a new linelengths table for edita
    --
    linelengths=repeat(0,80)
    for i=1 to outln-1 do
        linelength=ExpLength8(file[i])
        if linelength>=length(linelengths) then
            linelengths&=repeat(0,linelength-length(linelengths)+1)
        end if
--      linelengths[linelength+1]+=1
        linelengths[linelength+1] = linelengths[linelength+1] + 1
    end for
    return file[1..outln-1]
end function

