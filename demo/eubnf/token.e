--
-- tokeniser routines.
--
global object line

global constant
--      TAB       =1,  -- Now treated as SPACE
    EOL =2, -- End of line
    SPACE =3, -- Spaces & tabs
    SYMBOL=4, -- General symbols !&*+,./<>=?
    DQUOTE=5, -- Double quotation mark
    HEXDEC=6, -- Hexadecimal (#) mark
    SQUOTE=7, -- Single quotation mark
    BRACES=8, -- ()[]{}
    MINUSS=9, -- minus sign
    DIGIT =10, -- 0..9
    DIGIBAD=23, -- partials, eg "-."; "3.0e"
    FLOAT=24, -- float, eg 1.0 or 1e4
    LETTER=11, -- A..Z,_,a..z
    LETEND=25, -- A special form of SYMBOL, (':' in fact), see below
    COMMENT=12,-- comment to end of line
    ILLEGAL=13, -- illegal character
--      IFW=14, -- if,for,while
--      EIFW=15, -- end ""
--      FPT=16, -- func,proc,type
--      EFPT=17, -- end ""
--      EE=18, -- else, elsif
--      AO=19, -- and,or
--      WAO=20, -- <WAS> and,or
--      DT=21, -- do,then
    AISO=22 -- atom, integer, sequence ,object

-- Quick explanation of LETEND:
--  include fred.ew as f
--      f:a=1
--      "f:" is returned as a single token to match the namespace table.

global constant tokdesc={
    "TAB",--      =1,  -- Now treated as SPACE
    "EOL",-- =2, -- End of line
    "SPACE",-- =3, -- Spaces & tabs
    "SYMBOL",--=4, -- General symbols !&*+,./<>=?
    "DQUOTE",--=5, -- Double quotation mark
    "HEXDEC",--=6, -- Hexadecimal (#) mark
    "SQUOTE",--=7, -- Single quotation mark
    "BRACES",--=8, -- ()[]{}
    "MINUSS",--=9, -- minus sign
    "DIGIT",-- =10, -- 0..9
    "LETTER",--=11, -- A..Z,_,a..z
    "COMMENT",--=12,-- comment to end of line
    "ILLEGAL",--=13, -- illegal character
    "IFW",--=14, -- if,for,while
    "EIFW",--=15, -- end ""
    "FPT",--=16, -- func,proc,type
    "EFPT",--=17, -- end ""
    "EE",--=18, -- else, elsif
    "AO",--=19, -- and,or
    "WAO",--=20, -- <WAS> and,or
    "DT",--=21, -- do,then
    "AISO",--=22 -- atom, integer, sequence ,object     
    "DIGIBAD" --=23, -- partials, eg "-."; "3.0e"
}

global constant True=1, False=0

global sequence charset
    charset=repeat(ILLEGAL,256)
    charset['\n']=EOL
    charset['\r']=EOL
    charset['\t']=SPACE
    charset[' ']=SPACE
    charset['!']=SYMBOL
    charset['\"']=DQUOTE
    charset['#']=HEXDEC
--?charset[36..40]
--  charset['$']=SYMBOL -- no help
--?charset[36..40]
--?'$'
    charset['&']=SYMBOL
    charset['\'']=SQUOTE
    charset['('..')']=BRACES
    charset['*'..'/']=SYMBOL
    charset['-']=MINUSS
    charset['0'..'9']=DIGIT
--DEV I relied on this in eubnf load; now I restore it later...
--      charset[':']=LETTER -- namespaces
    charset[':']=SYMBOL -- namespaces
--      charset[':']=LETEND -- namespaces
    charset['<'..'>']=SYMBOL
    charset['=']=SYMBOL
    charset['?']=SYMBOL
    charset['A'..'Z']=LETTER
    charset['_']=LETTER
    charset['[']=BRACES
--charset['\\']=LETTER
    charset[']']=BRACES
    charset['a'..'z']=LETTER
    charset['{']=BRACES
    charset['}']=BRACES
--?{charset[36],'$',ILLEGAL,SYMBOL}


global sequence token token=repeat(0,10) -- a token from that file
integer tokcol
global integer toklen, toktype
global integer c -- next character from that file
global integer col -- column it was in

without trace
procedure nextch()
--
-- extend the token by one character and read another from the input file
--
    toklen+=1
    token[toklen]=c
--      c=getc(fn)
    if col>length(line) then
        c=-1
    else
        c=line[col]
        col+=1
        if toklen=length(token) then -- auto-extend length of token
            token&=repeat(0,10)
        end if
    end if
end procedure

global procedure error(sequence etxt)
    puts(1,etxt&'\n')
    if getc(0) then end if
end procedure

with trace
function illcheck()
--
-- report unrecognised characters and badly formed numbers; before returning [True].
--
--DEV and MINUSS?
    if find(toktype,{ILLEGAL,DIGIBAD}) then
        if toklen>1 or (token[1]>' ' and token[1]<='}') then
trace(1)
            error(sprintf("Illegal token %s found.",{token[1..toklen]}))
        else
            error(sprintf("Illegal character (#%x) found.",token[1]))
        end if
    end if
pp({toktype,token[1..toklen]})
    return True
end function


--without trace
integer ellipsesym      -- flag for eg "1.." being over-parsed as a number.
        ellipsesym=0    -- (obviously it is really "1" and the symbol "..")

global function gettoken()
--
-- Returns settings in global variables token, toklen, toktype, tokcol, and
-- True when a token has been found, False at end of file.
--
-- toktype signifies (using BNF rules []=optional, {}=0 or many, |=or):
--
--              SPACE {'\t'|' '}
--              EOL {'\n'|'\r'}
--              SYMBOL '!'|'&'|'*'|'+'|','|'.'|'/'|'<'|'>'|'='|                         eg "="
--                              '-='|'+='|'&='|'*='|'/='|'!='|'<='|'>='                         or "+="
--              SQUOTE/DQUOTE \"<string>\"|\'<string>\'
--              HEXDEC [-]'#'{'0'..'9'|'A'..'F'}                                                        eg "#00", "-#E9C6"
--              BRACES '['|']'|'{'|'}'|'('|')'                                                          one at a time
--              DIGIT [-]{'0'..'9'}['.'{'0'..'9'}]['e'['-']{'0'..'9'}]          eg "1", "-2.3e-4"
--              LETTER 'A'..'Z'|'_'|'a'..'z'{'A'..'Z'|'_'|'a'..'z'|'0'..'9'} eg "i", "plan9"
--                (note: reserved words, keywords, etc returned as plaintext)
--              COMMENT '--'<to eol>
--              
-- Note: MINUSS is only used temporarily,
--               types IFW,FPT,EIFW,EFPT,EE,AO,DT,AISO set later in stacktoken()
--
integer pc -- previous character
integer sflag -- skipping escape flag

    if ellipsesym then
        ellipsesym=0
        nextch()
        toktype=SYMBOL
        toklen=2
        token[1..2]=".."
        return True
    end if
    if c=-1 then return False end if -- eof

    toktype=charset[c]
    toklen=0
    tokcol=col

    if toktype=DQUOTE or toktype=SQUOTE then -- special deal for strings
        pc=c sflag=0
        while 1 do
            nextch()
            if c=-1 or charset[c]=EOL then
                error("Missing end quote.")
                return True
            end if
            if sflag=0 then
                if c='\\' then --skip specials (namely "\"" and '\'')
                    sflag=1
                else
                    if charset[c]=toktype then -- closing DQUOTE or SQUOTE
                        nextch()
                        return True
                    end if
                end if
            else
                sflag=0
            end if
        end while
    end if

    pc=c
    nextch()

--DEV: may as well return an error if MINUSS and c=-1 ? - see dev in illcheck() tho
    if c!=-1 and toktype=MINUSS then -- possibly a number or a comment
--DEV now why did I take this out???
--PL 30/06 because in eg "1-.4" it is still a binary operator.
-- eubnf seems to handle unary minus anyway.
--              if charset[c]=DIGIT then
--                      toktype=DIGIT
--              elsif
--PL3006                if c='.' then -- "-.4" is a number
--PL3006                        nextch()
--PL3006                        toktype=DIGIBAD -- "-." is not yet a valid number
--DEV and this...
--              elsif charset[c]=HEXDEC then
--                      nextch()
--                      toktype=HEXDEC
--PL3006                elsif charset[c]=MINUSS then
        if charset[c]=MINUSS then
            -- special deal for comments: to end of line
            toktype=COMMENT
            while 1 do
                nextch()
                if c=-1 or charset[c]=EOL then exit end if
            end while
            return True
        else
            toktype=SYMBOL -- '-' or '-=' then
        end if
    end if

    if toktype=HEXDEC then -- process hex number
        -- reject "#" or "-#" alone or "#z" as illegal
        toktype=DIGIBAD
        while 1 do
            if c=-1 then exit end if
            if charset[c]!=DIGIT then
                if (c<'A' or c>'F') then exit end if
            end if
            toktype=HEXDEC
            nextch()
        end while
        if toktype=DIGIBAD and c='i' then
--trace(1)
            -- match #ilASM (etc)
            while 1 do
                if c=-1 then exit end if
                if charset[c]!=LETTER then exit end if
                toktype = LETTER
                nextch()
            end while
        end if
        return illcheck()
    end if

    if c!=-1 and pc='.' and charset[c]=DIGIT then -- ".4" is a number
--              toktype=DIGIT
        toktype=FLOAT
    end if

--      if find(toktype,{DIGIT,DIGIBAD}) then -- integer or float (partial)
    if find(toktype,{DIGIT,DIGIBAD,FLOAT}) then -- integer or float (partial)
        -- number, eg 1, -1.0, 1.0e1, 1e-1, .4, -.4e-10, 2e25, etc.
        -- reject things like "3.e-" and "-." as illegal
        while 1 do
            if c=-1 then exit end if
            if charset[c]!=DIGIT then exit end if
            toktype=DIGIT
            nextch()
        end while
        if c='.' then -- float
            nextch()
            if c='.' then   --no, ".." symbol
                toklen-=1
                ellipsesym=1
                return illcheck()
            end if
            toktype=FLOAT
            while 1 do
                if c=-1 then exit end if
                if charset[c]!=DIGIT then exit end if
--                              toktype=DIGIT
                nextch()
            end while
        end if
        if c='e' and find(toktype,{DIGIT,FLOAT}) then -- float, and the pre-"e" part is OK
            nextch()
            if c!=-1 and charset[c]=MINUSS then -- exponent may be negative
                nextch()
            end if
            toktype=DIGIBAD
            while 1 do
                if c=-1 then exit end if
                if charset[c]!=DIGIT then exit end if
--                              toktype=DIGIT
                toktype=FLOAT
                nextch()
            end while
        end if
        return illcheck()
    end if

    -- < and > at start of line should be left in place (comparison differences)
    -- hence "<=" & ">=" at start of line return two tokens, not one.
    if toktype=SYMBOL then
        if tokcol=1 and find(pc,"<>") then
            error("Existing errors or unedited file difference markers detected.")
            return True
        end if
        -- can be one of -= += &= *= /= != <= >=
        if (c='=' and find(pc,"-+&*/!<>")) 
        or (c='.' and pc='.') then
            nextch()
        end if
        return True -- NB any other 'x'= treated as two tokens
    end if

    if toktype=BRACES then -- always handle individually
        return True
    end if

    while 1 do
        if c=-1 then exit end if
        if toktype=LETTER and charset[c]=DIGIT then -- digits allowed after char 1 in idents.
        else
            if toktype!=charset[c] then exit end if
        end if
        nextch()
    end while

    if toktype=EOL then
        col=1
    end if

    if toktype=LETTER then
        -- the namespace fudgeeroo...
        if charset[':']=LETEND then
            while c!=-1 and find(charset[c],{SPACE}) do
                nextch()
                toklen-=1
            end while
            if c=':' then
                nextch()
            end if
        end if
        charset['\\']=ILLEGAL -- reset following include processing
    end if
    return illcheck()
end function
with trace
