--
-- pprntfN.e
-- =========
--
-- The Phix sprintf() function.
-- This file is automatically included by the compiler if (and only if) needed.
-- Compared to the C version, this is just over twice as slow, which is rather
-- surprising (I expected it would be around 8 to 10 times slower). I suspect
-- the biggest overhead is allocating space for the result (string), which is
-- pretty much going to be the same for the C and Phix routines. This may be
-- converted to asm at a later date, but not until it has been completely
-- bug- and enhancement- free for at least six months.
--
--
-- **************************************
-- **** WARNING: FRAGILE CODE AHEAD! ****
-- **************************************
--  Test changes exhaustively before rebuilding p.exe!
--  In particular, take care with type string; subtle changes (such as a
--  missing floor()) can make the back-end expand a string to a sequence
--  (which is something prepend, as opposed to append, /always/ does)
--  which causes a fatal error, eg see 'elsif fmt[i]='s' then'.

--!/**/without debug -- remove to debug (just keeps ex.err clutter-free)
--!/**/with debug
--  NB the "without debug" in both pdiag.e and ppp.e overshadow the one
--      here; use "with debug" and/or "-nodiag" to get a listing.

without trace -- ditto, plus important this be off when running trace(3)

-- Bugfix history:
--  14/01/06. Did not handle negative numbers ("sign" added).
--  09/03/06. printed 2700000000 as "27".
--  22/05/06. precision errors on big numbers.
--  19/08/07. nzdigitprinted flag added. [ummm?]
--  19/06/08. bug with trailing 0s in 'g'.
--  20/03/09. moved exp to end for K_noclr reasons.
--  05/03/12. made thread-safe (no file-level vars) [DEV multiple returns rqd?]

include builtins\VM\pUnassigned.e   -- :%pRTErn (DEV/temp)
include builtins\VM\pPower.e

--/* Not required for Phix (string is builtin):
type string(object o) return sequence(o) end type
--*/

function round(string result, atom f, integer exponent, integer charflag, integer digit)--, integer minfieldwidth)
--
-- Apply rounding to partially printed float, if required
--
integer tmp
integer dot, dotm1
--?result   --DOH, infinite loop! (use puts(1,<string>) instead!)
integer one = iff(result[1]='-'?2:1)
    if exponent>=1 then
        f /= power(10,exponent)
    end if
    if f>5 or (f=5 and remainder(digit,2)=1) then
        for i=length(result) to one by -1 do
            dot = result[i]
            if dot='9' then
                result[i] = '0'
            elsif dot!='.' then
--              result[i] += 1      --DEV 26/9/9: try dot += 1 result[i] = dot... (better type info)
                dot += 1
                result[i] = dot
                exit
            end if
            if i=one then
--DEV: (oldschool, from when prepend string did not work) [prepend always yields T_Seq now anyways] [DEV: lies, 28/3/2014]
--              result = prepend(result,'1')
                if one=1 then
                    result = "1"&result
                else
                    result = "-1"&result[2..$]
                end if
                if charflag!='f' then
                    dot = find('.',result)
                    if dot then
                        dotm1 = dot-1
                        result[dot] = result[dotm1]
--                      result[dotm1] = '.'
                        result[dotm1] = '!'
--DEV gives bounds error...
--DEV I suppose we should check it is a '0' as well.
--                      result = result[1..length(result)-1]
--                      exponent += 1
                    end if
                end if
            end if
        end for
    end if
--  if charflag='g' then
--  if charflag='g' and find('.',result) then   -- find '.' added 9/3/6
    if charflag='g' 
    and (find('.',result) or find('!',result)) then
        tmp = length(result)
--14/1/15:
--      while tmp>1 and tmp>minfieldwidth do
        while tmp>1 do
            dot = result[tmp]
-- 20/10/15: sprintf("%g",1e-14) was yielding "1e-15"!! It now keeps the the trailing '!' to adjust exponent.
--          if dot='.' 
--          or dot='!' then
            if dot='!' then exit end if
            if dot='.' then
                tmp -= 1
                exit
            end if
            if dot!='0' then exit end if
            tmp -= 1
        end while
        result = result[1..tmp]
    end if
    return result
end function

integer init2
        init2 = 0

atom inf,nan

-- do we still need these?? (yes)
function Nan() -- thread-safe alternative to "nan"
string res
    res = repeat('n',3)
    res[2] = 'a'
    return res
end function

function Inf() -- thread-safe alternative to "inf"
string res
    res = repeat('i',3)
    res[2] = 'n'
    res[3] = 'f'
    return res
end function

function sprintf2(atom pf, integer charflag, integer showplus, integer minfieldwidth, integer precision)
integer dotdone, nzdigitprinted
string reve
integer revelen
integer expadj
integer capE    -- 'e' or 'E'
integer ewk
integer tmp
integer digit
atom f, fwk, epwr
string result
integer exponent, k

--?result   --DOH, infinite loop! (use puts(1,<string>) instead!)
    f = pf
--  result = ""
    result = repeat(' ',0)
    dotdone = 0
    nzdigitprinted = 0
    if f=nan then
--DEV not thread safe
--      return "nan"
        return Nan()
    end if
    if f<0 then
--      result = "-"
        result = repeat('-',1)
        f = -f
    elsif showplus then
--      result = "+"
        result = repeat('+',1)
    end if
    exponent = 0
    if f=nan then
--      return result&"nan"
        return result&Nan()
    end if
    if f=inf then
--      return result&"inf"
        return result&Inf()
    end if
    if f>=10 then
        fwk = f
        while fwk>=10 do
            exponent += 1
            fwk /= 10
        end while
    else
        while f<1 and f!=0 do
            exponent -= 1
            f *= 10
        end while
    end if
    capE = 'e'
    if charflag<'a' then
        charflag += 32
        capE = 'E'
    end if
--  if charflag='e' or (charflag='g' and (exponent>9 or exponent<-4)) then
--1/11/15:
--  if charflag='e' or (charflag='g' and (exponent>precision or exponent<-4)) then
    if charflag='e' or (charflag='g' and (exponent>=precision or exponent<-4)) then
        ewk = exponent
        if exponent>0 then
--DEV problems on 64bit: (opPow needs improvement!)
--          epwr = power(10,exponent)
epwr = 10
for i=2 to exponent do
    epwr *= 10
end for
            fwk = 0--epwr
            digit = 0
            while f>=fwk+epwr do
                fwk += epwr
                digit += 1
            end while
        else
            digit = floor(f)
        end if
        result &= digit+'0'
        expadj = 1
        if precision>0 then
            result &= '.'
            dotdone = 1
--1/11/15
--          for i=1 to precision do
            for i=1 to precision-(charflag='g') do
                if ewk>0 then
                    f -= fwk
                    ewk -= 1
                    epwr = power(10,ewk)
                    fwk = 0--epwr
                    digit = 0
                    while f>=fwk+epwr do
                        fwk += epwr
                        digit += 1
                    end while
                else
                    f = (f-digit)*10
                    digit = floor(f)
                end if
--12/7/16:
if digit=10 then exit end if
                result &= digit+'0'
                expadj += 1
            end for
        else
            if ewk>0 then
                f -= fwk
            else
                f = (f-digit)*10
            end if
        end if
        exponent -= expadj
        result = round(result,f,exponent,charflag,digit)--,minfieldwidth)
        k = find('!',result)
        if k then
            if k=length(result) then
                result = result[1..-2]
            else
                result[k] = '.'
            end if
            exponent += 1
        end if
        exponent += expadj
        result &= capE
        if exponent<0 then
            result &= '-'
            exponent = 0-exponent
        else
            result &= '+'
        end if
        if exponent=0 then
            reve = "0"
        else
--          reve = ""
            reve = repeat(' ',0)
            while exponent do
                reve = append(reve,floor(remainder(exponent,10)+'0'))
                exponent = floor(exponent/10)
            end while
            revelen = length(reve)
            for j=1 to revelen do
                if j>=revelen then exit end if
                tmp = reve[j]
                reve[j] = reve[revelen]
                reve[revelen] = tmp
                revelen -= 1
            end for
        end if
        result &= reve
    else
        digit = 0
        if exponent<-1 then
--DEV not thread safe
--          result &= "0."
            result &= '0'
            if charflag!='g' or f!=0 then
                result &= '.'
                dotdone = 1
                while exponent<-1
                  and (charflag!='g' or f!=0) do
                    exponent += 1
                    if precision then
                        result &= '0'
                        if minfieldwidth>0 then
                            precision -= 1
                        end if
                    else
                        f /= 10
                    end if
                end while
            end if
        end if

        while 1 do
            if exponent=-1 then
                if precision>0 then
                    if not dotdone then
                        if charflag='g' then
                            if f=0 then exit end if
                        end if
                        if find(result,{"","-","+"}) then
                            result &= '0'
                        end if
                        result &= '.'
                    end if
                    for j=1 to precision do
                        digit = floor(f)
                        result &= digit+'0'
                        f = (f-digit)*10
                        if charflag='g' then
                            if f=0 then exit end if
                        end if
                    end for
                end if
                exit
            end if
            if exponent>=1 then
                epwr = power(10,exponent)
                fwk = 0--epwr
                digit = 0
                while f>=fwk+epwr do
                    fwk += epwr
                    digit += 1
                end while
                f -= fwk    --epwr
            else
                digit = floor(f)
                f = (f-digit)*10
            end if
            result &= digit+'0'
            if digit then
                nzdigitprinted = 1
            end if
            exponent -= 1
            if charflag='g' then
                if length(result)>=minfieldwidth and f=0 then exit end if
--DEV: try printf(1,"%6.2f\n%6.2g\n",96.5)
-- (same results as RDS Eu, but that does not necessarily make it right!)
-- Possible fix is to remove this line...(19/8 put the new flag test round it instead)
                if nzdigitprinted then
                    precision -= 1
                end if
            end if
        end while
        if length(result)=0 then
--          result = "0"
            result = repeat('0',1)
        end if
        result = round(result,f,exponent,charflag,digit)--,minfieldwidth)
        k = find('!',result)
        if k then
            if k=length(result) then
                result = result[1..-2]
            else
                result[k] = '.'
            end if
            exponent += 1
        end if
        if exponent>-1 then
            while exponent>-1 do
                result &= '0'
                exponent -= 1
            end while
        end if
    end if
    return result
end function

procedure badfmt()
--/**/  #ilASM{ mov al,69                       -- Phix
--!/**/         xor edi,edi     -- ep1 unused   -- Phix
--!/**/         xor esi,esi     -- ep2 unused   -- Phix
--DEV (this is :%pRTErn:, see also pfileioN.e for ebp/calledfrom/fatalN etc)
--      pop edx -- era
--      sub edx,1
--      jmp :!iDiag
--      int3
--/**/          call :%pRTErn } -- fatal error  -- Phix
--/**/                                  --/*    -- Phix
        puts(1,"error in format string\n")      -- RDS
        if getc(0) then end if                  -- RDS
        abort(1)                                -- RDS --*/
end procedure

function useFlatString(sequence args, integer nxt, sequence fmt, integer i)
-- permit printf(1,"%s","Hello") to work as {"Hello"} - but only if:
-- 1) this is the first % (nxt=1)
-- 2) there are no more %'s in the fmt, except for %%
-- 3) args is a flat string
object o
    if nxt!=1 then return 0 end if
    for j=i+1 to length(fmt) do
        if fmt[j]='%' then
            if j=length(fmt) or fmt[j+1]!='%' then return 0 end if
        end if
    end for
    for j=1 to length(args) do
        o = args[j]
        if not integer(o) then return 0 end if
        if o<1 or o>255 then return 0 end if
    end for
    return 1
end function

string hexchar
sequence bases

--without trace
function sprintf_(sequence fmt, object args)
integer i, fi, fidx
integer nxt
string result, r1
object o, oj
atom work
integer base, sign, r1len, hc
integer lowerHex
--?result   --DOH, infinite loop! (use puts(1,<string>) instead!)
integer zerofill
integer leftjustify
integer showplus
integer showcommas
integer minfieldwidth
--      minfieldwidth = 0
integer precision
--      precision = 0
integer tmp

    if not init2 then
-- [DEV] technically this isn't thread safe... (code shown commented out should be enough, once those routines work)
        -- (uncommented 25/11/16)
        enter_cs()
--DEV make INF a builtin (like PI), ditto NAN:
--      inf = 1e300*1e300
        #ilASM{ fld1
                fldz
                fdivp
            [32]
                lea edi,[inf]
            [64]
                lea rdi,[inf]
            []
                call :%pStoreFlt }

        -- Erm, this one is a bit bizarre...
        -- On the one hand it seems RDS Eu does not support nan properly, but then it somehow does...
        -- If you try testing for nan, it seems to go all pear-shaped, but avoiding the tests
        --  seems to make it happy again, and yet print "nan" and "inf" like a good little boy...
        -- Of course, you shouldn't be using this code on RDS Eu anyway.
        --
--/**/  nan = -(inf/inf)        --/* Phix
        nan = 3.245673689e243   --   RDS --*/

        bases = {10,16,8,2}
        hexchar = "0123456789ABCDEFabcdef"
        init2 = 1
        leave_cs()
    end if
    nxt = 1
--  result = ""
    result = repeat(' ',0)
    i = 1
    while i<=length(fmt) do
        fi = fmt[i]
        if fi='%' then
            i += 1
            if i>length(fmt) then badfmt() end if
            fi = fmt[i]
            if fi='%' then
                result &= '%'
            else
                zerofill = 0
                leftjustify = 0
                showplus = 0
                showcommas = 0
                if fi='0' then
                    zerofill = 1
                    i += 1
                elsif fi='-' then
                    leftjustify = 1
                    i += 1
                elsif fi='+' then
                    showplus = 1
                    i += 1
                elsif fi=',' then
                    showcommas = 3
                    i+=1
                end if
                if i>length(fmt) then badfmt() end if
                minfieldwidth = 0
                while 1 do
                    fi = fmt[i]
                    if fi<'0' or fi>'9' then exit end if
                    minfieldwidth = minfieldwidth*10+fi-'0'
                    i += 1
                    if i>length(fmt) then badfmt() end if
                end while
                precision = -1
                if fi='.' then
                    i += 1
                    if i>length(fmt) then badfmt() end if
                    precision = 0
                    while 1 do
                        fi = fmt[i]
                        if fi<'0' or fi>'9' then exit end if
                        precision = precision*10+fi-'0'
                        i += 1
                        if i>length(fmt) then badfmt() end if
                    end while
                end if

                lowerHex = 0
                -- 23/2/10 'b' added
                fidx = find(fi,"dxobscefgEXG")
                if fidx=11 then -- 'X'
                    --
                    -- Yup, I know it's a wee bit confusing, but for compatibility
                    --  reasons, %x is upper case hex and %X is lower case hex!
                    -- Assuming any objections from the backwards compatibility crowd 
                    --  are drowned out by many more from the logical behaviour crowd,
                    --  set lowerHex to 1/0 rather than the 0/1 it is now.
                    --
                    lowerHex = 1
                    fidx = 2
                end if

                if fidx=0
                or (showcommas and find(fi,"df")=0) then
                    badfmt()
                end if
                if not atom(args) and nxt>length(args) then
--/**/              #ilASM{
--!/**/                 [32]
--/**/                      mov al,70                           -- Phix
--!/**/                     xor edi,edi         -- ep1 unused   -- Phix
--!/**/                     xor esi,esi         -- ep2 unused   -- Phix
--!/**/                 [64]
--/**/                      call :%pRTErn }     -- fatal error  -- Phix
--/**/                                                  --/*    -- Phix
                    puts(1,"insufficient values for sprintf\n") -- RDS
                    if getc(0) then end if                      -- RDS
                    abort(1)                                    -- RDS --*/
                end if
                if fidx<=4 then -- dxob
                    base = bases[fidx]  --{10,16,8,2}
                    o = args
                    work = 0
                    if atom(o) then
                        if o!=nan and o!=-nan and o!=inf then
                            work = floor(o)
                        end if
                    else
                        o = args[nxt]
                        if atom(o) then
                            if o!=nan and o!=-nan and o!=inf then
                                work = floor(o)
                            end if
                        else
                            o = 0
                        end if
                    end if
                    if work then
                        sign = 0
                        if work<0 then
                            sign = 1
                            if base=10 then
                                work = 0-work
                            else
--DEV 
--if machine_bits()=64 then
--                              work = and_bits(work,#7FFFFFFFFFFFFFFF)+#8000000000000000
--else
                                work = and_bits(work,#7FFFFFFF)+#80000000
--end if
                            end if
                        end if
--                      r1 = ""
                        r1 = repeat(' ',0)
                        while work do
                            -- NB: The result of prepend is always a sequence, 
                            --      for performance reasons. Hence use append 
                            --      to build it backwards, then reverse it.
                            hc = floor(remainder(work,base)+1)
--20/3/2013:
--                          if hc=0 then exit end if
                            if lowerHex and hc>10 then
                                hc += 6
                            end if
                            if showcommas and showcommas=length(r1) then
                                r1 = append(r1,',')
                                showcommas += 4
                            end if
                            r1 = append(r1,hexchar[hc])
                            -- this is ok, ^ , hexchar[] is equally valid for 
                            --  base 16/10/8/2 (just less chars get used).
                            work = floor(work/base)
                        end while
                        if sign then
if base=10 then
                            r1 = append(r1,'-')
elsif minfieldwidth>length(r1) then
                            r1 &= repeat(hexchar[base],minfieldwidth-length(r1))
end if
                        elsif showplus then
if base=10 then
                            r1 = append(r1,'+')
elsif minfieldwidth>length(r1) then
                            r1 &= repeat('0',minfieldwidth-length(r1))
end if
                        end if
                        r1len = length(r1)
                        -- as promised, reverse it:
                        for j=1 to r1len do     -- stops at mid-point[-1]
                            if j>=r1len then exit end if
                            tmp = r1[j]
                            r1[j] = r1[r1len]
                            r1[r1len] = tmp
                            r1len -= 1
                        end for
                    else
--DEV not thread safe:
                        if o=nan then
--                          r1 = "nan"
                            r1 = Nan()
                        elsif o=-nan then
--                          r1 = "-nan"
                            r1 = '-'&Nan()
                        elsif o=inf then
--                          r1 = "inf"
                            r1 = Inf()
                        else
--                          r1 = "0"
                            r1 = repeat('0',1)
                        end if
                    end if
                elsif fidx<=6 then  -- 's' or 'c'
                    if atom(args) then
                        o = args
--12/9/15:
--                  elsif useFlatString(args,nxt,fmt,i) then
                    elsif fidx!=6 and useFlatString(args,nxt,fmt,i) then -- (not %c (ie %s) and useFlat..)
                        o = args
                        args = {}
                    else
                        o = args[nxt]
                    end if
                    if atom(o) then
                        r1 = " "
                        r1[1] = and_bits(#FF,o) -- (nb: keeps r1 a string)
                    elsif fidx=6 then -- 'c'
--/**/                  #ilASM{ mov al,76                           -- Phix
--!/**/                         xor edi,edi         -- ep1 unused   -- Phix
--!/**/                         xor esi,esi         -- ep2 unused   -- Phix
--/**/                          call :%pRTErn }     -- fatal error  -- Phix
--/**/                                                      --/*    -- Phix
                        puts(1,"%c requires an atom value\n")       -- RDS
                        if getc(0) then end if                      -- RDS
                        abort(1)                                    -- RDS --*/
                    elsif not string(o) then
                        if precision!=-1 and precision<length(o) then
                            r1 = repeat(' ',precision)
                        else
                            r1 = repeat(' ',length(o))
                        end if
                        for j=1 to length(r1) do
                            oj = o[j]
                            if not integer(oj) then
--                          if not integer(oj) or oj<#07 or oj>#FF then
--/**/                          #ilASM{ mov al,65                               -- Phix
--!/**/                                 xor edi,edi         -- ep1 unused       -- Phix
--!/**/                                 xor esi,esi         -- ep2 unused       -- Phix
--/**/                                  call :%pRTErn }     -- fatal error      -- Phix
--/**/                                                                  --/*    -- Phix
                                puts(1,"sequence found in character string\n")  -- RDS
                                if getc(0) then end if                          -- RDS
                                abort(1)                                        -- RDS --*/
                            end if
--                          r1[j] = oj
                            r1[j] = and_bits(oj,#FF)
                        end for
                    else
                        if precision!=-1 and precision<length(o) then
                            r1 = o[1..precision]
                        else
                            r1 = o
                        end if
                    end if
                else    -- efg/EG
                    if precision=-1 then
                        precision = 6
--                  elsif precision>16 then
--                      precision = 16
                    else
                        if machine_bits()=32 then
                            if precision>16 then
--                          if precision>17 then
                                precision = 16
                            end if
                        else -- machine_bits()=64
                            if precision>20 then
                                precision = 20
                            end if
                        end if
                    end if
                    if atom(args) then
                        o = args
                    else
                        o = args[nxt]
                        if not atom(o) then
                            o = 0
                        end if
                    end if
                    r1 = sprintf2(o,fi,showplus,minfieldwidth,precision)
                    if showcommas then -- ('f' only)
                        showcommas = find('.',r1)
                        if showcommas=0 then showcommas = length(r1)+1 end if
                        while showcommas>4 do
                            showcommas -= 3
                            r1 = r1[1..showcommas-1]&','&r1[showcommas..length(r1)]
                        end while
                    end if
                end if
-- replaced 19/10/17:
--              minfieldwidth -= length(r1)
                minfieldwidth -= length(utf8_to_utf32(r1))
                if minfieldwidth>0 then
                    if zerofill then
                        r1 = repeat('0',minfieldwidth)&r1
                    elsif leftjustify then
                        r1 = r1&repeat(' ',minfieldwidth)
                    else
                        r1 = repeat(' ',minfieldwidth)&r1
                    end if
                end if
                result &= r1
                nxt += 1
            end if
        else
--puts(1,"?{result,fi}\n")
--?{result,fi}
            result &= fi
--?result
        end if
        i += 1
    end while
--DEV this should be a warning??? (Interpret mode only!)
--  if not atom(args) and nxt<=length(args) then
----/**/    #ilASM{ mov al,105                              -- Phix
----!/**/           xor edi,edi         -- ep1 unused       -- Phix
----!/**/           xor esi,esi         -- ep2 unused       -- Phix
----/**/            call :%pRTErn }     -- fatal error      -- Phix
----/**/                                            --/*    -- Phix
--      puts(1,"not enough format strings to print data\n") -- RDS
--      if getc(0) then end if                              -- RDS
--      abort(1)                                            -- RDS --*/
--  end if
    return result
end function

-- (This is a wrapper so that the internal routine can apply 
--  identical error handling for both sprintf and printf: we
--  typically want to point at the application source code
--  line, rather than confuse by landing developer in here.)
global function sprintf(sequence fmt, object args)
    return sprintf_(fmt,args)
end function

--DEV move this (once newEmit is done) [better yet put itin the optable]
-- note: printf is now defined in pfileioN.e
global procedure printf(integer fn, sequence fmt, object args={})
    puts(fn,sprintf_(fmt,args))
end procedure


