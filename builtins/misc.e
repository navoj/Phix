--
-- Phix compatible version of misc.e
--
-- Euphoria 2.4
-- Miscellaneous routines and constants

--/* Not required for Phix (defined by loadBuiltins):
-- platform() values:
global constant DOS32 = 1,  -- ex.exe
                WIN32 = 2,  -- exw.exe
                LINUX = 3,  -- exu
                FREEBSD = 3 -- exu
--*/

--/* Not required for Phix (defined as opInstance)
constant M_INSTANCE = 55
global
function instance()
-- WIN32: returns hInstance - handle to this instance of the program
-- DOS32: returns 0
      return machine_func(M_INSTANCE, 0)
end function
--*/

--/* Not required for Phix (defined as opSleep)
constant M_SLEEP = 64
global
procedure sleep(integer t)
-- go to sleep for t seconds
-- allowing (on WIN32 and Linux) other processes to run
      if t > 0 then
          machine_proc(M_SLEEP, t)
      end if
end procedure
--*/

--global function reverse(sequence s)
---- reverse the top-level elements of a sequence.
---- Thanks to Hawke' for helping to make this run faster.
--integer rlower, n, n2
--sequence t
--
--  n = length(s)
--  n2 = floor(n/2)+1
----!/**/  t = repeat(iff(string(s)?' ':0),n) --/*
----/**/ if string(s) then      --   -- Phix
----/**/     t = repeat(' ',n)
----/**/ else
----/**/     t = repeat(0,n)
----/**/ end if                 --/*
--  t = repeat(0, n)            --*/ -- RDS
--  rlower = 1
--  for rupper=n to n2 by -1 do
--      t[rupper] = s[rlower]
--      t[rlower] = s[rupper]
--      rlower += 1
--  end for
--  return t
--end function

--global function reverse_subset(sequence s, integer pFrom = 1, integer pTo = -1)
--global function reverse(sequence s, integer pFrom = 1, integer pTo = -1)
global function reverse(sequence src, sequence pFromTo = {1,-1})
--integer {pFrom,pTo} = pFromTo, len = length(src)
integer pFrom = pFromTo[1], pTo = pFromTo[2], len = length(src)
integer uppr, midpoint
sequence res

    if pFrom<0 then
        pFrom = len+1+pFrom
    end if
    if pTo<0 then
        pTo = len+1+pTo
    end if
    if len<2 or pFrom=pTo then
        return src
    end if

    midpoint = floor((pFrom+pTo-1)/2)
    res = src
    uppr = pTo
    for lowr=pFrom to midpoint do
        res[uppr] = src[lowr]
        res[lowr] = src[uppr]
        uppr -= 1
    end for
    return res
end function

--/* Not required for Phix (defined in psprint.e):
global
function sprint(object x)
-- Return the string representation of any Euphoria data object. 
-- This is the same as the output from print(1, x) or '?', but it's
-- returned as a string sequence rather than printed.
sequence s
                 
    if atom(x) then
        return sprintf("%.10g", x)
    else
        s = "{"
        for i = 1 to length(x) do
            s &= sprint(x[i])  
            if i < length(x) then
                s &= ','
            end if
        end for
        s &= "}"
        return s
    end if
end function
--*/

-- pretty print variables
integer pretty_end_col, pretty_chars, pretty_start_col, pretty_level,
    pretty_file, pretty_ascii, pretty_indent, pretty_ascii_min,
    pretty_ascii_max
sequence pretty_fp_format, pretty_int_format, pretty_line


procedure pretty_out(object text)
-- output text, keeping track of line length    
-- buffering lines speeds up Windows console    
    pretty_line &= text
    if equal(text, '\n') then
        puts(pretty_file, pretty_line)
        pretty_line = ""
    end if
    if atom(text) then
        pretty_chars += 1
    else
        pretty_chars += length(text)
    end if
end procedure

procedure cut_line(integer n)
-- check for time to do line break
    if pretty_chars+n>pretty_end_col then
        pretty_out('\n')
        pretty_chars = 0
    end if
end procedure

procedure indent()
-- indent the display of a sequence
    if pretty_chars>0 then
        pretty_out('\n')
        pretty_chars = 0
    end if
    pretty_out(repeat(' ', (pretty_start_col-1)+
                      pretty_level*pretty_indent))
end procedure

procedure rPrint(object a)
-- recursively print a Euphoria object  
sequence sbuff
integer multi_line, all_ascii
object ai

    if atom(a) then
        if integer(a) then
        -- By Al Getz 26/5 2004: replace the above line with this to print 32-bit ints
        --  if a-floor(a)=0 and -#FFFFFFFF<=a and a<=#FFFFFFFF then
            sbuff = sprintf(pretty_int_format, a)
            if pretty_ascii
            and a>=pretty_ascii_min
            and a<=pretty_ascii_max then
                if pretty_ascii>2 then
                    sbuff = '\'' & a & '\''
                else
                    sbuff &= '\'' & a & '\''
                end if
            end if
        else
            sbuff = sprintf(pretty_fp_format, a)
        end if
        pretty_out(sbuff)

    else
        -- sequence 
        cut_line(1)
        multi_line = 0
        all_ascii = pretty_ascii>1
        for i=1 to length(a) do
            ai = a[i]
            if sequence(ai) and length(ai)>0 then
                multi_line = 1
                all_ascii = 0
                exit
            end if
            if not integer(ai)
            or ai<pretty_ascii_min
            or ai>pretty_ascii_max then
                all_ascii = 0
            end if
        end for

        if all_ascii then
            pretty_out('\"')
        else
            pretty_out('{')
        end if
        pretty_level += 1
        for i=1 to length(a) do
            if multi_line then
                indent()
            end if
            if all_ascii then
                pretty_out(a[i])
            else
                rPrint(a[i])
            end if
            if i!=length(a) and not all_ascii then
                pretty_out(',')
                cut_line(6)
            end if
        end for
        pretty_level -= 1
        if multi_line then
            indent()
        end if
        if all_ascii then
            pretty_out('\"')
        else
            pretty_out('}')
        end if
    end if
end procedure


global procedure pretty_print(integer fn, object x, sequence options)
-- Print any Euphoria object x, to file fn, in a form that shows 
-- its structure.
--
-- argument 1: file number to write to
-- argument 2: the object to display
-- argument 3: is an (up to) 8-element options sequence:
--   Pass {} to select the defaults, or set options as below:
--   [1] display ASCII characters:
--       0: never
--       1: alongside any integers in printable ASCII range (default)
--       2: display as "string" when all integers of a sequence 
--          are in ASCII range
--       3: show strings, and quoted characters (only) for any integers 
--          in ASCII range
--   [2] amount to indent for each level of sequence nesting - default: 2
--   [3] column we are starting at - default: 1
--   [4] approximate column to wrap at - default: 78
--   [5] format to use for integers - default: "%d"
--   [6] format to use for floating-point numbers - default: "%.10g"
--   [7] minimum value for printable ASCII - default 32
--   [8] maximum value for printable ASCII - default 127
-- 
-- If the length is less than 8, unspecified options at 
-- the end of the sequence will keep the default values.    
-- e.g. {0, 5} will choose "never display ASCII", 
-- plus 5-character indentation, with defaults for everything else  
integer n

    -- set option defaults 
    pretty_ascii = 1           --[1] 
    pretty_indent = 2          --[2]
    pretty_start_col = 1       --[3]
    pretty_end_col = 78        --[4]
    pretty_int_format = "%d"   --[5]
    pretty_fp_format = "%.10g" --[6]
    pretty_ascii_min = 32      --[7]
    pretty_ascii_max = 127     --[8]

    n = length(options)
    if n>=1 then
        pretty_ascii = options[1]
        if n>=2 then
            pretty_indent = options[2]
            if n>=3 then
                pretty_start_col = options[3]
                if n>=4 then
                    pretty_end_col = options[4]
                    if n>=5 then
                        pretty_int_format = options[5]
                        if n>=6 then
                            pretty_fp_format = options[6]
                            if n>=7 then
                                pretty_ascii_min = options[7]
                                if n>=8 then
                                    pretty_ascii_max = options[8]
                                end if
                            end if
                        end if
                    end if
                end if
            end if
        end if
    end if

    pretty_chars = pretty_start_col
    pretty_file = fn
    pretty_level = 0
    pretty_line = ""
    rPrint(x)
    puts(pretty_file, pretty_line)
end procedure


-- trig formulas provided by Larry Gregg

--/* Not required for Phix:
global constant PI = 3.141592653589793238
--*/

-- PL removed 20/3/09 (no real saving, adds opCallOnce)
--constant PI_HALF =    PI / 2.0  -- this is pi/2

--
--/**/  -- Simpler version for Phix (see sq_arccos etc)
--/**/  type trig_range(atom x)
--/**/  --  values passed to arccos and arcsin must be [-1,+1]
--/**/      return x>=-1 and x<=1
--/**/  end type
--
--/*    -- Original for RDS Eu:
type trig_range(object x)
--  values passed to arccos and arcsin must be [-1,+1]
    if atom(x) then
        return x >= -1 and x <= 1
    else
        for i = 1 to length(x) do
            if not trig_range(x[i]) then
                return 0
            end if
        end for
        return 1
    end if
end type
--*/

global function arccos(trig_range x)
--  returns angle in radians
--  return PI_HALF-2*arctan(x/(1.0+sqrt(1.0-x*x)))
    return PI/2-2*arctan(x/(1.0+sqrt(1.0-x*x)))
end function

global function arcsin(trig_range x)
--  returns angle in radians
    return 2*arctan(x/(1.0+sqrt(1.0-x*x)))
end function


