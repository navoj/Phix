-- (c) Copyright - See License.txt
--
--namespace text

--!/**/ **NOT PHIX COMPATIBLE!**

-- I started this (builtins\text.e) while failing to get gtk on windows working. 
-- You should probably consider it incomplete/abandoned.
--
-- My current thinking is: get gtk working on Phix on Linux (after IUP), and 
--  only then (if successful) return to trying to get it to work on Windows,
--  at which point this (builtins\text.e) may yet prove valuable.

--/**/ forward global function trim_tail(sequence source, object what=" \t\r\n", integer ret_index = 0)
--/**/ forward global function trim(sequence source, object what=" \t\r\n", integer ret_index = 0)
--/**/ forward global function keyvalues(sequence source, object pair_delim = ";,",
--/**/                               object kv_delim = ":=", object quotes =  "\"'`",
--/**/                               object whitespace = " \t\n\r", integer haskeys = 1)

--****
-- == Text Manipulation
-- **Page Contents**
--
-- <<LEVELTOC depth=2>>
--

--****
-- === Routines

--/*
include std/filesys.e
include std/types.e
include std/sequence.e
include std/io.e
include std/search.e
include std/convert.e
include std/serialize.e
include std/pretty.e
include std/error.e
include std/eds.e
--*/

--****
-- Signature:
-- <built-in> function sprintf(sequence format, object values)
--
-- Description:
-- This is exactly the same as [[:printf]](), except that the output is returned as a sequence
-- of characters, rather than being sent to a file or device.
--
-- Parameters:
--              # ##format## : a sequence, the text to print. This text may contain format specifiers.
--              # ##values## : usually, a sequence of values. It should have as many elements as format specifiers in ##format##, as these values will be substituted to the specifiers.
--
-- Returns:
-- A **sequence**, of printable characters, representing ##format## with the values in ##values## spliced in.
--
-- Comments:
--
-- ##printf(fn, st, x)## is equivalent to ##puts(fn, sprintf(st, x))##.
--
-- Some typical uses of ##sprintf()## are:
--
-- # Converting numbers to strings.
-- # Creating strings to pass to system().
-- # Creating formatted error messages that can be passed to a common error message handler.
--
-- Example 1:
-- <eucode>
-- s = sprintf("%08d", 12345)
-- -- s is "00012345"
-- </eucode>
--
-- See Also:
--   [[:printf]], [[:sprint]], [[:format]]

--**
-- Returns the representation of any Euphoria object as a string of characters.
--
-- Parameters:
--   # ##x## : Any Euphoria object.
--
-- Returns:
--   A **sequence**, a string representation of ##x##.
--
-- Comments:
--
-- This is exactly the same as ##print(fn, x)##, except that the output is returned as a sequence of characters, rather
-- than being sent to a file or device. x can be any Euphoria object.
--
-- The atoms contained within ##x## will be displayed to a maximum of 10 significant digits,
-- just as with [[:print]]().
--
-- Example 1:
-- <eucode>
-- s = sprint(12345)
-- -- s is "12345"
-- </eucode>
--
-- Example 2:
-- <eucode>
-- s = sprint({10,20,30}+5)
-- -- s is "{15,25,35}"
-- </eucode>
--
-- See Also:
--    [[:sprintf]], [[:printf]]

--/* Not Phix (defined in psprint.e)
public function sprint(object x)
-- Return the string representation of any Euphoria data object.
-- This is the same as the output from print(1, x) or '?', but it's
-- returned as a string sequence rather than printed.
sequence s

    if atom(x) then
        return sprintf("%.10g", x)
    else
        s = "{"
        for i=1 to length(x) do
            if atom(x[i]) then
                s &= sprintf("%.10g", x[i])
            else
                s &= sprint(x[i])
            end if
            s &= ','
        end for
        if s[$]=',' then
            s[$] = '}'
        else
            s &= '}'
        end if
        return s
    end if
end function
--*/

--/* Not Phix (defined in ptrim.e)
--**
-- Trim all items in the supplied set from the leftmost (start or head) of a sequence.
--
-- Parameters:
--   # ##source## : the sequence to trim.
--   # ##what## : the set of item to trim from ##source## (defaults to " \t\r\n").
--   # ##ret_index## : If zero (the default) returns the trimmed sequence, otherwise
--                    it returns the index of the leftmost item **not** in ##what##.
--
-- Returns:
--   A **sequence**, if ##ret_index## is zero, which is the trimmed version of ##source##\\
--   A **integer**, if ##ret_index## is not zero, which is index of the leftmost
--                  element in ##source## that is not in ##what##.
--
-- Example 1:
-- <eucode>
-- object s
-- s = trim_head("\r\nSentence read from a file\r\n", "\r\n")
-- -- s is "Sentence read from a file\r\n"
-- s = trim_head("\r\nSentence read from a file\r\n", "\r\n", TRUE)
-- -- s is 3
-- </eucode>
--
--
-- See Also:
--   [[:trim_tail]], [[:trim]], [[:pad_head]]

public function trim_head(sequence source, object what=" \t\r\n", integer ret_index = 0)
integer lpos
    if atom(what) then
        what = {what}
    end if

    lpos = 1
    while lpos<=length(source) do
        if not find(source[lpos], what) then
            exit
        end if
        lpos += 1
    end while

    if ret_index then
        return lpos
    else
        return source[lpos..$]
    end if
end function

--**
-- Trim all items in the supplied set from the rightmost (end or tail) of a sequence.
--
-- Parameters:
--   # ##source## : the sequence to trim.
--   # ##what## : the set of item to trim from ##source## (defaults to " \t\r\n").
--   # ##ret_index## : If zero (the default) returns the trimmed sequence, otherwise
--                    it returns the index of the rightmost item **not** in ##what##.
--
-- Returns:
--   A **sequence**, if ##ret_index## is zero, which is the trimmed version of ##source##\\
--   A **integer**, if ##ret_index## is not zero, which is index of the rightmost
--                  element in ##source## that is not in ##what##.
--
-- Example 1:
-- <eucode>
-- object s
-- s = trim_tail("\r\nSentence read from a file\r\n", "\r\n")
-- -- s is "\r\nSentence read from a file"
-- s = trim_tail("\r\nSentence read from a file\r\n", "\r\n", TRUE)
-- -- s is 27
-- </eucode>
--
-- See Also:
--   [[:trim_head]], [[:trim]], [[:pad_tail]]

public function trim_tail(sequence source, object what=" \t\r\n", integer ret_index = 0)
integer rpos

    if atom(what) then
        what = {what}
    end if

    rpos = length(source)
    while rpos>0 do
        if not find(source[rpos], what) then
            exit
        end if
        rpos -= 1
    end while

    if ret_index then
        return rpos
    else
        return source[1..rpos]
    end if
end function

--**
-- Trim all items in the supplied set from both the left end (head/start) and right end (tail/end)
-- of a sequence.
--
-- Parameters:
--   # ##source## : the sequence to trim.
--   # ##what## : the set of item to trim from ##source## (defaults to " \t\r\n").
--   # ##ret_index## : If zero (the default) returns the trimmed sequence, otherwise
--                    it returns a 2-element sequence containing the index of the
--                    leftmost item and rightmost item **not** in ##what##.
--
-- Returns:
--   A **sequence**, if ##ret_index## is zero, which is the trimmed version of ##source##\\
--   A **2-element sequence**, if ##ret_index## is not zero, in the form {left_index, right_index}.
--
-- Example 1:
-- <eucode>
-- object s
-- s = trim("\r\nSentence read from a file\r\n", "\r\n")
-- -- s is "Sentence read from a file"
-- s = trim("\r\nSentence read from a file\r\n", "\r\n", TRUE)
-- -- s is {3,27}
-- </eucode>
--
-- See Also:
--   [[:trim_head]], [[:trim_tail]]

public function trim(sequence source, object what=" \t\r\n", integer ret_index = 0)
integer rpos
integer lpos

    if atom(what) then
        what = {what}
    end if

    lpos = 1
    while lpos<=length(source) do
        if not find(source[lpos], what) then
            exit
        end if
        lpos += 1
    end while

    rpos = length(source)
    while rpos>lpos do
        if not find(source[rpos], what) then
            exit
        end if
        rpos -= 1
    end while

    if ret_index then
        return {lpos, rpos}
    else
        if lpos=1 then
            if rpos=length(source) then
                return source
            end if
        end if
        if lpos>length(source) then
            return {}
        end if
        return source[lpos..rpos]
    end if
end function
--*/

--/* not Phix
constant TO_LOWER = 'a' - 'A'

sequence lower_case_SET = {}
sequence upper_case_SET = {}
sequence encoding_NAME = "ASCII"

function load_code_page(sequence cpname)
object cpdata
integer pos
sequence kv
sequence cp_source
sequence cp_db
integer fh
object idx
object vers

    cp_source = defaultext(cpname, ".ecp")
    cp_source = locate_file(cp_source)

    cpdata = read_lines(cp_source)
    if sequence(cpdata) then

        pos = 0
        while pos<length(cpdata) do
            pos += 1
            cpdata[pos]  = trim(cpdata[pos])
            if not begins("--HEAD--", cpdata[pos])
            and cpdata[pos][1]!=';' then    -- not a comment line
                if begins("--CASE--", cpdata[pos]) then
                    exit
                end if

--DEV earein.e does not like this:
-- (hopefully I got this right! [that ,, stuff is bloody awful innit!])
--/**/          kv = keyvalues(cpdata[pos],whitespace:="")
--/*
                kv = keyvalues(cpdata[pos],,,,"")
--*/
                if equal(lower(kv[1][1]), "title") then
                    encoding_NAME = kv[1][2]
                end if
            end if
        end while
        if pos>length(cpdata) then
            return -2 -- No Case Conversion table found.
        end if

        upper_case_SET = ""
        lower_case_SET = ""
        while pos<length(cpdata) do
            pos += 1
            cpdata[pos]  = trim(cpdata[pos])
            if length(cpdata[pos])>=3
            and cpdata[pos][1]!=';' then    -- not a comment line
                if cpdata[pos][1]='-' then
                    exit
                end if

                kv = keyvalues(cpdata[pos])
                upper_case_SET &= hex_text(kv[1][1])
                lower_case_SET &= hex_text(kv[1][2])
            end if
        end while

    else
        -- See if its in the database.
        cp_db = locate_file("ecp.dat")
        fh = open(cp_db, "rb")
        if fh=-1 then
            return -2 -- Couldn't open DB
        end if
        vers = deserialize(fh)  -- get the database version
        if vers[1]=1 then
            idx = deserialize(fh)  -- get Code Page index offset
            pos = seek(fh, idx)
            idx = deserialize(fh)   -- get the Code Page Index
            pos = find(cpname, idx[1])
            if pos!=0 then
                pos = seek(fh, idx[2][pos])
                upper_case_SET = deserialize(fh) -- "uppercase"
                lower_case_SET = deserialize(fh) -- "lowercase"
                encoding_NAME = deserialize(fh) -- "title"
            end if
        end if
        close(fh)

    end if
    return 0
end function

--**
-- Sets the table of lowercase and uppercase characters that is used by
-- [[:lower]] and [[:upper]]
--
-- Parameters:
--   # ##en## : The name of the encoding represented by these character sets
--   # ##lc## : The set of lowercase characters
--   # ##uc## : The set of upper case characters
--
--
-- Comments:
-- * ##lc## and ##uc## must be the same length.
-- * If no parameters are given, the default ASCII table is set.
--
-- Example 1:
-- <eucode>
-- set_encoding_properties( "Elvish", "aeiouy", "AEIOUY")
-- </eucode>
--
-- Example 1:
-- <eucode>
-- set_encoding_properties( "1251") -- Loads a predefined code page.
-- </eucode>
--
-- See Also:
--   [[:lower]], [[:upper]], [[:get_encoding_properties]]

public procedure set_encoding_properties(sequence en = "", sequence lc = "", sequence uc = "")
integer res

    if length(en)>0 and length(lc)=0 and length(uc)=0 then
        res = load_code_page(en)
        if res!=0 then
            printf(2, "Failed to load code page '%s'. Error # %d\n", {en, res})
        end if
        return
    end if

    if length(lc)=length(uc) then
        if length(lc)=0 and length(en)=0 then
            en = "ASCII"
        end if
        lower_case_SET = lc
        upper_case_SET = uc
        encoding_NAME = en
    end if
end procedure

--**
-- Gets the table of lowercase and uppercase characters that is used by
-- [[:lower]] and [[:upper]]
--
-- Parameters:
-- none
--
-- Returns:
-- A **sequence**, containing three items.\\
--   {Encoding_Name, LowerCase_Set, UpperCase_Set}
--
-- Example 1:
-- <eucode>
-- encode_sets = get_encoding_properties()
-- </eucode>
--
-- See Also:
--   [[:lower]], [[:upper]], [[:set_encoding_properties]]
--
public function get_encoding_properties()
    return {encoding_NAME, lower_case_SET, upper_case_SET}
end function


ifdef WINDOWS then
include std/dll.e
include std/machine.e
--include std/types.e
atom
    user32 = open_dll("user32.dll"),
    api_CharLowerBuff = define_c_func(user32, "CharLowerBuffA", {C_POINTER, C_INT}, C_INT),
    api_CharUpperBuff = define_c_func(user32, "CharUpperBuffA", {C_POINTER, C_INT}, C_INT),
--/*
    tm_size = 1024,
--*/
    temp_mem = allocate(1024)

function change_case(object x, object api)
sequence changed_text
integer single_char = 0
integer len

    if not string(x) then
        if atom(x) then
            if x=0 then
                return 0
            end if
            x = {x}
            single_char = 1
        else
            for i=1 to length(x) do
                x[i] = change_case(x[i], api)
            end for
            return x
        end if
    end if
    if length(x)=0 then
        return x
    end if
    if length(x)>=tm_size then
        tm_size = length(x)+1
        free(temp_mem)
        temp_mem = allocate(tm_size)
    end if
    poke(temp_mem, x)
    len = c_func(api, {temp_mem, length(x)})
    if len<1 then
        len = length(x)
    end if
    changed_text = peek({temp_mem, len})
    if single_char then
        return changed_text[1]
    else
        return changed_text
    end if
end function
end ifdef
--*/

--**
-- Convert an atom or sequence to lower case.
--
-- Parameters:
--   # ##x## : Any Euphoria object.
--
-- Returns:
--   A **sequence**, the lowercase version of ##x##
--
-- Comments:
-- * For Windows systems, this uses the current code page for conversion
-- * For non-Windows, this only works on ASCII characters. It alters characters in
--   the 'a'..'z' range. If you need to do case conversion with other encodings
--   use the [[:set_encoding_properties]] first.
-- * ##x## may be a sequence of any shape, all atoms of which will be acted upon.
--
-- **WARNING**, When using ASCII encoding, this can also affects floating point
-- numbers in the range 65 to 90.
--
-- Example 1:
-- <eucode>
-- s = lower("Euphoria")
-- -- s is "euphoria"
--
-- a = lower('B')
-- -- a is 'b'
--
-- s = lower({"Euphoria", "Programming"})
-- -- s is {"euphoria", "programming"}
-- </eucode>
--
-- See Also:
--   [[:upper]], [[:proper]], [[:set_encoding_properties]], [[:get_encoding_properties]]
--/* Phix: defined in builtins/pcase.e (auto-include)
public function lower(object x)
-- convert atom or sequence to lower case
    if length(lower_case_SET) != 0 then
        return mapping(x, upper_case_SET, lower_case_SET)
    end if
    
    ifdef WINDOWS then
        return change_case(x, api_CharLowerBuff)
    elsedef 
        return x + (x >= 'A' and x <= 'Z') * TO_LOWER
    end ifdef
end function

--**
-- Convert an atom or sequence to upper case.
--
-- Parameters:
--   # ##x## : Any Euphoria object.
--
-- Returns:
--   A **sequence**, the uppercase version of ##x##
--
-- Comments:
-- * For Windows systems, this uses the current code page for conversion
-- * For non-Windows, this only works on ASCII characters. It alters characters in
--   the 'a'..'z' range. If you need to do case conversion with other encodings
--   use the [[:set_encoding_properties]] first.
-- * ##x## may be a sequence of any shape, all atoms of which will be acted upon.
--
-- **WARNING**, When using ASCII encoding, this can also affects floating point
-- numbers in the range 97 to 122.
--
-- Example 1:
-- <eucode>
-- s = upper("Euphoria")
-- -- s is "EUPHORIA"
--
-- a = upper('b')
-- -- a is 'B'
--
-- s = upper({"Euphoria", "Programming"})
-- -- s is {"EUPHORIA", "PROGRAMMING"}
-- </eucode>
--
-- See Also:
--     [[:lower]], [[:proper]], [[:set_encoding_properties]], [[:get_encoding_properties]]

public function upper(object x)
-- convert atom or sequence to upper case
    if length(upper_case_SET) != 0 then
        return mapping(x, lower_case_SET, upper_case_SET)
    end if
    ifdef WINDOWS then
        return change_case(x, api_CharUpperBuff)
    elsedef 
        return x - (x >= 'a' and x <= 'z') * TO_LOWER
    end ifdef

end function
--*/

--**
-- Convert a text sequence to capitalized words.
--
-- Parameters:
--   # ##x## : A text sequence.
--
-- Returns:
--   A **sequence**, the Capitalized Version of ##x##
--
-- Comments:
-- A text sequence is one in which all elements are either characters or
-- text sequences. This means that if a non-character is found in the input,
-- it is not converted. However this rule only applies to elements on the
-- same level, meaning that sub-sequences could be converted if they are
-- actually text sequences.
--
--
-- Example 1:
-- <eucode>
-- s = proper("euphoria programming language")
-- -- s is "Euphoria Programming Language"
-- s = proper("EUPHORIA PROGRAMMING LANGUAGE")
-- -- s is "Euphoria Programming Language"
-- s = proper({"EUPHORIA PROGRAMMING", "language", "rapid dEPLOYMENT", "sOfTwArE"})
-- -- s is {"Euphoria Programming", "Language", "Rapid Deployment", "Software"}
-- s = proper({'a', 'b', 'c'})
-- -- s is {'A', 'b', c'} -- "Abc"
-- s = proper({'a', 'b', 'c', 3.1472})
-- -- s is {'a', 'b', c', 3.1472} -- Unchanged because it contains a non-character.
-- s = proper({"abc", 3.1472})
-- -- s is {"Abc", 3.1472} -- The embedded text sequence is converted.
-- </eucode>
--
-- See Also:
--     [[:lower]] [[:upper]]

global function proper(string x)
-- Converts text to lowercase and makes each word start with an uppercase.
integer ch
integer inword

    inword = 0      -- Initially not in a word
    for i=1 to length(x) do
        ch = x[i]
        -- Check for upper case
        if ch>='A' and ch<='Z' then
            if inword=1 then
                -- Upper, but as we are in a word convert it to lower.
                x[i] = lower(ch)
            else
                inword = 1      -- now we are in a word
            end if
        else
            -- Not upper, so check for lower case
            if ch>='a' and ch<='z' then
                if inword=0 then
                    -- start of word, so convert only lower to upper.
                    x[i] = upper(ch)
                    inword = 1      -- now we are in a word
                end if
            else
                -- Not lower so check for digits
                -- n.b. digits have no effect on if its in a word or not.
                if ch<'0' or ch>'9' then
                    -- not digit so check for special word chars
                    if ch='_' then
                        inword = 1
                    else
                        inword = 0
                    end if
                end if
            end if
        end if
    end for
    return x
end function

--**
-- Converts a string containing Key/Value pairs into a set of
-- sequences, one per K/V pair.
--
-- Parameters:
-- # ##source## : a text sequence, containing the representation of the key/values.
-- # ##pair_delim## : an object containing a list of elements that delimit one
--                   key/value pair from the next. The defaults are semi-colon (;)
--                   and comma (,).
-- # ##kv_delim## : an object containing a list of elements that delimit the
--                key from its value. The defaults are colon (:) and equal (=).
-- # ##quotes## : an object containing a list of elements that can be used to
--                enclose either keys or values that contain delimiters or
--                whitespace. The defaults are double-quote ("), single-quote (')
--                and back-quote (`)
-- # ##whitespace## : an object containing a list of elements that are regarded
--                as whitespace characters. The defaults are space, tab, new-line,
--                and carriage-return.
-- # ##haskeys## : an integer containing true or false. The default is true. When
-- ##true##, the ##kv_delim## values are used to separate keys from values, but
-- when ##false## it is assumed that each 'pair' is actually just a value.
--
-- Returns:
--              A **sequence**, of pairs. Each pair is in the form {key, value}.
--
-- Comments:
--
-- String representations of atoms are not converted, either in the key or value part, but returned as any regular string instead.
--
-- If ##haskeys## is ##true##, but a substring only holds what appears to be a value, the key
-- is synthesized as ##p[n]##, where ##n## is the number of the pair. See example #2.
--
-- By default, pairs can be delimited by either a comma or semi-colon ",;" and
-- a key is delimited from its value by either an equal or a colon "=:".
-- Whitespace between pairs, and between delimiters is ignored.
--
-- If you need to have one of the delimiters in the value data, enclose it in
-- quotation marks. You can use any of single, double and back quotes, which
-- also means you can quote quotation marks themselves. See example #3.
--
-- It is possible that the value data itself is a nested set of pairs. To do
-- this enclose the value in parentheses. Nested sets can nested to any level.
-- See example #4.
--
-- If a sub-list has only data values and not keys, enclose it in either braces
-- or square brackets. See example #5.
-- If you need to have a bracket as the first character in a data value, prefix
-- it with a tilde. Actually a leading tilde will always just be stripped off
-- regardless of what it prefixes. See example #6.
--
-- Example 1:
-- <eucode>
-- s = keyvalues("foo=bar, qwe=1234, asdf='contains space, comma, and equal(=)'")
-- -- s is { {"foo", "bar"}, {"qwe", "1234"}, {"asdf", "contains space, comma, and equal(=)"}}
-- </eucode>
--
-- Example 2:
-- <eucode>
-- s = keyvalues("abc fgh=ijk def")
-- -- s is { {"p[1]", "abc"}, {"fgh", "ijk"}, {"p[3]", "def"} }
-- </eucode>
--
-- Example 3:
-- <eucode>
-- s = keyvalues("abc=`'quoted'`")
-- -- s is { {"abc", "'quoted'"} }
-- </eucode>
--
-- Example 4:
-- <eucode>
-- s = keyvalues("colors=(a=black, b=blue, c=red)")
-- -- s is { {"colors", {{"a", "black"}, {"b", "blue"},{"c", "red"}}  } }
-- s = keyvalues("colors=(black=[0,0,0], blue=[0,0,FF], red=[FF,0,0])")
-- -- s is { {"colors", {{"black",{"0", "0", "0"}}, {"blue",{"0", "0", "FF"}},{"red", {"FF","0","0"}}}} }
-- </eucode>
--
-- Example 5:
-- <eucode>
-- s = keyvalues("colors=[black, blue, red]")
-- -- s is { {"colors", { "black", "blue", "red"}  } }
-- </eucode>
--
-- Example 6:
-- <eucode>
-- s = keyvalues("colors=~[black, blue, red]")
-- -- s is { {"colors", "[black, blue, red]"}  } }
-- -- The following is another way to do the same.
-- s = keyvalues("colors=`[black, blue, red]`")
-- -- s is { {"colors", "[black, blue, red]"}  } }
-- </eucode>

public function keyvalues(sequence source, object pair_delim = ";,",
                          object kv_delim = ":=", object quotes =  "\"'`",
                          object whitespace = " \t\n\r", integer haskeys = 1)

sequence lKeyValues
sequence value_
sequence key_
sequence lAllDelim
sequence lWhitePair
sequence lStartBracket
sequence lEndBracket
sequence lBracketed
integer lQuote
integer pos_
integer lChar
integer lBPos
integer lWasKV

    source = trim(source)
    if length(source)=0 then
        return {}
    end if

    if atom(pair_delim) then
        pair_delim = {pair_delim}
    end if
    if atom(kv_delim) then
        kv_delim = {kv_delim}
    end if
    if atom(quotes) then
        quotes = {quotes}
    end if
    if atom(whitespace) then
        whitespace = {whitespace}
    end if

    lAllDelim = whitespace & pair_delim & kv_delim
    lWhitePair = whitespace & pair_delim
    lStartBracket = "{[("
    lEndBracket   = "}])"

    lKeyValues = {}
    pos_ = 1
    while pos_<=length(source) do
        -- ignore leading whitespace
        while pos_<length(source) do
            if find(source[pos_], whitespace)=0 then
                exit
            end if
            pos_ += 1
        end while

        -- Get key. Ends at any of unquoted whitespace or unquoted delimiter
        key_ = ""
        lQuote = 0
        lChar = 0
        lWasKV = 0
        if haskeys then
            while pos_<=length(source) do
                lChar = source[pos_]
                if find(lChar, quotes)!=0 then
                    if lChar=lQuote then
                        -- End of quoted span
                        lQuote = 0
                        lChar = -1
                    elsif lQuote=0 then
                        -- Start of quoted span
                        lQuote = lChar
                        lChar = -1
                    end if

                elsif lQuote=0 and find(lChar, lAllDelim)!=0 then
                    exit

                end if
                if lChar>0 then
                    key_ &= lChar
                end if
                pos_ += 1
            end while

            -- ignore next whitespace
            if find(lChar, whitespace)!=0 then
                pos_ += 1
                while pos_<=length(source) do
                    lChar = source[pos_]
                    if find(lChar, whitespace)=0 then
                        exit
                    end if
                    pos_ += 1
                end while
            end if
        else
            pos_ -= 1       -- Put back the last char.
        end if

        value_ = ""
        if find(lChar, kv_delim)!=0  or not haskeys then

            if find(lChar, kv_delim)!=0 then
                lWasKV = 1
            end if

            -- ignore next whitespace
            pos_ += 1
            while pos_<=length(source) do
                lChar = source[pos_]
                if find(lChar, whitespace)=0 then
                    exit
                end if
                pos_ += 1
            end while

            -- Get value. Ends at any of unquoted whitespace or unquoted delimiter
            lQuote = 0
            lChar = 0
            lBracketed = {}
            while pos_<=length(source) do
                lChar = source[pos_]
                if length(lBracketed)=0 and find(lChar, quotes)!=0 then
                    if lChar=lQuote then
                        -- End of quoted span
                        lQuote = 0
                        lChar = -1
                    elsif lQuote=0 then
                        -- Start of quoted span
                        lQuote = lChar
                        lChar = -1
                    end if
                elsif find(lChar, lStartBracket)>0 then
                    lBPos = find(lChar, lStartBracket)
                    lBracketed &= lEndBracket[lBPos]

                elsif length(value_)=1 and value_[1]='~' and find(lChar, lStartBracket)>0 then
                    lBPos = find(lChar, lStartBracket)
                    lBracketed &= lEndBracket[lBPos]

                elsif length(lBracketed)!=0 and lChar=lBracketed[$] then
                    lBracketed = lBracketed[1..$-1]

                elsif length(lBracketed)=0 and lQuote=0 and find(lChar, lWhitePair)!=0 then
                    exit

                end if

                if lChar>0 then
                    value_ &= lChar
                end if
                pos_ += 1
            end while

            if find(lChar, whitespace)!=0  then
                -- ignore next whitespace
                pos_ += 1
                while pos_<=length(source) do
                    lChar = source[pos_]
                    if find(lChar, whitespace)=0 then
                        exit
                    end if
                    pos_ += 1
                end while
            end if

            if find(lChar, pair_delim)!=0  then
                pos_ += 1
                if pos_<=length(source) then
                    lChar = source[pos_]
                end if
            end if
        end if

        if find(lChar, pair_delim)!=0  then
            pos_ += 1
        end if

        if length(value_)=0
        and length(key_)=0 then
            lKeyValues = append(lKeyValues, {})
        else

            if length(value_)=0 then
                if not lWasKV then
                    value_ = key_
                    key_ = ""
                end if
            end if

            if length(key_)=0 then
                if haskeys then
                    key_ =  sprintf("p[%d]", length(lKeyValues)+1)
                end if
            end if

            if length(value_)>0 then
                lChar = value_[1]
                lBPos = find(lChar, lStartBracket)
                if lBPos>0 and value_[$]=lEndBracket[lBPos] then
                    if lChar='(' then
                        value_ = keyvalues(value_[2..$-1], pair_delim, kv_delim, quotes, whitespace, haskeys)
                    else
                        value_ = keyvalues(value_[2..$-1], pair_delim, kv_delim, quotes, whitespace, 0)
                    end if
                elsif lChar='~' then
                    value_ = value_[2..$]
                end if
            end if

            key_ = trim(key_)
            value_ = trim(value_)
            if length(key_)=0 then
                lKeyValues = append(lKeyValues, value_)
            else
                lKeyValues = append(lKeyValues, {key_, value_})
            end if
        end if

    end while

    return lKeyValues
end function

--**
-- Escape special characters in a string
--
-- Parameters:
--   # ##s##: string to escape
--   # ##what##: sequence of characters to escape
--     defaults to escaping a double quote.
--
-- Returns:
--   An escaped ##sequence## representing ##s##.
--
-- Example 1:
-- <eucode>
-- sequence s = escape("John \"Mc\" Doe")
-- puts(1, s)
-- -- output is: John \"Mc\" Doe
-- </eucode>
--
-- See Also:
--  [[:quote]]
--

--/* Not Phix (erm... can do better [\r\t\n])
global function escape(string s, string what="\"")
sequence r = ""

    for i=1 to length(s) do
        integer ch = s[i]
        if find(ch, what) then
            r &= '\\'
        end if
        r &= ch
    end for

    return r
end function
--*/

--**
-- Return a quoted version of the first argument.
--
-- Parameters:
--   # ##text_in## : The string or set of strings to quote.
--   # ##quote_pair## : A sequence of two strings. The first string is the opening
--              quote to use, and the second string is the closing quote to use.
--              The default is {"\"", "\""} which means that the output will be
--              enclosed by double-quotation marks.
--   # ##esc## : A single escape character. If this is not negative (the default),
--              then this is used to 'escape' any embedded quote characters and
--              'esc' characters already in the ##text_in## string.
--   # ##sp##  : A list of zero or more special characters. The ##text_in## is only
--             quoted if it contains any of the special characters. The default
--             is "" which means that the ##text_in## is always quoted.
--
-- Returns:
--   A **sequence**, the quoted version of ##text_in##.
--
-- Example 1:
-- <eucode>
-- -- Using the defaults. Output enclosed in double-quotes, no escapes and no specials.
-- s = quote("The small man")
-- -- 's' now contains '"the small man"' including the double-quote characters.
-- </eucode>
--
-- Example 2:
-- <eucode>
-- s = quote("The small man", {"(", ")"} )
-- -- 's' now contains '(the small man)'
-- </eucode>
--
-- Example 3:
-- <eucode>
-- s = quote("The (small) man", {"(", ")"}, '~' )
-- -- 's' now contains '(the ~(small~) man)'
-- </eucode>
--
-- Example 4:
-- <eucode>
-- s = quote("The (small) man", {"(", ")"}, '~', "#" )
-- -- 's' now contains "the (small) man"
-- -- because the input did not contain a '#' character.
-- </eucode>
--
-- Example 5:
-- <eucode>
-- s = quote("The #1 (small) man", {"(", ")"}, '~', "#" )
-- -- 's' now contains '(the #1 ~(small~) man)'
-- -- because the input did contain a '#' character.
-- </eucode>
--
-- Example 6:
-- <eucode>
-- -- input is a set of strings...
-- s = quote({"a b c", "def", "g hi"},)
-- -- 's' now contains three quoted strings: '"a b c"', '"def"', and '"g hi"'
-- </eucode>
--
-- See Also:
--   [[:escape]]
--

--#withtype t_text
--/* 31/12/16: (replace_all long since deprecated)
--  public function quote(sequence text_in, object quote_pair = {"\"", "\""}, integer esc = -1, t_text sp = "")
--      if length(text_in)=0 then
--          return text_in
--      end if
--
--      if atom(quote_pair) then
--          quote_pair = {{quote_pair}, {quote_pair}}
--      elsif length(quote_pair)=1 then
--          quote_pair = {quote_pair[1], quote_pair[1]}
--      elsif length(quote_pair)=0 then
--          quote_pair = {"\"", "\""}
--      end if
--
--      if sequence(text_in[1]) then
--          for i=1 to length(text_in) do
--              if sequence(text_in[i]) then
--                  text_in[i] = quote(text_in[i], quote_pair, esc, sp)
--              end if
--          end for
--
--          return text_in
--      end if
--
--      -- Only quote the input if it contains any of the items in 'sp'
--      for i=1 to length(sp) do
--          if find(sp[i], text_in) then
--              exit
--          end if
--
--          if i=length(sp) then
--              -- Contains none of them, so just return the input untouched.
--              return text_in
--          end if
--      end for
--
--      if esc>=0  then
--          -- If the input already contains a quote, replace them with esc-quote,
--          -- but make sure that if the input already contains esc-quote that all
--          -- embedded escapes are replaced with esc-esc first.
--          if atom(quote_pair[1]) then
--              quote_pair[1] = {quote_pair[1]}
--          end if
--          if atom(quote_pair[2]) then
--              quote_pair[2] = {quote_pair[2]}
--          end if
--
--          if equal(quote_pair[1], quote_pair[2]) then
--              -- Simple case where both open and close quote are the same.
--              if match(quote_pair[1], text_in) then
--                  if match(esc & quote_pair[1], text_in) then
--                      text_in = replace_all(text_in, esc, esc & esc)
--                  end if
--                  text_in = replace_all(text_in, quote_pair[1], esc & quote_pair[1])
--              end if
--          else
--              if match(quote_pair[1], text_in) or
--              match(quote_pair[2], text_in) then
--                  if match(esc & quote_pair[1], text_in) then
--                      text_in = replace_all(text_in, esc & quote_pair[1], esc & esc & esc & quote_pair[1])
--                  end if
--                  text_in = replace_all(text_in, quote_pair[1], esc & quote_pair[1])
--              end if
--
--              if match(quote_pair[2], text_in) then
--                  if match(esc & quote_pair[2], text_in) then
--                      text_in = replace_all(text_in, esc & quote_pair[2], esc & esc & esc & quote_pair[2])
--                  end if
--                  text_in = replace_all(text_in, quote_pair[2], esc & quote_pair[2])
--              end if
--          end if
--      end if
--
--      return quote_pair[1] & text_in & quote_pair[2]
--
--  end function
--*/

--**
-- Removes 'quotation' text from the argument.
--
-- Parameters:
--   # ##text_in## : The string or set of strings to de-quote.
--   # ##quote_pairs## : A set of one or more sub-sequences of two strings.
--              The first string in each sub-sequence is the opening
--              quote to look for, and the second string is the closing quote.
--              The default is {{{"\"", "\""}}} which means that the output is
--              'quoted' if it is enclosed by double-quotation marks.
--   # ##esc## : A single escape character. If this is not negative (the default),
--              then this is used to 'escape' any embedded occurrences of the
--              quote characters. In which case the 'escape' character is also
--              removed.
--
-- Returns:
-- A **sequence**, the original text but with 'quote' strings stripped of quotes.
--
-- Example 1:
-- <eucode>
-- -- Using the defaults.
-- s = quote("\"The small man\"")
-- -- 's' now contains "The small man"
-- </eucode>
--
-- Example 2:
-- <eucode>
-- -- Using the defaults.
-- s = quote("(The small ?(?) man)", {{"[","]"}}, '?')
-- -- 's' now contains "The small () man"
-- </eucode>
--
public function dequote(object text_in, object quote_pairs = {{"\"", "\""}}, integer esc = -1)
integer pos

    if length(text_in)=0 then
        return text_in
    end if

    if atom(quote_pairs) then
        quote_pairs = {{quote_pairs}, {quote_pairs}}
    elsif length(quote_pairs)=1 then
        quote_pairs = {quote_pairs[1], quote_pairs[1]}
    elsif length(quote_pairs)=0 then
        quote_pairs = {"\"", "\""}
    end if

    if sequence(text_in[1]) then
        for i=1 to length(text_in) do
            if sequence(text_in[i]) then
                text_in[i] = dequote(text_in[i], quote_pairs, esc)
            end if
        end for

        return text_in
    end if

    -- If the text begins and ends with a quote-pair then strip them off and
    -- remove the 'escape' from any 'escaped' quote_pairs within the text.
    for i=1 to length(quote_pairs) do
        if length(text_in)>=length(quote_pairs[i][1])+length(quote_pairs[i][2]) then
            if begins(quote_pairs[i][1], text_in) and ends(quote_pairs[i][2], text_in) then
                text_in = text_in[1+length(quote_pairs[i][1])..$-length(quote_pairs[i][2])]
                pos = 1
                while 1 do
                    pos = find(esc, text_in, pos)
                    if pos<=0 then exit end if
                    if begins(quote_pairs[i][1], text_in[pos+1..$]) then
                        text_in = text_in[1..pos-1] & text_in[pos+1..$]
                    elsif begins(quote_pairs[i][2], text_in[pos+1..$]) then
                        text_in = text_in[1..pos-1] & text_in[pos+1..$]
                    else
                        pos += 1
                    end if
                end while
                exit
            end if
        end if
    end for

    return text_in
end function

--**
-- Formats a set of arguments in to a string based on a supplied pattern.
--
-- Parameters:
--   # ##format_pattern## : A sequence: the pattern string that contains zero or more tokens.
--   # ##arg_list## : An object: Zero or more arguments used in token replacement.
--
-- Returns:
-- A string **sequence**, the original ##format_pattern## but with tokens replaced by
-- corresponding arguments.
--
-- Comments:
-- The ##format_pattern## string contains text and argument tokens. The resulting string
-- is the same as the format string except that each token is replaced by an
-- item from the argument list.
--
-- A token has the form **##[<Q>]##**, where <Q> is are optional qualifier codes.
--
-- The qualifier. ##<Q>## is a set of zero or more codes that modify the default
-- way that the argument is used to replace the token. The default replacement
-- method is to convert the argument to its shortest string representation and
-- use that to replace the token. This may be modified by the following codes,
-- which can occur in any order.
-- |= Qualifier |= Usage                                              |
-- |  N         | ('N' is an integer) The index of the argument to use|
-- | {id}       | Uses the argument that begins with "id=" where "id" \\
--                is an identifier name.                              |
-- | %envvar%   | Uses the Environment Symbol 'envar' as an argument  |
-- |  w         | For string arguments, if capitalizes the first\\
--                letter in each word                                 |
-- |  u         | For string arguments, it converts it to upper case. |
-- |  l         | For string arguments, it converts it to lower case. |
-- |  <         | For numeric arguments, it left justifies it.        |
-- |  >         | For string arguments, it right justifies it.        |
-- |  c         | Centers the argument.                               |
-- |  z         | For numbers, it zero fills the left side.           |
-- |  :S        | ('S' is an integer) The maximum size of the\\
--                resulting field. Also, if 'S' begins with '0' the\\
--                field will be zero-filled if the argument is an integer|
-- |  .N        | ('N' is an integer) The number of digits after\\
--                 the  decimal point                                 |
-- |  +         | For positive numbers, show a leading plus sign      |
-- |  (         | For negative numbers, enclose them in parentheses   |
-- |  b         | For numbers, causes zero to be all blanks           |
-- |  s         | If the resulting field would otherwise be zero\\
--                length, this ensures that at least one space occurs\\
--                between this token's field                          |
-- |  t         | After token replacement, the resulting string up to this point is trimmed. |
-- |  X         | Outputs integer arguments using hexadecimal digits. |
-- |  B         | Outputs integer arguments using binary digits.      |
-- |  ?         | The corresponding argument is a set of two strings. This\\
--                uses the first string if the previous token's argument is\\
--                not the value 1 or a zero-length string, otherwise it\\
--                uses the second string.                             |
-- |  [         | Does not use any argument. Outputs a left-square-bracket symbol |
-- |  ,X        | Insert thousands separators. The <X> is the character\\
--                to use. If this is a dot "." then the decimal point\\
--                is rendered using a comma. Does not apply to zero-filled\\
--                fields.                         \\
--                N.B. if hex or binary output was specified, the \\
--                separators are every 4 digits otherwise they are \\
--                every three digits. |
--
-- Clearly, certain combinations of these qualifier codes do not make sense and in
-- those situations, the rightmost clashing code is used and the others are ignored.
--
-- Any tokens in the format that have no corresponding argument are simply removed
-- from the result. Any arguments that are not used in the result are ignored.
--
-- Any sequence argument that is not a string will be converted to its
-- //pretty// format before being used in token replacement.
--
-- If a token is going to be replaced by a zero-length argument, all white space
-- following the token until the next non-whitespace character is not copied to
-- the result string.
--
-- Examples:
-- <eucode>
-- format("Cannot open file '[]' - code []", {"/usr/temp/work.dat", 32})
-- -- "Cannot open file '/usr/temp/work.dat' - code 32"
--
-- format("Err-[2], Cannot open file '[1]'", {"/usr/temp/work.dat", 32})
-- -- "Err-32, Cannot open file '/usr/temp/work.dat'"
--
-- format("[4w] [3z:2] [6] [5l] [2z:2], [1:4]", {2009,4,21,"DAY","MONTH","of"})
-- -- "Day 21 of month 04, 2009"
--
-- format("The answer is [:6.2]%", {35.22341})
-- -- "The answer is  35.22%"
--
-- format("The answer is [.6]", {1.2345})
-- -- "The answer is 1.234500"
--
-- format("The answer is [,,.2]", {1234.56})
-- -- "The answer is 1,234.56"
--
-- format("The answer is [,..2]", {1234.56})
-- -- "The answer is 1.234,56"
--
-- format("The answer is [,:.2]", {1234.56})
-- -- "The answer is 1:234.56"
--
-- format("[] [?]", {5, {"cats", "cat"}})
-- -- "5 cats"
--
-- format("[] [?]", {1, {"cats", "cat"}})
-- -- "1 cat"
--
-- format("[<:4]", {"abcdef"})
-- -- "abcd"
--
-- format("[>:4]", {"abcdef"})
-- -- "cdef"
--
-- format("[>:8]", {"abcdef"})
-- -- "  abcdef"
--
-- format("seq is []", {{1.2, 5, "abcdef", {3}}})
-- -- `seq is {1.2,5,"abcdef",{3}}`
--
-- format("Today is [{day}], the [{date}]", {"date=10/Oct/2012", "day=Wednesday"})
-- -- "Today is Wednesday, the 10/Oct/2012"
-- </eucode>
--
-- See Also:
--   [[:sprintf]]
--

global function text_format(sequence format_pattern, object arg_list = {})
sequence result
integer in_token
integer tch
integer i
integer tstart
integer tend
integer cap
integer align
integer psign
integer msign
integer zfill
integer bwz
integer spacer
integer alt
integer width
integer decs
integer pos
integer argn
integer argl
integer trimming
integer hexout
integer binout
integer tsep
object prevargv
object currargv
sequence idname
object envsym
object envvar
integer sp
sequence argtext
object tempv
integer dpos
integer dist
integer bracketed

    if atom(arg_list) then
        arg_list = {arg_list}
    end if

    result = ""
    in_token = 0


    i = 0
    tstart = 0
    tend = 0
    argl = 0
    spacer = 0
    prevargv = 0
    while i<length(format_pattern) do
        i += 1
        tch = format_pattern[i]
        if not in_token then
            if tch='[' then
                in_token = 1
                tstart = i
                tend = 0
                cap = 0
                align = 0
                psign = 0
                msign = 0
                zfill = 0
                bwz = 0
                spacer = 0
                alt = 0
                width = 0
                decs = -1
                argn = 0
                hexout = 0
                binout = 0
                trimming = 0
                tsep = 0
                idname = ""
                envvar = ""
                envsym = ""
            else
                result &= tch
            end if
        else
            if tch=']' then
                in_token = 0
                tend = i

            elsif tch='[' then
                result &= tch
                while i<length(format_pattern) do
                    i += 1
                    if format_pattern[i]=']' then
                        in_token = 0
                        tstart = 0
                        tend = 0
                        exit
                    end if
                end while

            elsif tch='w'
               or tch='u'
               or tch='l' then
                cap = tch

            elsif tch='b' then
                bwz = 1

            elsif tch='s' then
                spacer = 1

            elsif tch='t' then
                trimming = 1

            elsif tch='z' then
                zfill = 1

            elsif tch='X' then
                hexout = 1

            elsif tch='B' then
                binout = 1

            elsif tch='c'
               or tch='<'
               or tch='>' then
                align = tch

            elsif tch='+' then
                psign = 1

            elsif tch='(' then
                msign = 1

            elsif tch='?' then
                alt = 1

            elsif tch=':' then
                while i<length(format_pattern) do
                    i += 1
                    tch = format_pattern[i]
                    pos = find(tch, "0123456789")
                    if pos=0 then
                        i -= 1
                        exit
                    end if
                    width = width*10+pos-1
                    if width=0 then
                        zfill = '0'
                    end if
                end while

            elsif tch='.' then
                decs = 0
                while i<length(format_pattern) do
                    i += 1
                    tch = format_pattern[i]
                    pos = find(tch, "0123456789")
                    if pos=0 then
                        i -= 1
                        exit
                    end if
                    decs = decs*10+pos-1
                end while

            elsif tch='{' then
                -- Use a named argument.

                sp = i+1
                i = sp
                while i<length(format_pattern) do
                    if format_pattern[i]='}' then
                        exit
                    end if
                    if format_pattern[i]=']' then
                        exit
                    end if
                    i += 1
                end while
                idname = trim(format_pattern[sp..i-1]) & '='
                if format_pattern[i]=']' then
                    i -= 1
                end if

                for j=1 to length(arg_list) do
--                  if begins(idname, arg_list[j]) then
                    if match(idname, arg_list[j])=1 then
                        if argn=0 then
                            argn = j
                            exit
                        end if
                    end if
                    if j=length(arg_list) then
                        idname = ""
                        argn = -1
                    end if
                end for
            elsif tch='%' then
                -- Use the environment symbol

                sp = i+1
                i = sp
                while i<length(format_pattern) do
                    if format_pattern[i]='%' then
                        exit
                    end if
                    if format_pattern[i]=']' then
                        exit
                    end if
                    i += 1
                end while
                envsym = trim(format_pattern[sp..i-1])
                if format_pattern[i]=']' then
                    i -= 1
                end if

                envvar = getenv(envsym)

                argn = -1
                if atom(envvar) then
                    envvar = ""
                end if


            elsif tch>='0'
              and tch<='9' then
                if argn=0 then
                    i -= 1
                    while i<length(format_pattern) do
                        i += 1
                        tch = format_pattern[i]
                        pos = find(tch, "0123456789")
                        if pos=0 then
                            i -= 1
                            exit
                        end if
                        argn = argn*10+pos-1
                    end while
                end if

            elsif tch=',' then
                if i<length(format_pattern) then
                    i += 1
                    tsep = format_pattern[i]
                end if

--          else
                -- ignore it

            end if

            if tend>0 then
                -- Time to replace the token.
                argtext = ""

                if argn=0 then
                    argn = argl+1
                end if
                argl = argn

                if argn<1 or argn>length(arg_list) then
                    if length(envvar)>0 then
                        argtext = envvar
                        currargv = envvar
                    else
                        argtext = ""
                        currargv = ""
                    end if
                else
                    if string(arg_list[argn]) then
                        if length(idname)>0 then
                            argtext = arg_list[argn][length(idname)+1..$]
                        else
                            argtext = arg_list[argn]
                        end if

                    elsif integer(arg_list[argn]) then
                        if bwz!=0 and arg_list[argn]=0 then
                            argtext = ""
                        elsif binout=1 then
--/**/                      argtext = sprintf("%b", arg_list[argn])                                     --/*
--/!**!/                    argtext = sq_add(reverse(int_to_bits(arg_list[argn], 32)),'0')              --/!*
                            argtext = stdseq:reverse( convert:int_to_bits(arg_list[argn], 32)) + '0'    --*/
                            for ib=1 to length(argtext) do
                                if argtext[ib]='1' then
                                    argtext = argtext[ib..$]
                                    exit
                                end if
                            end for

                        elsif hexout=0 then
                            argtext = sprintf("%d", arg_list[argn])
                            if zfill!=0 and width>0 then
                                if length(argtext)>0 then
                                    if argtext[1]='-' then
                                        if width>length(argtext) then
                                            argtext = '-' & repeat('0', width-length(argtext)) & argtext[2..$]
                                        end if
                                    else
                                        if width>length(argtext) then
                                            argtext = repeat('0', width-length(argtext)) & argtext
                                        end if
                                    end if
                                else
                                    argtext = repeat('0', width-length(argtext)) & argtext
                                end if
                            end if

                            if arg_list[argn]>0 then
                                if psign then
                                    if zfill=0 then
                                        argtext = '+' & argtext
                                    elsif argtext[1]='0' then
                                        argtext[1] = '+'
                                    end if
                                end if
                            elsif arg_list[argn]<0 then
                                if msign then
                                    if zfill=0 then
                                        argtext = '(' & argtext[2..$] & ')'
                                    else
                                        if argtext[2]='0' then
                                            argtext = '(' & argtext[3..$] & ')'
                                        else
                                            argtext = argtext[2..$] & ')'
                                        end if
                                    end if
                                end if
                            end if
                        else
                            argtext = sprintf("%x", arg_list[argn])
                            if zfill!=0 and width>0 then
                                if width>length(argtext) then
                                    argtext = repeat('0', width-length(argtext)) & argtext
                                end if
                            end if
                        end if

                    elsif atom(arg_list[argn]) then
                        if bwz!=0 and arg_list[argn]=0 then
                            argtext = ""
                        else
                            if hexout then
                                argtext = sprintf("%x", arg_list[argn])
                                if zfill!=0 and width>0 then
                                    if width>length(argtext) then
                                        argtext = repeat('0', width-length(argtext)) & argtext
                                    end if
                                end if
                            else
                                argtext = trim(sprintf("%15.15g", arg_list[argn]))
                                if zfill!=0 and width>0 then
                                    if length(argtext)>0 then
                                        if width>length(argtext) then
                                            if argtext[1]='-' then
                                                argtext = '-' & repeat('0', width-length(argtext)) & argtext[2..$]
                                            else
                                                argtext = repeat('0', width-length(argtext)) & argtext
                                            end if
                                        end if
                                    else
                                        argtext = repeat('0', width-length(argtext)) & argtext
                                    end if
                                end if
                                if arg_list[argn]>0 then
                                    if psign then
                                        if zfill=0 then
                                            argtext = '+' & argtext
                                        elsif argtext[1]='0' then
                                            argtext[1] = '+'
                                        end if
                                    end if
                                elsif arg_list[argn]<0 then
                                    if msign then
                                        if zfill=0 then
                                            argtext = '(' & argtext[2..$] & ')'
                                        else
                                            if argtext[2]='0' then
                                                argtext = '(' & argtext[3..$] & ')'
                                            else
                                                argtext = argtext[2..$] & ')'
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                        end if

                    else
                        if alt!=0 and length(arg_list[argn])=2 then
                            if atom(prevargv) then
                                if prevargv!=1 then
                                    tempv = arg_list[argn][1]
                                else
                                    tempv = arg_list[argn][2]
                                end if
                            else
                                if length(prevargv)=0 then
                                    tempv = arg_list[argn][1]
                                else
                                    tempv = arg_list[argn][2]
                                end if
                            end if

                            if string(tempv) then
                                argtext = tempv
                            elsif integer(tempv) then
                                if bwz!=0 and tempv=0 then
                                    argtext = ""
                                else
                                    argtext = sprintf("%d", tempv)
                                end if

                            elsif atom(tempv) then
                                if bwz!=0 and tempv=0 then
                                    argtext = ""
                                else
                                    argtext = trim(sprintf("%15.15g", tempv))
                                end if
                            else
--/**/                          argtext = ppf(tempv)                                            --/*
                                argtext = pretty_sprint(tempv,
                                                        {2,0,1,1000,"%d","%.15g",32,127,1,0}
                                                       )                                        --*/
                            end if
                        else
--/**/                          argtext = ppf(arg_list[argn])                                   --/*
                            argtext = pretty_sprint(arg_list[argn],
                                                    {2,0,1,1000,"%d","%.15g",32,127,1,0}
                                                   )                                            --*/
                        end if
                    end if
                    currargv = arg_list[argn]
                end if


                if length(argtext)>0 then
                    if cap='u' then
                        argtext = upper(argtext)
                    elsif cap='l' then
                        argtext = lower(argtext)
                    elsif cap='w' then
--DEV:
--                      argtext = proper(argtext)
                    elsif cap=0 then
                        -- do nothing
                        cap = cap

                    else
                        crash("logic error: 'cap' mode in format.")

                    end if

                    if atom(currargv) then
                        if find('e', argtext)=0 then
                            -- Only applies to non-scientific notation.
                            if decs!=-1 then
                                pos = find('.', argtext)
                                if pos then
                                    if decs=0 then
                                        argtext = argtext[1..pos-1]
                                    else
                                        pos = length(argtext)-pos
                                        if pos>decs then
                                            argtext = argtext[1..$-pos+decs]
                                        elsif pos<decs then
                                            argtext = argtext & repeat('0', decs-pos)
                                        end if
                                    end if
                                elsif decs>0 then
                                    argtext = argtext & '.' & repeat('0', decs)
                                end if
                            end if

                        end if
                    end if

                    if align=0 then
                        if atom(currargv) then
                            align = '>'
                        else
                            align = '<'
                        end if
                    end if

                    if atom(currargv) then
                        if tsep!=0 and zfill=0 then

                            if binout or hexout then
                                dist = 4
                            else
                                dist = 3
                            end if
                            bracketed = (argtext[1]='(')
                            if bracketed then
                                argtext = argtext[2..$-1]
                            end if
                            dpos = find('.', argtext)
                            if dpos=0 then
                                dpos = length(argtext)+1
                            else
                                if tsep='.' then
                                    argtext[dpos] = ','
                                end if
                            end if
                            while dpos>dist do
                                dpos -= dist
                                if dpos>1 then
                                    argtext = argtext[1..dpos-1] & tsep & argtext[dpos..$]
                                end if
                            end while
                            if bracketed then
                                argtext = '(' & argtext & ')'
                            end if
                        end if
                    end if

                    if width<=0 then
                        width = length(argtext)
                    end if


                    if width<length(argtext) then
                        if align='>' then
                            argtext = argtext[$-width+1..$]
                        elsif align='c' then
                            pos = length(argtext)-width
                            if remainder(pos, 2)=0 then
                                pos = pos/2
                                argtext = argtext[pos+1..$-pos]
                            else
                                pos = floor(pos/2)
                                argtext = argtext[pos+1..$-pos-1]
                            end if
                        else
                            argtext = argtext[1..width]
                        end if
                    elsif width>length(argtext) then
                        if align='>' then
                            argtext = repeat(' ', width-length(argtext)) & argtext
                        elsif align='c' then
                            pos = width-length(argtext)
                            if remainder(pos, 2)=0 then
                                pos = pos/2
                                argtext = repeat(' ', pos) & argtext & repeat(' ', pos)
                            else
                                pos = floor(pos/2)
                                argtext = repeat(' ', pos) & argtext & repeat(' ', pos+1)
                            end if

                        else
                            argtext = argtext & repeat(' ', width-length(argtext))
                        end if
                    end if
                    result &= argtext

                else
                    if spacer then
                        result &= ' '
                    end if
                end if

                if trimming then
                    result = trim(result)
                end if

                tend = 0
                prevargv = currargv
            end if
        end if
    end while

    return result
end function

--**
-- Get the text associated with the message number in the requested locale.
--
-- Parameters:
--   # ##MsgNum## : An integer. The message number whose text you are trying to get.
--   # ##LocalQuals## : A sequence. Zero or more locale codes. Default is {}.
--   # ##DBBase##: A sequence. The base name for the database files containing the
--                 locale text strings. The default is "teksto".
--
-- Returns:
-- A string **sequence**, the text associated with the message number and locale.\\
-- An **integer**, if not associated text can be found.
--
-- Comments:
-- * This first scans the database(s) linked to the locale codes supplied.
-- * The database name for each locale takes the format of "<DBBase>_<Locale>.edb"
-- so if the default DBBase is used, and the locales supplied are {"enus", "enau"}
-- the databases scanned are "teksto_enus.edb" and "teksto_enau.edb".
-- The database table name searched is "1" with the key being the message number,
-- and the text is the record data.
-- * If the message is not found in these databases (or the databases don't exist)
-- a database called "<DBBase>.edb" is searched. Again the table name is "1" but
-- it first looks for keys with the format {<locale>,msgnum} and failing that it
-- looks for keys in the format {"", msgnum}, and if that fails it looks for a
-- key of just the msgnum.
--
--/*
public function get_text(integer MsgNum, sequence LocalQuals = {}, sequence DBBase = "teksto")
integer idx = 1
integer db_res
object lMsgText

    db_res = -1
    lMsgText = 0
    -- First, scan through the specialized local dbs
    for i=1 to length(LocalQuals) do
        db_res = db_select(locate_file(DBBase & "_" & LocalQuals[i] & ".edb"), DB_LOCK_NO)
        if db_res=DB_OK then
            db_res = db_select_table("1")
            if db_res=DB_OK then
                lMsgText = db_fetch_record(MsgNum)
                if sequence(lMsgText) then
                    exit
                end if
            end if
        end if
    end for

    -- Next, scan through the generic db
    if atom(lMsgText) then
        db_res = db_select(locate_file(DBBase & ".edb"), DB_LOCK_NO)
        if db_res=DB_OK then
            db_res = db_select_table("1")
            if db_res=DB_OK then
                for i=1 to length(LocalQuals) do
                    lMsgText = db_fetch_record({LocalQuals[i],MsgNum})
                    if sequence(lMsgText) then
                        exit
                    end if
                end for
                if atom(lMsgText) then
                    lMsgText = db_fetch_record({"",MsgNum})
                end if
                if atom(lMsgText) then
                    lMsgText = db_fetch_record(MsgNum)
                end if
            end if
        end if
    end if

    return lMsgText

end function
--*/
