--
-- substitute.e
--
--  Phix implementation of substitute (auto-include)
--

global function substitute(string text, string s, string r)
-- replace all instances of s in text with r
integer k = 1, 
        l = length(s),
        startidx = 1
sequence chunks = {}
    while 1 do
        k = match(s,text,k)
        if k=0 then exit end if
        chunks = append(chunks,text[startidx..k-1])
        k += l
        startidx = k
    end while
    if length(chunks) then
        chunks = append(chunks,text[startidx..$])
        text = chunks[1]
        for i=2 to length(chunks) do
            text &= r
            text &= chunks[i]
        end for
    end if
    return text
end function

global function substitute_all(string text, sequence strings, sequence replacements)
    for i=1 to length(strings) do
        if string(strings) then
            if string(replacements) then
                -- in this case a naive in situ approach is faster:
                integer ch = strings[i], repch = replacements[i]
                for j=1 to length(text) do
                    if text[j]=ch then text[j] = repch end if
                end for
            else
                text = substitute(text,strings[i..i],replacements[i])
            end if
        else
            text = substitute(text,strings[i],replacements[i])
        end if
    end for
    return text
end function


