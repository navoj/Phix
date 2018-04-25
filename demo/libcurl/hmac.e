--
-- hmac.e - keyed-hash message authentication code routines
--
include sha256.e
include sha512.e
--
-- Some possible alternatives (just in case...):
--  http://forums.purebasic.com/english/viewtopic.php?t=47360&p=441076
--  https://www.nayuki.io/page/fast-sha2-hashes-in-x86-assembly
--
-- free online testers:
--  https://www.freeformatter.com/hmac-generator.html#ad-output
--  https://quickhash.com/
--
-- https://en.wikipedia.org/wiki/HMAC
--

global function hmac_digest(string s)
-- return s as a hex-string
string res = ""
    for i=1 to length(s)-3 by 4 do
        for j=i+3 to i by -1 do
            res &= sprintf("%02x",s[j])
        end for
    end for
    return res
end function

global
function rev4(string s)
string res = ""
    for i=1 to length(s)-3 by 4 do
        for j=i+3 to i by -1 do
            res &= s[j]
        end for
    end for
    return res
end function

function hmac(string key, message, integer hashrtn, blocksize, outputsize)
--   Inputs:
--    key:        Bytes     array of bytes
--    message:    Bytes     array of bytes to be hashed
--    hashrtn:    Function  the hash function to use (e.g. routine_id("sha256"))
--    blocksize:  Integer   the block size of the underlying hash function (e.g. 64 bytes for SHA-1)
--    outputsize: Integer   the output size of the underlying hash function (e.g. 20 bytes for SHA-1)
            
    -- Keys longer than blocksize are shortened by hashing them
    if length(key)>blocksize then
        key := call_func(hashrtn,{key}) -- Key becomes outputsize bytes long
        if length(key)!=outputsize then ?9/0 end if -- sanity check
    end if
   
    -- Keys shorter than blocksize are padded to blocksize by padding with zeros on the right
    if length(key)<blocksize then
        -- pad key with zeros to make it blocksize bytes long
        key &= repeat('\0',blocksize-length(key))
    end if
    if length(key)!=blocksize then ?9/0 end if
    
    string i_key_pad = sq_xor_bits(key,0x36)    -- Inner padded key
    string o_key_pad = sq_xor_bits(key,0x5c)    -- Outer padded key
    
    string res = call_func(hashrtn,{o_key_pad & rev4(call_func(hashrtn,{i_key_pad & message}))})

    return res
end function

global function hmac_sha256(string key, message)
    return hmac(key,message,routine_id("sha256"),blocksize:=64,outputsize:=32)
end function

global function hmac_sha512(string key, message)
    return hmac(key,message,routine_id("sha512"),blocksize:=128,outputsize:=64)
end function

