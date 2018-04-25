--format "p.fmt"    --NO (6/3/14)
--DEV/SUG:
-- constant(/.ini setting) showIdxOnLongSequences = 0/1,
--                         sIOLSover=50 (0=all)
--  ie/eg   symtab[1] = {blah}
--          symtab[2] = {blah}
--          ...
--          symtab[19] = 0
--          ...
--          symtab[4993] = {blah}
--  and/or:
--      showFullIdxOnVarsNamed = {"symtab",...}
--  and/or:
--      constant/.ini maxShownIdx = 2000
--  also, the constant maxlen below should be moved if/when we
--      create(/extend) any .ini file...

-- Demo for above, much simpler than I thought it would be:
--constant MAXLEN=20
--procedure errorise(sequence s, string name)
--object si
--  if string(s) then
--      if length(s)>MAXLEN then
--          s = s[1..MAXLEN]&".."
--      end if
--      printf(1,"%s = \"%s\"\n",{name,s})
--  else
--      for i=1 to length(s) do
--          if i>MAXLEN then
--              printf(1,"%s[%d..%d] = ...\n",{name,i,length(s)})
--              exit
--          end if
--          si = s[i]
--          if atom(si) then
--              if integer(si) then
--                  if si>=' ' and si<=#255 then
--                      printf(1,"%s[%d] = %d'%c'\n",{name,i,si,si})
--                  else
--                      printf(1,"%s[%d] = %d\n",{name,i,si})
--                  end if
--              else
--                  printf(1,"%s[%d] = %s\n",{name,i,sprintf("%.10g",si)})
--              end if
--          else
--              errorise(si,sprintf("%s[%d]",{name,i}))
--          end if
--      end for
--  end if
--end procedure
--sequence thing
--  thing = {1,2,3,"string",{'a','d','c'},"ad c"&-1,44.557,1e217/7}
--  thing = prepend(thing,thing)
--  thing = prepend(thing,thing)
--?thing
--pp(thing)
--errorise(thing,"thing")
--errorise("thing","thing")
--thing = repeat('x',2000)
--errorise(thing,"string2000")
--if getc(0) then end if
--
-- pdiag.e
-- =======
--
-- code responsible for writing the ex.err file
--
-- Technical note:
--  This should be coded as defensively as possible, rather than relying on
--  itself to give meaningful messages about errors in itself. In other words,
--  pdiag.e should not rely on pdiag.e to catch runtime errors. Strangely, it
--  tends to manage better than expected, but even so that does not make it
--  a good idea, or mean you should be surprised, when it self-implodes.
--  This means (without going overboard) that variables should be defined as 
--  object and then explicitly tested for the expected type, instead of being
--  declared as the expected type and relying on the builtin type checking, 
--  that all subscripts should be explicitly tested to be in range, and that  
--  all peeks are checked first with xIsBadReadPtr, plus anything else that
--  you can think of!
--
--  If you get any error message (before you start hacking this code) which 
--  begins with "diag.e: oops," then please reduce the program to the smallest
--  possible one which still exhibits the error and send it to me. Since this
--  has been passed a nonsense [era] or [ep1], there is nothing you can do to 
--  improve matters here; it is a low-level bug that I alone must fix, sorry.
--  The same is true for line numbers of -1, except of course when an error
--  is being reported in a dll, or some non-#ilasm assembly code, or maybe
--  when "without debug" is in force at the point where the error occurred.
--
--include builtins\pmt.e    -- test pmach.e (passed with flying colours)
--
--global constant diagBase = 2  -- temp, checked by p.exw to match newBase

-- See also pmsgs.e, which is responsible for the two-liners created by
--  compile-time errors.
--
-- This file is automatically included as part of any exe file.
--
-- ******************************************
-- ******************************************
-- *****  WARNING: FRAGILE CODE AHEAD!  *****
-- ******************************************
-- ******************************************
--
-- Needless to say, I hope, take extra care here as if this should crash,
-- well, it will probably crash while trying to report the bug in itself...
--
-- When interpreting, errors in the user app are handled by the copy of 
--  pdiag.e in p.exe, which is also sitting ready to handle any errors
--  in the p.exw it was compiled from, whereas, of course, all compiled
--  applications must be shipped with their own private copy. This also
--  means you CANNOT "edit/test" this code in interpreted mode; instead
--  you MUST use -c to actually execute any modifications. "p -c test"
--  is strongly advised as opposed to using "p -c p" for testing, since
--  the latter may confuse by always being "one-step-out-of-date" (plus
--  of course you really do not want a broken p.exe lying around).
--
-- In general I would say there is no way to trace() this file, in any
--  case not when it is actually handling a crash. While theoretically
--  it may be possible to fudge a partial trace "in situ", it is going
--  to be far easier if you just copy/paste/rename and try out any new
--  ideas in some other/new/temporary file.
-- In many cases I have needed to add a slew of console displays to
--  narrow down the location of a bug in this code.
-- Of course it took me a while to figure out the proper way to code
--  this is to test everything and leave clues in the [partial] ex.err
--  should anything go wrong, such as those at the start of getValue().
--
-- TIP: In some cases where "p -c test" does not seem to want to work,
--  "p p -c test" may do the trick, particularly when adding/changing 
--  an opcode or making some other change to the p.exw sources.
-- A favourite trick is to replace the "if bind then" in Compile() in
--  pmain.e with "if 0 then", and [perhaps] manually include pdiag.e 
--  in a test program, to force the issue. YMMV, though.
--
-- TO DO: [DONE, I think]
--  Unify compile-time and run-time file handling so that the 
--  warning messages (from pmsgs.e) can be written to a run-time
--  .err report. Or perhaps just re-open it in append mode?
--
--/**/without debug -- removal may or may not ease debugging!
                    -- (this option probably makes v. little difference
                    --  here, but see without type_check below.)
-- NB: the above "without debug" propagates into ppp.e and prntf.e, 
--     since they are used in the following code.

--/**/  -- not really needed, but avoids an opCallOnce:
--/**/  include builtins\pcfunc.e
--/**/  include builtins\pprntf.e
--/**/  include builtins\psprint.e
--/**/  include builtins\platform.e
--/**/  include builtins\pcurrdir.e
--/**/  include builtins\pcase.e

--include pgets0.ew     --DEV removed 16/6/08...
include builtins\ppp.e
-- 01/08/2013:
include builtins\pdelete.e

without type_check  -- NB. This code is just too low-level.
-- If you remove the above in the hope that it will help, you will 
--  probably be disappointed. You tend to get eg:
--      diag looping (minimal diagnostics follow); error code is:30
--      ep1 is C0000005
--      ep2 is 00000000
--      era is 00409E9B
--      ern is 3186

constant swod = 0 -- 1=show without debug routines and vars

-- use the bigger values to get full-length var dumps
--  (std release should NOT have long settings both to save time,
--   specifically when big vars get displayed on the console, and
--   to avoid showstoppers after phroom/disk full errors/etc)
--constant maxlen = 500
constant maxlen = 50000

--
-- *NB* These must be kept in very strict order, never delete or insert entries.
--
constant msgs =
{
 "type check failure, %s is %s\n",                              -- e01tcf
    -- As called from opTchk, when var-id is known (idx in ep1).
    -- See also e110tce, called when var_id not known (addr in ep1). [DEV may no longer be used]
 "attempt to divide by 0\n",                                    -- e02atdb0
 "true/false condition must be an ATOM\n",                      -- e03tfcmbaa
    -- Usually only happens on "if x then" where x is not 
    -- a relational expression (eg a=b) but is either a 
    -- single variable or a function result. (unlike RDS)
    -- see also e14NNsoXa. Note this message may not occur
    -- on subscripted items when a program is compiled, eg
    -- if x is {1,2,"fred",4} then "if x[3] then" is just 
    -- treated as true (not zero), though you should get
    -- an error when the same code is interpreted. This is
    -- a deliberate optimisation.
--DEV maybe we shouldn't inline unless it's a sequence of integer?
 "attempt to subscript an atom\n",                              -- e04atsaa
 "subscript is not an atom\n",                                  -- e05sinaa
 "index %d out of bounds, assigning to sequence length %d\n",   -- e06ioob
 "slice start is less than 1 (%d)\n",                           -- e07ssilt1
    -- Note that the value shown is that after adjustment 
    -- for negative indexes, eg if length(x) is 10, then
    -- x[-11..10] will complain ssilt1 (0) as -11 maps to 0.
    -- see e10sspeos. In fact e07ssilt1 only ocurs for 0. [Erm? DEV test that]
    -- Obviously if the slice start is a variable, rather
    -- than an expression, the "true" value can be found
    -- elsewhere in the ex.err file.
 "slice end is not an integer\n",                               -- e08seinai
 "slice length is negative [%d..%d]\n",                         -- e09slin
    -- values shown are as adjusted for negative indexes, [DEV?]
    --  eg if length(s)=4, then s[-1..-3] shows as [4..2]
    -- see also comments against e07ssilt1.
 "slice starts past end of sequence (%d > %d)\n",               -- e10sspeos
    -- or slice start(%d) less than negative length (%d+%d+1=%d), see below
 "slice ends past end of sequence (%d > %d)\n",                 -- e11sepeos
    -- or slice end(%d) less than negative length (%d+%d+1=%d), see below
 "program aborted\n",                                           -- e12pa
    -- Operator has typed '!' in the trace() window.
 "attempt to exit a function without returning a value\n",      -- e13ateafworav
    -- For an example of why this cannot/should not be trapped 
    -- as a compile-time error, see isChecked() in arwen.ew.
 "sequence op (%s) attempted (use sq_%s?)\n",                   -- e14NNsoXa
    -- Phix does not support implicit/infix sequence ops;
    -- you must use explicit function-style calls, ie/eg
    -- replace "{1,2}+3" with "sq_add({1,2},3)" to get {4,5}.
    --  (Acutally, in the name of compatibility with legacy code,
    --   it will replace some of the most blatently obvious cases,
    --   see sqopWarn in p.exw/pmain.e)
    -- Note that name="Pete" yields 1 or 0 (True/False) on Phix,
    -- instead of eg {0,1,0,1}, "sequence lengths not the same",
    -- or the infamous "true/false condition must be an ATOM".
    -- (the latter can still happen, just nowhere near as often)
    -- Forcing "+" to be replaced with "sq_add" is better, IMNSHO, than
    -- forcing "=" to be replaced with "equal", as happens with RDS Eu.
    -- (nb some legacy code may need "=" to be replaced with "sq_eq")
    -- Also the compile-time errors "type error (use sq_add?)" et al
    -- catch a significant number of cases before it gets to this.
    -- Lastly, there is no sense, for example, in changing the infix
    -- relational ops (<,<=,=,!=,>=,>) to always return a boolean but
    -- still allowing maths ops (+,-,*,/) to do sequence ops. This 
    -- would spanner all legacy code even more, for example the old
    -- upper/lower would work fine on chars but leave all sequences/
    -- strings completely unchanged. It is far more helpful to sound
    -- this alarm than silently go wrong.
 "unrecognised c_func return type\n",                           -- e15ucfrt
    -- Note that C_FLOAT, E_INTEGER, E_ATOM, and E_OBJECT have not
    --  been attempted/tested and hence report this error.
    -- (for the latter 3 I require a suitable RDS-Eu-compiled DLL)  --DEV
    -- BTW: E_INTEGER, E_ATOM, E_SEQUENCE, and E_OBJECT are only
    --  used for RDS-Eu-compiled DLLs, new values (P_XXX?) will
    --  have to be devised for Phix-compiled DLLs, if/when that
    --  becomes possible.
 "call_backs cannot have optional parameters\n",                -- e16cbchop
    -- There is no way for Phix to determine how many parameters
    --  some C/asm/other language has pushed onto the stack, shy 
    --  of entire program dissassembly/analysis that is, and not
    --  that I have ever seen a callback with anything other than
    --  a fixed number of parameters (and if I ever did, then my
    --  answer would be a separate "call_back_var_args" routine).
    -- If you want optional/defaulted parameters for the benefit
    --  of other Phix code, then you may need a "thin wrapper":
    --  function varfunc(a=?, b=?,...)
    --      ....
    --  end function
    --  ---nono = call_back(routine_id("varfunc")) -- this error
    --  function fixfunc(a,b,...)
    --      return varfunc(a,b,...) -- a "thin wrapper"
    --  end function
    --  cb_xx = call_back(routine_id("fixfunc"))
    -- Hence you can call varfunc with more or less parameters,
    --  whereas cb_xx is always invoked with a known fixed set.
 "sequence op (mul) attempted (use sq_mul?)\n",                 -- e17soma      -- """"""""""""""" --
 "sequence op (remainder) attempted (use sq_remainder?)\n",     -- e18sora      -- """"""""""""""" --
 "sequence op (floor) attempted (use sq_floor?)\n",             -- e19sofa      -- """"""""""""""" --
 "invalid match start index\n",                                 -- e20imsi
 "invalid find start index\n",                                  -- e21ifsi
    -- In find('3',"123",s), s of 1..3 and -1..-3 yield 3,
    --  4 yields 0, but all other values, including non-atoms,
    --  unassigned variables, and s<=-4, yield this error.
    --  Of course -1, being shorthand for length(), is the 
    --  same as 3 in the above, and -3 is the same as 1.
    -- Aside: find('.',filename,-5) could be used to quickly
    --  find a file extension of 4 or less characters. While
    --  length+1 can be helpful, as resuming on lastresult+1
    --  is a common idiom, there is no similar equivalent for
    --  negative subscripts. It may turn out that just simply
    --  ignoring bad (integer) starts and returning 0 makes
    --  for an easier life, I could easily do that if the 
    --  common consus suggests it would be better, though it
    --  seems to me more likely to catch bugs/typos this way.
 "invalid mem_copy length\n",                                   -- e22imcl
    -- number of bytes to copy is negative
 "invalid mem_set length\n",                                    -- e23imsl
    -- number of bytes to set is negative
    --  (ditto)
 "invalid mem_copy memory address\n",                           -- e24imcma
    -- a machine exception occurred in a mem_copy operation
 "invalid mem_set memory address\n",                            -- e25imsma
    -- a machine exception occurred in a mem_set operation
 "invalid argument type for integer := peek()\n",               -- e26iatfpi
    -- Occurs, for example, in:
    --  integer i
    --      i = peek({addr,4}).
    -- Technically speaking, opPeeki is called (if not inlined,
    --  that is) instead of the normal opPeek, because it knows
    --  the result ought to be an integer. The former has no
    --  dealloc code, and no code for the above, so instead it
    --  displays this message. Arguably, it should perform the
    --  peek anyway, then typecheck - but that would only make
    --  things slower. Also arguably, the above should fail to
    --  compile, though we would still have to handle the more
    --  general i=peek(object) case with this run-time message.
 "argument to rand() must be >= 1\n",                           -- e27atrmbge1
 "argument to %s() must be an atom (use sq_%s?)\n",             -- e28NNatXmbausq
 "argument to set_rand() must be an atom\n",                    -- e29atsrmba
 "fatal exception %s at #%08x\n",                               -- e30ume
    -- Unknown machine error.
    --
    -- It is pretty much the job of this program, with help
    -- from the back end, to map such errors in Phix hll
    -- code to human-readable form. As per the note above,
    -- when an error occurs in some dll/asm code, this is
    -- about the best I can do (with a line no of "-1", unless
    -- it is part of a #ilasm statement), and hopefully there 
    -- are enough clues later on in the ex.err to guide you 
    -- towards solving the problem. However this message should 
    -- not occur for "pure hll code".
    --
    -- There are literally thousands of places in the backend 
    -- where it catches/maps exceptions, and without any doubt
    -- there will be several left that were accidentally missed.
    --
    -- Generally speaking, addresses in the range #00400000 
    -- to #0040C000 indicate a problem in the back-end, please
    -- contact the author (Pete Lomax) for assistance, and/or
    -- see plist.e, flag dumpVM/the list2.asm that creates.
    --
 "memory corruption: eax is #%08x, edx is #%08x\n",             -- e31mce
    -- only occurs on debug builds
 -1,                                                            -- e32 no longer in use
 "argument to arctan() must be atom (use sq_arctan?)\n",        -- e33atatmba   -- no longer in use (see e28)
 "power() function underflow\n",                                -- e34pfu
    -- result is less than -1.7976931348623146e308
    -- (technically the term underflow is usually
    --  used to mean "too near zero", btw, which
    --  just makes power() quietly return a zero.)
 "power() function overflow\n",                                 -- e35pfo
    -- result is more than +1.7976931348623146e308
 "length of an atom is not defined\n",                          -- e36loaaind
 "argument to allocate() must be positive integer\n",           -- e37atambpi
 "argument to free() must be an atom\n",                        -- e38atfmba
 "arguments to mem_copy() must be atoms\n",                     -- e39atmcmba
 "arguments to mem_set() must be atoms\n",                      -- e40atmsmba
 "first argument to poke() must be atom\n",                     -- e41fatpmba
 "first argument to poke4() must be atom\n",                    -- e42fatp4mba
 "argument to peek() must be atom or sequence of two atoms\n",  -- e43atpmbaoso2a
 "argument to peek4s() must be atom or sequence of two atoms\n", -- e44atpmbaoso2a
 "argument to peek4u() must be atom or sequence of two atoms\n", -- e45atpmbaoso2a
 "argument to float32_to_atom() must be sequence of length 4\n", -- e46atf32tambsol4
 "argument to float64_to_atom() must be sequence of length 8\n", -- e47atf64tambsol8
    -- btw, the above messages occur for an unassigned argument, rather
    --  than the usual e92/"variable xxx has not been assigned a value".
-- "argument to chdir() must be string\n",                      -- e48atcdmbs
 -1,                                                            -- no longer in use
 "argument to atom_to_float32() must be atom\n",                -- e49atatf32mba
 "argument to atom_to_float64() must be atom\n",                -- e50atatf64mba
 "HeapFree error code [%08x]\n",                                -- e51hfec
    -- Should not happen. Suggests that your program has
    -- corrupted memory, the operating system free chain, 
    -- for instance. Try using safe.e (see that file for
    -- instructions) and/or a debug version of p.exe. -- DEV
    -- Make a copy of the program source, then repeatedly
    -- delete as many lines as possible while the error
    -- still occurs. If you can get it to under 100 lines
    -- (program no longer has to do anything useful, btw)
    -- then you can submit it for further investigation.
 "repeat count must be non negative integer\n",                 -- e52rcmbnni
 -1,--"for loop error\n",                                       -- e53fle           --DEV see e120,1
 "attempt to raise negative number to non-integer power\n",     -- e54atrnntnip
    -- mathematically, power(-3,-3) is an imaginary number.
 "first argument to append() must be sequence\n",               -- e55fatambs
    -- You may mean a&b instead of append(a,b)
    -- Note that append("one","two") is {"one","two"},
    -- whereas "one"&"two" is "onetwo", although they 
    -- give the same results if b is an atom.
 "first argument to prepend() must be sequence\n",              -- e56fatpmbs
    -- You may mean b&a instead of prepend(a,b)
    -- Note that prepend("two","one") is {"one","two"},
    -- whereas "one"&"two" is "onetwo", although they 
    -- give the same results if b is an atom.
 "invalid file name\n",                                         -- e57ifn
    -- A common cause of this is using append instead of &:
    --  append("C:\test\","fred.txt") returns the nested
    --  {'C',':','\','t','e','s','t','\',"fred.txt"}, whereas
    --  "C:\test\"&"fred.txt" returns "C:\test\fred.txt".
    -- Remember that "append(s,x)" always returns a sequence (or 
    --  string) of length(s)+1, whereas "s&x" returns a sequence 
    --  (or string) of length(s)+length(x) [that is, except when 
    --  x is an atom, in which case they are equivalent].
 "invalid file number (%d)\n",                                  -- e58bfn
    -- file must be open for getc, puts, seek, where, etc.
 "wrong file mode for attempted operation\n",                   -- e59wfmfao
    -- eg attempt to read a file after open(x,"w").
 "file number is not an integer\n",                             -- e60fninai
    -- this error is also common for unassigned vars.
 "invalid open mode\n",                                         -- e61iom
    -- second parameter to open must be (r|w|a|u)[b].
    -- BTW, Phix allows single-character modes, eg 'r',
    -- whereas RDS Eu does not.
 "file number %d is not open\n",                                -- e62fnnino
 "second parameter of seek() must be an atom\n",                -- e63sposmba
 "seek fail on open append\n",                                  -- e64sfooa
    -- after successfully opening a file for append
    -- (fn=open(xxx,"a")), it automatically seeks to
    -- the end of file. This seek has failed.
    -- This should not happen, maybe you found a bug,
    -- or maybe your hard drive has errors.
 "sequence found in character string\n",                        -- e65sfics
    -- second parameter to puts or [s]printf may not
    -- contain nested sequences. See e55/56/57, or
    -- try using pp(), ppf(), ?, or [s]print().
 "invalid lock type\n",                                         -- e66ilt
 "byterange must be {} or pair of atoms\n",                     -- e67bre
 -1,--"argument to dir() must be string\n",                     -- e68atcdmbs (not actually used/see pdir.e)
    -- See e73atodmbs
 "error in format string\n",                                    -- e69eifs (see pprntf.e/badfmt())
    -- Missing or unrecognised format character after a '%',
    --  eg "%", "%3.2", "%q". See also e73atodmbs.
 "insufficient values for (s)printf()\n",                       -- e70ivfpf
 -1,--"argument to getenv() must be string",                    -- e71atgmbs (not actually used/see penv.e)
    -- See e73atodmbs
 "invalid routine_id(%d)\n",                                    -- e72iri
    -- The first argument to call_proc/func, or call_back (which
    -- can also accept {'+',rtnid} as the first argument) is not
    -- an integer, is not in the range 1..length(symtab), or
    -- symtab[i] is not a type, function, or procedure. Usually 
    -- occurs after a previous call to routine_id, define_c_func, 
    -- etc returned -1.
--DEV++
 "argument to open_dll() must be string\n",                     -- e73atodmbs
    -- Either the parameter is not a sequence, or some element
    -- of it is not a character. Note that strings and flat
    -- dword sequences are equally acceptable, eg/ie "kernel32"
    -- or {'k','e','r','n','e','l','3','2'} work the same.
 "define_c_func/proc parameter error\n",                        -- e74dcfpe
    -- the first argument to define_c_func/proc is:
    --   an atom, and the second is either unassigned,
    --                       a sequence of length zero,
    --               or a sequence containing non-chars, or
    --   a sequence, with non-zero length, or the second
    --               parameter is unassigned or sequence.
    -- ie the legal forms of define_c_func/proc are:
    --      define_c_func/proc(atom,name,...)
    --      define_c_func/proc({},addr,...)
 "call back routine parameters must all be atoms\n",            -- e75cbrpmaba
 "%c requires an atom value\n",                                 -- e76pcraav
 "program has run out of memory\n",                             -- e77phroom
 "attempt to get_text() >1GB file\n",                           -- e78atgtgt1gbf
    -- You *can* read very large files line-by-line, or 
    --  byte-by-byte, or via seeks, but *not* load the 
    --  whole thing into memory at once (1GB is about
    --  300 copies of the bible, a lot of text).
 -1,                                                            -- e79 no longer in use
 -1,                                                            -- e80 no longer in use
 "insufficient parameters in call_func/proc()\n",               -- e81ipicfp
    -- second argument to call_func/proc must be a sequence
    -- containing at least the number of non-defaluted elements 
    -- declared as parameters for the specified routine.
 "argument to call() must be atom\n",                           -- e82atcmba
    -- Note that Phix allows a call() to a call_back()
    -- whereas RDS Eu suffers a machine exception.
 "arguments to position() must be integer\n",                   -- e83atpmbi
 "call_back parameter must be routine_id or {'+',routine_id}\n", -- e84cbpmbropr

 -1,                                                            -- e85 no longer used
 "argument to trace() must be integer 0..3\n",                  -- e86attmbi03
    -- technically -1 is also valid, and implements the same as
    -- keying 'Q' in the trace() window, ie permanently off.
 "abort() code must be integer\n",                              -- e87acmbi
 "arguments to c_func() must be atoms or strings\n",            -- e88atcfpmbaos
 -1,                                                            -- e89 no longer used
 "argument to profile() must be 0 or 1\n",                      -- e90atpmb01
 "profile internal error\n",                                    -- e91pie
 "variable %s has not been assigned a value\n",                 -- e92vhnbaav
 "too many parameters in call_func/proc()\n",                   -- e93tmpicfp
    -- second argument to call_func/proc must be a sequence
    -- containing no more than the number of elements declared 
    -- as parameters for the specified routine.
 "variable %s has not been assigned a value\n",                 -- e94vhnbaav as e92 but ep1 is varno
--DEV these appear untested:::
 "text_color error [%08x]\n",                                   -- e95tce
 "bk_color error [%08x]\n",                                     -- e96bce
 "heap error [%s]\n",                                           -- e97he
 "flush error [%s]\n",                                          -- e98fiofe
    -- internal kernel32 WriteFile failure when writing the
    -- contents of a file buffer. Code is from Microsoft.
    -- Unlikely, should not happen, maybe a scandisk
    -- is needed, maybe your hard drive is failing...
 "invalid peek memory address\n",                               -- e99ipma
    -- A MEMORY VIOLATION (#C0000005) exception occured when
    --  trying to read from the supplied memory address.
    -- Can occur on peek(), peek4s(), and peek4u() calls, for
    --  example if you free(mem) then attempt to peek(mem),
    --  or supply some impossible length in peek({mem,len}).
 "invalid poke memory address\n",                               -- e100ipma
    -- A MEMORY VIOLATION (#C0000005) exception occured when
    --  trying to write to the supplied memory address.
    -- Can occur on poke() and poke4() calls, for example if
    --  you free(mem) then attempt to poke(mem,x), or if x is
    --  much longer than the memory allocated.
 "attempt to allocate string of negative length\n",             -- e101atasonl
    -- internal error in the back end. The only way application
    -- code could attempt something similar is repeat(' ',-n),
    -- which is caught as e52rcmbnni before getting this far,
    -- and, e37atambpi handles -ve values passed to allocate().
    -- NB: a line no of -1 is expected should this occur; there
    --     is no known way to deliberately cause this error.
    -- (this message was added to catch bugs in gets().)
 "attempt to raise 0 to power <= 0\n",                          -- e102cr0tple0
 "attempt to get remainder of a number divided by 0\n",         -- e103atgrondb0
 "call back error?\n",                                          -- e104cbe [DEV no longer used]
 "not enough format strings to print data\n",                   -- e105nefstpd (pprntf.e only)
    -- May be removed for compatibility reasons, see pprntf.e.
 "index %d out of bounds, reading sequence length %d\n",        -- e106ioob
 -1,                                                            -- e107 - DEV no longer used
 "position error [%s]\n",                                       -- e108pe
    -- Maybe the co-ordinates specified are outside the boundaries
    -- of the (Windows) screen buffer. See also e83atpmbi, which
    -- occurs for attempts to position at negative coordinates.
 "clear_screen error\n",                                        -- e109cse
    -- Internal error, should not happen (and in fact this
    --  message has never been successfully triggered)
 "type check failure, %s is %s\n",                              -- e110tcf  (DEV not used in newEBP?)
    -- as e01tcf but ep1 is var addr not idx
 "bitwise operations are limited to 32-bit numbers\n",          -- e111bolt32b
    -- DEV: it may be sensible to permit and_bits(x,#FFFFFFFF),
    -- or in fact any and_bits op where either param is 32-bit:
    -- In the case of and_bits, this message only occurs if both
    -- arguments are larger than 32 bits.
 "second argument of find() must be a sequence\n",              -- e112saofmbs
 "second argument of match() must be a sequence\n",             -- e113saommbs
 "sequence to be poked must only contain atoms\n",              -- e114stbpmoca
 "argument to sleep() must be atom\n",                          -- e115atsmba
 "routine requires %d parameters, not %d\n",                    -- e116rrnp
    -- either the define_c_func/proc statement is wrong, or
    --  the c_func/proc statement is wrong.
 "routine does not return a value\n",                           -- e117rdnrav
    -- typically this means the program is using c_func
    --  to invoke a routine defined using define_c_proc
 "routine returns a value\n",                                   -- e118rrav
    -- typically this means the program is using c_proc
    --  to invoke a routine defined using define_c_func
--DEV this should go:
 "file number is not an integer or {fn,c}\n",                   -- e119fninaiofnc
    -- gets() can accept an integer file number or a sequence
    -- {fn,c} to read an entire text file in one operation.
    --  fn is an open file (>2) with filepos 0, and c is one of:
    --  -2: read file as one long string, with embedded '\n',
    --  -1: return sequence of '\n'-stripped lines,
    --   0: return sequence of lines as-is,
    --   1: return sequence of lines with '\n' forced on last.
    -- While gets({fn,c}) may be a useful shorthand for small
    --  files, it is not recommended for anything over 1MB.
    --  When processing larger files, a strategy which reads
    --  a chunk (or just one line), processes it and discards
    --  it before moving onto the next is much more likely to
    --  achieve better performance than reading the whole hog
    --  into memory at the start. Also, here are three sound
    --  reasons not to meddle with working legacy code:
    --      * You may introduce bugs
    --      * It is unlikely to be noticeably faster
    --      * It will be incompatible with RDS Eu
 "for loop error, %s is %s\n",                              -- e120fle
    -- Phix does not permit floating point for loops, since
    -- they do not work (eg on RDS Eu, try for x=1.1 to 1.3 
    -- by 0.1 do ?x end for; you only get 1.1 and 1.2 output).
    -- Replace eg 'for x=1.0 to 2.0 by 0.1 do ... end for'
    -- with 'atom x=1.0 for j=10 to 20 do ... x+=0.1 end for'
    -- Can also be triggered by using large integers.
    -- The "illegal expression type" compile-time error also 
    -- helps to catch most such problems in legacy code.
    -- NB: ep1 is init value (not var no)
 "for loop error, limit is %s, step is %s\n",                   -- e121flelimstep
    -- As above, Phix does not permit floating point for loops.
    -- This extends to final values, for example if you get
    --  for loop error, limit is 900,000,000, step is 800,000,000
    -- then it is because 1,700,000,000 is > 1,073,741,823.
    -- NB: ep1 is limit value, ep2 is step value (no var nos)
 -1}

constant e14ops = {"add","sub","div","mul","remainder","floor","unary minus","not",
                   "and_bits","or_bits","xor_bits","not_bits","power","xor"},
         e28ops = {"rand","cos","sin","tan","arctan","log","sqrt"}

--DEV use NTdesc from pglobals.e?:
constant rtndescs = {"type","function", "procedure"}

--
-- Symbol table constants/structure
--  duplicates from pglobals.e, needed for bound .exes
--   (there are *no globals* in this file, unless you count
--    external refs to printf/sprintf/stuff from ppp.e)
--
constant S_Name = 1,    -- const/var/rtn name
         S_NTyp = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_FPno = 3,    -- File and Path number
         S_State = 4,   -- state flag. S_fwd/S_used/S_set
--       S_Nlink = 5,   -- hash link
         S_Slink = 6,   -- localscopeX link
-- constants and variables [S_NTyp<=S_TVar]
--       S_vtype = 7,   -- variable type or namespace fileno
--       S_value = 8,   -- value
         S_Tidx = 9,    -- thread idx (S_NTyp=S_Tvar only)
--       S_ErrV = 10,   -- {'v', file, line, col}; see pmain.e[-35]
--       S_Init = 11,   -- Initialised chain (known init if non-0/see S_Const note below)
-- routines [S_NTyp>=S_Type]
--       S_sig  = 7,    -- routine signature
         S_Parm1 = 8,   -- first parameter. (idx to symtab, follow S_Slink)
--       S_ParmN = 9,   -- minimum no of parameters (max is length(S_sig)-1)
--       S_Ltot = 10,   -- total no of parameters, locals, and temporary vars
                        -- (needed to allocate the stack frame space)
         S_il   = 11,   -- intermediate code
         S_ltab = 12,   -- line table
         S_1stl = 13--, -- first line
--       S_Efct = 14,
--       S_ErrR = 15    -- {'R', file, line, col}; see pmain.e[-60]


constant S_Const = 1,   -- symtab[i][S_NTyp] values
         S_GVar2 = 2,   -- global or static variable
         S_TVar3 = 3,   -- temp or threadstack (local) variable/parameter
--       S_Nspc = 4,    -- namespace
--       S_Rsvd = 5,
         S_Type = 6,    -- Type of thermal yellow portable encryptor
--       S_Func = 7,    -- Function of finding unusual nonsense comments
--       S_Proc = 8     -- Procedure for private rotating obstacle counter
         K_wdb = #100 -- with debug setting

constant T_pathset = 16,
         T_fileset = 17,
         T_callstk = 20,
         T_maintls = 21,
--       T_const1  = 24
--       T_const1  = 25
         T_const1  = 26

--DEV should this just be a parameter to getVal?
integer lc  -- limit counter (set to 500)
integer showellipse -- set if lc blown
integer novalue

constant
    kernel32 = open_dll("kernel32.dll"),

    C_PTR = C_POINTER,

--#without reformat
    xIsBadReadPtr = define_c_func(kernel32, "IsBadReadPtr",
        {C_PTR,     --  CONST VOID  * lp,   // address of memory block
         C_INT},    --  UINT  ucb   // size of block
        C_INT)      -- BOOL
--#with reformat

function getVal(atom addr)
object  result,
        o
integer vtyp, len, keep
    novalue = 0         -- control flag, to prevent ppExf of "<no value>" result
    result = peek4s(addr)   --DEV crash here after e91pie
    if result<#40000000 then    -- a 31-bit integer
        return result
    end if
    result -= #40000000
    if result=0 then
        novalue = 1
        return "<no value>"
    end if
    addr = result*4

    if c_func(xIsBadReadPtr,{addr,1}) then
        result = sprintf("<**diag.e: bad ptr** (#%08x)>\n",addr)
        puts(1,result)
        return result
    end if

    vtyp = peek(addr-1)
    if vtyp=#12 then        -- a 64-bit float
        result = peek({addr,8})
        return float64_to_atom(result)
    end if
    len = peek4s(addr-12)
--  if not diagBase then    -- (old style handling)
--      addr = peek4u(addr-20)
--  end if
    if vtyp=#82 then        -- an 8-bit ascii string
        if len>lc then
            len = lc
            lc = 0
            showellipse = 1
        end if
        return peek({addr,len})
    end if
    if vtyp!=#80 then       -- sanity check: must be a sequence then.
        novalue = 1
        result = sprintf("<**GARBAGE/CORRUPT TYPE BYTE** (#%02x at [#%08x])>\n",{vtyp,addr-1})
        puts(1,result)
        return result
    end if
    result = {}
    while len and lc do
        lc -= 1
        len -= 1
--#without reformat
if 0 then -- new code 12/6/10 (show more of eg allfiles)
        if lc>99 and len and showellipse=0 then
            keep = lc-99
            lc = 99
            o = getVal(addr)
            if showellipse then
                if string(o) then
                    o &= "..."
                    showellipse = 0
                else
                    -- (oops!) quit, so ellipse shows near the break
                    -- (which is added in getValue, once outta here)
                    exit
                end if
            end if
            result = append(result,o)
            lc += keep
        else
            result = append(result,getVal(addr))
        end if
else
        result = append(result,getVal(addr))
end if
--#with reformat
        addr += 4
    end while
    if len then
        showellipse = 1
    end if
    return result
end function

integer rtn             -- routine no, initially from symtab[T_callstk], then from callstack
atom ebp,               -- frame addr in callstack block
     era,               -- return address, initially from symtab[T_callstk] then callstack
     etd                -- threadstack addr

constant repch = "\r\n\t",
         repstrs = {"\\r","\\n","\\t"}

object symtab       -- copy of symtab obtained via opGetST

function getValue(integer symidx, integer limit, integer indent, integer crop)
object  o,
        ss   -- symtab[symidx]
integer r, k,
        nTyp, tidx

    lc = limit
    showellipse = 0
    -- obviously none of these should ever happen, but if they do then leave
    --  as many clues as you can in the ex.err to help resolve things.
    if symidx<0 or symidx>length(symtab) then
        return sprintf("pdiag:getValue bad symidx[=%d]",symidx)
    end if
    ss = symtab[symidx]
    if atom(ss) then
        return sprintf("pdiag:symtab[symidx[=%d]] is an atom",symidx)
    end if
    nTyp = ss[S_NTyp]
    if nTyp>S_TVar3 or nTyp<S_Const then
        return sprintf("pdiag:getValue bad symtab[symidx][S_NTyp]=%d",nTyp)
    end if
    if nTyp=S_TVar3 then
        tidx = ss[S_Tidx]
        o = getVal(ebp+tidx*4)
    else
        tidx = ss[S_Slink]
        o = getVal(etd+tidx*4-4)
    end if
    if not novalue then
--DEV try that new routine here...?
        if indent then
            o = ppExf(o,{pp_Indent,indent+7})
        else
            o = ppf(o)
        end if
        if showellipse then
            if crop then
                lc = find('\n',o)
                if lc then o = o[1..lc-1] end if
            end if
            lc = length(o)
            if o[lc]='}' then
                o[lc..lc] = ",...}"
            else
                o &= "..."
            end if
        end if
--#without reformat
--DEV 24/6/10 (need to experiment a bit here...)
if 0 then -- (added 21/8/2010)
        r = 1
        while 1 do
            k = find(repch[r],o)
            if k then
                o[k..k] = repstrs[r]
            else
                r += 1
                if r>length(repch) then exit end if
            end if
        end while
end if
--#with reformat
    end if
    return o
end function

integer fn, lines
object crash_msg

-- copy of the one in p.exw:
integer batchmode       -- set by -batch command line option
        batchmode = 0   -- 1=suppress displays/prompts [incomplete]

procedure put2x(sequence txt1, sequence txtErr)
-- allows a shorter version for on-screen (txt1) than ex.err (txtErr)
    if not batchmode then
        if lines<15 then    --DEV this (15) should be a parameter (or setting?):
            if sequence(crash_msg) then
                puts(1,crash_msg)
                lines = 999
            else
                lines += 1
                puts(1,txt1)
            end if
        end if
    end if
    if fn!=-1 then
        puts(fn,txtErr)
    end if
end procedure

procedure put2(sequence txt)
    put2x(txt,txt)
end procedure

integer dcount      -- number of dropped callstack blocks

--newEBP...
function retD()
atom prev_ebp
    while 1 do
        prev_ebp = peek4u(ebp+20)
        if prev_ebp=0 then return 0 end if
        era = peek4u(ebp+16)        -- return address
        ebp = prev_ebp
        if era=0 then
            put2("(^^^) call_back from Windows/dll/asm\n")
        else
            if c_func(xIsBadReadPtr,{ebp,12}) then
                put2(sprintf("<**diag.e: bad prev_ebp** (#%08x)>\n",ebp))
                return 0
            end if
            rtn = peek4u(ebp+8)
            era -= 1    -- ensure within code emitted for line [DEV??]
            return 1
        end if
    end while
end function
--function retDX()
--  --
--  -- The callstack is managed as a linked list of 8K virtual stack blocks (vsb).
--  -- The header of each block is 24 bytes:
--  --
--  --  vsb_prev, vsb_next, spare, [threadstackptr], vsb_used, [symtabptr]
--  --
--  --  [threadstackptr] and [symtabptr] are not relevant here, included as a
--  --   precursor to full thread handling, and subject to change. In fact we
--  --   got ecs (which now points at this header) from symtab[T_callstk], which 
--  --   also contains etd, the real threadstack address, when we used opGetST as
--  --   the very first step.
--  --
--  -- spare is set to #DC0DC0DC at the point where dcount blocks were 
--  --  dropped, that is in the e77phroom case.
--  --
--  -- The remainder of each block contains frames, minimum of 6 dwords each:
--  --
--  --  called_from addr
--  --  first (address of first item being saved)
--  --  items 1..N (params and locals as they were before the call)
--  --  N (number of params and locals which got saved)
--  --  calling routine (index to symtab)
--  --  routine being called (index to symtab)
--  --  return addr
--  --
--  -- Each block can hold up to 339 frames, or a single frame can contain 
--  --  up to 2032 parameters, local variables, and temporaries.
--  -- To simplify handling, frames do not span blocks. If there is not
--  --  enough space for the new frame, it is put in a new vsb and the old
--  --  one is left not-quite-full. Each frame is intended to be read from
--  --  the end downwards; attempts to read forwards are doomed to failure
--  --  - though that should cause no great difficulty for anyone.
--  -- The called from address is used for debug handling only and will 
--  --  match the routine name, whereas the return address will match with
--  --  the closing ')' or possibly the following statement.
--  -- While strictly speaking the calling routine is redundant, it does
--  --  allow some verification as the call stack is walked.
--  --
--  -- At startup, the lowest callstack block is created with a dummy pair
--  -- of {T_maintls(=21),0}, which helps opRetf behave correctly and can/
--  -- should be used to signal the bottom of stack.
--  -- 
----puts(1,"retD()\n")
--  while 1 do
--      if vsb_used=2 then return 0 end if  -- must be our {21,0} pair.
--      if vsb_used=0 then
--if newEBP then -- (DEV [nonsense])
--          if dcount then
--              put2(sprintf("<%d callstack blocks skipped>\n",dcount))
--              dcount = 0
--          end if
--else
--          if peek4u(ecs+8)=#DC0DC0DC then
--              put2(sprintf("<%d callstack blocks skipped>\n",dcount))
--          end if
--end if
--          ecs = peek4u(ecs)   -- follow vsb_prev link
----DEV: not newEBP
--          vsb_used = peek4u(ecs+16)
--      end if
----o = peek4u({ecs,vsb_used+20})
----?o
----?vsb_used
--      base = ecs+vsb_used*4
--      rtnX = peek4u(base+16)
--      if rtn!=rtnX then
--          printf(1,"diag callee internal error (rtn %d!=%d)\n",{rtn,rtnX})
--          return 0
--      end if
--      rtn = peek4u(base+12)
----printf(1,"new rtn=%d\n",rtn)
--      N = peek4u(base+8)
--      if N<0 or N>vsb_used then
--          printf(1,"diag callee internal error (N=%d, vsb_used=%d)\n",{N,vsb_used})
--          return 0
--      end if
----printf(1,"N=%d\n",N)
--      base += 4-N*4
--      first = peek4u(base)
--      if N=0 then
--          if first!=0 then
--              printf(1,"diag callee internal error (N=0, first=%08x)\n",first)
--              return 0
--          end if
--      else
--          if first<etd or first>etd+vmax*4then
--              printf(1,"diag callee internal error (first=%08x, etd=%08x, vmax=%d)\n",{first,etd,vmax})
--              return 0
--          end if
----printf(1,"first=%d\n",first)
--          mem_copy(first,base+4,N)
--      end if
--      era = peek4u(base-4)
----printf(1,"new era=%d\n",era)
--      vsb_used -= (N+6)
----?vsb_used
--      if era=0 then
----            if vsb_used=2 then exit end if
--          put2("(^^^) call_back from Windows/dll/asm\n")
------      vsb_used -= ??
--      else
--          era -= 1    -- ensure within code emitted for line [DEV??]
--          return 1
--      end if
--  end while
--end function

integer vmax        -- highest permitted threadstack entry
        vmax = 0    -- (equal to the length of vmap)

sequence vmap   -- variable map; var address --> offset into threadstack
                --  (a flat array of all static and dynamic var refs)
                -- ==> index into symtab for var name, type, etc.

function varIdx(atom addr)
integer tidx, stidx
integer N, rtnid
object sr
integer nTyp
    stidx = floor((addr-etd)/4)+1
--  stidx = floor((etd-addr)/4)+1
    if stidx>0 and stidx<=length(vmap) then
        return vmap[stidx]
    end if
    -- a local var then:
    N = peek4u(ebp+4)
--  tidx = floor((addr-ebp)/4)
    tidx = floor((ebp-addr)/4)
--DEV isn't this <=0? aren't both N and tidx +ve?!
--  if tidx<0 and tidx<N then
    if tidx>=0 and tidx<N then
        rtnid = peek4u(ebp+8)   --DEV?? rtn not good enough for ya?
-- this may be temp!
if rtnid!=rtn then
    puts(1,"pdiag.e:varIdx - rtnid!=rtn\n")
end if
        if rtnid<1 or rtnid>length(symtab) then
            puts(1,"pdiag.e:symtab[rtnid] ioob!\n")
            return -1
        end if
        sr = symtab[rtnid]
        if atom(sr) then
            puts(1,"pdiag.e:atom(symtab[rtnid])!\n")
            return -1
        end if
        nTyp = sr[S_NTyp]
        if nTyp>=S_Type then
            N = tidx
            tidx = sr[S_Parm1]
            while N do
                if tidx<1 or tidx>length(symtab) then
                    puts(1,"\n\n**pdiag.e:bad S_Parm1/S_link chain!\n\n")
                    return -1
                end if
                sr = symtab[tidx]
                if atom(sr) then
                    printf(1,"pdiag.e:atom(symtab[tidx(=%d)])!\n",tidx)
                    return -1
                end if
                tidx = sr[S_Slink]
                N -= 1
            end while
            return tidx
        end if
    end if
    printf(1,"\n**pdiag.e:tidx(=%d) out of range!\n",tidx)
    printf(1,"  (addr=#%08x, ebp=#%08x, N=%d)\n",{addr,ebp,N})
    printf(1,"  (stidx=%d, etd=#%08x, length(vmap)=%d)\n",{stidx,etd,length(vmap)})
    return -1   -- oops!
end function

constant cmp_eax_imm32  = #3D           -- 0o075 imm32              -- cmp eax,imm32

function diag(atom msg_id)
--
-- create ex.err.
--
object  si,             -- copy of symtab[i]
        name,           -- var name or -1 for temporaries we should skip
        o,o2,           -- output vars
        crashfile
integer lineno,         -- linenumber as calculated from return addr/offset & linetab
        linenxt,        -- see lineno calculation
        lti,            -- copy of linetab[i] used in lineno calculation
        fileno,         -- for grouping symtab entries into files
        fpno,           -- copy of si[S_FPno]
        sNTyp           -- copy of sr[S_NTyp]
integer c               -- scratch var
atom    returnoffset    -- era as offset into code block, used in lineno calc

sequence msg,           -- error message, from msgs[msg_id] plus any params
         wmsg,          -- work var, used for building msg
         s8,            -- copy of symtab[T_callstk], see below
         sr,            -- copy of symtab[rtn]
         sp,            -- copy of symtab[<param/local var>]
         linetab,       -- copy of symtab[rtn][S_ltab]
         filename,      -- output var
         pathset,       -- copy of symtab[T_pathset] with mainpath added if .exe
         x6             -- e30->e92 fixup

atom ep1, ep2           -- error parameters

integer lastline

integer p

--/*
    This will definitely never work on RDS Eu!
--*/
    if msg_id=#FFFFFFFF then    -- (-1 as an unsigned 32-bit value!)
--puts(1,"setting batchmode to 1...\n") --DEV temp!
        batchmode = 1
        return 0
    end if
--  puts(1,"d1\n")
--DEV
--  crash_msg = ""      -- /necessary/: ensure compiler knows this is string/integer
    crash_msg = "abc"       -- /necessary/: ensure compiler knows this is string/integer
                        --  (needed since it is only ever set by assembly code)
    crash_msg = 0   -- get callstack as at enumbset.
    crashfile = 0
--/* since reindent does not like this (**DEV**) --*/
--!/**/ #ilASM{ mov_edi_imm32,%isVar,0,0,symtab,    -- mov edi,p1   symtab addr
--!/**/         mov_esi_imm32,%isVar,0,0,crash_msg, -- mov esi,p2   flag/crashmsg
--!!/**/        mov_ecx_imm32,%isVar,0,0,crashfile, -- mov ecx,p3   crashfile
--!/**/         opLeaMov,%ecx,crashfile,            -- mov ecx,addr crashfile
--!/**/         call_rel32,%isOpCode,0,0,%opGetST}  -- [edi] = symtab
--/**/  #ilASM{ lea edi,[symtab]        -- mov edi,p1   symtab addr
--/**/          lea esi,[crash_msg]     -- mov esi,p2   flag/crashmsg
--/**/          lea ecx,[crashfile]     -- mov ecx,addr crashfile
--/**/          call %opGetST}          -- [edi] = symtab
    lines = 0
--puts(1,"d2\n")

--?msg_id
--puts(1,"d2a-\n")
--DEV (temp)
    if not batchmode then
        if msg_id<1 or msg_id>length(msgs) then
            msg = "**BAD MESSAGE ID**"
        else
            msg = msgs[msg_id][1..-2] -- (strip trailing \n)
        end if
        printf(1,"\ndiag(%d[%s]) called...\n",{msg_id,msg})
        lines += 1
    end if
--puts(1,"d2a\n")
    if msg_id<1 or msg_id>length(msgs) then
        msg = sprintf("diag(%d) [**BAD MESSAGE ID**] called\n",msg_id)
    else
        msg = msgs[msg_id]
    end if
--puts(1,"d2b\n")
    --
    -- NB. symtab may contain uninitialised fields (especially S_value).
    --
    --    Attempts to modify symtab, which now has a reference count of 2,
    --    will attempt a clone operation and may therefore crash, as well
    --    as being about as likely to succeed as changing the brake pads,
    --    clutch, gearbox, tyres, steering wheel, and engine oil, all at 
    --    the same time, on a juggernaut careering out of control down 
    --    a steep mountain road.
    --
    --    Likewise attempts to (eg) print symtab may also crash.
    --
    --    Lastly, note that \constants\ are not necessarily initialised yet, 
    --    not just the obvious main=create(Window...) but also name="name",
    --    though literal integer constants (eg DEBUG=1) should be fine.
    --
--puts(1,"d2c\n")
--if atom(symtab) then return 0 end if  --DEV??!
    s8 = symtab[T_callstk]  -- {ep1,ep2,era,etd,ern,ebp,?vsb_used?,dcount}
    --
    -- NB. s8 is "volatile". Hopefully this is of no concern to you, but in 
    -- order to avoid allocating space on the heap (which may be full), or 
    -- messing with bytesallocated/freed counts, opGetST uses a rather dim
    -- ref-count-agnostic-hack when it updates symtab[T_callstk]. If opGetST 
    -- is invoked again (eg it is used in both routine_id and command_line) 
    -- then this s8, or more specifically any float elements extracted from 
    -- this s8, may also get modified (from afar). An atom z z=s8[5] is not
    -- necessarily safe from a following opGetST either. Of course if you 
    -- create eg a string version of s8, or z, then that won't change.
    --
    -- ep1 and ep2 are error code specific, for example e09slin is
    --  "slice length is negative [%d..%d]\n" and ep1/2 are those idx.
    -- era is a return addr, possibly adjusted to pick up a var address,
    --  which can be converted to a line number via routineno/linetab.
    -- etd is a raw pointer to the threadstack static ref bank.
    --  Note this is non-subscriptable and may contain unassigned items.
    -- ern is a routine no/index into symtab, eg 21 for main file code.
    -- ebp is a raw pointer to frame in the callstack (see function retD)
--DEV currently not reported!
    -- dcount is the number of dropped callstack blocks. If the program
    --  runs out of memory all but the first and last two callstack 
    --  blocks are freed, which will hopefully release enough memory
    --  for this routine to complete successfully. This program should 
    --  also limit the entries printed to keep ex.err reasonably sized.
    --
--puts(1,"d2d\n")
    if not batchmode then
        puts(1,"{ep1,ep2,era,etd,ern,ebp,???,dcount}:\n")
        printf(1,"s8=#%08x,#%08x,#%08x,#%08x,%d,#%08x,%d,%d\n",s8)  lines += 2
    end if
    --?8
    ep1 = s8[1]             -- error parameter 1
    ep2 = s8[2]             -- error parameter 2
    era = s8[3]             -- return addr (adjusted to be within code emitted for line)
    etd = s8[4]             -- threadstack ptr
    rtn = s8[5]             -- active routine number
    ebp = s8[6]             -- frame ptr (at point of failure)
--vsb_used [DEV]
    dcount = s8[8]          -- dropped callstack blocks
--?9
    if not batchmode then
        ppOpt({pp_Ascii,{' ',#7E},pp_Nest,1})
    end if
--  ppOpt({pp_Pause,10})
--ppOpt({pp_Pause,0})

--puts(1,"d3\n")
--?10
    --
    -- First create a vmap to allow gvar idx/addr to be mapped to symtab
    --
    if vmax=0 then
        vmap = {}
        for i=length(symtab) to T_maintls by -1 do
            si = symtab[i]
            if sequence(si)
            and si[S_NTyp]<=S_GVar2 then
                c = si[S_Slink]
                if c>vmax then
                    vmap &= repeat(0,c-vmax)
                    vmax = c
                end if
                vmap[c] = i
            end if
        end for
    end if
    --
    -- initialise pathset [DEV do we actually want this?!]
    --
    pathset = symtab[T_pathset]
    for j=1 to length(pathset) do
        if length(pathset[j])<2 or pathset[j][2]!=':' then
            pathset[j] = current_dir()&SLASH&pathset[j]
        end if
    end for

    if msg_id=1         -- e01tcf (ep1 is var idx)
    or msg_id=110 then  -- e110tcf (ep1 is var addr)
        if msg_id=110 then
            ep1 = varIdx(ep1)
        end if
        si = "???"
        if ep1>0 and ep1<=length(symtab) then
            si = symtab[ep1][S_Name]
            if atom(si) then
                si = sprintf("???(name=%d)",si)
            end if
        end if
        o = getValue(ep1, 50, length(si)+17, 1)
        msg = sprintf(msg,{si,o})       -- "type check failure, %s is %s\n"
    elsif msg_id=10         -- e10sspeos
      and ep1<0 then
        msg = sprintf("slice start(%d) less than negative length(%d)\n",{ep1,-ep2})
    elsif msg_id=11         -- e11sepeos
      and ep1<0 then
        msg = sprintf("slice end(%d) less than negative length(%d)\n",{ep1,-ep2})
    elsif msg_id=6          -- e06ioob
       or msg_id=106        -- e106ioob
       or msg_id=9          -- e09slin
       or msg_id=10         -- e10sspeos
       or msg_id=11         -- e11sepeos
       or msg_id=31         -- e31mce
       or msg_id=116 then   -- e116rrnp
        msg = sprintf(msg,{ep1,ep2})
    elsif msg_id=120 then   -- e120fle
--      c = varIdx(ep1)
        o = getValue(ep1, 5, 0, 1)
        if ep2=1 then
            si = "init"
        elsif ep2=2 then
            si = "limit"
        elsif ep2=4 then
            si = "step"
        else
            si = "???"
        end if
        msg = sprintf(msg,{si,o})
--      msg = sprintf(msg,{ep1})
    elsif msg_id=121 then   -- e121flelimstep
        c = varIdx(ep1)
        o = getValue(c, 5, 0, 1)
        c = varIdx(ep2)
        o2 = getValue(c, 5, 0, 1)
        msg = sprintf(msg,{o,o2})
    elsif msg_id=14 then    -- e14NNsoXa
        if ep1>=1 and ep1<=length(e14ops) then
            o = e14ops[ep1]
        else
            o = "???"
        end if
        msg = sprintf(msg,{o,o})
    elsif msg_id=28 then    -- e28NNatXmbausq
        if ep1>=1 and ep1<=length(e28ops) then
            o = e28ops[ep1]
        else
            o = "???"
        end if
        msg = sprintf(msg,{o,o})
    elsif msg_id=30 then    -- e30ume
        -- Map any machine exceptions that occur on inc/add1 (refcount) 
        --  followed by a "helper" cmp eax,<varno>; ==> to e92:
        x6 = peek({era,6})
        --  inc dword[ebx+src*4-8]      377104 2s3 F8
        --  add dword[ebx+src*4-8],1    203104 2s3 F8 01
        if x6[2]=0o104
        and and_bits(x6[3],0o307)=0o203         -- sib(maybe!) of 0o2s3,
        and x6[4]=#F8 then                      -- displacement is -8
            if x6[1]=0o377 -- inc
            and x6[5]=cmp_eax_imm32 then
                ep1 = peek4u(era+5)
                msg_id = 92
            elsif x6[1]=0o203 -- add
              and x6[5]=#01                     -- ie [ebx+idx*4-8],1
              and x6[6]=cmp_eax_imm32 then
                ep1 = peek4u(era+6)
                msg_id = 92
            end if
        end if
        if msg_id=92 then -- ie e92vhnbaav, aka "variable %s has not been assigned a value"
            msg = msgs[92]
            if ep1>0 and ep1<=length(symtab) then
                si = symtab[ep1]
                msg = sprintf(msg,si[S_Name..S_Name])
            else
                printf(1,"diag.e: oops(4), var no[=%d] out of range\n",ep1) -- See note at top
            end if
        else
            if ep1=#C0000005-#100000000 then
                wmsg = "[MEMORY VIOLATION]"
            elsif ep1=#C00000FD-#100000000 then
                wmsg = "[STACK OVERFLOW]"
            else
                wmsg = sprintf("#%08x",ep1)
            end if
            msg = sprintf(msg,{wmsg,era})
        end if
    elsif msg_id=92 then    -- e92vhnbaav
        c = varIdx(ep1)
        si = symtab[c][S_Name]
        if atom(si) then
            si = sprintf("???(name=%d)",si)
        end if
        msg = sprintf(msg,{si})
    elsif msg_id=94 then    -- e94vhnbaav
        if ep1<1 or ep1>length(symtab) then
            si = sprintf("???(varno=%d)",ep1)
        else
            si = symtab[ep1]
            if atom(si) then
                si = sprintf("???(atom(symtab[%d]))",ep1)
            else
                si = symtab[ep1][S_Name]
                if atom(si) then
                    si = sprintf("???(name=%d)",si)
                end if
            end if
        end if
        msg = sprintf(msg,{si})
    elsif msg_id=97 then    -- e97he
--      if ep1=112 then
--          o = {"112(ERROR_DISK_FULL)"}
--      else
            o = {sprintf("%d",ep1)}
--      end if
        msg = sprintf(msg,o)
    elsif msg_id=98 then    -- e98fiofe
        if ep1=112 then
            o = {"112(ERROR_DISK_FULL)"}
        else
            o = {sprintf("%d",ep1)}
        end if
        msg = sprintf(msg,o)
        crashfile = "NUL"
    elsif msg_id>=120 and msg_id<=122 then  -- for loop errors:
        c = varIdx(ep1)
        o = getValue(c, 50, 18, 1)
        msg = sprintf(msg,{o})
    elsif msg_id=58             -- e58bfn
       or msg_id=72 then        -- e72iri
        msg = sprintf(msg,ep1)
    elsif msg_id=108 then       -- e108pe
        if ep1=87 then
            wmsg = "ERROR_INVALID_PARAMETER"
        else
            wmsg = sprintf("#%08x",ep1)
        end if
        msg = sprintf(msg,{wmsg})
    end if
--puts(1,"d4\n")

    if equal(crashfile,"") then return batchmode end if
    if find(crashfile,{"NUL","NULL","/dev/null"}) then
        fn = -1
    elsif not atom(crashfile) then
        fn = open(crashfile,"w")
    else
        fn = open("ex.err","w")
    end if
    while 1 do
        if rtn<1 or rtn>length(symtab) then -- See note at top
            printf(1,"diag.e: oops, rtn[=%d] out of range[1..%d]\n",{rtn,length(symtab)})
            exit
        end if
        sr = symtab[rtn]
        sNTyp = sr[S_NTyp]
        if sNTyp>=S_Type
        and (swod or and_bits(sr[S_State],K_wdb)) then -- skip without debug items

            lineno = sr[S_1stl]     -- line no of "procedure"/"function"/"type" keyword
            linetab = sr[S_ltab]
            lastline = linetab[$]

            if era=-1 then
                -- (error occurred inside an opRetf.
                --  btw: for if return else return end if, this
                --  shows error on either return as the latter,
                --  since it is jmp opRetf not call opRetf...).
                returnoffset = lastline-1
            else
                returnoffset = era-sr[S_il]
            end if
            if returnoffset<0 or returnoffset>=lastline then
                lineno = -1
            else
                --
                -- Entries in the line table are negative to indicate a number
                --  of lines which emitted no code, or >=0 for a start offset
                --  relative to S_1stl/S_il. Additionally there is a dummy max 
                --  entry (added at the end of ilxlate) which helps to prevent 
                --  this running off into a subscript out of bounds error.
                -- Skip down the line table until the return address is (b)reached,
                --  - we only know this when we hit the next entry, hence linenxt.
                -- EG if linetab is {-2,0,24,36} then offsets 0..23 are S_1stl+2,
                --  offsets 24..35 are S_1stl+3; we only know that we should have
                --  stopped for an offset of 17 when we hit the 24, and the lineno 
                --  we want is that before the +1 triggered by the 0 (or whatever
                --  line adjustment we made on [2] when we decide to stop on [3]).
                --  Of course if the return offset is 24 then the error occurred
                --  on line S_1stl+2 since no code from S_1stl+3 has executed yet,
                --  whereas conversely if a machine error occurs at offset 24 then 
                --  clearly the problem is on line S_1stl+3 rather than S_1stl+2.
                --  Another example is linetab of {-14,#14,-3,#47...} and offset
                --  of #22. We only know that #14 is the right entry when we hit
                --  the #47, so there'll be a +1 and -(-3) that we must ignore.
                -- Lastly note that [era] is incredibly fiddly to set, especially
                --  for low-level routines several calls deep from user code. If
                --  this yields -1 suspect [era] rather than this code.
                --
                linenxt = lineno
                for i=1 to length(linetab) do
                    lti = linetab[i]
                    if lti<0 then       -- -n lines emitted no code
                        linenxt -= lti
                    else                -- start offset of next line
                        if returnoffset<=lti then exit end if   -- all done
                        lineno = linenxt
                        linenxt += 1
                    end if
                end for
            end if
--linetab = {}
            if lineno=-1 and find(msg_id,{92,30}) then
--              --
--              -- If you have opFrame / mov a,b / mov c,d / opCall, where b or d is
--              --  unassigned (a,c are parameters of the routine about to be called),
--              --  then the e92 will try to locate the failure address within the 
--              --  routine about to be called; pop (at most one not yet active) frame 
--              --  and try again:
--              --
--DEV done above, I think: [needs testing in terror.exw!]
--              if msg_id=30 then
----DEV:
----                    if c_func(xIsBadReadPtr,{era,1}) then
----                        printf(1,"diag.e: oops, invalid <era>[%08x]\n",era)
----                    els
--                  if peek({era,2})=incd_sib then
----DEV:
----                        if c_func(xIsBadReadPtr,{era-5,1}) then
----                            printf(1,"diag.e: oops, invalid <era-5>[%08x]\n",era)
----                        els
--                      if peek(era-5)=cmp_eax_imm32 then
--                          msg = msgs[92]  -- ie e92vhnbaav, aka
--                                  -- "variable %s has not been assigned a value"
--                          ep1 = peek4u(era-4)
--                          if ep1>0 and ep1<=length(symtab) then
--                              si = symtab[ep1]
--                              msg = sprintf(msg,si[S_Name..S_Name])
--                          else
--                              printf(1,"diag.e: oops(2), var no[=%d] out of range\n",ep1) -- See note at top
--                          end if
--                      end if
--                  end if
--              end if
                msg_id = 0
                if not retD() then  -- See note at top
                    printf(1,"diag.e: oops, lineno=-1/e92/not retD(), era=#%08x\n",era)
                    exit
                end if
            else
                filename = symtab[T_fileset][sr[S_FPno]][1..2]&lineno
                filename[1] = pathset[filename[1]]
                put2(sprintf("%s%s:%d",filename))
                if sr[S_Name]=-1 then
--              if sr[S_Name]=-1 or sr[S_NTyp]=S_Rsvd then
                    put2("\n")
                else
                    put2(sprintf(" in %s %s()\n",{rtndescs[sr[S_NTyp]-S_Type+1],sr[S_Name]}))
                end if
                if length(msg) then     -- first time only
                    put2(msg)
                    msg = ""
                end if
                p = sr[S_Parm1]
                while p do
                    sp = symtab[p]
                    name = sp[S_Name]
                    if sequence(name) then
                        o = getValue(p, maxlen, length(name), 0)
--                      if showellipse or lc<maxlen-50 then
--                          -- we need a (much) shorter version for on-screen display:
--                          o2 = getValue(p,50,length(name), 1)
--                          o = sprintf("    %s = %s\n",{name,o})
--                          o2 = sprintf("    %s = %s\n",{name,o2})
--                          put2x(o2,o)
--                      else
                            put2(sprintf("    %s = %s\n",{name,o}))
--                      end if
                    elsif name!=-1 then -- should not happen!
--                      put2(sprintf("    %d[!!] = ???\n",name))
                        name = sprintf("%d[!!]",name)
                        o = getValue(p, maxlen, length(name), 0)
                        put2(sprintf("    %s = %s\n",{name,o}))
                    end if
                    p = sp[S_Slink]
                end while
                if not retD() then exit end if
                put2("... called from ")
            end if  -- lineno!=-1
        else -- K_wdb
            if sNTyp<S_Type then
                put2(sprintf("diag.e: symtab[%d] bad S_NTyp[%d]\n",{rtn,sNTyp}))
--          else
--              put2(sprintf("diag.e: symtab[%d] skipped (no debug)\n",{rtn}))
            end if
            msg_id = 0
            if not retD() then exit end if
        end if  -- K_wdb
    end while

    if fn!=-1 then
        puts(fn,"\nGlobal & Local Variables\n")
        fileno = 0

        for i=T_const1+1 to length(symtab) do
--DEV (untested, do we have T_Ainc when bound?)
--      for i=T_Ainc+1 to length(symtab) do
--  T_Ainc = symlimit

--          if i!=T_fileset and i!=T_pathset then
            si = symtab[i]
            if sequence(si) then                            -- might not be dumped (unused)
                name = si[S_Name]
                if equal(si[S_NTyp],S_GVar2)
                and (swod or and_bits(si[S_State],K_wdb))   -- skip without debug items
                and sequence(name) then                     -- skip unnamed items
                    fpno = si[S_FPno]
                    if fileno!=fpno then
                        fileno = fpno
                        filename = symtab[T_fileset][fileno][1..2]
                        filename[1] = pathset[filename[1]]
                        printf(fn,"\n %s%s:\n",filename)
                    end if
                    o = getValue(i, maxlen, length(name), 0)
                    printf(fn,"    %s = %s\n",{name,o})
                end if
            end if
        end for
        if not batchmode then
            puts(1,"\n")
--DEV
--          #isginfo{crash_msg,0b1001,0,0,integer,0} -- (verify compiler is working properly)
            #isginfo{crash_msg,0b1001,0,0,integer,3} -- (verify compiler is working properly)
            if atom(crash_msg) and fn!=-1 then
                if atom(crashfile) then
                    puts(1,"--> see "&current_dir()&"\\ex.err\n")
--              elsif not find(crashfile,{"NUL","/dev/null"}) then
                else -- (above not necessary, fn would be -1)
                    puts(1,"--> see "&crashfile&"\n")
                end if
            end if
        end if
--DEV if interperting, leave this open and have p.exw/main() dump Warnings() to it:
        close(fn)
    end if
    return batchmode
end function

atom diagcb
    diagcb = call_back(routine_id("diag"))
--#ilasm{mov_edi_imm32,%isVar,0,0,diagcb,   -- mov edi,diagcb
--     call_rel32,%isOpCode,0,0,%opCrshRtn} -- save [edi]
#ilASM{lea edi,[diagcb]
       call %opCrshRtn} -- save [edi]


--DEV./SUG:
--/*
    #ilASM{ :%opErrf    -- use the called from address in the frame
                mov esi,[ebp+12]    (except esi is ep2)
            :%opErr     -- called from address in esi
          }
--*/

--end of new code
--===============
---- "without debug" stops the run-time update of line and file info, and
---- this program from dumping local vars, which in this case would be [DEV?]
---- just crash_rtn, crash_msg, crashfile, and stoploop. While it remains
---- quite sensible to specify this here, it should be perfectly OK to 
---- turn it back on for a while rather than play completely in the dark.
---- If you get any problems, comment this line out, suffer the 4 extra 
---- lines in your .err, some pretty wierd and wacky misleading line nos
---- (eg the line in this source rather than where the user app actually 
----  crashed), and hopefully/maybe get a better clue where it 
---- (this, ie pdiag.e) went wrong ;-)):
--
----/**/without debug -- Phix: disables all debug/diagnostic stuff!
--------/**/with debug
--without type_check
------with trace
--
--
----without trace   -- NB no effect under "without debug"
----with trace      -- NB no effect under "without debug"
--
--
---- TODO: 
---- "Traced lines leading up to the failure:", eg:
----
----C:\Program Files\Phix\test.exw:6    procedure d(sequence s)
----C:\Program Files\Phix\test.exw:8        d(s[2..length(s)-1])
----C:\Program Files\Phix\test.exw:6    procedure d(sequence s)
----C:\Program Files\Phix\test.exw:8        d(s[2..length(s)-1])
----
---- (Personally, though, I've never found that much use)
--
--constant binderrs = {
--"infile is not string",                   --1 --*
--"error opening infile (p.exe)",           --2 -- in use? hard disk problems?
--"error seeking to infile eof",                --3 --          ""
--"error allocating sizeof(p.exe|new.exe)", --4 -- insufficient memory? (2MB should be plenty!)
--"error reading p.exe",                        --5 -- as 2,3
--"MZ header not found",                        --6 -- p.exe corrupt? hard disk problems?
--"PE header not found",                        --7 -- ""               ""
--"subvers not atom",                       --8 --*
--"length(optable)!=length(opNames)",       --9 -- [details already shown]
--"calcsize!=dumpsize",                     --10 -- serious! will need soure to reproduce
--"incorrect image size",                   --11    ""
--"outfile not string",                     --12 --*
--"error writing new.exe",                  --13 -- in use? hard disk problems?
--"sig not sequence"}                       --14 --*
---- items marked --* should not happen (unless p.exw sources badly hacked)
--
--
--constant rtndescs = {"type","function", "procedure"}
--
--object crash_rtn crash_rtn = -1
--object crashfile crashfile = -1
--
--integer stoploop      -- this is independently tested for in the backend...
--      stoploop = 0        --  (but it does not hurt any to re-test it here)
--
--function diag(integer msg_id)
-- removed from e01:
    -- Note: since the diag routine uses some of the builtins,
    --  then eg object o o="fred" getc(o) will not generate 
    -- 'type check error, fn is "fred"', but instead
    -- 'type check error, getc parameter fn is wrong'. [?DEV I may have fixed this since?]
    --  [as opposed to getc("fred"), which causes compile-time error]
    -- When you see "builtin parameter", look up the routine
    --  in the documentation to find out exactly which 
    --  parameter it is referring to.
    -- For more details, also see variable builtinparamwrong.
--integer builtinparamwrong
--integer k, rtn, callee, fileno, pathno, noofparams, i2, i3, i4, km1, km2, km3, kp1
--integer lineno
--object si, codeseg, o, o2, name
--integer tidx, idx
--integer pn
--object linetab
--integer line
--object ugh --DEV!
--
------  puts(1,"diag called...\n")
--------/**/if getc(0) then end if
--  if stoploop then
--      -- this is independently tested for in the backend...
--      --  (flag is intended to catch errors in pdiag.e itself)
----        puts(1,"diag looping!\n")
--      puts(1,"diag looping!\n")
----        stoploop = 2
----    elsif stoploop=2 then
----        puts(1,"diag re-looping!\n")
--      return 0
----        if getc(0) then end if
----        abort(0)
----    else
--  end if
--  stoploop=1
--
------  puts(1,"diag called2...\n")
--
--  if equal(crashfile,"") then return 0 end if
----    if equal(crashfile,"") then abort(1) end if
--
--  builtinparamwrong = 0
--  callcount = 0
--  if find(crashfile,{"NUL","/dev/null"}) then
--      pn = -1
----DEV: if we can't open .err, display to screen anyway...
----        [I think we're OK, but not tested, let me know if it works as it should]
--  elsif crashfile!=-1 then
--      pn = open(crashfile,"w")
--  else
--      pn = open("p.err","w")
--  end if
--
--  while 1 do
--      while 1 do
--              callcount += 1
--                  if msg_id<=length(msgs) then
--                      i2 = ep1  i3 = ep2
--                      elsif msg_id = 108 then
--                          -- position error [%08x]
--                          o = {i2}
--                      elsif msg_id = 98 then
--                          -- flush error [%08x]
--                          -- add human-readables for common ones as follows:
--                          -- (see eg arwen/Constants.ew, ERROR_INVALID_HANDLE etc)
----                            if i2=? then
----                                o = {i2," desc"}
----                            elsif i2=? then
----                                o = {i2," desc"}
----                            else
--                              o = {i2,""}
----                            end if
--                      elsif msg_id = 62
--                         or msg_id = 58
--                         or msg_id = 7 then
--                          -- file number %d is not open
--                          -- invalid file number (%d)
--                          -- slice start is less than 1 (%d)
--                          o = {i2}
--                      elsif msg_id = 6
--                         or msg_id = 106 then
--                          -- index %d out of bounds, assigning to sequence length %d
--                          -- index %d out of bounds, reading from sequence length %d
--                          o = {i2,i3}
--                      elsif msg_id >= 9 
--                        and msg_id <= 11 then
--                          -- slice length is negative (%d..%d)
--                          -- slice starts past end of sequence (%d > %d)
--                          -- slice ends past end of sequence (%d > %d)
--                          o = {i2,i3}
--                      elsif msg_id = 116 then
--                          -- routine requires %d parameters, not %d
--                          o = {i2,i3}
--                      elsif msg_id = 1
--                      or msg_id = 110 then
--                          if symtab[i2][S_NTyp] = S_TVar3 then
--                              i4 = -symtab[i2][S_Tidx]
--                          else
--                              o = symtab[i2][S_value] (DEV)
--                          end if
--                          if i3=0 then
--                              o = {"<diag.e: oops, i3 is zero>",o}
--                          elsif builtinparamwrong then
--                              o = {symtab[builtinparamwrong][S_Name]&" parameter "&
----DEV: (testme!)
----                                     symtab[i3][S_Name],"wrong"}
--                                   symtab[i3][S_Name],o}
--                          else
--                              o = {symtab[i3][S_Name],o}
--                          end if
--
--                      elsif msg_id = 93 then
--                          -- bind error %d (%s)
--                          o = {i2,binderrs[i2]}
--                      else
--                          o = 0
--                      end if
--                      put2(pn,msgs[msg_id],o)
--                  else
--                      put2(pn,"unknown error code %d\n",{msg_id})
--                  end if
--
--          elsif msg_id=1 then
--              -- Instead of 'type check error, fn is "fred"', for
--              -- the builtins, which we are likely to use all the
--              -- time and hence trash any "current value", output
--              -- 'type check error, builtin parameter fn is wrong'
--              --  (user is expected to lookup "fn" in the docs)
--              -- FWIW, RDS Eu tends to output routine-specific
--              -- messages, eg "first parameter to match must be 
--              -- a sequence", "file number is not an integer",
--              -- and likwise not show any "current value".
--              builtinparamwrong=rtn
--          end if
--
--flush(pn)
--      end while
--  end while
--flush(pn)
--
--  if pn!=-1 and callcount>100 then
--      printf(pn,"  (skipping %d levels)\n",callcount-100)
--  end if
------  puts(1,"hey7!\n")
--
--  fileno = 0
--
--  if pn!=-1 then
--      puts(pn,"\nGlobal & Local Variables\n")
--
--      for i=1 to length(symtab) do
--      end for
--      puts(1,"\n")
--      close(pn)
--  end if
--  if crash_rtn!=-1 then
----DEV setup 2nd p_crash.err file.. [better: use the old asm stubs to screen]
--      for i=length(crash_rtn) to 1 by -1 do
--          if call_func(crash_rtn[i],{0}) then exit end if
--      end for
--  end if
----puts(1,"done!\n")
----abort(1)
----if getc(0) then end if
----if stoploop then abort(1) end if
--  stoploop = 0
--  return 0
--end function
--
----DEV these need to be moved (to asm)!?
--global procedure crash_file(object file_path)
---- Specify a file path name in place of "ex.err" where you want
---- any diagnostic information to be written.
---- May be called multiple times, at the point of a crash the
---- last value passed to this routine is used.
---- A value of "" turns off diagnostics completely.
---- A value of "NUL" or "/dev/null" displays messages to screen
---- but does not create a p.err file.
---- A value of -1 restores default handling.
---- 
--  crashfile = file_path
--end procedure
--
----DEV this needs to be moved to asm:
--global procedure crash_routine(integer proc)
---- specify the routine id of a Phix procedure to call in the
---- event that Phix must shut down your program due to an error.
--  if crash_rtn=-1 then
--      crash_rtn = {proc}
--  else
--      crash_rtn = append(crash_rtn,proc)
--  end if
--end procedure
--
--
--procedure setup()
--  diagcb = routine_id("diag") --  if diagcb <= 0 then puts(1,"diagcb <= 0\n") abort(1) end if
----?diagcb
--  diagcb = call_back(diagcb)  --  if diagcb = -1 then puts(1,"callback = -1\n") abort(1) end if
----?diagcb
--
--  #ilasm{mov_edi_imm32,%isVar,0,0,diagcb,     -- mov edi,diagcb
--         call_rel32,%isOpCode,0,0,%opCrshRtn} -- save [edi]
--end procedure
--if 01 then
--setup()
--end if
--

