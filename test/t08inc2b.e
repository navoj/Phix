global integer j
j=2
include t08inc2a.e as a
include t08inc2b.e as b
include t08inc2c.e as c
include t08inc2d.e as d
--if sprintf("b.e: %d %d %d %d\n",{a:i,b:j,c:k,d:l})!="b.e: 1 2 3 4\n" then puts(1,"b:fail\n") abort(1) end if
--if {a:i,b:j,c:k,d:l}!={1,2,3,4} then puts(1,"b:fail\n") abort(1) end if
if a:i!=1 then puts(1,"fail[b1]\n") abort(1) end if
if b:j!=2 then puts(1,"fail[b2]\n") abort(1) end if
if c:k!=3 then puts(1,"fail[b3]\n") abort(1) end if
if d:l!=4 then puts(1,"fail[b4]\n") abort(1) end if
