--
-- timestamp.e
--

include builtins\timedate.e

timedate t0 = parse_date_string("1/1/1970 UTC",{"D/M/YYYY tz"})

object timezone = 0

--DEV passing in a date and time has not been tested
global function timestamp(string name, bool withDot=false, sequence d={})
    if d={} then
        d = date(bMsecs:=true)
    else
        d[DT_MSEC] = 0
    end if
--  d = set_timezone(d,"UTC")
--  d = set_timezone(d,"CST")
--  d = change_timezone(d,"UTC")
--  d = change_timezone(d,"CST")
    if timezone=0 then
        -- (this is probably a temporary file, but good enough for now)
        integer tzfn = open("timezone.txt","r")
        if tzfn=-1 then
            -- specify dst even when out-of-season, so it kicks in naturally.
            puts(1,"Enter daylight savings timezone (eg BST rather than GMT):")
            timezone = upper(trim(gets(0)))
        else
            timezone = trim(gets(tzfn))
            close(tzfn)
        end if
        -- this will crash if timezone is not in timedate.e/timezones:
        d = set_timezone(d,timezone)
        d = change_timezone(d,"UTC")
        if tzfn=-1 then
            tzfn = open("timezone.txt","w")
            puts(tzfn,timezone)
            close(tzfn)
        end if
    else
        d = set_timezone(d,timezone)
        d = change_timezone(d,"UTC")
    end if
    d = adjust_timedate(d,timedelta(milliseconds:=get_drift(name)))
    string res = sprintf("%d%s%03d",{timedate_diff(t0,d,DT_SECOND),
                                     iff(withDot?".":""),
                                     d[DT_MSEC]})
--  sleep(0.001)    -- (ensure uniqueness - probably not necessary)
    return res
end function

global function timestamp_ms(string name)
    sequence d = date(bMsecs:=true)
    d = set_timezone(d,timezone)
    d = change_timezone(d,"UTC")
    atom drift = get_drift(name)
    atom delta = timedelta(milliseconds:=drift)
    d = adjust_timedate(d,delta)
    atom res = timedate_diff(t0,d,DT_SECOND)*1000+d[DT_MSEC]
    return res
end function
