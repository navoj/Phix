--
-- get_interpreter.e
--
--  Implements get_interpreter(), see docs for details.
--
--DEV make this a standard builtin? (see also demo/PGUI/pdemo.exw and filedump.exw)

function matchew(string path, string endswith)
    if length(endswith)=0 then return 1 end if
    sequence segments = split_path(path)
    return length(segments) and lower(segments[$])=lower(endswith)
end function

function add_path(sequence pathset, string path)
    if length(path)
    and not find(path,pathset)
    and get_file_type(path)=FILETYPE_DIRECTORY then
        pathset = append(pathset,path)
    end if
    return pathset
end function

function add_paths(sequence pathset, sequence paths, sequence endswiths)
    for i=1 to length(paths) do
        string path = paths[i]
        for j=1 to length(endswiths) do
            string endswith = endswiths[j]
            if matchew(path,endswith) then
                pathset = add_path(pathset,path)
                exit
            end if
        end for
    end for
    return pathset
end function

function validexe(string s, sequence vset)
--
-- Just ensures that a filename is reasonable
--
-- s is a (path-less) filename such as "p.exe"
-- vset is a list of permitted filenames (platform dependent)
--
    if find(s,vset) then
        return 1
    else
        string vs1 = vset[1]            --      "pw.exe"/"p"
        integer {nlen, n} = iff(platform()=WINDOWS?{7,3}:{2,2})
        if length(s)=nlen               --    ?"pw9.exe":"p9"
        and find(s[n],"123456789") then
            -- Feel free to ignore this part
            -- (When developing phix, I use pw1.exe..pw9.exe 
            --  on Windows, and p1..p9 on Linux, so that any
            --  running applications need not be shutdown.)
            s[n..n] = ""
            if s=vs1 then
                return 1
            end if
        end if
    end if
    return 0
end function

function ibits(string filepath, integer plat=platform())
--
-- filepath should be a fully qualified filename, of FILETYPE_FILE.
-- returns 32 or 64 by examining low-level headers, or 0 if the file is 
-- corrupt, empty, for a different platform, or otherwise not recognised.
--
-- DEV/SUG may also be sensible to verify data section starts "Phix"?
--
integer fn = open(filepath,"rb")
integer bits = 0
    if plat=WINDOWS then
        if seek(fn,#80)=SEEK_OK then
            sequence s6 = {}
            for i=1 to 6 do
                s6 = append(s6,getc(fn))
            end for
            if s6="PE\0\0\x4C\x01" then
                bits = 32
            elsif s6="PE\0\0\x64\x86" then
                bits = 64
            end if
        end if
    else -- platform()=LINUX
        sequence s5 = {}
        for i=1 to 5 do
            s5 = append(s5,getc(fn))
        end for
        if s5="\x7FELF\x01" then
            bits = 32
        elsif s5="\x7FELF\x02" then
            bits = 64
        end if
    end if
    close(fn)
    return bits
end function

--/* seems ok:
constant itestset = {{"C:\\Program Files (x86)\\Phix\\pw.exe",WINDOWS},
                     {"C:\\Program Files (x86)\\Phix\\pth.exe",WINDOWS},
                     {"C:\\Program Files (x86)\\Phix\\phix",LINUX}}
for i=1 to length(itestset) do
    ?{itestset[i],ibits(itestset[i][1],itestset[i][2])}
end for
--*/

global function get_interpreter(bool enquote=false, object mb=machine_bits(), integer plat=platform())
-- returns "" on failure
sequence vset   -- permitted filenames
string filepath
    sequence cl = command_line()
    string res = cl[1]
    string file = get_file_name(res)
    sequence cpaths = split_path(res)
    string crun = cpaths[$]
    cpaths = cpaths[1..$-1]
    cpaths = cpaths[1..find("demo",lower(cpaths))-1]
    res = join_path(cpaths,1)   -- eg/ie ../Phix/demo/pGUI/ -> ../Phix/

    if plat=WINDOWS then
        vset = {"pw.exe","p.exe","pw64.exe","p64.exe","pw32.exe","p32.exe","pth.exe"}
    else -- platform()=LINUX
--      vset = {"p","phix","phix64","phix32","pth"}
        vset = {"p","p32","p64","pth"}
    end if
    filepath = join_path({res,crun})
    if not validexe(crun,vset)
    or get_file_type(filepath)!=FILETYPE_FILE
    or ibits(filepath,plat)!=mb then

        sequence paths = {res}
        paths = add_path(paths,current_dir())
        if plat=WINDOWS then
            paths = add_path(paths,"C:\\Program Files (x86)\\Phix")
            paths = add_path(paths,"C:\\Program Files\\Phix")
            paths = add_paths(paths,split(getenv("PATH"),';'),{"phix","bin"})
        else
            paths = add_path(paths,join_path({getenv("HOME"),"phix"}))
            paths = add_paths(paths,split(getenv("PATH"),':'),{"phix","bin"})
        end if

        string maybe = "", definitely = ""
        integer mbmb = sequence(mb)
        if mbmb then
            mb = mb[1]
        end if
        for i=1 to length(paths) do
            for j=1 to length(vset) do
                filepath = join_path({paths[i],vset[j]})
                if get_file_type(filepath)=FILETYPE_FILE then
                    integer mbi = ibits(filepath,plat)
                    if mbi!=0 then
                        maybe = filepath
                        if mbi=mb then
                            definitely = maybe
                            exit
                        end if
                    end if
                end if
            end for
            if length(definitely) then exit end if
        end for
        filepath = iff(mbmb?definitely:maybe)
    end if
    if enquote then
        if find(' ',filepath) then
            filepath = '\"' & filepath & '\"'
        end if
    end if
    return filepath
end function

