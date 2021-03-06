--
-- undo.e
--
without trace
--with trace

-- Handles undo and redo. There are four global entry points:
--
--  addAction(object Type, object data) stores info as user types etc.
--  
--  DeleteBlockPos() gets info needed to re-insert a block which is about
--                   to be deleted (shortly passed back to addAction).
--
--  restoreBlockPos(sequence pos) inverse of DeleteBlockPos()
--
--  extendOverStrike(integer newchar, integer replacedchar) is like
--                   addAction but caters for OVR mode by maintaining the
--                   top two stack elements as two parallel chunks of 
--                   inserted and overstruck text, so that a single undo
--                   (Ctrl Z) first restores the overstruck text, and then
--                   a second undoes the text insertion, rather than needing
--                   two Ctrl Z for every character overstruck.
--
--  UndoRedo(integer redo) handles Ctrl Z and Ctrl Y input.
--
--
-- There are seven possible addAction Types.
-- INSERTCHAR is a type of INSERTBLOCK which is extendable,
-- and similarly DELETECHAR and BACKSPACE are extendable 
-- DELETEBLOCK actions. Otherwise they are the same.
-- INDENT and UNINDENT (in UndoRedo) call indent() and unindent(),
-- passing either VK_TAB or comment (as originally passed to 
-- addAction) over the same range of lines.
-- UndoRedo() only performs the four (middle[2..-2]) operations.
--
global constant
    INSERTCHAR  =  3,   -- treated as 2 by undo/redo
    INSERTBLOCK =  2,
    INDENT      =  1,
    UNINDENT    = -1,
    DELETEBLOCK = -2,
    DELETECHAR  = -3,   -- treated as -2 by undo/redo
    BACKSPACE   = -4    -- treated as -2 by undo/redo

--
-- A single action is {timestamp, type, cursorX, cursorY, selX, selY, selON, lines)
-- for INSERTBLOCK and DELETEBLOCK (and extendable versions),
-- or (timestamp, type, cursorX, cursorY, selX, selY, selON, {what,flags} for
-- INDENT/UNINDENT actions, where "what" is VK_TAB or (eg) "--" and flags is
-- a list of 0/1 as returned by indent(all 1s) or unindent (which will return 0
-- for those lines which did not begin with the required tab/comment).
--
-- In the case of insert/delete blocks, carriage returns are implied
-- between lines, so eg {"",""} represents a single CR, and likewise
-- {"","",""} represents (exactly) two CR characters, a chunk beginning 
-- with {"",... starts with a CR, and a chunk ending with ...,""} ends
-- with a CR.
--

constant TIMESTAMP = 1, TYPE = 2, CURSORX = 3, CURSORY = 4, SELX = 5, SELY = 6, SELON = 7, LINESET = 8

sequence action

function mergeable(integer Type, integer data)
-- Make sure the cursor is in the right place for a merge
-- Only used for INSERTCHAR, DELETECHAR, and BACKSPACE blocks.
integer l
    l = length(action[LINESET])
    if l!=1 then return 0 end if
    l = length(action[LINESET][1])
    if action[CURSORY]!=CursorY then return 0 end if
    if Type=INSERTCHAR then
        if action[CURSORX]+l!=CursorX then return 0 end if
    elsif Type=BACKSPACE then
        if action[CURSORX]-1!=CursorX then return 0 end if
    elsif Type=DELETECHAR then
        if action[CURSORX]!=CursorX then return 0 end if
    else ?9/0 -- should not be called.
    end if
    if not find(data," \t") then
        -- break up typed blocks into words:
        if find(action[LINESET][1][l]," \t") then
            return 0
        end if
    end if
    return 1
end function

global procedure addAction(object Type, object data)
integer pointer, l
    pointer = actionptr[currfile]
    if pointer then
        if pointer<length(actions[currfile]) then
            -- remove any later actions (redo no longer possible)
            actions[currfile] = actions[currfile][1..pointer]
        end if
        action = actions[currfile][pointer]
        if atom(Type) then
            if Type=action[TYPE] and pointer!=actionsave[currfile] then
                --
                -- merge with existing action
                --
                if Type=BACKSPACE then
                    if mergeable(Type,data) then
                        action[LINESET][1] = prepend(action[LINESET][1],data)
                        action[CURSORX] = CursorX
                        action[CURSORY] = CursorY
                        action[TIMESTAMP] = floor(time())
                        actions[currfile][pointer] = action
                        return
                    end if
                elsif Type=INSERTCHAR or Type=DELETECHAR then
                    if mergeable(Type,data) then
--                      if data=VK_RETURN then
                        if data=K_CR then
                            action = append(action,"")
                        else
                            l = length(action[LINESET])
                            action[LINESET][l] = append(action[LINESET][l],data)
                        end if
                        action[TIMESTAMP] = floor(time())
                        actions[currfile][pointer] = action
                        return
                    end if
                end if -- INSERTBLOCK and DELETEBLOCK are not merged
                        -- INDENT and UNINDENT are not merged
            end if
        end if
    else
        -- empty the action list (redo no longer possible)
        actions[currfile] = {}
    end if
    -- trim start of actions by timestamp
    if isUndoTime then
        for i=1 to pointer do
            if actions[currfile][i][TIMESTAMP]<floor(time())-isUndoTime then
                if i>1 then
                    actions[currfile] = actions[currfile][i..pointer]
                    actionsave[currfile] = actionsave[currfile] - i --DEV more testing required
                end if
                exit
            end if
        end for
    end if
    action = repeat(0,LINESET)
    action[TIMESTAMP] = floor(time())
    if atom(Type) then
        action[TYPE] = Type
        action[CURSORX] = CursorX
        action[CURSORY] = CursorY
        action[SELX] = selX
        action[SELY] = selY
        action[SELON] = selON
    else
        action[TYPE..SELON] = Type
    end if
    if find(Type,{BACKSPACE,DELETECHAR,INSERTCHAR}) then
--      action[LINESET] = {{data}}
        action[LINESET] = {" "}
        action[LINESET][1][1] = data
    else
--DEV where did this come from? (30/6/16)
--      if not sequence(data) then ?9/0 end if
--      for i=1 to length(data) do
--          if not string(data[i]) then ?9/0 end if
--      end for
        action[LINESET] = data
    end if
    actions[currfile] = append(actions[currfile],action)
    pointer = length(actions[currfile])
    actionptr[currfile] = pointer
    if actionsave[currfile]>=pointer then
        actionsave[currfile] = -1   -- there is now no place where actionlist matches disk.
    end if
--DEV
    setSaveIcon()--currfile,1,1)
    forceCursorOnscreen()
end procedure

global function DeleteBlockPos()
    return {DELETEBLOCK,CursorX,CursorY,selX,selY,selON}
end function

global procedure restoreBlockPos(sequence pos)
-- keep this in step with the above!.
    CursorX = pos[2]
    CursorY = pos[3]
    selX = pos[4]
    selY = pos[5]
    selON = pos[6]
end procedure

--with trace
global function extendOverStrike(integer newchar, integer replacedchar)
-- in OVRstrike mode, attempt to maintain a pair of insert & delete blocks,
-- rather than {insert 1 char, delete 1 char, insert 1 char, delete 1 char...
-- This makes undo behave much better, with the first undo restoring all 
-- the overwritten text.
integer pointer, pairlen
sequence action2
    --
    -- first, check everything to make sure it is mergeable, else return 0
    --
    pointer = actionptr[currfile]
    if pointer<2 then return 0 end if
    if pointer!=length(actions[currfile]) then return 0 end if
    action = actions[currfile][pointer-1]
    if action[TYPE]!=INSERTCHAR then return 0 end if
    if action[CURSORY]!=CursorY then return 0 end if
    if length(action[LINESET])!=1 then return 0 end if  -- must be single line
    pairlen = length(action[LINESET][1])
    if action[CURSORX]+pairlen!=CursorX then return 0 end if
    action2 = actions[currfile][pointer]
    if action2[TYPE]!=DELETECHAR then return 0 end if
    if action2[CURSORY]!=CursorY then return 0 end if
    if action2[CURSORX]!=CursorX then return 0 end if   
    if length(action2[LINESET])!=1 then return 0 end if -- must be single line
    if length(action2[LINESET][1])!=pairlen then return 0 end if
    --
    -- All seems fine, so append and return 1
    --
    action[LINESET][1] = append(action[LINESET][1],newchar)
    action[TIMESTAMP] = floor(time())
    actions[currfile][pointer-1] = action
    action2[LINESET][1] = append(action2[LINESET][1],replacedchar)
    action2[CURSORX] = action2[CURSORX] + 1
    action2[TIMESTAMP] = floor(time())
    actions[currfile][pointer] = action2
    return 1
end function

global constant UNDO = 0, REDO = 1

--with trace
global procedure UndoRedo(integer redo)
-- call using the constants UNDO and REDO.
integer pointer, l, k, aT
sequence oneline
object chunk
object dbg
integer mY, mX
object void
    if currfile<=length(actions) then
        clearSelection()
        pointer = actionptr[currfile]
        if (redo=UNDO and pointer>0)
        or (redo=REDO and pointer<length(actions[currfile])) then
            -- get current or next action:
            action = actions[currfile][pointer+redo]    -- hence 1/0.
            --
            -- Put the cursor at the start of the block
            --
            paintCursorY()
            CursorX = action[CURSORX]
            CursorY = action[CURSORY]
            selX = action[SELX]
            selY = action[SELY]
            selON = action[SELON]
            --DEV I'm not absolutely sure this is the 'right' thing to do.
            -- it certainly 'fixes' a bug: with say "include" in the clipboard,
            -- block select zero chars (by pressing shift right then shift left), 
            -- then paste, then undo.
            -- My first attempt was to remove the if selON = 0 line below (find
            --  16/10...) but that caused another bug: select block, delete, 
            --  and undo deleted length(action[LINESET]) lines too many.
            if selON and CursorX=selX and CursorY=selY then -- PL 16/10/05
                selON = 0
            end if
            aT = action[TYPE]
            if (redo=UNDO and aT<=DELETEBLOCK)
            or (redo=REDO and aT>=INSERTBLOCK) then
                --
                -- Re-insert it
                --
                if selON=2 then
                    mY = min(selY,CursorY)
                    mX = min(selX,CursorX)
                    for i=1 to length(action[LINESET]) do
                        if length(action[LINESET][i]) then
                            CursorY = mY+i-1
                            CursorX = mX
                            selON = 0
                            InsertBlock({action[LINESET][i]})
                        end if
                    end for
                    selON = 2
                else
                    if selON then
                        if CursorY>selY then
                            CursorY = selY
                            CursorX = selX
                        elsif CursorY=selY then
                            CursorX = min(selX,CursorX)
                        end if
                        selON = 0
                    end if
                    selX = CursorX
                    selY = CursorY
                    InsertBlock(action[LINESET])
                    -- Special case for combined overstrike pair
                    if action[TYPE]=DELETECHAR
                    and pointer>1
                    and not insertMode  --Umm: this may need (well, want) saving in actionlist
                                        --(not that CursorX being wrong (well, different) if we undo
                                        -- something after INS/OVR mode is changed is that critical)
                    and actions[currfile][pointer-1][TYPE]=INSERTCHAR
                    and length(action[LINESET])=1
                    and length(actions[currfile][pointer-1][LINESET])=1
                    and length(action[LINESET][1])=length(actions[currfile][pointer-1][LINESET][1]) then
                        mX = CursorX
                        CursorX = selX
                        selX = mX
                    end if
--DEV pass in the trigger key?? (I don't use this anyway)
--                  if getKeyState(VK_SHIFT) or selY=CursorY then
                    if selY=CursorY then
                        selON=1
                    end if
                end if

            elsif (redo=UNDO and aT>=INSERTBLOCK)
               or (redo=REDO and aT<=DELETEBLOCK) then
                --
                -- Mark the block as selected and delete it
                --
                if selON=0 then
                    selON = 1
                    l = length(action[LINESET])
                    if l>1 then
                        selX = ExpLength(action[LINESET][l])
                    else
                        oneline = filetext[currfile][CursorY+1]
                        k = MapToByte(oneline,CursorX)
                        if k then
                            selX = ExpLength(oneline[1..k+length(action[LINESET][1])-1])
                        else
                            selX = ExpLength(oneline)
                        end if
                    end if
                    selY = CursorY + l - 1
                end if
                chunk = getSelection(SEL_DELETE)
                selON = 0
                -- sanity check:
                if not equal(chunk,action[LINESET]) then
                    dbg = action[LINESET]
                    -- Not to be translated: (deliberately awful as no-one should ever see it!)
--                  void = messageBox("WHOOPS!",
                    IupMessage("WHOOPS!",
                    "undo/redo has done wrong!\n\n"&
                    "You should obviously never see this message.\n\n"&
                    "Please check the results of the last undo/redo carefully,\n"&
--                  "save your work, and restart the editor.",0)
                    "save your work, and restart the editor.")
                end if

            else
                if redo=UNDO then
                    aT = 0 - aT             -- flip INDENT/UNINDENT
                end if
                if selON then
                    CursorY = selY
                end if
                for j=1 to length(action[LINESET][2]) do
                    if action[LINESET][2][j] then
                        if aT=UNINDENT then
                            void = unindentoneline(action[LINESET][1])
                        else
                            void = indentoneline(action[LINESET][1])
                        end if
                    end if
                    CursorY += 1
                    selY = CursorY
                end for
                CursorY = action[CURSORY]
                selON = 0
            end if
            if redo=UNDO then
                actionptr[currfile] = pointer - 1
            else
                actionptr[currfile] = pointer + 1
            end if
--DEV
            setSaveIcon()--currfile,1,1)
        end if
    end if
    forceCursorOnscreen()
end procedure
