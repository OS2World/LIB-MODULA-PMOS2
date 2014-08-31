(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE Windows;

        (********************************************************)
        (*                                                      *)
        (*                  Text-mode screen windows.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        27 September 1996               *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (*    Faults:                                           *)
        (*      1. (fixed)                                      *)
        (*      2. (fixed)                                      *)
        (*                                                      *)
        (*      This is the version that uses ANSI escape       *)
        (*      sequences for things like cursor control.       *)
        (*                                                      *)
        (*      For further thought at some later stage:        *)
        (*         -    (maybe) extend the set of procedures    *)
        (*              for which w^.access0p5 is locked.       *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8,
    (* proc *)  ADR;

IMPORT StdChans, IOChan;

FROM Types IMPORT
    (* proc *)  FarPointer, FarCharPointer;

FROM LowLevel IMPORT
    (* proc *)  Far, MakePointer, FarAddOffset, Copy, FarCopy, CopyUp, IXORB;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM TerminationControl IMPORT
    (* proc *)  SetTerminationProcedure, TerminationMessage;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateTask, CreateLock, DestroyLock,
                Obtain, Release, ReleaseAllLocks;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM Keyboard IMPORT
    (* proc *)  InKey, PutBack;

(************************************************************************)
(*      If you want black-and-white operation even though your          *)
(*      display supports colour (e.g. for the case where the colours    *)
(*      are not very readable), set ForcedMonochrome to TRUE.           *)
(*      Otherwise, this module selects colour operation if it           *)
(*      thinks that the hardware can support it.                        *)
(************************************************************************)

CONST ForcedMonochrome = FALSE;

(************************************************************************)

CONST
    BytesPerChar = 2;                   (* # bytes/char in video buffer *)
    CharsPerRow = MaxColumnNumber + 1;  (* characters per row           *)
    BytesPerRow = BytesPerChar*CharsPerRow;
    CharsInBuffer = CharsPerRow * (MaxRowNumber+1);
    BytesInBuffer = BytesPerChar * CharsInBuffer;
                                        (* size of window buffer        *)
    PageSize = 4096;                    (* offset to next display page  *)

    DefaultPage = 0;
    DefaultTabs =
"        T       T       T       T       T       T       T       T       T";

(************************************************************************)

TYPE
    BufferSubscript = [0..CharsInBuffer - 1];
    ExtendedBufferSubscript = [0..CharsInBuffer];

    CloseHandlerList = POINTER TO CloseHandlerRecord;
    CloseHandlerRecord = RECORD
                            next: CloseHandlerList;
                            proc: CloseHandlerProc;
                         END (*RECORD*);

    PageChangeList = POINTER TO PageChangeRecord;
    PageChangeRecord = RECORD
                          next: PageChangeList;
                          proc: PageChangeHandler;
                       END (*RECORD*);

    ScreenChar = RECORD
                     val: CHAR;
                     attr: CARD8;
                 END (*RECORD*);

    Window = POINTER TO WindowData;

        (****************************************************************)
        (*                                                              *)
        (* WindowData records are linked (by next and previous) as a    *)
        (* doubly linked list, to implement a stack of windows.         *)
        (* There is a separate stack for each display page.             *)
        (* Variable TopWindow[page] points to the top of this stack.    *)
        (*                                                              *)
        (* The access0p5 lock is to control simultaneous access         *)
        (* to a single window by multiple tasks.  This is a new         *)
        (* feature, so the protection is implemented for some but not   *)
        (* all operations at this stage.  I'm still working on whether  *)
        (* access0p5 should be a Lock or a Semaphore.  From the         *)
        (* viewpoint of the external caller it's better to make it a    *)
        (* Lock, but that creates problems during shutdown.             *)
        (*                                                              *)
        (* CloseList points to a linked list of procedures to be        *)
        (* called when this window is closed.                           *)
        (*                                                              *)
        (* The row and column values stored in this record are actual   *)
        (* screen row and column, i.e. they are not window-relative.    *)
        (*                                                              *)
        (* The ScreenPosition field is a subscript into the hardware    *)
        (* video buffer.  It can be computed easily from the "page",    *)
        (* "row", and "column" fields, but it is more convenient to     *)
        (* keep this technically redundant variable.                    *)
        (*                                                              *)
        (* BufferBias is the difference between a byte location in the  *)
        (* window's buffer and the corresponding ScreenPosition value.  *)
        (* In the present version it's always equal to PageSize*page,   *)
        (* but I might want to change that in future.                   *)
        (*                                                              *)
        (* A window's InputWaiting flag is set when it is waiting for   *)
        (* a keyboard character.  When the character arrives it is      *)
        (* stored in the InputChar field of this record, and we are     *)
        (* notified of this by a Release(CharAvailable).  Because the   *)
        (* requests for input are not necessarily satisfied in a FIFO   *)
        (* order - because the input focus can change before the input  *)
        (* arrives - we need a separate semaphore and InputChar field   *)
        (* for each window, even though these fields remain unused for  *)
        (* most windows.                                                *)
        (*                                                              *)
        (* A window with the "hidden" flag set is not part of the       *)
        (* stack of windows, and is not visible on the screen.          *)
        (*                                                              *)
        (* The "obscured" field indicates whether this window is wholly *)
        (* or partially obscured by another window on the screen.  By   *)
        (* keeping track of this, we can avoid some unnecessary screen  *)
        (* refreshing.                                                  *)
        (*                                                              *)
        (* The "blockcursor" field specifies what kind of cursor to     *)
        (* display: a block cursor if TRUE, an underline cursor if      *)
        (* FALSE.  Most of the time this is irrelevant, as we display   *)
        (* a cursor only when the window's CursorWanted flag is TRUE.   *)
        (*                                                              *)
        (* The "buffer" array holds a copy of what is supposed to be    *)
        (* transferred to the video buffer.                             *)
        (*                                                              *)
        (****************************************************************)

    WindowData = RECORD
                    next, previous: Window;
                    access0p5: Semaphore;
                    CloseList: CloseHandlerList;
                    frame: FrameType;  divider: DividerType;
                    tabstops: ARRAY ColumnRange OF CHAR;
                    ScrollRegion, DefaultScrollRegion: Rectangle;
                    page: DisplayPage;
                    FirstRow, LastRow, row: RowRange;
                    FirstColumn, LastColumn: ColumnRange;
                    column: [0..MAX(ColumnRange)+1];
                    ScreenPosition, BufferBias: CARDINAL;
                    foreground, background: Colour;
                    CurrentAttributes: CARD8;
                    InputChar: CHAR;
                    CharAvailable: Semaphore;
                    InputWaiting, hidden, obscured,
                        CursorWanted, blockcursor, WrapOption: BOOLEAN;
                    buffer: ARRAY BufferSubscript OF ScreenChar;
                 END (*RECORD*);

(************************************************************************)
(*                 NOTE ON CRITICAL SECTION PROTECTION                  *)
(* The potential deadlock problems in this module are surprisingly      *)
(* subtle, arising from the fact that a procedure incorporating one     *)
(* form of critical section protection may call other procedures which  *)
(* themselves contain critical section protection.  To avoid these      *)
(* problems, we use an ordered resource policy.  Each critical section  *)
(* protection semaphore is given a "level", which for clarity is shown  *)
(* as the last character of its name.  A piece of code is said to be    *)
(* executing at level N if it is inside a critical section protected    *)
(* by a semaphore whose level is N (and not inside a critical section   *)
(* protected by any semaphore of any higher level).  The rule which     *)
(* avoids deadlock is: to lock a semaphore at level N, we must be       *)
(* executing at a level < N.                                            *)
(* For the purposes of this analysis, Locks and Semaphores are treated  *)
(* as being equivalent.                                                 *)
(*                                                                      *)
(*                      LATEST DEVELOPMENT                              *)
(* Because I'm adding a new semaphore, but haven't yet updated the      *)
(* notation, a new level 0p5 (meaning 0.5) has been added.              *)
(************************************************************************)

VAR

    (* Standard output channel. *)

    StdOut: StdChans.ChanId;

    (* BlackAndWhite is true if we have a monochrome display.           *)

    BlackAndWhite: BOOLEAN;

    (* Variables needed for the shutdown operation. *)

    ShutdownInProgress: BOOLEAN;
    KeyTaskFinished: Semaphore;

    (* ActivePage is the display page currently visible on the screen.  *)

    ActivePage: DisplayPage;

    (* A list of procedures to call whenever the current display page   *)
    (* is changed, and a lock to control access to this list.           *)

    PageChangeProcs: PageChangeList;
    PageChangeListAccess: Lock;

    (* A semaphore to say that somebody wants some keyboard input.      *)

    InputRequest: Semaphore;

    (* BlankRow is set up by the initialisation code as a row of space  *)
    (* characters.  Note however that the attribute codes need to be    *)
    (* filled in before each use.                                       *)

    BlankRow: ARRAY [0..CharsPerRow-1] OF ScreenChar;

    (* Access to BlankRow is a critical section, so we protect it with  *)
    (* a Lock.                                                          *)

    BlankRowAccess1: Lock;

    (* StackAccess2 is used to protect access to the shared data        *)
    (* structure which defines the stacks of windows.                   *)

    StackAccess2: Lock;

    (* TopWindow[p] is the current top of the stack of windows for      *)
    (* display on physical display page p.                              *)

    TopWindow: ARRAY DisplayPage OF Window;

    (* ScreenAccess3 is used to protect access to memory in segment     *)
    (* ScreenSeg, i.e. the memory belonging to the physical screen.     *)

    ScreenAccess3: Lock;

    (* PhysicalCursor keeps track of the blinking screen cursor.  The   *)
    (* CursorWindow field shows which window, if any, currently "owns"  *)
    (* the physical cursor.  CursorVisible[page] shows whether the      *)
    (* cursor should be visible when "page" is the active display page. *)
    (* ScreenPos and Attributes are the position and display attributes *)
    (* that we expect for the next character to be physically written   *)
    (* to the screen.                                                   *)
    (* Lock access4 is used to protect these variables and the hardware *)
    (* operations of turning the cursor on and off.                     *)
    (* For now, access4 also protects alterations to the active         *)
    (* display page.                                                    *)

    PhysicalCursor: RECORD
                        access4: Lock;
                        CursorWindow: ARRAY DisplayPage OF Window;
                        CursorVisible: ARRAY DisplayPage OF BOOLEAN;
                        ScreenPos: CARDINAL;
                        Attributes: CARD8;
                    END (*RECORD*);

(************************************************************************)
(*                       ANSI ESCAPE SEQUENCES                          *)
(************************************************************************)

PROCEDURE PutCode (ch: CHAR);

    BEGIN
        IOChan.RawWrite (StdOut, ADR(ch), 1);
    END PutCode;

(************************************************************************)

PROCEDURE PutEscBracket;

    CONST Esc = CHR(27);

    BEGIN
        PutCode (Esc);  PutCode ("[");
    END PutEscBracket;

(************************************************************************)

PROCEDURE PutNumber (N: CARDINAL);

    (* Assumption: N<99.  (This lets us make the code a little more efficient.) *)

    BEGIN
        IF N >= 10 THEN
            PutCode (CHR(ORD("0") + N DIV 10));
            N := N MOD 10;
        END (*IF*);
        PutCode (CHR(ORD("0") + N));
    END PutNumber;

(************************************************************************)

PROCEDURE ClearWholeScreen;

    (* Clears the physical screen. *)

    BEGIN
        PutEscBracket;  PutCode ("2");  PutCode ("J");
        PhysicalCursor.ScreenPos := 0;
    END ClearWholeScreen;

(************************************************************************)

PROCEDURE SetCursorPosition (row, col: CARDINAL);

    (* Sets the cursor to the specified row and column.      *)

    VAR NewScreenPos: CARDINAL;

    BEGIN
        NewScreenPos := BytesPerRow*row + BytesPerChar*col;
        IF NewScreenPos <> PhysicalCursor.ScreenPos THEN
            PutEscBracket;  PutNumber (row+1);  PutCode (";");
            PutNumber (col+1);  PutCode ("H");
            PhysicalCursor.ScreenPos := NewScreenPos;
        END (*IF*);
    END SetCursorPosition;

(************************************************************************)

PROCEDURE SetAttributes (attributes: CARD8);

    (* Ensures that the physical screen colour/blinking attributes      *)
    (* match what is specified in the parameter.                        *)

    TYPE ColourTable = ARRAY [0..7] OF CARDINAL;

    CONST
        (* Input ordering is black,blue,green,cyan,red,magenta,brown,white. *)
        (* Result ording is black,red,green,yellow,blue,magenta,cyan,white. *)

        ForeCode = ColourTable {30, 34, 32, 36, 31, 35, 33, 37};
        BackCode = ColourTable {40, 44, 42, 46, 41, 45, 43, 47};

    VAR foreground, background: CARDINAL;

    BEGIN
        IF PhysicalCursor.Attributes <> attributes THEN
            foreground := attributes MOD 16;
            background := attributes DIV 16;
            PutEscBracket;
            IF background >= 8 THEN
                (* Turn blinking on. *)
                PutCode ("5");
                DEC (background, 8);
            ELSE
                (* Turn blinking off. *)
                PutCode ("0");
            END (*IF*);
            IF foreground >= 8 THEN
                (* Turn bold on. *)
                PutCode (";");  PutCode ("1");
                DEC (foreground, 8);
            END (*IF*);

            (* Now set the foreground and background colours. *)

            PutCode (";");  PutNumber (BackCode[background]);
            PutCode (";");  PutNumber (ForeCode[foreground]);
            PutCode ("m");

            PhysicalCursor.Attributes := attributes;

        END (*IF*);

    END SetAttributes;

(************************************************************************)

PROCEDURE PutChar (ch: CHAR;  attributes: CARD8);

    (* Writes ch to the screen, and updates PhysicalCursor.ScreenPos.   *)

    BEGIN
        SetAttributes (attributes);
        IOChan.RawWrite (StdOut, ADR(ch), 1);
        INC (PhysicalCursor.ScreenPos, BytesPerChar);
    END PutChar;

(************************************************************************)
(*                 MISCELLANEOUS HARDWARE OPERATIONS                    *)
(************************************************************************)

PROCEDURE SetTextPage (page: CARDINAL);

    (* Switches the display to the specified hardware screen page.      *)
    (* Present version does nothing.                                    *)

    BEGIN
    END SetTextPage;

(************************************************************************)
(*                 TURNING THE SCREEN CURSOR ON AND OFF                 *)
(************************************************************************)

PROCEDURE CursorOff;

    (* Turns the cursor off.  Current version does nothing.      *)

    BEGIN
        (*PositionCursor (FALSE, 0, FALSE);*)
    END CursorOff;

(************************************************************************)

PROCEDURE CursorOn (position: CARDINAL;  blockcursor: BOOLEAN);

    (* Displays a blinking screen cursor at the specified position.     *)
    (* Current version sets the cursor position but does not do         *)
    (* anything about its visibility.                                   *)

    BEGIN
        SetCursorPosition (position DIV BytesPerRow, position MOD BytesPerRow DIV 2);
    END CursorOn;

(************************************************************************)

PROCEDURE UpdatePhysicalCursor;

    (* Turns the physical cursor on or off, as appropriate.  Also       *)
    (* signals a new input request, if a window on the active page is   *)
    (* waiting for input, in case the input task has gone idle.         *)
    (* The caller must be running at level<4.                           *)

    VAR w: Window;

    BEGIN
        WITH PhysicalCursor DO
            Obtain (access4);
            w := CursorWindow[ActivePage];
            IF w <> NIL THEN
                Signal (InputRequest);
            END (*IF*);
            IF CursorVisible[ActivePage] THEN
                CursorOn (w^.ScreenPosition, w^.blockcursor);
            ELSE
                CursorOff;
            END (*IF*);
            Release (access4);
        END (*WITH*);
    END UpdatePhysicalCursor;

(************************************************************************)
(*                  HARDWARE DISPLAY PAGE CHANGES                       *)
(************************************************************************)

PROCEDURE SetActivePage (page: DisplayPage);

    (* Changes the active display page. *)

    VAR PCL: PageChangeList;

    BEGIN
        Obtain (PhysicalCursor.access4);
        SetTextPage (page);
        ActivePage := page;
        Release (PhysicalCursor.access4);

        (* Call the procedures which want notification of the change.   *)

        Obtain (PageChangeListAccess);
        PCL := PageChangeProcs;
        WHILE PCL <> NIL DO
            PCL^.proc (page);
            PCL := PCL^.next;
        END (*WHILE*);
        Release (PageChangeListAccess);

        (* Turn the cursor off or on, as appropriate. *)

        UpdatePhysicalCursor;

    END SetActivePage;

(************************************************************************)

PROCEDURE RequestPageChangeNotification (Proc: PageChangeHandler);

    (* Sets up Proc as a procedure to be called on a page change.       *)

    VAR PCL: PageChangeList;

    BEGIN
        NEW (PCL);
        Obtain (PageChangeListAccess);
        WITH PCL^ DO
            next := PageChangeProcs;
            proc := Proc;
        END (*WITH*);
        PageChangeProcs := PCL;
        Release (PageChangeListAccess);
    END RequestPageChangeNotification;

(************************************************************************)

PROCEDURE PageOf (w: Window): DisplayPage;

    (* Returns the display page on which window w resides. *)

    BEGIN
        RETURN w^.page;
    END PageOf;

(************************************************************************)
(*              MANIPULATION OF THE STACK OF WINDOWS                    *)
(************************************************************************)

PROCEDURE UnLink (w: Window);

    (* Removes w^ from the stack, but otherwise leaves it unchanged.    *)
    (* Caller must have locked StackAccess2.                            *)

    BEGIN
        WITH w^ DO
            IF previous <> NIL THEN previous^.next := next END (*IF*);
            IF next <> NIL THEN next^.previous := previous END (*IF*);
            IF TopWindow[page] = w THEN TopWindow[page] := next END (*IF*);
            previous := NIL;  next := NIL;
        END (*WITH*);
    END UnLink;

(************************************************************************)

PROCEDURE IdentifyTopWindow (VAR (*OUT*) w: Window;
                                VAR (*INOUT*) row: RowRange;
                                VAR (*INOUT*) col: ColumnRange): BOOLEAN;

    (* On entry w is unspecified and (row,col) describes a position on  *)
    (* the screen.  On exit w is equal to the top window containing     *)
    (* this screen location, and (row,col) have been altered to be      *)
    (* window-relative coordinates.  Exception: if there is no visible  *)
    (* window containing the given point, the function result is FALSE, *)
    (* the returned w is NIL, and row and col are unchanged.            *)

    BEGIN
        Obtain (StackAccess2);
        w := TopWindow[ActivePage];
        LOOP
            IF w = NIL THEN EXIT(*LOOP*);
            ELSIF (col >= w^.FirstColumn) AND (col <= w^.LastColumn)
                        AND (row >= w^.FirstRow) AND (row <= w^.LastRow) THEN
                DEC (row, w^.FirstRow);  DEC (col, w^.FirstColumn);
                EXIT (*LOOP*);
            ELSE
                w := w^.next;
            END (*IF*);
        END (*LOOP*);
        Release (StackAccess2);
        RETURN w <> NIL;
    END IdentifyTopWindow;

(************************************************************************)

PROCEDURE ComputeCursorWindow (page: DisplayPage);

    (* Rechecks which window on this page should have the physical      *)
    (* screen cursor, and displays or turns off the cursor, as          *)
    (* appropriate, if a change is needed.  This procedure should be    *)
    (* called whenever there is a chance that the input focus might     *)
    (* need to be shifted.                                              *)

    VAR w, wtop: Window;  visible: BOOLEAN;
        row: RowRange;  col: ColumnRange;

    BEGIN
        (* Find the top window that's waiting for input. *)

        Obtain (StackAccess2);
        w := TopWindow[page];
        LOOP
            IF w = NIL THEN EXIT(*LOOP*) END(*IF*);
            IF w^.InputWaiting THEN EXIT(*LOOP*) END(*IF*);
            w := w^.next;
        END (*LOOP*);
        Release (StackAccess2);

        (* Check whether the cursor should be visible. *)

        IF w = NIL THEN
            visible := FALSE;
        ELSE
            row := w^.row;  col := w^.column;
            visible := IdentifyTopWindow (wtop, row, col) AND (wtop = w);
        END (*IF*);

        (* Turn the cursor on or off, if necessary. *)

        WITH PhysicalCursor DO
            Obtain (access4);
            IF (w = NIL) OR (CursorWindow[page] <> w) THEN
                CursorWindow[page] := w;
                CursorVisible[page] := visible AND w^.CursorWanted;
                IF page = ActivePage THEN
                    IF CursorVisible[page] THEN
                        CursorOn (w^.ScreenPosition, w^.blockcursor);
                    ELSE
                        CursorOff;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            Release (access4);
        END (*WITH*);
    END ComputeCursorWindow;

(************************************************************************)
(*                         SCREEN REFRESHING                            *)
(************************************************************************)

PROCEDURE WritePartialRow (w: Window;  row: RowRange;  startcol: ColumnRange;
                                       count: CARDINAL);

    (* Writes a string to the screen, starting at position              *)
    (* (row,startcol) and writing a total of count characters.  The row *)
    (* and startcol parameters are screen-relative, not window-relative.*)

    VAR offset: ExtendedBufferSubscript;

    BEGIN
        offset := CharsPerRow*row + startcol;
        Obtain (ScreenAccess3);
        SetCursorPosition (row, startcol);
        WHILE count > 0 DO
            WITH w^.buffer[offset] DO
                PutChar (val, attr);
            END (*WITH*);
            INC (offset);
            DEC (count);
        END (*WHILE*);
        Release (ScreenAccess3);
    END WritePartialRow;

(************************************************************************)

PROCEDURE WriteSpaces (row: RowRange;  startcol: ColumnRange;
                                       count: CARDINAL;  attributes: CARD8);

    (* Like WritePartialRow, but writes space characters. *)

    BEGIN
        Obtain (ScreenAccess3);
        SetCursorPosition (row, startcol);
        WHILE count > 0 DO
            PutChar (" ", attributes);
            DEC (count);
        END (*WHILE*);
        Release (ScreenAccess3);
    END WriteSpaces;

(************************************************************************)

PROCEDURE PartialRefresh (w: Window;  startrow, endrow: RowRange;
                                        startcol, endcol: ColumnRange);

    (* Re-draws the image of window w on the screen, in the area        *)
    (* bounded by the given absolute screen coordinates.  The ranges    *)
    (* specified are inclusive limits.                                  *)
    (* Caller must be executing at level <3.                            *)

    VAR i: RowRange;  count: CARDINAL;

    BEGIN
        WITH w^ DO

            (* Work out the overlap between the region and the window.  *)

            IF FirstRow > startrow THEN startrow := FirstRow END (*IF*);
            IF LastRow < endrow THEN endrow := LastRow END (*IF*);
            IF FirstColumn > startcol THEN startcol := FirstColumn END (*IF*);
            IF LastColumn < endcol THEN endcol := LastColumn END (*IF*);

            (* Refresh that region, if it is nonempty.  *)

            IF (startrow <= endrow) AND (startcol <= endcol) THEN
                count := endcol - startcol + 1;
                FOR i := startrow TO endrow DO
                    WritePartialRow (w, i, startcol, count);
                END (*FOR*);
            END (*IF*);
        END (*WITH*);
    END PartialRefresh;

(************************************************************************)

PROCEDURE Refresh (w: Window);

    (* Re-draws the image of window w on the screen.    *)
    (* Caller must be executing at level <3.            *)

    VAR i: RowRange;  count: CARDINAL;

    BEGIN
        WITH w^ DO
            count := LastColumn - FirstColumn + 1;
            FOR i := FirstRow TO LastRow DO
                WritePartialRow (w, i, FirstColumn, count);
            END (*FOR*);
            obscured := FALSE;
        END (*WITH*);
    END Refresh;

(************************************************************************)

PROCEDURE ComputeCollisions (w: Window);

    (* Updates the "obscured" field of all windows which are below this *)
    (* one on the stack, and sets w^.obscured to FALSE.  Also updates   *)
    (* the cursor visibility information, based on the assumption that  *)
    (* w is the window on top of its stack.                             *)
    (* Caller must have locked StackAccess2.                            *)
    (* Caller must be executing at level <4.                            *)

    VAR left, right: ColumnRange;
        top, bottom: RowRange;
        w2: Window;  p: DisplayPage;

    BEGIN
        (* Take note of the screen location of this window. *)

        WITH w^ DO
            obscured := FALSE;
            left := FirstColumn;  right := LastColumn;
            top := FirstRow;  bottom := LastRow;
            w2 := next;  p := page;
        END (*WITH*);

        (* Update the cursor visibility information. *)

        WITH PhysicalCursor DO
            Obtain (access4);
            IF w^.InputWaiting THEN
                CursorWindow[p] := w;
                CursorVisible[p] := w^.CursorWanted;
            ELSIF CursorVisible[p] THEN
                WITH CursorWindow[p]^ DO
                    CursorVisible[p] := (row < top) OR (row > bottom)
                                OR (column < left) OR (column > right);
                END (*WITH*);
            END (*IF*);
            Release (access4);
        END (*WITH*);

        (* Update the "obscured" information for all windows under      *)
        (* the current window.  (For those which are already obscured   *)
        (* by some other window, no further check is needed.)           *)

        WHILE w2 <> NIL DO
            WITH w2^ DO
                IF NOT obscured THEN
                    obscured := (LastColumn >= left) AND (FirstColumn <= right)
                                AND (LastRow >= top) AND (FirstRow <= bottom);
                END (*IF*);
            END (*WITH*);
            w2 := w2^.next;
        END (*WHILE*);
    END ComputeCollisions;

(************************************************************************)

PROCEDURE PutOnTopI (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen. *)
    (* This procedure does the same job as PutOnTop (see below), but    *)
    (* different entry assumptions.                                     *)
    (* The caller must have locked StackAccess2.                        *)
    (* The caller must be executing at level 2.                         *)

    BEGIN
        UnLink (w);
        IF TopWindow[w^.page] <> NIL THEN
            TopWindow[w^.page]^.previous := w
        END (*IF*);
        w^.next := TopWindow[w^.page];  TopWindow[w^.page] := w;
        Refresh (w);  ComputeCollisions (w);
        IF w^.page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
    END PutOnTopI;

(************************************************************************)

PROCEDURE PutOnTop (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen. *)
    (* This also unhides w if it was hidden.                            *)
    (* This is the externally callable version.                         *)
    (* The caller must be executing at a level <0.5.                    *)

    BEGIN
        Wait (w^.access0p5);
        Obtain (StackAccess2);
        w^.hidden := FALSE;
        IF TopWindow[w^.page] <> w THEN
            PutOnTopI (w);
        ELSIF w^.page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
        Release (StackAccess2);
        Signal (w^.access0p5);
    END PutOnTop;

(************************************************************************)

PROCEDURE Repaint (page: DisplayPage;  startrow, endrow: RowRange;
                                        startcol, endcol: ColumnRange);

    (* Repaints the specified (inclusive) rectangular region on the     *)
    (* screen, and sets the physical screen cursor as necessary.        *)
    (* The caller must be executing at level <1.                        *)

    CONST NormalVideo = 07H;

    VAR i: RowRange;
        count: CARDINAL;
        p: Window;

    BEGIN
        (* First, clear the region.     *)

        IF (startrow = 0) AND (endrow = MaxRowNumber) AND (startcol = 0)
                         AND (endcol = MaxColumnNumber) THEN
            ClearWholeScreen;
        ELSE
            count := endcol - startcol + 1;
            FOR i := startrow TO endrow DO
                WriteSpaces (i, startcol, count, NormalVideo);
            END (*FOR*);
        END (*IF*);

        (* Now refresh all open windows (or, more precisely, the parts  *)
        (* of them which lie in the affected region).                   *)

        Obtain (StackAccess2);
        WITH PhysicalCursor DO
            Obtain (access4);
            CursorWindow[page] := NIL;
            CursorVisible[page] := FALSE;
            Release (access4);
        END (*WITH*);
        IF TopWindow[page] <> NIL THEN
            p := TopWindow[page];
            WHILE p^.next <> NIL DO  p := p^.next  END (*WHILE*);
            REPEAT
                PartialRefresh (p, startrow, endrow, startcol, endcol);
                ComputeCollisions (p);  p := p^.previous;
            UNTIL p = NIL;
        END (*IF*);
        IF page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
        Release (StackAccess2);
    END Repaint;

(************************************************************************)

PROCEDURE RefreshDisplay;

    (* Rewrites every open window.  Should not normally be needed, but  *)
    (* available for use in cases the display is corrupted by, for      *)
    (* example, software which bypasses this module and writes directly *)
    (* to the screen.                                                   *)
    (* The caller must be executing at level <1.                        *)

    VAR p: DisplayPage;

    BEGIN
        FOR p := 0 TO MAX(DisplayPage) DO
            Repaint (p, 0, MAX(RowRange), 0, MAX(ColumnRange));
        END (*FOR*);
    END RefreshDisplay;

(************************************************************************)

PROCEDURE Hide (w: Window);

    (* Makes this window invisible on the screen.  It is still possible *)
    (* to write to the window, but the output will not appear until     *)
    (* a PutOnTop(w) is executed.                                       *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        Wait (w^.access0p5);
        Obtain (StackAccess2);
        w^.hidden := TRUE;
        UnLink (w);
        Release (StackAccess2);
        Signal (w^.access0p5);

        (* Repaint the part of the screen which this window occupied.   *)

        WITH w^ DO
            Repaint (page, FirstRow, LastRow, FirstColumn, LastColumn);
        END (*WITH*);

    END Hide;

(************************************************************************)

PROCEDURE PutOnPage (w: Window;  p: DisplayPage);

    (* Moves window w to another display page.  The default is to put   *)
    (* every window on page 0 when it is first opened.  To override     *)
    (* the default, call this procedure after opening the window.       *)

    VAR wasvisible: BOOLEAN;

    BEGIN
        wasvisible := NOT w^.hidden;
        Hide (w);

        WITH w^ DO

            (* Change the page. *)

            ScreenPosition := ScreenPosition + PageSize*(p - page);
            page := p;
            BufferBias := PageSize*page;

            IF wasvisible THEN
                hidden := FALSE;  PutOnTop (w);
            END(*IF*);

        END (*WITH*);

    END PutOnPage;

(************************************************************************)
(*                      SETTING THE SCROLLING REGION                    *)
(************************************************************************)

PROCEDURE InScrollingRegion (w: Window): BOOLEAN;

    (* Returns TRUE iff the current cursor position of window w is      *)
    (* inside its scrolling region.                                     *)

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                RETURN (row >= top) AND (row <= bottom)
                        AND (column >= left) AND (column <= right);
            END (*WITH*);
        END (*WITH*);
    END InScrollingRegion;

(************************************************************************)

PROCEDURE ChangeScrollingRegion (w: Window;  firstline, lastline: RowRange);

    (* Changes the scrolling region of window w to the new line         *)
    (* boundaries given, and sets its cursor to the start of the new    *)
    (* scrolling region.  The line numbers are window-relative.         *)

    VAR horizontal, vertical, leftT, rightT: CHAR;
        j: ColumnRange;
        place2: BufferSubscript;

    BEGIN
        (* Although the user specifies window-relative line numbers,    *)
        (* we use screen-relative numbers internally.  Adjust the       *)
        (* parameters to take this into account.                        *)

        INC (firstline, w^.FirstRow);
        INC (lastline, w^.FirstRow);

        (* Work out what characters to use for the frame and divider.   *)

        horizontal := 'Ä';  vertical := '³';
        leftT := 'Ã';  rightT := '´';
        WITH w^ DO
            IF divider = doubledivider THEN
                horizontal := 'Í';
            END (*IF*);
            IF frame = doubleframe THEN
                vertical := 'º';
                IF divider = doubledivider THEN
                    leftT := 'Ì';  rightT := '¹';
                ELSE
                    leftT := 'Ç';  rightT := '¶';
                END (*IF*);
            ELSIF divider = doubledivider THEN
                leftT := 'Æ';  rightT := 'µ';
            END (*IF*);

            (* Clean up the frame. *)

            ScrollRegion := DefaultScrollRegion;
            IF frame <> noframe THEN

                (* Remove the left and right T belonging to the *)
                (* old divider bars, if necessary.              *)

                IF ScrollRegion.top - 1 > FirstRow THEN
                    place2 := CharsPerRow*(ScrollRegion.top - 1) + FirstColumn;
                    buffer[place2].val := vertical;
                    buffer[place2 + LastColumn - FirstColumn].val := vertical;
                END (*IF*);

                IF ScrollRegion.bottom + 1 < LastRow THEN
                    place2 := CharsPerRow*(ScrollRegion.bottom+1) + FirstColumn;
                    buffer[place2].val := vertical;
                    buffer[place2 + LastColumn - FirstColumn].val := vertical;
                END (*IF*);
            END (*IF*);

            (* Put in the new divider bars.     *)

            IF divider <> nodivider THEN

                (* Put in the top horizontal bar.       *)

                IF firstline > FirstRow + 1 THEN
                    place2 := CharsPerRow*(firstline-1) + FirstColumn;
                    IF frame <> noframe THEN
                        buffer[place2].val := leftT;  INC (place2);
                    END (*IF*);
                    FOR j := ScrollRegion.left TO ScrollRegion.right DO
                        buffer[place2].val := horizontal;  INC(place2);
                    END (*FOR*);
                    IF frame <> noframe THEN
                        buffer[place2].val := rightT;
                    END (*IF*);
                END (*IF*);

                (* Put in the bottom horizontal bar.    *)

                IF lastline < LastRow - 1 THEN
                    place2 := CharsPerRow*(lastline+1) + FirstColumn;
                    IF frame <> noframe THEN
                        buffer[place2].val := leftT;  INC (place2);
                    END (*IF*);
                    FOR j := ScrollRegion.left TO ScrollRegion.right DO
                        buffer[place2].val := horizontal; INC (place2);
                    END (*FOR*);
                    IF frame <> noframe THEN
                        buffer[place2].val := rightT;
                    END (*IF*);
                END (*IF*);

            END (*IF*);

            (* Finally, update the scrolling region parameters. *)

            WITH ScrollRegion DO
                top := firstline;  bottom := lastline;
            END (*WITH*);
            DefaultScrollRegion := ScrollRegion;
            SetCursor (w, firstline - FirstRow,
                                ScrollRegion.left - FirstColumn);
            Obtain (StackAccess2);
            IF NOT (hidden OR obscured) THEN
                Refresh (w);
            END (*IF*);
            Release (StackAccess2);
        END (*WITH*);
    END ChangeScrollingRegion;

(************************************************************************)

PROCEDURE NewScrollingRegion (w: Window;  firstline, lastline: RowRange;
                                firstcolumn, lastcolumn: ColumnRange);

    (* Changes the scrolling region of w to be the specified rectangle, *)
    (* but unlike ChangeScrollingRegion this procedure does not redraw  *)
    (* the dividers.  Furthermore the old scrolling region set by       *)
    (* ChangeScrollingRegion is remembered and may be restored by a     *)
    (* call to ResetScrollingRegion.                                    *)

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                top := FirstRow+firstline;  bottom := FirstRow+lastline;
                left := FirstColumn+firstcolumn;
                right := FirstColumn+lastcolumn;
            END (*WITH*);
        END (*WITH*);
    END NewScrollingRegion;

(************************************************************************)

PROCEDURE ResetScrollingRegion (w: Window);

    (* Changes the scrolling region of w back to what it was the last   *)
    (* time ChangeScrollingRegion was called.  If ChangeScrollingRegion *)
    (* was never called, the scrolling region goes back to being the    *)
    (* entire window minus the frame (if any).                          *)

    BEGIN
        w^.ScrollRegion := w^.DefaultScrollRegion;
    END ResetScrollingRegion;

(************************************************************************)
(*                          OPENING A WINDOW                            *)
(************************************************************************)

PROCEDURE FillInFrame (w: Window);

    (* Puts the box around the window into the window buffer.   *)

    VAR i: RowRange;  j: ColumnRange;
        corner: ARRAY [1..4] OF CHAR;
        horizontal, vertical: CHAR;
        place2, offset2: BufferSubscript;

    BEGIN
        IF w^.frame = simpleframe THEN
            corner[1] := 'Ú';  corner[2] := '¿';
            corner[3] := 'À';  corner[4] := 'Ù';
            horizontal := 'Ä';  vertical := '³';
        ELSE
            corner[1] := 'É';  corner[2] := '»';
            corner[3] := 'È';  corner[4] := '¼';
            horizontal := 'Í';  vertical := 'º';
        END (*IF*);

        WITH w^ DO

            offset2 := LastColumn - FirstColumn;
            place2 := CharsPerRow*FirstRow + FirstColumn;
            buffer[place2].val := corner[1];
            buffer[place2+offset2].val := corner[2];
            INC (place2, CharsPerRow);

            FOR i := FirstRow + 1 TO LastRow - 1 DO
                buffer[place2].val := vertical;
                buffer[place2+offset2].val := vertical;
                INC (place2, CharsPerRow);
            END (*FOR*);

            buffer[place2].val := corner[3];
            buffer[place2+offset2].val := corner[4];

            offset2 := CharsPerRow*(LastRow - FirstRow);
            FOR j := FirstColumn + 1 TO LastColumn - 1 DO
                INC (place2);
                buffer[place2-offset2].val := horizontal;
                buffer[place2].val := horizontal;
            END (*FOR*);

        END (*WITH*);
    END FillInFrame;

(************************************************************************)

PROCEDURE MakeMonochrome (VAR (*INOUT*) foreground, background: Colour);

    (* Changes the two given colours to a suitable B/W combination.     *)

    BEGIN
        IF (foreground = black) OR (foreground = darkgrey) THEN
            background := white
        ELSE
            IF foreground > white THEN
                foreground := intensewhite
            ELSE
                foreground := white;
            END (*IF*);
            background := black;
        END (*IF*);
    END MakeMonochrome;

(************************************************************************)

PROCEDURE OpenWindow (VAR (*OUT*) w: Window;
                        ForegroundColour, BackgroundColour: Colour;
                        firstline, lastline: RowRange;
                        firstcol, lastcol: ColumnRange;
                        FrameDesired: FrameType;
                        DividerDesired: DividerType);

    (* Creates a new window, and makes it the current window, filled    *)
    (* initially with space characters.                                 *)
    (* The caller must be executing at level <0.5.                      *)

    VAR i: RowRange;  j: ColumnRange;  k: BufferSubscript;

    BEGIN

        (* Create the new window, and fill in all its fields.   *)

        NEW (w);
        WITH w^ DO
            CreateSemaphore (access0p5, 0);
            CloseList := NIL;
            previous := NIL;  next := NIL;  blockcursor := FALSE;
            page := DefaultPage;  hidden := FALSE;
            InputWaiting := FALSE;  CursorWanted := FALSE;
            WrapOption := TRUE;
            CreateSemaphore (CharAvailable, 0);
            foreground := ForegroundColour;  background := BackgroundColour;
            IF BlackAndWhite OR ForcedMonochrome THEN
                MakeMonochrome (foreground, background);
            END (*IF*);
            CurrentAttributes := 16*ORD(background) + ORD(foreground);
            frame := FrameDesired;  divider := DividerDesired;
            FirstRow := firstline;  LastRow := lastline;
            FirstColumn := firstcol;  LastColumn := lastcol;
            tabstops := DefaultTabs;
            IF frame <> noframe THEN
                FOR j := MAX(ColumnRange) TO 1 BY -1 DO
                    tabstops[j] := tabstops[j-1];
                END (*FOR*);
                tabstops[0] := " ";
            END (*IF*);

            (* Set the window contents to all space characters. *)

            Obtain (BlankRowAccess1);
            FOR k := 0 TO CharsPerRow-1 DO
                BlankRow[k].attr := CurrentAttributes;
            END (*FOR*);
            FOR i := 0 TO MaxRowNumber DO
                Copy (ADR(BlankRow), ADR(buffer[i*CharsPerRow]), BytesPerRow);
            END (*FOR*);
            Release (BlankRowAccess1);

            (* Set up a default scrolling region.       *)

            WITH ScrollRegion DO
                top := FirstRow;  bottom := LastRow;
                left := FirstColumn;  right := LastColumn;
            END (*WITH*);

            (* Make the frame.  *)

            IF frame <> noframe THEN
                FillInFrame(w);
                WITH ScrollRegion DO
                    INC (top);  INC (left);  DEC (bottom);  DEC (right);
                END (*WITH*);
            END (*IF*);

            DefaultScrollRegion := ScrollRegion;
            row := ScrollRegion.top;  column := ScrollRegion.left;
            ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
            BufferBias := PageSize*page;
            Signal (access0p5);

        END (*WITH*);

        (* Put the new window on top of the stack of active windows.    *)

        PutOnTop (w);

    END OpenWindow;

(************************************************************************)

PROCEDURE OpenSimpleWindow (VAR (*OUT*) w: Window;
                        firstline, lastline: RowRange;
                        firstcol, lastcol: ColumnRange);

    (* Identical to OpenWindow, except that you don't get any choice    *)
    (* about the colours or frame.  The window is white-on-black with   *)
    (* a simple frame and no dividers for the scrolling region.  This   *)
    (* version of OpenWindow is useful for those with monochrome        *)
    (* displays who don't want to be bothered with importing the types  *)
    (* Colour, FrameType, and DividerType.                              *)

    BEGIN
        OpenWindow (w, white, black, firstline, lastline,
                        firstcol, lastcol, simpleframe, nodivider);
    END OpenSimpleWindow;

(************************************************************************)
(*                 CHANGING THE POSITION OF A WINDOW                    *)
(************************************************************************)

PROCEDURE ShiftWindowRel (w: Window;  rowchange, columnchange: INTEGER);

    (* Moves w on the screen.  The second and third arguments may be    *)
    (* negative.  The amount of move may be reduced to prevent a move   *)
    (* off the edge of the screen.                                      *)
    (* The caller must be executing at level <0.5.                      *)

    VAR byteshift: INTEGER;  wasvisible: BOOLEAN;

    BEGIN
        IF (rowchange <> 0) OR (columnchange <> 0) THEN
            WITH w^ DO
                (* Temporarily remove the window from the stack of windows. *)

                wasvisible := NOT hidden;  Hide(w);

                Wait (access0p5);

                (* Clip the shift amount to avoid going off the screen. *)

                IF VAL(INTEGER,FirstRow)+rowchange < 0 THEN
                    rowchange := - VAL(INTEGER,FirstRow)
                ELSIF VAL(INTEGER,LastRow)+rowchange > MaxRowNumber THEN
                    rowchange := MaxRowNumber - LastRow
                END (*IF*);

                IF VAL(INTEGER,FirstColumn)+columnchange < 0 THEN
                    columnchange := -VAL(INTEGER,FirstColumn)
                ELSIF VAL(INTEGER,LastColumn)+columnchange > MaxColumnNumber THEN
                    columnchange := MaxColumnNumber - LastColumn
                END (*IF*);

                byteshift := BytesPerRow*rowchange + BytesPerChar*columnchange;

                (* Shift the buffer contents.   *)

                IF byteshift < 0 THEN
                    Copy (ADR(buffer[-byteshift DIV 2]), ADR(buffer[0]),
                                                BytesInBuffer+byteshift);
                ELSE
                    CopyUp (Far(ADR(buffer[0])), Far(ADR(buffer[byteshift DIV 2])),
                                                BytesInBuffer-byteshift);
                END (*IF*);

                (* Adjust the affected window parameters.       *)

                IF rowchange > 0 THEN
                    WITH ScrollRegion DO
                        INC (top, rowchange);  INC (bottom, rowchange);
                    END (*WITH*);
                    WITH DefaultScrollRegion DO
                        INC (top, rowchange);  INC (bottom, rowchange);
                    END (*WITH*);
                    INC (FirstRow, rowchange);  INC (LastRow, rowchange);
                    INC (row, rowchange);
                ELSE
                    rowchange := -rowchange;
                    WITH ScrollRegion DO
                        DEC (top, rowchange);  DEC (bottom, rowchange);
                    END (*WITH*);
                    WITH DefaultScrollRegion DO
                        DEC (top, rowchange);  DEC (bottom, rowchange);
                    END (*WITH*);
                    DEC (FirstRow, rowchange);  DEC (LastRow, rowchange);
                    DEC (row, rowchange);
                END (*IF*);

                IF columnchange > 0 THEN
                    WITH ScrollRegion DO
                        INC (left, columnchange);  INC (right, columnchange);
                    END (*WITH*);
                    WITH DefaultScrollRegion DO
                        INC (left, columnchange);  INC (right, columnchange);
                    END (*WITH*);
                    INC (FirstColumn, columnchange);
                    INC (LastColumn, columnchange);
                    INC (column, columnchange);
                ELSE
                    columnchange := -columnchange;
                    WITH ScrollRegion DO
                        DEC (left, columnchange);  DEC (right, columnchange);
                    END (*WITH*);
                    WITH DefaultScrollRegion DO
                        DEC (left, columnchange);  DEC (right, columnchange);
                    END (*WITH*);
                    DEC (FirstColumn, columnchange);
                    DEC (LastColumn, columnchange);
                    DEC (column, columnchange);
                END (*IF*);

                IF byteshift > 0 THEN
                    INC (ScreenPosition, byteshift);
                ELSE
                    DEC (ScreenPosition, -byteshift);
                END (*IF*);

                Signal (access0p5);

                (* Put w back onto the stack and onto the screen.       *)

                IF wasvisible THEN
                    hidden := FALSE;  PutOnTop (w);
                END (*IF*);

            END (*WITH*);

        END (*IF*);

    END ShiftWindowRel;

(************************************************************************)

PROCEDURE ShiftWindowAbs (w: Window;  top: RowRange;  left: ColumnRange);

    (* Like ShiftWindowRel, except that we directly specify the target  *)
    (* position of the top left corner in screen coordinates.           *)

    BEGIN
        ShiftWindowRel (w, VAL(INTEGER,top)-VAL(INTEGER,w^.FirstRow),
                                VAL(INTEGER,left)-VAL(INTEGER,w^.FirstColumn));
    END ShiftWindowAbs;

(************************************************************************)

PROCEDURE WindowLocation (w: Window): Rectangle;

    (* Returns the current location of w on the screen. *)

    VAR result: Rectangle;

    BEGIN
        WITH w^ DO
            WITH result DO
                top := FirstRow;  bottom := LastRow;
                left := FirstColumn;  right := LastColumn;
            END (*WITH*);
        END (*WITH*);
        RETURN result;
    END WindowLocation;

(************************************************************************)
(*                          CLOSING A WINDOW                            *)
(************************************************************************)

PROCEDURE InstallCloseHandler (w: Window;  P: CloseHandlerProc);

    (* Sets up P as a procedure to be called when the window is closed. *)
    (* It is legal to define multiple handlers for the same window.     *)

    VAR HLP: CloseHandlerList;

    BEGIN
        NEW (HLP);
        WITH w^ DO
            Wait (access0p5);
            HLP^.next := CloseList;
            HLP^.proc := P;
            CloseList := HLP;
            Signal (access0p5);
        END (*WITH*);
    END InstallCloseHandler;

(************************************************************************)

PROCEDURE CloseWindow (w: Window);

    (* Reclaims the buffer space used for this window, and removes its  *)
    (* image on the screen.                                             *)
    (* The caller must be executing at level <0.5.                      *)

    VAR p: CloseHandlerList;

    BEGIN
        Hide (w);
        WITH w^ DO
            Wait (access0p5);
            WHILE CloseList <> NIL DO
                p := CloseList^.next;
                CloseList^.proc (w, w^.page);
                DISPOSE (CloseList);
                CloseList := p;
            END (*WHILE*);
            Signal (access0p5);
            DestroySemaphore (access0p5);
        END (*WITH*);
        DISPOSE (w);
    END CloseWindow;

(************************************************************************)
(*                      CHANGING OPTIONS                                *)
(************************************************************************)

PROCEDURE SetWrapOption (w: Window;  enabled: BOOLEAN);

    (* If the parameter is TRUE - this is the initial default - then    *)
    (* subsequent text written to the window will wrap to the next      *)
    (* line when it hits the right of the scrolling region.  Setting    *)
    (* the parameter to FALSE disables this feature.                    *)

    BEGIN
        w^.WrapOption := enabled;
    END SetWrapOption;

(************************************************************************)
(*              OPERATIONS ON CHARACTER ATTRIBUTES                      *)
(************************************************************************)

PROCEDURE SetColours (w: Window; r: RowRange; c: ColumnRange;
                                nchar: CARDINAL;  fore, back: Colour);

    (* Sets a field of nchar characters, starting at (row,col), to      *)
    (* the specified foreground and background colours.  The location   *)
    (* is given in window-relative coordinates, not absolute screen     *)
    (* positions.  NOTE: This procedure will not wrap around to a new   *)
    (* row.  The caller must be executing at level <3.                  *)

    VAR k, start2: BufferSubscript;  attributes: CARD8;

    BEGIN
        attributes := 16*ORD(back) + ORD(fore);
        WITH w^ DO
            INC (r, FirstRow);  INC (c, FirstColumn);
            start2 := CharsPerRow*r + c;
            FOR k := start2 TO start2+nchar-1 DO
                buffer[k].attr := attributes;
            END (*FOR*);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSE
                    WritePartialRow (w, r, c, nchar)
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END SetColours;

(************************************************************************)

PROCEDURE ColourSwap (w: Window; r: RowRange; c: ColumnRange;
                                                        nchar: CARDINAL);

    (* Switches the foreground and background colours for nchar         *)
    (* characters, starting at location (r,c).  The row and column      *)
    (* numbers are window-relative, not absolute screen coordinates.    *)
    (* This is our colour equivalent of the "reverse video" operation.  *)
    (* NOTE: This procedure will not wrap around to a new row.          *)
    (* The caller must be executing at level <3.                        *)

    VAR k, start2: BufferSubscript;  oldattribute: CARD8;

    BEGIN
        WITH w^ DO
            start2 := CharsPerRow*(r+FirstRow) + c + FirstColumn;
            FOR k := start2 TO start2+nchar-1 DO
                oldattribute := buffer[k].attr;
                buffer[k].attr := 16*(oldattribute MOD 16)
                                        + (oldattribute DIV 16);
            END (*FOR*);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSE
                    WritePartialRow (w, r+FirstRow, c+FirstColumn, nchar);
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END ColourSwap;

(************************************************************************)

PROCEDURE Blink (w: Window; r: RowRange; c: ColumnRange; nchar: CARDINAL);

    (* Toggles the blinking status - that is, turns blinking on if it   *)
    (* was off, and vice versa - for nchar characters, starting at      *)
    (* relative location (r,c) in window w.                             *)
    (* NOTE: This procedure will not wrap around to a new row.          *)
    (* The caller must be executing at level <3.                        *)

    VAR k, start2: BufferSubscript;

    BEGIN
        WITH w^ DO
            start2 := CharsPerRow*(r+FirstRow) + c + FirstColumn;
            FOR k := start2 TO start2+nchar-1 DO
                buffer[k].attr := IXORB(buffer[k].attr, 80H);
            END (*FOR*);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSE
                    WritePartialRow (w, r+FirstRow, c+FirstColumn, nchar);
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END Blink;

(************************************************************************)
(*                          CURSOR OPERATIONS                           *)
(************************************************************************)

PROCEDURE SetCursor (w: Window; r: RowRange; c: ColumnRange);

    (* Sets the cursor for window w to relative row r, column c.        *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        WITH w^ DO
            Wait (access0p5);
            row := r + FirstRow;  column := c + FirstColumn;
            ScreenPosition := PageSize*page + BytesPerRow*row
                                        + BytesPerChar*column;
            Signal (access0p5);
        END (*WITH*);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor (w: Window; VAR (*OUT*) r, c: CARDINAL);

    (* Returns the current cursor position of window w.         *)

    BEGIN
        WITH w^ DO
            r := row - FirstRow;  c := column - FirstColumn;
        END (*WITH*);
    END SaveCursor;

(************************************************************************)

PROCEDURE CursorLeft (w: Window);

    (* Moves the window cursor one position left.  If it falls off the  *)
    (* left edge of the window, move to the right edge in the same row. *)

    BEGIN
        WITH w^ DO
            IF column = FirstColumn THEN
                column := LastColumn;
                ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
            ELSE
                DEC (column);  DEC (ScreenPosition, BytesPerChar);
            END (*IF*);
        END (*WITH*);
    END CursorLeft;

(************************************************************************)

PROCEDURE CursorRight (w: Window);

    (* Moves the window cursor one position right.  If it falls off the *)
    (* right edge of the window, move to the left edge in the same row. *)

    BEGIN
        WITH w^ DO
            IF column = LastColumn THEN
                column := FirstColumn;
                ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
            ELSE
                INC (column);  INC (ScreenPosition, BytesPerChar);
            END (*IF*);
        END (*WITH*);
    END CursorRight;

(************************************************************************)

PROCEDURE CursorUp (w: Window);

    (* Moves the window cursor one position up.  If it falls off the    *)
    (* top edge of the window, it moves to the bottom edge in the same  *)
    (* column.                                                          *)

    BEGIN
        WITH w^ DO
            IF row = FirstRow THEN
                row := LastRow;
                ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
            ELSE
                DEC (row);  DEC (ScreenPosition, BytesPerRow);
            END (*IF*);
        END (*WITH*);
    END CursorUp;

(************************************************************************)

PROCEDURE CursorDown (w: Window);

    (* Moves the window cursor one position down.  If it falls off the  *)
    (* bottom edge of the window, it moves to the top edge in the same  *)
    (* column.                                                          *)

    BEGIN
        WITH w^ DO
            IF row = LastRow THEN
                row := FirstRow;
                ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
            ELSE
                INC (row);  INC (ScreenPosition, BytesPerRow);
            END (*IF*);
        END (*WITH*);
    END CursorDown;

(************************************************************************)

PROCEDURE ScrollUpI (w: Window);

    (* The version of ScrollUp (see below) for internal use.            *)
    (* The caller must be executing at level <1.                        *)

    VAR rownum: RowRange;  count: CARDINAL;
        k: BufferSubscript;
        srcptr, destptr: FarPointer;

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                k := CharsPerRow*top + left;
                count := BytesPerChar*(right-left+1);
                destptr := Far(ADR(buffer[k]));

                (* Move the contents of the scrolling region up. *)

                FOR rownum := top TO bottom-1 DO
                    srcptr := FarAddOffset (destptr, BytesPerRow);
                    FarCopy (srcptr, destptr, count);
                    IF NOT (obscured OR hidden) THEN
                        WritePartialRow (w, rownum, left, right-left+1);
                    END (*IF*);
                    destptr := srcptr;
                END (*FOR*);
                Obtain (BlankRowAccess1);

                (* Fill in the attributes of BlankRow. *)

                FOR k := 0 TO CharsPerRow-1 DO
                    BlankRow[k].attr := CurrentAttributes;
                END (*FOR*);

                (* Blank the bottom line of scrolling region. *)

                FarCopy (Far(ADR(BlankRow)), destptr, count);
                Release (BlankRowAccess1);
                IF NOT (obscured OR hidden) THEN
                    WriteSpaces (bottom, left, right-left+1, CurrentAttributes);
                END (*IF*);

            END (*WITH*);

            IF obscured AND NOT hidden THEN
                Obtain (StackAccess2);
                PutOnTopI(w);
                Release (StackAccess2);
            END (*IF*);

        END (*WITH*);

    END ScrollUpI;

(************************************************************************)

PROCEDURE ScrollUp (w: Window);

    (* Scrolls window w up by one line, both on the screen and in its   *)
    (* buffer.  The last row is filled with spaces.                     *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        Wait (w^.access0p5);
        ScrollUpI (w);
        Signal (w^.access0p5);
    END ScrollUp;

(************************************************************************)

PROCEDURE ScrollDown (w: Window);

    (* Scrolls window w down by one line, both on the screen and in its *)
    (* buffer.  The first row is filled with spaces.                    *)
    (* The caller must be executing at level <0.5.                      *)

    VAR k: BufferSubscript;

    BEGIN
        WITH w^ DO
            Wait (access0p5);
            Obtain (BlankRowAccess1);
            FOR k := 0 TO CharsPerRow-1 DO
                BlankRow[k].attr := CurrentAttributes;
            END (*FOR*);
            WITH ScrollRegion DO
                k := CharsPerRow * top;
                IF bottom > top THEN
                    CopyUp (Far(ADR(buffer[k])), Far(ADR(buffer[k+CharsPerRow])),
                                    BytesPerRow*(bottom-top));
                END (*IF*);
                Copy (ADR(BlankRow), ADR(buffer[k+left]),
                        BytesPerChar*(right-left+1));
            END (*WITH*);
            Release (BlankRowAccess1);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w) ELSE Refresh (w);
                END (*IF*);
                Release (StackAccess2);
            END (*IF*);
            Signal (access0p5);
        END (*WITH*);
    END ScrollDown;

(************************************************************************)
(*                          MAIN OUTPUT ROUTINES                        *)
(************************************************************************)

PROCEDURE WriteLnI (w: Window);

    (* The internal version of WriteLn (see below).     *)
    (* The caller must be executing at level <1.        *)

    BEGIN
        WITH w^ DO
            IF InScrollingRegion (w) THEN
                column := ScrollRegion.left;
                IF row = ScrollRegion.bottom THEN ScrollUpI (w)
                ELSE INC (row);
                END (*IF*);
            ELSE
                column := DefaultScrollRegion.left;
                IF row >= LastRow THEN row := LastRow
                ELSE INC (row);
                END (*IF*);
            END (*IF*);
            ScreenPosition := PageSize*page + BytesPerRow*row
                                                + BytesPerChar*column;
        END (*WITH*);
    END WriteLnI;

(************************************************************************)

PROCEDURE WriteLn (w: Window);

    (* Moves the cursor of window w to the start of the next row.  If   *)
    (* we are already at the last row, the window scrolls up.           *)

    BEGIN
        Wait (w^.access0p5);
        WriteLnI (w);
        Signal (w^.access0p5);
    END WriteLn;

(************************************************************************)

PROCEDURE WriteChar (w: Window; ch: CHAR);

    (* Writes one character to window w, and updates the cursor for     *)
    (* this window.  As a side-effect, this window becomes the          *)
    (* currently active window if it was obscured.  Wraps around to the *)
    (* next line if we are about to run off the end of the current      *)
    (* line.  This procedure does not recognise the concept of a        *)
    (* control character.  Every possible value of ch produces          *)
    (* something readable on the screen.                                *)
    (* The caller must be executing at level <0.5.                      *)

    VAR place: CARDINAL;

    BEGIN
        WITH w^ DO
            Wait (access0p5);

            (* Wrap to a new line if we about to leave the scrolling    *)
            (* region or if we are outside the legal writing region.    *)

            IF WrapOption AND ((column = ScrollRegion.right + 1)
                           OR (column > DefaultScrollRegion.right)) THEN
                DEC (column);  WriteLnI (w);
            END (*IF*);

            place := (ScreenPosition - BufferBias) DIV 2;
            buffer[place].val := ch;
            buffer[place].attr := CurrentAttributes;

            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w) END (*IF*);
                WritePartialRow (w, row, column, 1);
                Release (StackAccess2);
            END (*IF NOT hidden*);

            (* Note that the following statement may cause column to    *)
            (* go beyond the edge of the window; but this will be       *)
            (* picked up on the next call to WriteChar.  We prefer not  *)
            (* to do a WriteLn just yet, because that could cause an    *)
            (* unintended scroll operation when writing to the bottom   *)
            (* right of the window.                                     *)

            INC (column);  INC (ScreenPosition, BytesPerChar);

            Signal (access0p5);

        END (*WITH*);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteString (w: Window; text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by  *)
    (* the end of the array.                                            *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF ORD (text[j]) = 0 THEN EXIT (*LOOP*)  END (*IF*);
            WriteChar (w, text[j]);  INC (j);
            IF j > HIGH (text) THEN EXIT (*LOOP*)  END (*IF*);
        END (*LOOP*);
    END WriteString;

(************************************************************************)

PROCEDURE Write (w: Window; ch: CHAR);

    (* A version of procedure WriteChar which looks after some of the   *)
    (* control characters.                                              *)

    BEGIN
        IF ch >= " " THEN WriteChar (w, ch)
        ELSIF ORD(ch) = 8 THEN          (* backspace *)
            CursorLeft(w)
        ELSIF ORD(ch) = 9 THEN          (* tab *)
            WITH w^ DO
                REPEAT
                    WriteChar (w, " ");
                UNTIL (column=MAX(ColumnRange)) OR (tabstops[column]="T");
            END (*WITH*);
        ELSIF ORD(ch) = 10 THEN         (* line feed - ignore *)
        ELSIF ORD(ch) = 13 THEN         (* carriage return *)
            WriteLn(w)
        ELSE                            (* other control character *)
            WriteChar (w, "^");  WriteChar (w, CHR(ORD(ch)+64))
        END (*IF*);
    END Write;

(************************************************************************)
(*                              INPUT                                   *)
(************************************************************************)

PROCEDURE ReadBack (w: Window;  r: RowRange;  c: ColumnRange): CHAR;

    (* Returns the character which currently occupies relative location *)
    (* (r,c) on the display of window w.                                *)

    BEGIN
        WITH w^ DO
            RETURN buffer[CharsPerRow*(r+FirstRow) + c + FirstColumn].val;
        END (*WITH*);
    END ReadBack;

(************************************************************************)

PROCEDURE KeyTask;

    (* Runs as a separate task, getting a character from the keyboard   *)
    (* as needed and making it available to the task which has input    *)
    (* focus.  If no task has input focus, the character is returned    *)
    (* to the keyboard module.                                          *)

    VAR ch: CHAR;  w: Window;

    BEGIN
        LOOP
            Wait (InputRequest);
            IF ShutdownInProgress THEN EXIT(*LOOP*) END(*IF*);
            ch := InKey();
            WITH PhysicalCursor DO
                Obtain (access4);
                w := CursorWindow[ActivePage];
                CursorWindow[ActivePage] := NIL;
                CursorVisible[ActivePage] := FALSE;
                Release (access4);
            END (*WITH*);
            IF w = NIL THEN
                PutBack(ch);
            ELSE
                WITH w^ DO
                    InputChar := ch;
                    InputWaiting := FALSE;
                    ComputeCursorWindow (ActivePage);
                    Signal (CharAvailable);
                END (*WITH*);
            END (*IF*);
        END (*LOOP*);
        Signal (KeyTaskFinished);
    END KeyTask;

(************************************************************************)

PROCEDURE GetKey (w: Window): CHAR;

    (* Read one character, without any prompt to the user (unless the   *)
    (* caller has already set w^.CursorWanted to TRUE).  The reason for *)
    (* specifying a window parameter is to ensure that keyboard input   *)
    (* comes to us only when this window has input focus.               *)

    BEGIN
        w^.InputWaiting := TRUE;
        ComputeCursorWindow (w^.page);
        Signal (InputRequest);
        Wait (w^.CharAvailable);
        RETURN w^.InputChar;
    END GetKey;

(************************************************************************)

PROCEDURE ReadCharWithoutEcho (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Read one character, with a blinking cursor in window w as a      *)
    (* prompt.                                                          *)

    VAR SaveCursorState: BOOLEAN;

    BEGIN
        SaveCursorState := w^.CursorWanted;
        w^.CursorWanted := TRUE;
        ch := GetKey (w);
        w^.CursorWanted := SaveCursorState;
    END ReadCharWithoutEcho;

(************************************************************************)

PROCEDURE ReadChar (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Like ReadCharWithoutEcho, but the input character is echoed.     *)

    BEGIN
        ReadCharWithoutEcho (w, ch);  Write (w, ch);
    END ReadChar;

(************************************************************************)

PROCEDURE LookaheadChar (w: Window): CHAR;

    (* Reads a character without consuming it.  That is, the character  *)
    (* remains available to be read by ReadChar.  This allows the       *)
    (* caller to check whether the character is really wanted.          *)

    VAR ch: CHAR;

    BEGIN
        ch := GetKey(w);  PutBack (ch);
        RETURN ch;
    END LookaheadChar;

(************************************************************************)

PROCEDURE PressAnyKey (w: Window);

    (* Types a "Press any key to continue" message.     *)

    VAR dummy: CHAR;

    BEGIN
        WriteLn (w);
        WriteString (w, "Press any key to continue.");
        ReadCharWithoutEcho (w, dummy);
        IF ORD(dummy) = 0 THEN ReadCharWithoutEcho (w, dummy) END (*IF*);
        EraseLine (w, 0);
    END PressAnyKey;

(************************************************************************)

PROCEDURE ReadString (w: Window;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Reads a character string, terminated by carriage return.         *)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
        FOR j := 0 TO HIGH(result) DO
            result[j] := " ";
        END (*FOR*);
        j := 0;
        LOOP
            ReadChar (w, ch);
            IF ORD(ch) = 13 THEN
                result[j] := CHR(0);  EXIT(*LOOP*)
            ELSIF ORD(ch) = 8 THEN      (* backspace *)
                IF j > 0 THEN
                    CursorLeft(w);  WriteChar(w, " ");  CursorLeft(w);
                    DEC (j);
                END (*IF*);
            ELSE
                result[j] := ch;
                IF j = HIGH(result) THEN EXIT(*LOOP*) END(*IF*);
                INC (j);
            END(*IF*);
        END (*LOOP*);
    END ReadString;

(************************************************************************)

PROCEDURE EditString (w: Window;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                                fieldsize: CARDINAL);

    (* Reads a character string, where a default result is supplied by  *)
    (* the caller.  The final result is the state of the string at the  *)
    (* time where the keyboard user types a carriage return or Esc, or  *)
    (* uses a cursor movement key to move out of the displayed field.   *)
    (* The terminating character remains available, via Keyboard.InKey, *)
    (* to the caller.  At most fieldsize characters of the string can   *)
    (* be edited, and perhaps fewer if the result array is smaller or   *)
    (* if there is insufficient space in the window.                    *)

    CONST Esc = CHR(01BH);  Space = " ";

    VAR place, k: CARDINAL;  ch: CHAR;  limit: ColumnRange;
        SavedAttributes: CARD8;
        startrow, startcolumn: CARDINAL;
        InsertMode, SavedCursorType: BOOLEAN;

    (********************************************************************)

    PROCEDURE RewriteString ();

        BEGIN
            SetCursor (w, startrow, startcolumn);
            WriteString (w, result);
            SetCursor (w, startrow, startcolumn+place);
        END RewriteString;

    (********************************************************************)

    PROCEDURE GoToEnd;

        (* Puts the cursor just after the last non-blank character.     *)

        BEGIN
            place := limit+1;
            WHILE (place > 0) AND (result[place-1] = Space) DO
                DEC (place);
            END (*WHILE*);
            SetCursor (w, startrow, startcolumn+place);
        END GoToEnd;

    (********************************************************************)

    PROCEDURE HandleControlChar(): BOOLEAN;

        (* Called after detecting the CHR(0) which means that a control *)
        (* character has been typed.  Performs the appropriate actions, *)
        (* returns TRUE iff editing is finished.                        *)

        VAR k: CARDINAL;

        BEGIN
            ch := GetKey (w);
            IF ch = "K" THEN                            (* cursor left *)
                IF place = 0 THEN
                    PutBack(ch);  PutBack(CHR(0));
                    RETURN TRUE;
                END (*IF*);
                CursorLeft(w);  DEC (place);
            ELSIF ch = "M" THEN                         (* cursor right *)
                IF place > limit THEN
                    PutBack(ch);  PutBack(CHR(0));
                    RETURN TRUE;
                END (*IF*);
                CursorRight(w);  INC (place);
            ELSIF (ch = "H") OR (ch = "P") THEN         (* cursor up/down *)
                PutBack(ch);  PutBack(CHR(0));
                RETURN TRUE;
            ELSIF ch = "G" THEN                         (* home *)
                place := 0;
                SetCursor (w, startrow, startcolumn);
            ELSIF ch = "O" THEN                         (* end *)
                GoToEnd;
            ELSIF ch = "R" THEN                         (* insert *)
                w^.blockcursor := InsertMode;
                InsertMode := NOT InsertMode;
            ELSIF ch = "S" THEN                         (* delete right *)
                IF place < limit THEN
                    FOR k := place TO limit-1 DO
                        result[k] := result[k+1];
                    END (*FOR*);
                END (*IF*);
                IF place <= limit THEN
                    result[limit] := Space;
                    RewriteString ();
                END (*IF*);
            END (*IF*);
            RETURN FALSE;
        END HandleControlChar;

    (********************************************************************)

    BEGIN       (* Body of EditString *)

        SaveCursor (w, startrow, startcolumn);
        SavedCursorType := w^.blockcursor;
        InsertMode := FALSE;  w^.blockcursor := TRUE;

        (* Compute a limit which stops us from running off the window.  *)

        WITH w^ DO
            IF InScrollingRegion(w) THEN
                limit := ScrollRegion.right;
            ELSE
                limit := DefaultScrollRegion.right;
            END (*IF*);
            DEC (limit, FirstColumn + startcolumn);
            SavedAttributes := CurrentAttributes;
        END (*WITH*);
        IF HIGH(result) < limit THEN
            limit := HIGH(result);
        END (*IF*);
        IF fieldsize <= limit THEN
            limit := fieldsize - 1;
        END (*IF*);

        (* Preprocessing: for a Nul-terminated string, remove the Nul   *)
        (* and pad out the string with spaces at the right.  Otherwise  *)
        (* we get problems if, for example, the Nul is deleted.         *)

        place := 0;
        LOOP
            IF result[place] = CHR(0) THEN
                FOR k := place TO limit DO
                    result[k] := Space;
                END (*FOR*);
                EXIT (*LOOP*);
            END (*IF*);
            IF place = limit THEN EXIT(*LOOP*) END(*IF*);
            INC (place);
        END (*LOOP*);
        FOR k := limit+1 TO HIGH(result) DO
            result[k] := CHR(0);
        END (*FOR*);

        (* Write the string, using reverse video.       *)

        WriteString (w, result);
        ColourSwap (w, startrow, startcolumn, limit+1);
        WITH w^ DO
            CurrentAttributes := 16*ORD(foreground) + ORD(background);
        END (*WITH*);
        place := 0;
        SetCursor (w, startrow, startcolumn);
        PutOnTop(w);

        (* Now the main editing loop.   *)

        LOOP
            ReadCharWithoutEcho (w, ch);
            IF ORD(ch) = 0 THEN                         (* control char *)
                IF HandleControlChar() THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF (ch = Esc) OR (ORD(ch) = 13) THEN     (* Esc or Return *)
                PutBack(ch);  EXIT(*LOOP*);
            ELSIF ORD(ch) = 8 THEN                      (* delete left *)
                IF place > 0 THEN
                    DEC (place);
                    IF place < limit THEN
                        FOR k := place TO limit-1 DO
                            result[k] := result[k+1];
                        END (*FOR*);
                    END (*IF*);
                    result[limit] := Space;
                    RewriteString ();
                END (*IF*);
            ELSIF place <= limit THEN                   (* any other char *)
                IF InsertMode THEN
                    FOR k := limit TO place+1 BY -1 DO
                        result[k] := result[k-1];
                    END (*FOR*);
                    RewriteString ();
                END (*IF*);
                result[place] := ch;  WriteChar (w, ch);
                INC (place);
            END(*IF*);
        END (*LOOP*);
        ColourSwap (w, startrow, startcolumn, limit+1);
        w^.blockcursor := SavedCursorType;
        w^.CurrentAttributes := SavedAttributes;
    END EditString;

(************************************************************************)

PROCEDURE EditAborted (): BOOLEAN;

    (* Checks the next keyboard input.  Returns TRUE for Escape, FALSE  *)
    (* for anything else.  Escape or Carriage Return are consumed, any  *)
    (* other character is returned to the Keyboard module.              *)

    CONST Esc = CHR(01BH);  CR = CHR(0DH);

    VAR ch: CHAR;

    BEGIN
        ch := InKey();
        IF ch = Esc THEN RETURN TRUE
        ELSIF ch = CR THEN RETURN FALSE
        ELSE
            PutBack(ch);  RETURN FALSE;
        END (*IF*);
    END EditAborted;

(************************************************************************)
(*                  MISCELLANEOUS CONTROL OPERATIONS                    *)
(************************************************************************)

PROCEDURE EraseLine (w: Window;  option: CARDINAL);

    (* Replaces some or all of the current line, except for the border, *)
    (* with space characters.  The window cursor is moved to the        *)
    (* location of the first erased character.  The options are:        *)
    (*          0       the whole of the line, except for the border    *)
    (*          1       from the current cursor position onwards        *)
    (*          2       from the start to just before the cursor        *)
    (* If we are inside a scrolling region, then only that part of the  *)
    (* line inside the scrolling region is affected.                    *)

    VAR first, last: ColumnRange;
        k, firstk, lastk: BufferSubscript;
        NewPosition: CARDINAL;

    BEGIN
        WITH w^ DO
            IF InScrollingRegion(w) THEN
                first := ScrollRegion.left;  last := ScrollRegion.right;
            ELSE
                first := DefaultScrollRegion.left;
                last := DefaultScrollRegion.right;
            END (*IF*);
            IF option = 1 THEN first := column
            ELSIF option = 2 THEN last := column - 1
            END (*IF*);
            IF last >= first THEN
                firstk := CharsPerRow*row + first;
                lastk := CharsPerRow*row + last;
                FOR k := firstk TO lastk DO
                    buffer[k].val := " ";
                    buffer[k].attr := CurrentAttributes;
                END (*FOR*);
                NewPosition := BufferBias + 2*firstk;
                IF NOT (hidden OR obscured) THEN
                    WritePartialRow (w, row, first, last-first+1);
                END (*IF*);
                column := first;  ScreenPosition := NewPosition;
            END (*IF*);
        END (*WITH*);
    END EraseLine;

(************************************************************************)
(*                           TERMINATION                                *)
(************************************************************************)

PROCEDURE CloseAllWindows;

    (* Shutdown of this module is done in two phases.  This procedure   *)
    (* is phase 2, executed after all interrupt handlers have been      *)
    (* de-installed.  At this stage we can be confident that the only   *)
    (* possible task switches can be those triggered by an explicit     *)
    (* kernel call.                                                     *)

    VAR w: Window;  p: DisplayPage;

    BEGIN
        ReleaseAllLocks;

        FOR p := 0 TO MAX(DisplayPage) DO
            LOOP
                w := TopWindow[p];
                IF w = NIL THEN EXIT(*LOOP*) END(*IF*);

                (* In phase 1 of the shutdown, we locked w^.access0p5.  *)
                (* We now want to permanently block any task which is   *)
                (* still waiting for the access.  The easiest way to do *)
                (* this is to destroy and re-create the semaphore.      *)

                DestroySemaphore (w^.access0p5);
                CreateSemaphore (w^.access0p5, 1);
                CloseWindow (w);

            END (*LOOP*);

        END (*FOR*);

    END CloseAllWindows;

(************************************************************************)

PROCEDURE CleanUp;

    (* Phase 1 of module termination.  If termination was caused by an  *)
    (* error, displays the error and waits for the user to press a key. *)
    (* In order to ensure that the error message is not obscured, we    *)
    (* freeze all windows.                                              *)

    VAR w: Window;  p: DisplayPage;
        message: ARRAY [0..57] OF CHAR;

    BEGIN
        (* Note that we cannot know which task is running the shutdown  *)
        (* code, or the point it was up to when termination was         *)
        (* triggered.  To avoid potential deadlocks, we must throw      *)
        (* away any locks we are holding.                               *)

        ReleaseAllLocks;

        (* Lock all open windows.       *)

        FOR p := 0 TO MAX(DisplayPage) DO
            Obtain (StackAccess2);
            w := TopWindow[p];
            Release (StackAccess2);
            WHILE w <> NIL DO
                Wait (w^.access0p5);
                w := w^.next;
            END (*WHILE*);
        END (*FOR*);

        (* For abnormal termination, write the error message, and wait  *)
        (* until the user has responded with a keystroke.               *)

        SetActivePage (0);
        IF TerminationMessage(message) THEN
            OpenSimpleWindow (w, 10, 13, 10, 69);
            WriteString (w, message);
            PressAnyKey (w);
            CloseWindow (w);
        END (*IF*);

        (* Shut down the keyboard input dispatcher for this module. *)

        ShutdownInProgress := TRUE;
        Signal (InputRequest);
        Wait (KeyTaskFinished);

        (* Enable phase 2 of the shutdown. *)

        SetTerminationProcedure (CloseAllWindows);

    END CleanUp;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

VAR j: BufferSubscript;  p: DisplayPage;

BEGIN
    StdOut := StdChans.StdOutChan();

    FOR j := 0 TO CharsPerRow-1 DO
        BlankRow[j].val := " ";
    END (*FOR*);
    CreateLock (BlankRowAccess1);

    FOR p := 0 TO MAX(DisplayPage) DO
        TopWindow[p] := NIL;
        PhysicalCursor.CursorWindow[p] := NIL;
        PhysicalCursor.CursorVisible[p] := FALSE;
    END (*FOR*);
    PhysicalCursor.ScreenPos := 0;
    PhysicalCursor.Attributes := 0;

    ShutdownInProgress := FALSE;
    CreateLock (StackAccess2);
    CreateLock (ScreenAccess3);
    CreateLock (PhysicalCursor.access4);
    CreateSemaphore (InputRequest, 0);
    CreateSemaphore (KeyTaskFinished, 0);
    CreateTask (KeyTask, 5, "keyboard/windows");

    SetTerminationProcedure (CleanUp);
    PageChangeProcs := NIL;
    CreateLock (PageChangeListAccess);

    (* Blank the screen, to erase otherwise annoying background stuff   *)
    (* left by other programs.                                          *)

    SetActivePage (DefaultPage);
    Repaint (DefaultPage, 0,MaxRowNumber,0,MaxColumnNumber);

END Windows.

