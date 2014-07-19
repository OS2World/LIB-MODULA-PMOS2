MODULE MPTest;

        (********************************************************)
        (*                                                      *)
        (*      Test of the "maintenance page" facility         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        3 November 1996                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      Bugs:                                           *)
        (*              Rubbish being written to an input       *)
        (*              window under certain circumstances.     *)
        (*              I still haven't pinned down exactly     *)
        (*              when it happens.  It looks to me like   *)
        (*              a storage allocation error, i.e.        *)
        (*              someone writing over someone else's     *)
        (*              window data.                            *)
        (*                                                      *)
        (*              As nearly as I can judge, it has nothing*)
        (*              to do with input.  It seems to be       *)
        (*              associated with the 5th task (in this   *)
        (*              module) to be created, i.e. I can move  *)
        (*              the problem to a different window by    *)
        (*              changing the order of task creation.    *)
        (*                                                      *)
        (*      Shortcomings:                                   *)
        (*         1.   (Fixed)                                 *)
        (*         2.   Mapping the same window to several      *)
        (*              different groups is almost, but not     *)
        (*              quite, supported.  I think that the     *)
        (*              only enhancement needed to support this *)
        (*              is a better way to decide whether to    *)
        (*              hide a newly introduced window.         *)
        (*         3.   Need to look into whether the order of  *)
        (*              Unhide operations is satisfactory.      *)
        (*                                                      *)
        (********************************************************)

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait;

FROM Keyboard IMPORT
    (* proc *)  HotKey;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, OpenWindowHidden, ChangeScrollingRegion, CloseWindow,
                PressAnyKey, WriteChar, WriteString, WriteLn, ShiftWindowAbs,
                ReadChar, ReadCharWithoutEcho;

FROM MaintenancePages IMPORT
    (* type *)  MaintenancePage,
    (* proc *)  CreateMaintenancePage, Associate;

FROM UserInterface IMPORT
    (* type *)  UIWindow, Capability, CapabilitySet,
    (* proc *)  AllowMouseControl;

FROM RandCard IMPORT
    (* proc *)  RandInt;

(************************************************************************)

VAR page1, page2: MaintenancePage;

(************************************************************************)

PROCEDURE Task1;

    VAR w1: Window;  dummy: UIWindow;

    BEGIN
        OpenWindowHidden (w1, blue, cyan, 6, 12, 8, 34, simpleframe, nodivider);
        Associate (w1, page1);
        dummy := AllowMouseControl (w1, "Task 1",
                                CapabilitySet {wshow, whide, wmove});
        LOOP
           WriteString (w1, "Message from Task 1 ... ");
           Sleep (400);
        END (*LOOP*);
    END Task1;

(************************************************************************)

PROCEDURE Task2;

    VAR w2: Window;  dummy: UIWindow;

    BEGIN
        OpenWindowHidden (w2, white, magenta, 12, 22, 20, 63, doubleframe, nodivider);
        Associate (w2, page2);
        dummy := AllowMouseControl (w2, "Task 2",
                                CapabilitySet {wshow, whide, wmove});
        LOOP
            WriteString (w2, "This is output from Task 2 on Page 2 ... ");
            Sleep (600);
        END (*LOOP*);
    END Task2;

(************************************************************************)

PROCEDURE Task3;

    VAR w1: Window;  dummy: UIWindow;

    BEGIN
        OpenWindowHidden (w1, black, green, 10, 20, 50, 78, doubleframe, nodivider);
        Associate (w1, page1);
        dummy := AllowMouseControl (w1, "Task 3",
                                CapabilitySet {wshow, whide, wmove});
        LOOP
           WriteString (w1, "Task 3 also uses page 1 ... ");
           Sleep (200);
        END (*LOOP*);
    END Task3;

(************************************************************************)

PROCEDURE Random(): INTEGER;

    (* Returns a small unsigned random number.  *)

    BEGIN
        RETURN RandInt (-1, +1);
    END Random;

(************************************************************************)

PROCEDURE Task4;

    VAR w1: Window;  count: CARDINAL;  flip: BOOLEAN;
        x0, y0: CARDINAL;  newx, newy, xspeed, yspeed: INTEGER;

    BEGIN
        x0 :=66;  y0 := 3;
        xspeed := -2;  yspeed := 2;
        OpenWindowHidden (w1, brown, black, y0, y0+3, x0, x0+6, simpleframe, nodivider);
        Associate (w1, page1);
        count := 0;  flip := FALSE;
        LOOP
            INC (count);
            IF count >= 1 THEN
                newx := VAL(INTEGER,x0) + xspeed + Random();
                IF newx < 0 THEN newx := 0;  xspeed := ABS(xspeed)
                ELSIF newx > 73 THEN newx := 73;  xspeed := -ABS(xspeed)
                END (*IF*);
                newy := VAL(INTEGER,y0) + yspeed + Random();
                IF newy < 0 THEN newy := 0;  yspeed := ABS(yspeed)
                ELSIF newy > 21 THEN newy := 21;  yspeed := -ABS(yspeed)
                END (*IF*);
                x0 := newx;  y0 := newy;
                ShiftWindowAbs (w1, y0, x0);
                count := 0;
            END (*IF*);
            flip := NOT flip;
            IF flip THEN WriteString (w1, "/\/\/")
            ELSE WriteString (w1, "\/\/\")
            END (*IF*);
            Sleep (400);
        END (*LOOP*);
    END Task4;

(************************************************************************)

PROCEDURE InputTask1;

    VAR w1: Window;  dummy: UIWindow;  ch: CHAR;

    BEGIN
        OpenWindowHidden (w1, black, white, 14, 20, 3, 30, doubleframe, nodivider);
        Associate (w1, page1);
        dummy := AllowMouseControl (w1, "Input Task 1",
                                CapabilitySet {wshow, whide, wmove});
        ch := 'A';
        LOOP
           WriteLn (w1);
           WriteString (w1, "Press any key");
           ReadCharWithoutEcho (w1, ch);
           WriteLn (w1);
           WriteString (w1, "The key you pressed was ");
           WriteChar (w1, ch);
           (*Sleep(2000);*)
        END (*LOOP*);
    END InputTask1;

(************************************************************************)

PROCEDURE InputTask2;

    VAR w1, w2: Window;  dummy: UIWindow;  ch: CHAR;

    BEGIN
        OpenWindowHidden (w1, magenta, white, 4, 9, 40, 70,simpleframe,simpledivider);
        Associate (w1, page2);
        dummy := AllowMouseControl (w1, "Input Task 2",
                                CapabilitySet {wshow, whide, wmove});
        WriteString (w1, "You can type into this window");
        ChangeScrollingRegion (w1, 3, 4);

        OpenWindowHidden (w2, white, black, 10, 12, 40, 70,simpleframe,nodivider);
        Associate (w2, page2);
        dummy := AllowMouseControl (w2, "Extra window",
                                CapabilitySet {wshow, whide, wmove});
        WriteString (w2, "An extra window");
        LOOP
           ReadChar (w1, ch);
           (*Sleep(1000);*)
        END (*LOOP*);
    END InputTask2;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST Esc = CHR(1BH);

    VAR w0, header1, header2: Window;
        closedown: Semaphore;

    BEGIN
        CreateSemaphore (closedown, 0);
        HotKey (FALSE, Esc, closedown);
        OpenWindow (w0, yellow, red, 8, 13, 19, 61, simpleframe, nodivider);
        WriteString (w0, "  TEST OF THE MAINTENANCE PAGE FACILITY");
        WriteLn (w0);
        WriteString (w0, "Type Alt/P to toggle maintenance function");
        WriteLn (w0);
        WriteString (w0, "   Then F6 to cycle through the pages");
        WriteLn (w0);
        WriteString (w0, "         Esc to exit program");
        CreateMaintenancePage (page1);
        OpenWindowHidden (header1, white, black, 1, 3, 30, 49, doubleframe, nodivider);
        Associate (header1, page1);
        WriteString (header1, "MAINTENANCE PAGE 1");
        CreateMaintenancePage (page2);
        OpenWindowHidden (header2, white, black, 1, 3, 30, 49, doubleframe, nodivider);
        Associate (header2, page2);
        WriteString (header2, "MAINTENANCE PAGE 2");
        CreateTask (Task1, 2, "Task 1");
        CreateTask (Task2, 2, "Task 2");
        CreateTask (Task4, 2, "Task 4");
        CreateTask (InputTask1, 2, "Input Task 1");
        CreateTask (InputTask2, 2, "Input Task 2");
        CreateTask (Task3, 2, "Task 3");
        Wait (closedown);
    END RunTheTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    RunTheTest;
END MPTest.

