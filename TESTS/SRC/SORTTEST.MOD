MODULE SortTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of module QuickSort                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        28 September 1996               *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      The sorting itself is OK, but the screen        *)
        (*      output is faulty in two ways: terribly slow,    *)
        (*      and scrolling is leaving a spurious blank       *)
        (*      line at the second-last line of the window.     *)
        (*                                                      *)
        (*      This second problem comes from a typical        *)
        (*      glitch in windowed output: when we write to     *)
        (*      the last character position of the screen,      *)
        (*      the library output procedure gives us a         *)
        (*      "scroll up" that we didn't want.  This problem  *)
        (*      probably can't be solved as long as I'm using   *)
        (*      ISO standard library procedures, i.e. the fix   *)
        (*      will have to wait until I have a more low-level *)
        (*      solution.  For now I'm resolving it by not      *)
        (*      using the bottom line of the screen.            *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS;

FROM QuickSortModule IMPORT
    (* proc *)  QuickSort;

FROM RandCard IMPORT
    (* proc *)  RandCardinal;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow, WriteLn, PressAnyKey,
                WriteString, OpenSimpleWindow;

FROM NumericIO IMPORT
    (* proc *)  WriteRJLongCard;

(************************************************************************)

CONST testing = FALSE;
VAR debug: Window;

TYPE
    Datum = CARDINAL;
    DataPtr = POINTER TO Datum;

(************************************************************************)

PROCEDURE GreaterOrEqual (p1, p2: ADDRESS): BOOLEAN;

    (* Comparison of two data elements.  Returns TRUE iff p1^ >= p2^.   *)

    VAR ptr1, ptr2: DataPtr;

    BEGIN
        ptr1 := p1;  ptr2 := p2;
(*
        IF testing THEN
            WriteLn (debug);  WriteString (debug, "Comparing ");
            WriteRJCard (debug, ptr1^, 8);
            WriteRJCard (debug, ptr2^, 8);
        END (*IF*);
*)
        RETURN ptr1^ >= ptr2^;
    END GreaterOrEqual;

(************************************************************************)

PROCEDURE RunTheTest;

    CONST TestSize = (*300*)100;

    VAR data: ARRAY [0..TestSize-1] OF Datum;
        w: Window;
        j: CARDINAL;

    BEGIN
        OpenWindow (w, black, green, 0, 23, 0, 79,
                                        simpleframe, nodivider);

        (* Generate some test data *)

        FOR j := 0 TO TestSize-1 DO
            data[j] := RandCardinal();
        END (*FOR*);

        (* Write out the test data *)

        WriteLn (w);  WriteString (w, "Original data");
        FOR j := 0 TO TestSize-1 DO
            IF j MOD 5 = 0 THEN
                WriteLn (w);
            END (*IF*);
            WriteRJLongCard (w, data[j], 14);
        END (*FOR*);
        WriteLn (w);
        PressAnyKey (w);

        (* Perform the sort *)

        QuickSort (data, TestSize-1, SIZE(Datum), GreaterOrEqual);

        (* Write out the sorted array *)

        WriteLn (w);  WriteString (w, "Sorted data");
        FOR j := 0 TO TestSize-1 DO
            IF j MOD 5 = 0 THEN
                WriteLn (w);
            END (*IF*);
            WriteRJLongCard (w, data[j], 14);
        END (*FOR*);

        PressAnyKey (w);
        CloseWindow (w);
    END RunTheTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    IF testing THEN
        OpenSimpleWindow (debug, 8, 15, 0, 79);
    END (*IF*);
    RunTheTest;
END SortTest.

