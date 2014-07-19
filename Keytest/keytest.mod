MODULE KeyTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of keyboard driver.                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        13 February 1998                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*     This module is essentially the same as the       *)
        (*     KeyTest in the PMOS/2 TESTS directory, but       *)
        (*     I'm rebuilding the project in a new directory    *)
        (*     in order to eliminate possible confusion about   *)
        (*     multiple versions of modules.                    *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2;

FROM GlassTTY IMPORT
    (* proc *)  WriteString, WriteLn, WriteChar, WriteCard, SetCursor;

FROM Keyboard IMPORT
    (* proc *)  InKey;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Keeps reading characters, and writing them on the screen, until  *)
    (* the Esc key has been typed twice in succession.                  *)

    CONST Esc = CHR(27);

    VAR code: CHAR;  EscDetected: BOOLEAN;

     BEGIN
        SetCursor (0,0);
        WriteString ("Test of keyboard driver.");
        WriteString ("  To exit, hit the Esc key TWICE.");
        WriteLn;
        WriteString ("Special case: the code 'm' takes the cursor to the centre of the screen.");
        WriteLn;
        EscDetected := FALSE;
        OS2.VioWrtCellStr("Heeeleleoe", 10, 6, 30, 0);

        LOOP
            code := InKey();
            IF code = Esc THEN
                IF EscDetected THEN EXIT(*LOOP*)
                ELSE EscDetected := TRUE
                END (*IF*);
            ELSE EscDetected := FALSE;
            END (*IF*);
            WriteCard (ORD(code));  WriteString ("   ");  WriteChar (code);
            WriteLn;

            (* Special case to test cursor movement: the code "m" should        *)
            (* move us to a different part of the screen.                       *)

            IF code = "m" THEN
                SetCursor (12, 40);
            END (*IF*);

        END (*LOOP*);

        WriteLn;
        WriteString ("End of test.");
        WriteLn;
    END RunTheTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

    BEGIN
        RunTheTest;
    END KeyTest.

