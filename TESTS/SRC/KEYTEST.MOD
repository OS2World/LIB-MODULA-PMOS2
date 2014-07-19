MODULE KeyTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of keyboard driver.                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        9 October 1996                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2;

FROM GlassTTY IMPORT
    (* proc *)  WriteString, WriteLn, WriteChar, WriteCard, SetCursor;

FROM KTrace IMPORT
    (* proc *)  InTrace, OutTrace, TraceOn;

FROM Keyboard IMPORT
    (* proc *)  InKey;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Keeps reading characters, and writing them on the screen, until  *)
    (* the Esc key has been typed twice in succession.                  *)

    CONST Esc = CHR(27);

    VAR code: CHAR;  EscDetected: BOOLEAN;

     BEGIN
        InTrace ("RunTheTest");
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
        OutTrace ("RunTheTest");
    END RunTheTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

    BEGIN
        TraceOn;
        RunTheTest;
    END KeyTest.

