MODULE Sort3;

        (********************************************************)
        (*                                                      *)
        (*                  File sort program                   *)
        (*                                                      *)
        (*      This is a test of the FileSort module.          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        21 January 2000                 *)
        (*  Status:             Working, I believe.             *)
        (*                                                      *)
        (*    Almost OK, it's just putting a couple of          *)
        (*    records in the wrong place.                       *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS;

FROM FileSort IMPORT
    (* proc *)  InplaceSort;

FROM Files IMPORT
    (* type *)  File,
    (* proc *)  OpenFile, CloseFile, FileSize;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow, Write, WriteString, WriteLn,
                EditString, EditAborted, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)  EditCardinal;

FROM IOErrorCodes IMPORT
    (* type *)  ErrorCode,
    (* proc *)  TranslateErrorCode;

(************************************************************************)

CONST testing = FALSE;

CONST MaxRecordSize = 65536;

TYPE
    StringSubscript = [0..MaxRecordSize-1];
    BufferPointer = POINTER TO ARRAY StringSubscript OF CHAR;

VAR debug: Window;
    RecordSize: CARDINAL;

(************************************************************************)

PROCEDURE GEproc (first, second: ADDRESS): BOOLEAN;

    (* Tests for first^ >= second^.     *)

    VAR p1, p2: BufferPointer;
        j: CARDINAL;

    BEGIN
        p1 := first;  p2 := second;
        j := 0;
        LOOP
            IF p1^[j] > p2^[j] THEN RETURN TRUE END(*IF*);
            IF p1^[j] < p2^[j] THEN RETURN FALSE END(*IF*);
            INC (j);
            IF j >= RecordSize THEN RETURN TRUE END(*IF*);
        END (*LOOP*);
    END GEproc;

(************************************************************************)

PROCEDURE DoTheSort;

    (* Opens the data file, and uses the FileSort module to sort it.    *)

    VAR datafile: File;  log: Window;
        name: ARRAY [0..40] OF CHAR;
        status: ErrorCode;

    BEGIN
        RecordSize := 20;
        OpenWindow (log, white, black, 17, 23, 0, 79,
                                        simpleframe, nodivider);
        WriteString (log, "Test of in-place file sort algorithm");
        WriteLn (log);
        WriteString (log, "File name: ");
        name := "test.dat";
        EditString (log, name, SIZE(name), SIZE(name));
        IF EditAborted() THEN status := OperationAborted
        ELSE status := OpenFile (datafile, name, FALSE);
        END (*IF*);
        IF status = OK THEN
            WriteLn (log);  WriteString (log, "Record size: ");
            EditCardinal (log, RecordSize, 4);
            WriteLn (log);  WriteString (log, "About to start sorting.");
            InplaceSort (datafile, 0,
                        FileSize(datafile) DIV RecordSize - 1,
                        RecordSize, 0, GEproc);
            WriteLn (log);  WriteString (log, "Sorting completed.");
        ELSE
            WriteLn (log);  WriteString (log, "Could not open data file");
            WriteLn (log);  WriteString (log, "Error code ");
            TranslateErrorCode (status, name);
            WriteString (log, name);
        END (*IF successfully opened file*);

        IF testing THEN
            WriteLn (debug);  WriteString (debug, "Calling CloseFile");
        END (*IF*);
        CloseFile (datafile);
        WriteLn (log);  WriteString (log, "End of Sort");
        PressAnyKey (log);
        CloseWindow (log);

    END DoTheSort;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    IF testing THEN
        OpenWindow (debug, white, black, 12, 16, 0, 79,
                                        simpleframe, nodivider);
    END (*IF*);
    DoTheSort;
END Sort3.

