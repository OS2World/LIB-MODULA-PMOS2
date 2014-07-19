IMPLEMENTATION MODULE DumpFile;

        (****************************************************************)
        (*                                                              *)
        (*   Debugging aid: writes data out to a file DUMP.$$$          *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    11 November 1996                        *)
        (*      Status:         OK                                      *)
        (*                                                              *)
        (****************************************************************)

IMPORT SYSTEM, IOChan, RawIO, SeqFile, FileSys, OS2;

(********************************************************************************)

VAR dump: IOChan.ChanId;
    DumpFileOpen: BOOLEAN;

(********************************************************************************)

PROCEDURE OpenDumpFile;

    CONST DumpFileName = "DUMP.$$$";

    VAR status: SeqFile.OpenResults;  done: BOOLEAN;

    BEGIN
        IF FileSys.Exists (DumpFileName) THEN
            FileSys.Remove (DumpFileName, done);
        END (*IF*);
        SeqFile.OpenWrite (dump, DumpFileName, SeqFile.raw, status);
        DumpFileOpen := TRUE;
    END OpenDumpFile;

(********************************************************************************)

PROCEDURE Dump (data: ARRAY OF SYSTEM.LOC);

    (* Writes the data to the dump file. *)

    BEGIN
        (* The critical section protection here is because we could have        *)
        (* several tasks all writing to the dump file.                          *)

        OS2.DosEnterCritSec();
        IF NOT DumpFileOpen THEN
            OpenDumpFile;
        END (*IF*);
        RawIO.Write (dump, data);
        OS2.DosExitCritSec();
    END Dump;

(********************************************************************************)

PROCEDURE DumpString (message: ARRAY OF CHAR);

    (* Writes a character string. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(message)) OR (message[j] = CHR(0)) THEN
                EXIT (*LOOP*);
            END (*IF*);
            Dump (message[j]);
            INC (j);
        END (*LOOP*);
    END DumpString;

(********************************************************************************)

PROCEDURE DumpCard (value: CARDINAL);

    (* Converts the number to a text string and writes it to the dump file. *)

    VAR ch: CHAR;

    BEGIN
        IF value > 9 THEN DumpCard (value DIV 10);  END (*IF*);
        ch := CHR(value MOD 10 + ORD("0"));
        Dump (ch);
    END DumpCard;

(********************************************************************************)

PROCEDURE DumpHexDigit (value: SYSTEM.CARD8);

    (* Dumps the value as a one-digit hexadecimal text string. *)

    VAR result: CHAR;

    BEGIN
        IF value < 10 THEN result := CHR(value + ORD("0"));
        ELSE result := CHR(value -10 + ORD("A"));
        END (*IF*);
        Dump (result);
    END DumpHexDigit;

(********************************************************************************)

PROCEDURE DumpHexByte (value: SYSTEM.CARD8);

    (* Dumps the value as a two-digit hexadecimal text string. *)

    BEGIN
        DumpHexDigit (value DIV 16);
        DumpHexDigit (value MOD 16);
    END DumpHexByte;

(********************************************************************************)

PROCEDURE DumpHex (value: ARRAY OF SYSTEM.LOC);

    (* Converts the value to a hexadecimal text string and writes it to the dump file. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := HIGH(value) TO 0 BY -1 DO
            DumpHexByte (SYSTEM.CAST(SYSTEM.CARD8,value[j]));
        END (*FOR*);
    END DumpHex;

(********************************************************************************)

PROCEDURE DumpEOL;

    (* Writes an "end of line" to the dump file. *)

    BEGIN
        Dump(CHR(13));  Dump(CHR(10));
    END DumpEOL;

(********************************************************************************)

PROCEDURE CloseDumpFile;

    BEGIN
        SeqFile.Close (dump);
        DumpFileOpen := FALSE;
    END CloseDumpFile;

(********************************************************************************)

BEGIN
    DumpFileOpen := FALSE;
FINALLY
    IF DumpFileOpen THEN
        DumpString ("About to close dump file and exit process.");  DumpEOL;
        CloseDumpFile;
        OS2.DosExit(OS2.EXIT_PROCESS, 0);
    END (*IF*);
END DumpFile.

