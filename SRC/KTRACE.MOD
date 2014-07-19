IMPLEMENTATION MODULE KTrace;

        (****************************************************************)
        (*                                                              *)
        (*      Trace routines for Modula 2 program development.        *)
        (*                                                              *)
        (*  This is the version which does NOT use windows.  It is      *)
        (*  intended for low-level tracing of the kernel, where a       *)
        (*  window-based tracing facility would be unsuitable because   *)
        (*  of critical section problems.  However, it is quite         *)
        (*  adequate for any application where we don't care too much   *)
        (*  about a pretty screen layout.                               *)
        (*                                                              *)
        (*  Note, however, that this module is missing the "Press any   *)
        (*  key to continue" option which my other trace modules have.  *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        22 July 1996                            *)
        (*  Status:             Just started OS/2 port                  *)
        (*                                                              *)
        (****************************************************************)

FROM GlassTTY IMPORT
    (* proc *)  WriteString, WriteLn, WriteHexWord;

VAR TraceEnabled: BOOLEAN;
    nesting: CARDINAL;

(************************************************************************)

PROCEDURE NYI (name: ARRAY OF CHAR);

    (* Types a "not yet implemented" message.   *)

    BEGIN
        WriteString (name);  WriteString (" is not yet implemented.");
        WriteLn;
    END NYI;

(************************************************************************)

PROCEDURE InTrace (name: ARRAY OF CHAR);

    (* To be called when entering a procedure.  *)

    VAR j: CARDINAL;

    BEGIN
        IF TraceEnabled THEN
            FOR j := 1 TO nesting DO
                WriteString ("   ");
            END (*FOR*);
            WriteString ("Entering ");  WriteString (name);
            WriteLn;
        END (*IF*);
        INC (nesting);
    END InTrace;

(************************************************************************)

PROCEDURE OutTrace (name: ARRAY OF CHAR);

    (* To be called when leaving a procedure.   *)

    VAR j: CARDINAL;

    BEGIN
        DEC (nesting);
        IF TraceEnabled THEN
            FOR j := 1 TO nesting DO
                WriteString ("   ");
            END (*FOR*);
            WriteString ("Leaving ");  WriteString (name);
            WriteLn;
        END (*IF*);
    END OutTrace;

(************************************************************************)

PROCEDURE TraceOn;

    (* Turns tracing on.                *)

    BEGIN
        TraceEnabled := TRUE;
    END TraceOn;

(************************************************************************)

PROCEDURE TraceOff;

    (* Turns tracing off.               *)

    BEGIN
        TraceEnabled := FALSE;
    END TraceOff;

(************************************************************************)

PROCEDURE TraceStatus (): BOOLEAN;

    (* Says whether tracing is currently on.            *)

    BEGIN
        RETURN TraceEnabled;
    END TraceStatus;

(************************************************************************)
(*                              INITIALISATION                          *)
(************************************************************************)

BEGIN
    TraceEnabled := FALSE;  nesting := 0;
END KTrace.

