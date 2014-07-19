IMPLEMENTATION MODULE MiscPMOS;

        (************************************************)
        (*                                              *)
        (*      Miscellaneous PMOS procedures           *)
        (*                                              *)
        (*  Programmer:         P. Moylan               *)
        (*  Last edited:        27 January 1998         *)
        (*  Status:             OK                      *)
        (*                                              *)
        (************************************************)

IMPORT SYSTEM;

FROM OS2 IMPORT
    (* const*)  SEM_INDEFINITE_WAIT,
    (* type *)  HMTX,
    (* proc *)  DosCreateMutexSem, DosRequestMutexSem, DosReleaseMutexSem;

VAR mutex: HMTX;

(************************************************************************)
(*                              STRING COPY                             *)
(************************************************************************)

PROCEDURE CopyString (source: ARRAY OF CHAR;  VAR (*OUT*) dest: ARRAY OF CHAR);

    (* Copies a string, with truncation or null termination as needed.  *)
    (* This function is provided in order to help software portability, *)
    (* i.e. to avoid having to rewrite code for no reason other than    *)
    (* a change of compilers.                                           *)

    VAR j, last: CARDINAL;  AddNull: BOOLEAN;

    BEGIN
        last := HIGH(dest);
        AddNull := HIGH(source) < last;
        IF AddNull THEN last := HIGH(source) END (*IF*);
        FOR j := 0 TO last DO dest[j] := source[j] END (*FOR*);
        IF AddNull THEN dest[last+1] := CHR(0) END (*IF*);
    END CopyString;

(************************************************************************)
(*                          STRING COMPARISON                           *)
(************************************************************************)

PROCEDURE Compare (first, second: ARRAY OF SYSTEM.LOC): INTEGER;

    (* Returns >0 if first>second, 0 if first=second, <0 if             *)
    (* first<second.                                                    *)

    VAR j: CARDINAL;  val1, val2: SYSTEM.CARD8;

    BEGIN
        j := 0;
        LOOP
            IF j > HIGH(first) THEN
                IF j <= HIGH(second) THEN RETURN -1
                ELSE RETURN 0;
                END (*IF*);
            ELSIF j > HIGH(second) THEN RETURN +1
            ELSE
                val1 := SYSTEM.CAST(SYSTEM.CARD8, first[j]);
                val2 := SYSTEM.CAST(SYSTEM.CARD8, second[j]);
                IF val1 > val2 THEN RETURN +1
                ELSIF val1 < val2 THEN RETURN -1
                ELSE INC(j);
                END (*IF*);
            END (*IF*);
        END (*LOOP*);
    END Compare;

(************************************************************************)
(*                   MISCELLANEOUS LOW-LEVEL OPERATIONS                 *)
(************************************************************************)

PROCEDURE EnterCriticalSection (): CARDINAL;

    (* Saves the processor flags word, including the current "interrupt *)
    (* enable" status, on the caller's stack, and returns with          *)
    (* interrupts disabled.   NOTE: this procedure and the following    *)
    (* one should be used as a matched pair.                            *)

    BEGIN
        DosRequestMutexSem (mutex, SEM_INDEFINITE_WAIT);
        RETURN 0;
    END EnterCriticalSection;

(************************************************************************)

PROCEDURE LeaveCriticalSection (SavedProcessorStatus: CARDINAL);

    (* Restores the processor flags word, including the "interrupt      *)
    (* enable" status, from the stack.  NOTE: this procedure and the    *)
    (* one above should be used as a matched pair.                      *)

    BEGIN
        DosReleaseMutexSem (mutex);
    END LeaveCriticalSection;

(************************************************************************)

BEGIN
    DosCreateMutexSem (NIL, mutex, 0, FALSE);
END MiscPMOS.
