IMPLEMENTATION MODULE FinalExit;

        (********************************************************)
        (*                                                      *)
        (*      Support for program termination procedures.     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        25 October 1996                 *)
        (*  Status:             Working on XDS port             *)
        (*                                                      *)
        (********************************************************)

IMPORT EXCEPTIONS;

FROM TERMINATION IMPORT
    (* proc *)  IsTerminating;

FROM DumpFile IMPORT
    (* proc *)  DumpString, DumpEOL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscPMOS IMPORT
    (* proc *)  CopyString;

(************************************************************************)

CONST
    testing = FALSE;

TYPE
    (* We have one ListElement for each termination procedure.  *)

    ListPointer = POINTER TO ListElement;
    ListElement =   RECORD
                        Procedure: PROC;
                        next: ListPointer;
                    END (*RECORD*);

VAR
    (* ListHead and ListTail point to the first and last elements,      *)
    (* respectively, of the list of termination procedures.             *)

    ListHead, ListTail: ListPointer;

    (* Text message supplied by caller of procedure Crash.      *)

    CrashMessage: ARRAY [0..79] OF CHAR;

    (* MessagePresent = TRUE iff procedure Crash has been called.       *)

    MessagePresent: BOOLEAN;

    VAR ESource: EXCEPTIONS.ExceptionSource;

(************************************************************************)
(*              UPDATING THE LIST OF TERMINATION PROCEDURES             *)
(************************************************************************)

PROCEDURE SetTerminationProcedure (TP: PROC);

    (* Adds TP to the list of procedures which will be called just      *)
    (* before program termination.  The list is ordered such that the   *)
    (* last procedure added will be the first one called.  Exception:   *)
    (* if termination is already in progress when this procedure is     *)
    (* called, then TP will not be called until all of the existing     *)
    (* termination procedures have been called.  This rule permits      *)
    (* multi-pass termination processing, where necessary, by letting   *)
    (* termination procedures themselves install more termination       *)
    (* procedures.                                                      *)

    VAR OldHead, NewTail: ListPointer;

    BEGIN
        IF IsTerminating() THEN

            (* Add the new list element to the tail of the list. *)

            NEW (NewTail);
            WITH NewTail^ DO
                Procedure := TP;
                next := NIL;
            END (*WITH*);
            IF ListTail = NIL THEN
                ListHead := NewTail;
            ELSE
                ListTail^.next := NewTail;
            END (*IF*);
            ListTail := NewTail;

        ELSE

            (* Termination not already in progress.  Add the new item   *)
            (* to the head of the list, to give the desired LIFO order. *)

            OldHead := ListHead;
            NEW (ListHead);
            WITH ListHead^ DO
                Procedure := TP;
                next := OldHead;
            END (*WITH*);
            IF OldHead = NIL THEN
                ListTail := ListHead;
            END (*IF*);

        END (*IF*);

    END SetTerminationProcedure;

(************************************************************************)
(*                      THE ACTUAL TERMINATION HANDLER                  *)
(************************************************************************)

PROCEDURE TerminationHandler;

    (* This is the procedure which is called on program termination.    *)
    (* It then calls all of the procedures which the user wants called. *)

    VAR OldHead: ListPointer;  UserProc: PROC;

    BEGIN

        (* Work through the list of termination procedures.  Note that  *)
        (* it's important to remove the termination handler from the    *)
        (* list before calling it, to avoid recursive calls in case the *)
        (* handler itself triggers another termination.                 *)

        WHILE ListHead <> NIL DO
            IF testing THEN
                DumpString ("Calling a user termination procedure.");  DumpEOL;
            END (*IF*);
            UserProc := ListHead^.Procedure;
            OldHead := ListHead;  ListHead := ListHead^.next;
            DISPOSE (OldHead);
            UserProc;
        END (*WHILE*);

    END TerminationHandler;

(************************************************************************)
(*                      RAISING AN ERROR CONDITION                      *)
(************************************************************************)

PROCEDURE Crash (message: ARRAY OF CHAR);

    (* Terminates the program with an error report.     *)

    BEGIN
        IF IsTerminating() THEN
            DumpString ("Termination.Crash called during termination.  Message is:");
            DumpEOL;
            DumpString (message);  DumpEOL;
        ELSE
            IF testing THEN
                DumpString ("Procedure Crash entered, message is:");  DumpEOL;
                DumpString (message);  DumpEOL;
            END (*IF*);
            CopyString (message, CrashMessage);
            MessagePresent := TRUE;

            (*EXCEPTIONS.RAISE (ESource, 1, message);*)

            HALT;
        END (*IF*);
    END Crash;

(************************************************************************)
(*                      USER-CALLABLE ERROR REPORTING                   *)
(************************************************************************)

PROCEDURE TerminationMessage (VAR (*OUT*) message: ARRAY OF CHAR): BOOLEAN;

    (* Returns the message supplied by the caller of the Crash          *)
    (* procedure.  The function result is TRUE if such a message        *)
    (* exists, and FALSE if Crash was never called.                     *)

    BEGIN
        IF MessagePresent THEN
            CopyString (CrashMessage, message);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END TerminationMessage;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

BEGIN
    EXCEPTIONS.AllocateSource(ESource);
    ListHead := NIL;  ListTail := NIL;
    MessagePresent := FALSE;
FINALLY
    IF testing THEN
        DumpString ("Entering TerminationHandler finalisation.");  DumpEOL;
    END (*IF*);
    TerminationHandler;
    IF testing THEN
        DumpString ("Leaving TerminationHandler finalisation.");  DumpEOL;
    END (*IF*);
END FinalExit.

