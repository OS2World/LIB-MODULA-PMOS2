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

IMPLEMENTATION MODULE FinalExit;

        (********************************************************)
        (*                                                      *)
        (*      Support for program termination procedures.     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        9 February 2001                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT EXCEPTIONS;

FROM TERMINATION IMPORT
    (* proc *)  IsTerminating;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM MiscPMOS IMPORT
    (* proc *)  CopyString;

FROM LowLevel IMPORT
    (* proc *)  Assert;

(************************************************************************)

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
        IF NOT IsTerminating() THEN
            CopyString (message, CrashMessage);
            MessagePresent := TRUE;
            Assert (FALSE);
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
    TerminationHandler;
END FinalExit.

