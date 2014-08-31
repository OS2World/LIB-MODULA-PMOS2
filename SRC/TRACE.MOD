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

IMPLEMENTATION MODULE Trace;

        (********************************************************)
        (*                                                      *)
        (*             Trace routines for Modula-2              *)
        (*                 program development.                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        6 February 1998                 *)
        (*  Status:             Starting new approach           *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* proc *)  CAST;

FROM DumpFile IMPORT
    (* proc *)  DumpString, DumpCard, DumpEOL;

(*
FROM Mouse IMPORT
    (* proc *)  MouseAvailable;
*)

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Windows IMPORT
    (* type *)  Window, RowRange, ColumnRange,
    (* proc *)  OpenSimpleWindow, CloseWindow, WriteString, WriteLn,
                ReadChar, EraseLine, CursorUp, PressAnyKey;

FROM NumericIO IMPORT
    (* proc *)  WriteCard;

(*
FROM UserInterface IMPORT
    (* type *)  UIWindow, Capability, CapabilitySet,
    (* proc *)  AllowMouseControl;
*)

FROM TaskControl IMPORT
    (* proc *)  CurrentTaskID;

(************************************************************************)

TYPE
    ThreadID = [0..100];

VAR ThreadInfo: ARRAY ThreadID OF
                    RECORD
                        TraceWindowOpen, TraceEnabled: BOOLEAN;
                        nesting: CARDINAL;  PauseLength: CARDINAL;
                        TraceWindow: Window;
                    END (*RECORD*);

(************************************************************************)

PROCEDURE CurrentID(): ThreadID;

    (* Returns an identifier for the current thread.  If we're tracing  *)
    (* too many threads, or if for any other reason the ID would go     *)
    (* outside the legal range, a result of 0 is returned.              *)

    VAR result: CARDINAL;

    BEGIN
        result := CAST(CARDINAL,CurrentTaskID()) MOD MAX(ThreadID) + 1;
        IF result > MAX(ThreadID) THEN result := 0 END (*IF*);
        RETURN result;
    END CurrentID;

(************************************************************************)

PROCEDURE Pause;

    (* Types a "Press any key to continue" message.     *)
    (* A temporary trace window is opened if necessary. *)

    BEGIN
        WITH ThreadInfo[CurrentID()] DO
            IF NOT TraceWindowOpen THEN
                OpenSimpleWindow (TraceWindow, 22, 24, 0, 28);
                (*
                IF MouseAvailable() THEN
                    UIW := AllowMouseControl (TraceWindow, "Pause message",
                                            CapabilitySet {wshow, wmove});
                END (*IF*);
                *)
            END (*IF*);
            WriteLn (TraceWindow);
            PressAnyKey (TraceWindow);
            IF TraceWindowOpen THEN    (* i.e. not just the temporary window *)
                EraseLine (TraceWindow, 0);  CursorUp(TraceWindow);
            ELSE
                CloseWindow (TraceWindow);
            END (*IF*);
        END (*WITH*);
    END Pause;

(************************************************************************)

PROCEDURE NYI (name: ARRAY OF CHAR);

    (* Types a "not yet implemented" message.  A trace window is opened *)
    (* if necessary.                                                    *)

    (*VAR UIW: UIWindow;*)

    BEGIN
        WITH ThreadInfo[CurrentID()] DO
            IF NOT TraceWindowOpen THEN
                OpenSimpleWindow (TraceWindow, 21, 24, 1, 45);
                TraceWindowOpen := TRUE;
                (*
                IF MouseAvailable() THEN
                    UIW := AllowMouseControl (TraceWindow, "Procedure trace",
                                    CapabilitySet {wshow, wmove, whide});
                END (*IF*);
                *)
            END (*IF*);
            WriteLn (TraceWindow);
            WriteString (TraceWindow, name);
            WriteString (TraceWindow, " is not yet implemented.");
        END (*WITH*);
    END NYI;

(************************************************************************)

PROCEDURE DeliberatePause;

    (* Inserts a delay in execution, for situations where the trace     *)
    (* messages would otherwise flash by on the screen too quickly      *)
    (* to read.                                                         *)

    BEGIN
        Sleep (10*ThreadInfo[CurrentID()].PauseLength);
    END DeliberatePause;

(************************************************************************)

PROCEDURE InTrace (name: ARRAY OF CHAR);

    (* To be called when entering a procedure.  *)

    VAR j: CARDINAL;

    BEGIN
        WITH ThreadInfo[CurrentID()] DO
            IF TraceEnabled THEN
                WriteLn (TraceWindow);
                FOR j := 1 TO nesting DO
                    WriteString (TraceWindow, "   ");
                END (*FOR*);
                WriteString (TraceWindow, "Entering ");
                WriteString (TraceWindow, name);
                DeliberatePause;
            END (*IF*);
            INC (nesting);
        END (*WITH*);
    END InTrace;

(************************************************************************)

PROCEDURE OutTrace (name: ARRAY OF CHAR);

    (* To be called when leaving a procedure.   *)

    VAR j: CARDINAL;

    BEGIN
        WITH ThreadInfo[CurrentID()] DO
            DEC (nesting);
            IF TraceEnabled THEN
                WriteLn (TraceWindow);
                FOR j := 1 TO nesting DO
                    WriteString (TraceWindow, "   ");
                END (*FOR*);
                WriteString (TraceWindow, "Leaving ");
                WriteString (TraceWindow, name);
                DeliberatePause;
            END (*IF*);
        END (*WITH*);
    END OutTrace;

(************************************************************************)

PROCEDURE TraceOn (firstrow, lastrow: RowRange;
                        firstcol, lastcol: ColumnRange;
                        SlowDownFactor: CARDINAL);

    (* Turns tracing on.                *)

    (*VAR UIW: UIWindow;*)

    BEGIN
        DumpString ("TraceOn called by task ");  DumpCard (CurrentID());  DumpEOL;
        WITH ThreadInfo[CurrentID()] DO
            IF TraceWindowOpen THEN

                (* Close any previous trace window. *)

                CloseWindow (TraceWindow);

            END (*IF*);
            PauseLength := SlowDownFactor;
            OpenSimpleWindow (TraceWindow, firstrow, lastrow, firstcol, lastcol);
            WriteString (TraceWindow, "Tracing task ");
            WriteCard (TraceWindow, CurrentID());  WriteLn (TraceWindow);
            (*
            IF MouseAvailable() THEN
                UIW := AllowMouseControl (TraceWindow, "Procedure trace",
                                    CapabilitySet {wshow, wmove, whide});
            END (*IF*);
            *)
            TraceWindowOpen := TRUE;  TraceEnabled := TRUE;
        END (*WITH*);
    END TraceOn;

(************************************************************************)

PROCEDURE TraceOff;

    (* Turns tracing off.               *)

    BEGIN
        WITH ThreadInfo[CurrentID()] DO
            IF TraceEnabled THEN
                CloseWindow (TraceWindow);  TraceWindowOpen := FALSE;
            END (*IF*);
            TraceEnabled := FALSE;
        END (*WITH*);
    END TraceOff;

(************************************************************************)

PROCEDURE TraceStatus (): BOOLEAN;

    (* Says whether tracing is currently on.            *)

    BEGIN
        RETURN ThreadInfo[CurrentID()].TraceEnabled;
    END TraceStatus;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

VAR j: ThreadID;

BEGIN
    FOR j := 0 TO MAX(ThreadID) DO
        WITH ThreadInfo[j] DO
            TraceEnabled := FALSE;  nesting := 0;  TraceWindowOpen := FALSE;
        END (*WITH*);
    END (*FOR*);
END Trace.

