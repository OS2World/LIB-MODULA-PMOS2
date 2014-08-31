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

IMPLEMENTATION MODULE TCdebug;

        (****************************************************************)
        (*                                                              *)
        (*            Debug utility for low-level problems              *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    16 November 2012                        *)
        (*      Status:         OK                                      *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, Strings;

FROM MyClock IMPORT
    (* proc *)   CurrentTimeToString;

FROM SplitScreen IMPORT
    (* proc *)   LockScreen, UnlockScreen, WriteString, WriteLn;

FROM FileOps IMPORT
    (* type *)   ChanId,
    (* proc *)   OpenNewFile, Exists, DeleteFile,
                 FWriteString, FWriteLn, Flush, CloseFile;

(************************************************************************)

CONST
    LogFileName = "DebugLog.txt";

VAR
    (* Flags saying whether we want output to screen. *)

    WriteToScreen: BOOLEAN;

    (* Flag saying whether the disk file is open. *)

    DiskFileOpen: BOOLEAN;

    (* Channel ID of the output file. *)

    logcid: ChanId;

(************************************************************************)

PROCEDURE StartDebugLogging (useScreen, useDisk: BOOLEAN);

    (* Enables or disables output from this module. *)

    BEGIN
        WriteToScreen := useScreen;
        IF useDisk <> DiskFileOpen THEN
            IF useDisk THEN
                IF Exists(LogFileName) THEN
                    DeleteFile (LogFileName);
                END (*IF*);
                logcid := OpenNewFile (LogFileName, FALSE);
                DiskFileOpen := TRUE;
            ELSE
                CloseFile (logcid);
                DiskFileOpen := FALSE;
            END (*IF*);
        END (*IF*);
    END StartDebugLogging;

(************************************************************************)

PROCEDURE AppendHex1 (N: CARDINAL;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Appends a 1-digit value in hexadecimal. *)

    VAR str: ARRAY [0..0] OF CHAR;

    BEGIN
        IF N > 9 THEN
            str[0] := CHR(ORD('a')+N-10);
        ELSE
            str[0] := CHR(ORD('0')+N);
        END (*IF*);
        Strings.Append (str, line);
    END AppendHex1;

(************************************************************************)

PROCEDURE AppendHex2 (N: CARDINAL;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Appends a 2-digit value in hexadecimal. *)

    BEGIN
        AppendHex1 (N DIV 010H, line);
        AppendHex1 (N MOD 010H, line);
    END AppendHex2;

(************************************************************************)

PROCEDURE AppendHex4 (N: CARDINAL;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Appends an 4-digit value in hexadecimal. *)

    BEGIN
        AppendHex2 (N DIV 0100H, line);
        AppendHex2 (N MOD 0100H, line);
    END AppendHex4;

(************************************************************************)

PROCEDURE AppendHex (N: CARDINAL;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Appends an 8-digit value in hexadecimal. *)

    BEGIN
        AppendHex4 (N DIV 010000H, line);
        AppendHex4 (N MOD 010000H, line);
    END AppendHex;

(************************************************************************)

PROCEDURE AppendCard (N: CARDINAL;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Appends a value in decimal. *)

    VAR str: ARRAY [0..0] OF CHAR;

    BEGIN
        IF N > 9 THEN
            AppendCard (N DIV 10, line);
            N := N MOD 10;
        END (*IF*);
        str[0] := CHR(ORD('0')+N);
        Strings.Append (str, line);
    END AppendCard;

(************************************************************************)

PROCEDURE NoteSemOperation (op: SemOpKind;  handle: OS2.HEV;
                             thread: CARDINAL;  errnum: OS2.APIRET);

    (* Logs the fact that this operation has been done. *)

    VAR Buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        IF WriteToScreen OR DiskFileOpen THEN

            (* Create a string containing date/time.  *)

            CurrentTimeToString (Buffer);
            Strings.Append (" ", Buffer);

            (* Append operation kind. *)

            CASE op OF
                | sem_creat:    Strings.Append ("sem_creat", Buffer);
                | sem_wait:     Strings.Append ("sem_wait ", Buffer);
                | sem_post:     Strings.Append ("sem_post ", Buffer);
                | sem_reset:    Strings.Append ("sem_reset", Buffer);
                | sem_close:    Strings.Append ("sem_close", Buffer);

                ELSE
                                Strings.Append ("sem_?????", Buffer);
            END (*CASE*);

            (* Append the other fields. *)

            Strings.Append ("  HEV=0x", Buffer);
            AppendHex (handle, Buffer);
            Strings.Append ("  thread=0x", Buffer);
            AppendHex (thread, Buffer);
            Strings.Append ("  errnum=", Buffer);
            AppendCard (errnum, Buffer);

            IF DiskFileOpen THEN

                (* Write this line to disk. *)

                FWriteString (logcid, Buffer);
                FWriteLn (logcid);
                Flush (logcid);

            END (*IF*);

            IF WriteToScreen THEN

                (* Write it to the screen. *)

                LockScreen;
                WriteString (Buffer);  WriteLn;
                UnlockScreen;

            END (*IF*);

        END (*IF*);

    END NoteSemOperation;

(************************************************************************)

PROCEDURE NoteThreadOperation (op: ThrOpKind;  thread: CARDINAL;  name: ARRAY OF CHAR);

    (* Logs the fact that this operation has been done. *)

    VAR Buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        IF WriteToScreen OR DiskFileOpen THEN

            (* Create a string containing date/time.  *)

            CurrentTimeToString (Buffer);
            Strings.Append (" ", Buffer);

            (* Append operation kind. *)

            CASE op OF
                | thr_start:    Strings.Append ("thr_start", Buffer);
                | thr_awake:    Strings.Append ("thr_awake", Buffer);
                | thr_exit:     Strings.Append ("thr_exit ", Buffer);

                ELSE
                                Strings.Append ("thr_?????", Buffer);
            END (*CASE*);

            (* Append the other fields. *)

            Strings.Append ("  thread=0x", Buffer);
            AppendHex (thread, Buffer);
            Strings.Append ("  ", Buffer);
            Strings.Append (name, Buffer);

            IF DiskFileOpen THEN

                (* Write this line to disk. *)

                FWriteString (logcid, Buffer);
                FWriteLn (logcid);
                Flush (logcid);

            END (*IF*);

            IF WriteToScreen THEN

                (* Write it to the screen. *)

                LockScreen;
                WriteString (Buffer);  WriteLn;
                UnlockScreen;

            END (*IF*);

        END (*IF*);

    END NoteThreadOperation;

(************************************************************************)
(*                        MODULE INITIALISATION                         *)
(************************************************************************)

BEGIN
    WriteToScreen := FALSE;
    DiskFileOpen := FALSE;
FINALLY
    IF DiskFileOpen THEN
        CloseFile (logcid);
    END (*IF*);
END TCdebug.

