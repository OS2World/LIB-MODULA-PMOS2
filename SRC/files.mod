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

IMPLEMENTATION MODULE Files;

        (****************************************************************)
        (*                                                              *)
        (*                      File operations.                        *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        2 July 2001                             *)
        (*  Status:             Apparently working                      *)
        (*                                                              *)
        (*     Limitation: In this version we assume that the size      *)
        (*     of a file can fit in a CARDINAL.  I'll think about the   *)
        (*     issue of very large files at some later stage.           *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8;

FROM SYSTEM IMPORT
    (* type *)  BYTE, ADDRESS,
    (* proc *)  ADR;

FROM IOErrorCodes IMPORT
    (* type *)  ErrorCode;

IMPORT RndFile;
FROM RndFile IMPORT read, write, raw, OpenResults;

IMPORT IOChan;

IMPORT IOConsts;
FROM IOConsts IMPORT ReadResults;

(************************************************************************)
(*                  TRANSLATING THE LIBRARY ERROR CODES                 *)
(************************************************************************)

PROCEDURE TranslateOpenResults (code: RndFile.OpenResults): ErrorCode;

    (* Returns the status of the last I/O operation.  We do a "best     *)
    (* guess" approximation to the error code.                          *)

    BEGIN
        CASE code OF
          opened:    RETURN OK;
         |
          wrongNameFormat: RETURN InvalidFileNameString;
         |
          wrongFlags: RETURN UndiagnosedFailure;
         |
          tooManyOpen: RETURN UndiagnosedFailure;
         |
          outOfChans: RETURN UndiagnosedFailure;
         |
          wrongPermissions: RETURN UndiagnosedFailure;
         |
          noRoomOnDevice: RETURN DeviceFull;
         |
          noSuchFile: RETURN NameNotFound;
         |
          fileExists: RETURN DuplicateFileName;
         |
          wrongFileType: RETURN UndiagnosedFailure;
         |
          noTextOperations: RETURN UndiagnosedFailure;
         |
          noRawOperations: RETURN UndiagnosedFailure;
         |
          noMixedOperations: RETURN UndiagnosedFailure;
         |
          alreadyOpen: RETURN UndiagnosedFailure;
         |
          otherProblem: RETURN UndiagnosedFailure;
         |
        ELSE
                RETURN UndiagnosedFailure;
        END (*CASE*);
    END TranslateOpenResults;

(************************************************************************)
(*                           OPENING A FILE                             *)
(************************************************************************)

PROCEDURE OpenFile (VAR (*OUT*) f: File;  name: ARRAY OF CHAR;
                                        newfile: BOOLEAN): ErrorCode;

    (* Opens the file named by the given character string, and returns  *)
    (* f as the identification to be used when specifying this file in  *)
    (* future.  If newfile is TRUE, a new file is created.  If newfile  *)
    (* is FALSE, the file must already exist.                           *)

    VAR result: RndFile.OpenResults;

    BEGIN
        IF newfile THEN
            RndFile.OpenClean (f, name, read+write+raw, result);
        ELSE
            RndFile.OpenOld (f, name, read+write+raw, result);
        END (*IF*);
        RETURN TranslateOpenResults (result);
    END OpenFile;

(************************************************************************)
(*                           CLOSING A FILE                             *)
(************************************************************************)

PROCEDURE CloseFile (VAR (*INOUT*) f: File);

    (* Closes file f. *)

    BEGIN
        RndFile.Close(f);
    EXCEPT
        (* Do nothing if file not open. *)
    END CloseFile;

(************************************************************************)
(*                        FILE POSITION CALCULATIONS                    *)
(************************************************************************)

PROCEDURE FilePosDifference (p2, p1: RndFile.FilePos): INTEGER;

    (* Returns p2-p1, saturating the result if needed. *)

    (* There's probably a better way to do this, but for now I'm playing it     *)
    (* safe and avoiding special tricks.                                        *)

    TYPE Position = RECORD
                        CASE :BOOLEAN OF
                            FALSE:  FP: RndFile.FilePos;
                          | TRUE:   high: INTEGER;  low: CARDINAL;
                        END (*CASE*);
                    END (*RECORD*);

    VAR pos1, pos2, result: Position;

    BEGIN
        pos1.FP := p1;  pos2.FP := p2;

        (* Negate p1. *)

        IF pos1.low = 0 THEN
            pos1.high := -pos1.high;
        ELSE
            pos1.low := MAX(CARDINAL)-pos1.low+1;
            pos1.high := -pos1.high - 1;
        END (*IF*);

        (* Add in p2. *)

        IF pos1.low  > MAX(CARDINAL) - pos2.low THEN
            result.low := pos1.low - (MAX(CARDINAL) - pos2.low) - 1;
            result.high := pos1.high + pos2.high + 1;
        ELSE
            result.low := pos1.low + pos2.low;
            result.high := pos1.high + pos2.high;
        END (*IF*);

        (* Truncate the result to INTEGER. *)

        IF result.high < -1 THEN RETURN -MAX(INTEGER)
        ELSIF result.high = -1 THEN
            IF result.low > MAX(INTEGER) THEN RETURN -VAL(INTEGER,MAX(CARDINAL)-result.low+1)
            ELSE RETURN -MAX(INTEGER)
            END (*IF*);
        ELSIF (result.high > 0) OR (result.low > MAX(INTEGER)) THEN RETURN MAX(INTEGER)
        ELSE RETURN result.low;
        END (*IF*);

    END FilePosDifference;

(************************************************************************)
(*                          END-OF-FILE TEST                            *)
(************************************************************************)

PROCEDURE EOF (f: File): BOOLEAN;

    (* Returns TRUE iff we are currently at the end of file f.          *)

    BEGIN
        RETURN FilePosDifference (RndFile.CurrentPos(f), RndFile.EndPos(f)) >= 0;
    END EOF;

(************************************************************************)
(*                              WRITING                                 *)
(************************************************************************)

PROCEDURE WriteByte (f: File;  value: BYTE): ErrorCode;

    (* Writes one byte to the file.  The returned value is an error     *)
    (* code (OK if no error).                                           *)

    BEGIN
        IOChan.RawWrite (f, ADR(value), 1);
        RETURN OK;
    EXCEPT
        RETURN UndiagnosedFailure;
    END WriteByte;

(************************************************************************)

PROCEDURE WriteRecord (f: File;  buffaddr: ADDRESS;
                                        count: CARDINAL): ErrorCode;

    (* Writes count bytes from memory location buffaddr.        *)

    BEGIN
        IOChan.RawWrite (f, buffaddr, count);
        RETURN OK;
    EXCEPT
        RETURN UndiagnosedFailure;
    END WriteRecord;

(************************************************************************)
(*                              READING                                 *)
(************************************************************************)

PROCEDURE ReadByte (f: File): CARD8;

    (* Returns the next byte from the file.     *)

    VAR datum: CARD8;  locsRead: CARDINAL;

    BEGIN
        IOChan.RawRead (f, ADR(datum), 1, locsRead);
        RETURN datum;
    END ReadByte;

(************************************************************************)

PROCEDURE ReadRecord (f: File;  buffaddr: ADDRESS;  desired: CARDINAL;
                                VAR (*OUT*) actual: CARDINAL): ErrorCode;

    (* Reads up to "desired" bytes from file f to memory location       *)
    (* "buffaddr".  On return, "actual" gives the number of bytes read. *)

    VAR status: IOConsts.ReadResults;

    BEGIN
        IOChan.RawRead (f, buffaddr, desired, actual);
        status := IOChan.ReadResult (f);
        IF status = allRight THEN RETURN OK
        ELSIF status = endOfInput THEN RETURN IllegalBlockNumber
        ELSE RETURN UndiagnosedFailure;
        END (*IF*);
    END ReadRecord;

(************************************************************************)
(*                              RANDOM ACCESS                           *)
(************************************************************************)

PROCEDURE SetPosition (f: File;  position: CARDINAL): ErrorCode;

    (* Ensures that the next read or write on this file will be at      *)
    (* byte number position in the file.  (The first byte in the file   *)
    (* is byte number 0.)  If a position greater than the file size     *)
    (* is specified, the length of the file will increase.              *)

    VAR target: RndFile.FilePos;

    BEGIN
        target := RndFile.NewPos (f, position, 1, RndFile.StartPos(f));
        IF FilePosDifference (target, RndFile.EndPos(f)) > 0 THEN

            (* Case where padding needed. *)

            RndFile.SetPos (f, RndFile.EndPos(f));
            LOOP
                IF WriteByte (f, CHR(0)) <> OK THEN EXIT(*LOOP*) END (*IF*);
                IF FilePosDifference (target, RndFile.EndPos(f)) = 0 THEN EXIT (*LOOP*) END(*IF*);
            END (*LOOP*);

        ELSE

            RndFile.SetPos (f, target);

        END (*IF*);

        RETURN OK;

    END SetPosition;

(************************************************************************)

PROCEDURE SavePosition (f: File): CARDINAL;

    (* Returns the current byte number in file f.       *)

    BEGIN
        RETURN FilePosDifference (RndFile.CurrentPos(f), RndFile.StartPos(f));
    END SavePosition;

(************************************************************************)

PROCEDURE FileSize (f: File): CARDINAL;

    (* Returns the length of the file in bytes. *)

    BEGIN
        RETURN FilePosDifference (RndFile.EndPos(f), RndFile.StartPos(f));
    END FileSize;

(************************************************************************)

END Files.

