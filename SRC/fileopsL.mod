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

IMPLEMENTATION MODULE FileOpsL;

        (********************************************************)
        (*                                                      *)
        (*                 File utilities                       *)
        (*                                                      *)
        (*       This is the 'long file pointer' version        *)
        (*       that uses the WSeB API extensions that         *)
        (*       permit file sizes bigger than 2 GB.            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            17 October 2001                 *)
        (*  Last edited:        18 October 2001                 *)
        (*  Status:             Just started                    *)
        (*                                                      *)
        (*    The original version of this module used          *)
        (*    the standard Modula-2 file I/O modules to do      *)
        (*    its job, but I suspect that those modules have    *)
        (*    an unresolved critical section problem, or        *)
        (*    something similar.  The present version bypasses  *)
        (*    those modules and goes directly to API calls.     *)
        (*                                                      *)
        (********************************************************)


IMPORT OS2, FileSys, Strings;

FROM SYSTEM IMPORT
    (* type *)  LOC,
    (* proc *)  ADR, CAST;

FROM LONGLONG IMPORT
    (* const*)  Zero64,
    (* type *)  CARD64,
    (* proc *)  High, Low;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR;

(************************************************************************)

VAR
    (* LongSupport is TRUE if the OS version is high enough to support  *)
    (* 64-bit file pointers.                                            *)

    LongSupport: BOOLEAN;

(************************************************************************)
(*                         VOLUME INFORMATION                           *)
(************************************************************************)

PROCEDURE FreeSpace (drive: CHAR): CARDINAL;

    (* Returns the amount of free space on the given drive.  The        *)
    (* result is in kilobytes.                                          *)

    VAR Buffer: OS2.FSALLOCATE;  result: REAL;

    BEGIN
        drive := CAP(drive);
        IF drive < 'A' THEN
            RETURN 0;
        END (*IF*);
        IF OS2.DosQueryFSInfo (ORD(drive) - ORD('A') + 1, 1,
                                     ADR(Buffer), SIZE(Buffer)) <> 0 THEN
            RETURN 0;
        END (*IF*);

        (* Use floating point to calculate the free space in    *)
        (* kilobytes, to avoid overflow errors on large disks.  *)

        WITH Buffer DO
            result := FLOAT(cSectorUnit) * FLOAT(cbSector)
                                         * FLOAT(cUnitAvail) / 1024.0;
        END (*WITH*);
        RETURN TRUNC(result);

    END FreeSpace;

(************************************************************************)
(*                      GENERAL FILE OPERATIONS                         *)
(************************************************************************)

PROCEDURE OpenOldFile (name: ARRAY OF CHAR;  WillWrite: BOOLEAN): ChanId;

    (* Opens an existing file and returns its channel ID.  If the       *)
    (* second parameter is TRUE we are requesting write as well as read *)
    (* access; if it's FALSE, we want read-only access.                 *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW
                    + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        Mode1 = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READONLY;
        Mode2 = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Mode, Action: CARDINAL;

    BEGIN
        IF WillWrite THEN
            Mode := Mode2;
        ELSE
            Mode := Mode1;
        END (*IF*);
        IF LongSupport THEN
            rc := OS2.DosOpenL (name, cid, Action, 0, 0,
                                           0, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, 0, OpenFlags, Mode, NIL);
        END (*IF*);
        IF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenOldFile;

(************************************************************************)

PROCEDURE OpenNewFile (name: ARRAY OF CHAR): ChanId;

    (* Opens a new file and returns its channel ID. *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_CREATE_IF_NEW
                    + OS2.OPEN_ACTION_FAIL_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Action: CARDINAL;

    BEGIN
        IF LongSupport THEN
            rc := OS2.DosOpenL (name, cid, Action, 0, 0,
                                       0, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, 0, OpenFlags, Mode, NIL);
        END (*IF*);
        IF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenNewFile;

(************************************************************************)

PROCEDURE OpenNewFile0 (name: ARRAY OF CHAR;  Attributes: CARDINAL;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile, but returns an indication of whether the       *)
    (* file couldn't be created because of a name duplication.  Also    *)
    (* allows attributes to be specified.                               *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_CREATE_IF_NEW
                    + OS2.OPEN_ACTION_FAIL_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Action: CARDINAL;

    BEGIN
        IF LongSupport THEN
            rc := OS2.DosOpenL (name, cid, Action, 0, 0,
                                       Attributes, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, Attributes, OpenFlags,
                                       Mode, NIL);
        END (*IF*);
        duplicate := rc = OS2.ERROR_FILE_EXISTS;
        IF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenNewFile0;

(************************************************************************)

PROCEDURE OpenNewFile1 (name: ARRAY OF CHAR;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile, but returns an indication of whether the       *)
    (* file couldn't be created because of a name duplication.          *)

    BEGIN
        RETURN OpenNewFile0 (name, 0, duplicate);
    END OpenNewFile1;

(************************************************************************)

PROCEDURE OpenNewHiddenFile (name: ARRAY OF CHAR;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile1, but creates the file hidden.  *)

    BEGIN
        RETURN OpenNewFile0 (name, OS2.FILE_HIDDEN, duplicate);
    END OpenNewHiddenFile;

(************************************************************************)

PROCEDURE OpenAtEnd (name: ARRAY OF CHAR): ChanId;

    (* If the file already exists, opens it and positions the file      *)
    (* pointer at the end of the file.  If the file doesn't already     *)
    (* exist, opens a new file.                                         *)

    VAR cid: ChanId;  Actual: FilePos;

    BEGIN
        IF FileSys.Exists (name) THEN
            cid := OpenOldFile (name, TRUE);
            IF cid <> NoSuchChannel THEN
                OS2.DosSetFilePtr (cid, 0, OS2.FILE_END, ADR(Actual));
            END (*IF*);
        ELSE
            cid := OpenNewFile (name);
        END (*IF*);
        RETURN cid;
    END OpenAtEnd;

(************************************************************************)

PROCEDURE CloseFile (cid: ChanId);

    (* Closes a file. *)

    BEGIN
        OS2.DosClose (cid);
    END CloseFile;

(************************************************************************)

PROCEDURE HideFile (name: ARRAY OF CHAR;  HideIt: BOOLEAN);

    (* Hides or unhides a file, depending on the second parameter. *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW
                    + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYREADWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  Action: CARDINAL;  InfoBuf: OS2.FILESTATUS3;
        success: BOOLEAN;

    BEGIN
        IF LongSupport THEN
            success := OS2.DosOpenL (name, cid, Action, 0, 0,
                                       0, OpenFlags, Mode, NIL) = 0;
        ELSE
            success := OS2.DosOpen (name, cid, Action, 0,
                                       0, OpenFlags, Mode, NIL) = 0;
        END (*IF*);
        IF success THEN
            OS2.DosQueryFileInfo(cid, OS2.FIL_STANDARD,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            IF HideIt THEN
                InfoBuf.attrFile := IOR (InfoBuf.attrFile, 2);
            ELSE
                InfoBuf.attrFile := IAND (InfoBuf.attrFile, MAX(CARDINAL)-2);
            END (*IF*);
            OS2.DosSetFileInfo(cid, OS2.FIL_STANDARD,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            OS2.DosClose (cid);
        END (*IF*);
    END HideFile;

(************************************************************************)

PROCEDURE DeleteFile (name: ARRAY OF CHAR);

    (* Deletes a named file. *)

    VAR dummy: BOOLEAN;

    BEGIN
        FileSys.Remove (name, dummy);
    END DeleteFile;

(************************************************************************)

PROCEDURE MoveFile (oldname, newname: ARRAY OF CHAR): BOOLEAN;

    (* Renames a file, returns TRUE iff successful.  The source and     *)
    (* destination files must be on the same drive.  This procedure is  *)
    (* also a mechanism for renaming a file.                            *)

    VAR code: CARDINAL;

    BEGIN
        code := OS2.DosMove (oldname, newname);
        RETURN code = 0;
    END MoveFile;

(************************************************************************)

PROCEDURE AppendFile (src, dst: ARRAY OF CHAR): BOOLEAN;

    (* Appends src to dst, returns TRUE iff successful. *)

    VAR code: CARDINAL;

    BEGIN
        code := OS2.DosCopy (src, dst, OS2.DCPY_APPEND);
        RETURN code = 0;
    END AppendFile;

(************************************************************************)
(*                      FILE POSITION AND SIZE                          *)
(************************************************************************)

PROCEDURE CurrentPosition (cid: ChanId): FilePos;

    (* Returns the current position within the file. *)

    VAR Actual: FilePos;

    BEGIN
        Actual.high := 0;
        IF LongSupport THEN
            OS2.DosSetFilePtrL (cid, 0, 0, OS2.FILE_CURRENT, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_CURRENT, ADR(Actual.low));
        END (*IF*);
        RETURN Actual;
    END CurrentPosition;

(************************************************************************)

PROCEDURE StartPosition (cid: ChanId):  FilePos;

    (* Returns the start-of-file position. *)

    BEGIN
        RETURN Zero64;
    END StartPosition;

(************************************************************************)

PROCEDURE EndPosition (cid: ChanId): FilePos;

    (* Returns the end-of-file position. *)

    VAR Actual, EndPos: FilePos;

    BEGIN
        Actual.high := 0;  EndPos.high := 0;
        IF LongSupport THEN
            OS2.DosSetFilePtrL (cid, 0, 0, OS2.FILE_CURRENT, ADR(Actual));
            OS2.DosSetFilePtrL (cid, 0, 0, OS2.FILE_END, ADR(EndPos));
            OS2.DosSetFilePtrL (cid, Actual.low, Actual.high, OS2.FILE_BEGIN, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_CURRENT, ADR(Actual.low));
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_END, ADR(EndPos.low));
            OS2.DosSetFilePtr (cid, Actual.low, OS2.FILE_BEGIN, ADR(Actual.low));
        END (*IF*);
        RETURN EndPos;
    END EndPosition;

(************************************************************************)

PROCEDURE SetPosition (cid: ChanId;  position: FilePos);

    (* Sets the current position within the file. *)

    VAR Actual: FilePos;

    BEGIN
        IF LongSupport THEN
            OS2.DosSetFilePtrL (cid, position.low, position.high,
                                OS2.FILE_BEGIN, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, position.low,
                                OS2.FILE_BEGIN, ADR(Actual.low));
        END (*IF*);
    END SetPosition;

(************************************************************************)

PROCEDURE SetFileSize (cid: ChanId;  newsize: CARD64);

    (* Changes the size of a file.  The file must already be open in    *)
    (* a mode that permits writing.                                     *)

    BEGIN
        IF LongSupport THEN
            OS2.DosSetFileSizeL (cid, newsize.low, newsize.high);
        ELSE
            OS2.DosSetFileSize (cid, newsize.low);
        END (*IF*);
    END SetFileSize;

(************************************************************************)
(*                              READ                                    *)
(************************************************************************)

PROCEDURE ReadRaw (cid: ChanId;  VAR (*OUT*) data: ARRAY OF LOC;
                   limit: CARDINAL;  VAR (*OUT*) NumberRead: CARDINAL);

    (* Reads a buffer-full of information from a file. *)

    BEGIN
        OS2.DosRead (cid, ADR(data), limit, NumberRead);
    END ReadRaw;

(************************************************************************)

PROCEDURE ReadLine (cid: ChanId;  VAR (*OUT*) data: ARRAY OF CHAR);

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid having to keep a lookahead character, I take     *)
    (* the LF as end of line and skip the CR.                           *)

    CONST Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);  CtrlZ = CHR(26);

    VAR j, NumberRead: CARDINAL;
        ch: CHAR;

    BEGIN
        j := 0;  ch := Nul;
        LOOP
            OS2.DosRead (cid, ADR(ch), 1, NumberRead);
            IF NumberRead = 0 THEN
                IF j = 0 THEN
                    data[0] := CtrlZ;  j := 1;
                END (*IF*);
                EXIT (*LOOP*);
            ELSIF ch = CR THEN
                (* ignore carriage return. *)
            ELSIF ch = LF THEN
                EXIT (*LOOP*);
            ELSIF j <= HIGH(data) THEN
                data[j] := ch;  INC(j);
            END (*IF*);
        END (*LOOP*);

        IF j <= HIGH(data) THEN
            data[j] := Nul;
        END (*IF*);

    END ReadLine;

(************************************************************************)
(*                               WRITE                                  *)
(************************************************************************)

PROCEDURE WriteRaw (cid: ChanId;  data: ARRAY OF LOC;  amount: CARDINAL);

    (* Writes a binary string to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(data), amount, actual);
    END WriteRaw;

(************************************************************************)

PROCEDURE FWriteChar (cid: ChanId;  character: CHAR);

    (* Writes a single character to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(character), 1, actual);
    END FWriteChar;

(************************************************************************)

PROCEDURE FWriteHexDigit (cid: ChanId;  value: CARDINAL);

    (* Writes a one digit hexadecimal number to a file. *)

    BEGIN
        IF value < 10 THEN
            FWriteChar (cid, CHR(ORD('0') + value));
        ELSE
            FWriteChar (cid, CHR(ORD('A') - 10 + value));
        END (*IF*);
    END FWriteHexDigit;

(************************************************************************)

PROCEDURE FWriteHexByte (cid: ChanId;  value: CARDINAL);

    (* Writes a two digit hexadecimal number to a file. *)

    BEGIN
        FWriteHexDigit (cid, value DIV 16);
        FWriteHexDigit (cid, value MOD 16);
    END FWriteHexByte;

(************************************************************************)

PROCEDURE FWriteString (cid: ChanId;  string: ARRAY OF CHAR);

    (* Writes a string to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(string), LENGTH(string), actual);
    END FWriteString;

(************************************************************************)

PROCEDURE FWriteLn (cid: ChanId);

    (* Writes end-of-line to the file. *)

    TYPE TwoChar = ARRAY [0..1] OF CHAR;
    CONST CRLF = TwoChar {CHR(13), CHR(10)};

    BEGIN
        WriteRaw (cid, CRLF, 2);
    END FWriteLn;

(************************************************************************)

PROCEDURE FWriteCard (cid: ChanId;  value, fieldwidth: CARDINAL);

    (* Converts value to decimal and writes it to the file. *)

    VAR Buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        WriteRaw (cid, Buffer, fieldwidth);
    END FWriteCard;

(************************************************************************)

PROCEDURE FWriteZCard (cid: ChanId;  value, fieldwidth: CARDINAL);

    (* Like FWriteCard, but with 0 fill at the left. *)

    VAR Buffer: ARRAY [0..63] OF CHAR;  j: CARDINAL;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        j := 0;
        WHILE Buffer[j] = ' ' DO
            Buffer[j] := '0';  INC(j);
        END (*WHILE*);
        WriteRaw (cid, Buffer, fieldwidth);
    END FWriteZCard;

(************************************************************************)

PROCEDURE FWriteLJCard (cid: ChanId;  value: CARDINAL);

    (* Converts value to decimal and writes it left justified. *)

    CONST fieldwidth = 64;

    VAR j: CARDINAL;  Buffer: ARRAY [0..fieldwidth-1] OF CHAR;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        j := 0;
        WHILE Buffer[j] = ' ' DO INC(j) END (*WHILE*);
        IF j > 0 THEN
            Strings.Delete (Buffer, 0, j);
        END (*IF*);
        j := 0;
        WHILE (j < fieldwidth) AND (Buffer[j] <> CHR(0)) DO
            INC (j);
        END (* WHILE *);
        WriteRaw (cid, Buffer, j);
    END FWriteLJCard;

(************************************************************************)
(*                        DIRECTORY SEARCHES                            *)
(************************************************************************)

PROCEDURE ConvertFindResult (VAR (*IN*) FindBuffer: OS2.FILEFINDBUF3;
                             VAR (*OUT*) D: DirectoryEntry);

    (* Copies the result of a directory lookup to the format we're using. *)

    BEGIN
        D.attr    := CAST (FileAttr, FindBuffer.attrFile);
        D.timePkd := FindBuffer.ftimeLastWrite;
        D.datePkd := FindBuffer.fdateLastWrite;
        D.size    := FindBuffer.cbFile;
        Strings.Assign (FindBuffer.achName, D.name);
    END ConvertFindResult;

(************************************************************************)

PROCEDURE FirstDirEntry (mask: ARRAY OF CHAR;
                             Subdirectory, AllowHidden: BOOLEAN;
                                  VAR (*OUT*) D: DirectoryEntry): BOOLEAN;

    (* Gets the first directory entry satisfying the conditions:        *)
    (*  (a) if Subdirectory is FALSE, we want the first entry that      *)
    (*      matches "mask".                                             *)
    (*  (b) if Subdirectory is TRUE, we want the first directory that   *)
    (*      matches "mask".                                             *)
    (* In either case "mask" is a filename specification that may       *)
    (* include wildcards.  Hidden files are included in the search iff  *)
    (* AllowHidden is TRUE.                                             *)

    CONST ResultBufLen = SIZE(OS2.FILEFINDBUF3);

    VAR FindBuffer: OS2.FILEFINDBUF3;
        attrib, FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        D.dirHandle := OS2.HDIR_CREATE;
        FindCount := 1;
        IF Subdirectory THEN
            attrib := 1035H;
        ELSE
            attrib := 035H;
        END (*IF*);
        IF AllowHidden THEN
            INC (attrib, 2);
        END (*IF*);
        rc := OS2.DosFindFirst (mask, D.dirHandle, attrib,
                                ADR(FindBuffer), ResultBufLen,
                                FindCount, OS2.FIL_STANDARD);
        ConvertFindResult (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END FirstDirEntry;

(************************************************************************)

PROCEDURE NextDirEntry (VAR (*INOUT*) D: DirectoryEntry): BOOLEAN;

    (* Read the next directory entry satisfying the search criteria     *)
    (* specified by the FirstDirEntry call.                             *)

    CONST ResultBufLen = SIZE(OS2.FILEFINDBUF3);

    VAR FindBuffer: OS2.FILEFINDBUF3;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        FindCount := 1;
        rc := OS2.DosFindNext(D.dirHandle, ADR(FindBuffer),
                              ResultBufLen, FindCount);
        ConvertFindResult (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END NextDirEntry;

(************************************************************************)

PROCEDURE DirSearchDone (VAR (*INOUT*) D: DirectoryEntry);

    (* Close the directory that D represents. *)

    BEGIN
        OS2.DosFindClose (D.dirHandle);
    END DirSearchDone;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

(************************************************************************)
(*                      QUERYING THE SYSTEM VERSION                     *)
(************************************************************************)

PROCEDURE CheckSystemVersion;

    (* Checks whether the system version is high enough to support      *)
    (* files of size > 2GB.                                             *)

    VAR Version: ARRAY [0..1] OF CARDINAL;

    BEGIN
        OS2.DosQuerySysInfo (11, 12, ADR(Version), SIZE(Version));
        LongSupport := (Version[0] > 20) OR
                           ((Version[0] = 20) AND (Version[1] >= 45));

        (* Suggestion from Daniela Engert:                              *)
        (* [...] instead of checking for versions I'd rather check for  *)
        (* features. What about watching DosQueryProcAddress (DosOpenL) *)
        (* to fail or succeed ? Runtime linking to the few LFS APIs     *)
        (* sounds more appropriate to me than static linking.           *)

    END CheckSystemVersion;

(************************************************************************)

PROCEDURE SetWorkingDirectory;

    (* Sets the working drive and directory to be the same as that  *)
    (* where the executable resides.                                *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        PathName: ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                        PathName) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (PathName);
            WHILE (j > 0) AND (PathName[j] <> '\') DO
                DEC (j);
            END (*WHILE*);
            PathName[j] := CHR(0);

            (* Extract the drive name and set working drive. *)

            IF (j > 1) AND (PathName[1] = ':') THEN
                OS2.DosSetDefaultDisk (ORD(CAP(PathName[0])) - ORD('A') + 1);
                Strings.Delete (PathName, 0, 2);
            END (*IF*);

            (* Set the working directory. *)

            OS2.DosSetCurrentDir (PathName);

        END (*IF*);

    END SetWorkingDirectory;

(************************************************************************)

BEGIN
    OS2.DosError (OS2.FERR_DISABLEHARDERR); (* disable hard error popups *)
    CheckSystemVersion;
    SetWorkingDirectory;
    OS2.DosSetMaxFH (100);
END FileOpsL.

