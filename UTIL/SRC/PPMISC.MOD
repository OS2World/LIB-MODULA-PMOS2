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

IMPLEMENTATION MODULE PPMisc;

        (********************************************************)
        (*                                                      *)
        (*      Miscellaneous procedures for preprocessor.      *)
        (*                                                      *)
        (*      The purpose of this module is to collect        *)
        (*      together the compiler-dependent of the          *)
        (*      preprocessor.  The module consists mostly       *)
        (*      of file and string operations.                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        3 August 1996                   *)
        (*  Status:             Working with FST, TopSpeed 1.17 *)
        (*                        TopSpeed 3.10, and XDS        *)
        (*                                                      *)
        (*  Rowley version is untested and probably wrong.      *)
        (*                                                      *)
        (********************************************************)

(*<FST
IMPORT System, InOut, FileSystem, Directories, PPTextIO, Strings;
>*)

(*<Rowley
IMPORT Environment, InOut, FileSystem, Directories, TextIO, Strings;
>*)

(*<TopSpeed
IMPORT SYSTEM, FIO, IO, Lib, Str;
>*)

(*<XDS*)
IMPORT SYSTEM, Strings, IOChan, STextIO, ProgramArgs, TextIO, FileSys,
       StreamFile, IOConsts;
(*>*)

(************************************************************************)

(*<TopSpeed

(* Warning: the code used for TopSpeed allocates file buffers on the    *)
(* assumption that we never have more than one input file and one       *)
(* output file open at any one time.                                    *)

VAR InFileBuffer, OutFileBuffer: ARRAY [1..2048+FIO.BufferOverhead] OF BYTE;
>*)

(*<XDS*) VAR argchannel: IOChan.ChanId; (*>*)

(************************************************************************)
(*                         STRING OPERATIONS                            *)
(************************************************************************)

PROCEDURE Length (s: ARRAY OF CHAR): CARDINAL;

    (* Returns the length of string s. *)

    BEGIN
        RETURN (*<TopSpeed Str.Length(s); >*)
               (*<FST|Rowley|XDS*) Strings.Length(s); (*>*)
    END Length;

(************************************************************************)

PROCEDURE StringMatch (s1, s2: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the two strings are equal. *)

    BEGIN
        (*<FST RETURN (Strings.CompareStr (s1, s2) = 0); >*)
        (*<TopSpeed RETURN (Str.Compare (s1, s2) = 0); >*)
        (*<Rowley RETURN (Strings.Compare (s1, s2) = 0); >*)
        (*<XDS*) RETURN (Strings.Compare (s1, s2) = Strings.equal); (*>*)
    END StringMatch;

(************************************************************************)

PROCEDURE CopyString (src: ARRAY OF CHAR; VAR (*OUT*) dst: ARRAY OF CHAR);

    (* Copies src to dst. *)

    BEGIN
        (*<FST|Rowley|XDS*) Strings.Assign (src, dst); (*>*)
        (*<TopSpeed Str.Copy (dst, src); >*)
    END CopyString;

(************************************************************************)

PROCEDURE Pos (pattern, string: ARRAY OF CHAR): CARDINAL;

    (* If pattern is a substring of string, returns the index in        *)
    (* string of the start of the first occurrence of pattern.  If      *)
    (* no match then the value returned is beyond the end of string.    *)

    (*<XDS*) VAR found: BOOLEAN;  position: CARDINAL; (*>*)

    BEGIN
        (*<FST|Rowley RETURN Strings.Pos (pattern, string); >*)
        (*<TopSpeed
        (* A trap for the unwary: note the order of the parameters. *)
        RETURN Str.Pos (string, pattern);
        >*)
        (*<XDS*)
        Strings.FindNext (pattern, string, 0, found, position);
        IF found THEN RETURN position
        ELSE RETURN Strings.Length(string)+1;
        END (*IF*);
        (*>*)
    END Pos;

(************************************************************************)
(*                              TERMINAL I/O                            *)
(************************************************************************)

PROCEDURE Message (mess: ARRAY OF CHAR);

    (* Writes a string to the screen. *)

    BEGIN
        (*<FST|Rowley InOut.WriteString (mess); >*)
        (*<TopSpeed IO.WrStr (mess); >*)
        (*<XDS*) STextIO.WriteString (mess); (*>*)
    END Message;

(************************************************************************)

PROCEDURE EndOfMessage;

    (* Goes to the next line on the screen. *)

    BEGIN
        (*<FST|Rowley InOut.WriteLn; >*)
        (*<TopSpeed IO.WrLn; >*)
        (*<XDS*) STextIO.WriteLn; (*>*)
    END EndOfMessage;

(************************************************************************)

PROCEDURE CommandLine (argNr: CARDINAL;  VAR (*OUT*) strg : ARRAY OF CHAR;
                                        VAR (*OUT*) arglen : CARDINAL);

    (* Picks up argument number argNr from the command line.  If there  *)
    (* is no such argument then arglen is returned as zero.             *)

(*<FST
    BEGIN
        System.GetArg (strg, arglen);
    END CommandLine;
>*)

(*<TopSpeed
    BEGIN
        Lib.ParamStr (strg, argNr);
        arglen := Str.Length (strg);
    END CommandLine;
>*)

(*<XDS*)
    BEGIN
        (* In this version argNr is ignored, and we assume that the arguments   *)
        (* are read in order.  I could improve this, but for the PP application *)
        (* this is good enough.                                                 *)
        IF ProgramArgs.IsArgPresent() THEN
            TextIO.ReadString (argchannel, strg);
            arglen := Strings.Length (strg);
            ProgramArgs.NextArg;
        ELSE
            Strings.Assign ("", strg);
            arglen := 0;
        END (*IF*);
    END CommandLine;
(*>*)

(*<Rowley
    VAR OK : BOOLEAN;
    BEGIN
        Environment.GetArg (argNr, strg, OK);
        IF NOT OK THEN
            arglen := 0;
        ELSE
            arglen := Strings.Length (strg);
        END;
    END CommandLine;
>*)

(************************************************************************)
(*                           FILE OPERATIONS                            *)
(************************************************************************)

PROCEDURE OpenFile (VAR (*OUT*) f: File;  filename: ARRAY OF CHAR;
                                                create: BOOLEAN): BOOLEAN;

    (* Opens the file specified as "filename".  We open it for input if *)
    (* create=FALSE, or create a new file for output if create=TRUE.    *)
    (* The function result indicates success.                           *)

    (*<TopSpeed VAR temp: BOOLEAN; >*)
    (*<XDS*) VAR status: StreamFile.OpenResults; (*>*)

    BEGIN
(*<TopSpeed
        temp := FIO.IOcheck;  FIO.IOcheck := FALSE;
        IF create THEN
            f := FIO.Create (filename);
        ELSE
            f := FIO.Open (filename);
        END (*IF*);
        FIO.IOcheck := temp;
        IF f < MAX(CARDINAL) THEN
            IF create THEN
                FIO.AssignBuffer (f, OutFileBuffer);
            ELSE
                FIO.AssignBuffer (f, InFileBuffer);
            END (*IF*);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
>*)
(*<XDS*)
        IF create THEN
            StreamFile.Open (f, filename, StreamFile.write, status);
        ELSE
            StreamFile.Open (f, filename, StreamFile.read, status);
        END (*IF*);
        RETURN status = StreamFile.opened;
(*>*)
(*<FST|Rowley
        FileSystem.Lookup (f, filename, create);
        RETURN (f.res = FileSystem.done);
>*)
    END OpenFile;

(************************************************************************)

PROCEDURE CloseFile (VAR (*INOUT*) f: File);

    (* Closes file f. *)

    BEGIN
        (*<FST|Rowley FileSystem.Close(f); >*)
        (*<TopSpeed FIO.Close(f); >*)
        (*<XDS*) StreamFile.Close(f); (*>*)
    END CloseFile;

(************************************************************************)

PROCEDURE EndOfFile (VAR (*INOUT*) f: File): BOOLEAN;

    (* Returns TRUE iff the end of file f has been reached.             *)
    (* Remark: in the TopSpeed case the library procedure FIO.EOF is    *)
    (* inadequate for this purpose, because (a) it is only updated      *)
    (* after a read, and (b) there is no feedback about WHICH file has  *)
    (* reached its end.                                                 *)

    (* The ISO libraries have the same fault; but for the present       *)
    (* application it probably doesn't matter.                          *)

    BEGIN
        (*<FST|Rowley RETURN f.eof; >*)
        (*<TopSpeed RETURN FIO.GetPos(f) >= FIO.Size(f); >*)
        (*<XDS*) RETURN IOChan.ReadResult(f) = IOConsts.endOfInput; (*>*)
    END EndOfFile;

(************************************************************************)

PROCEDURE WriteToFile (VAR (*INOUT*) f: File;
                                VAR (*IN*) str: ARRAY OF CHAR);

    (* Writes a text string to file f. *)

    BEGIN
        (*<FST PPTextIO.WriteString (f, str); >*)
        (*<Rowley|XDS*) TextIO.WriteString (f, str); (*>*)
        (*<TopSpeed FIO.WrStr (f, str); >*)
    END WriteToFile;

(************************************************************************)

PROCEDURE TerminateLine (VAR (*INOUT*) f: File);

    (* Writes an end-of-line to file f. *)

    BEGIN
        (*<FST PPTextIO.WriteLn (f); >*)
        (*<Rowley|XDS*) TextIO.WriteLn (f); (*>*)
        (*<TopSpeed FIO.WrLn (f); >*)
    END TerminateLine;

(************************************************************************)

PROCEDURE ReadLine (VAR (*INOUT*) f: File;  VAR (*OUT*) strg: ARRAY OF CHAR);

    (* Reads a line of text from file f. *)

    BEGIN
        (*<FST PPTextIO.ReadString (f,strg); >*)
        (*<Rowley TextIO.ReadString (f,strg);  TextIO.ReadLn (f); >*)
        (*<TopSpeed FIO.RdStr (f,strg); >*)
        (*<XDS*) TextIO.ReadString (f,strg);  TextIO.SkipLine (f); (*>*)
    END ReadLine;

(************************************************************************)

PROCEDURE DeleteFile (name: ARRAY OF CHAR);

    (* Deletes a file, if it exists.  The file must not be open. *)

    (*<XDS*) VAR done: BOOLEAN; (*>*)

    BEGIN
        (*<FST|Rowley
        Directories.Delete(name);
        >*)
        (*<TopSpeed
        IF FIO.Exists(name) THEN FIO.Erase(name); END (*IF*);
        >*)
        (*<XDS*)
        IF FileSys.Exists(name) THEN FileSys.Remove(name, done); END (*IF*);
        (*>*)
    END DeleteFile;

(************************************************************************)

PROCEDURE RenameFile (originalname, newname: ARRAY OF CHAR);

    (* Renames an existing file.  The file should not be open. *)

    (*<XDS*) VAR done: BOOLEAN; (*>*)

    BEGIN
        (*<FST|Rowley Directories.Rename(originalname, newname); >*)
        (*<TopSpeed FIO.Rename(originalname, newname); >*)
        (*<XDS*) FileSys.Rename(originalname, newname, done); (*>*)
    END RenameFile;

(************************************************************************)

BEGIN
    (*<XDS*) argchannel := ProgramArgs.ArgChan(); (*>*)
END PPMisc.

