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

IMPLEMENTATION MODULE GlassTTY;

        (********************************************************)
        (*                                                      *)
        (*           Simple screen output routines.             *)
        (*                                                      *)
        (*  This module handles screen output at a very low     *)
        (*  level, without supplying the advanced features      *)
        (*  which may be found in, for example, module Windows. *)
        (*  It is intended for things like error message        *)
        (*  output, and is designed for compactness rather      *)
        (*  than comprehensiveness.                             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        17 March 1998                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*     This is the VIO version.                         *)
        (*     I still need to explore a few of the VIO calls.  *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16, ADDRESS;

IMPORT OS2;

FROM Keyboard IMPORT
    (* proc *)  NotDetached;

FROM Types IMPORT
    (* proc *)  FarPointer, FarCharPointer;

FROM LowLevel IMPORT
    (* proc *)  LowByte, HighByte, RSB, IANDB,
                SEGMENT, OFFSET, Far, FarCopy, AddOffset, ALLOCATE64;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    HexDigit = CARD8 [0..15];
    LONGCARD = CARDINAL;

(*************************************************************************)

VAR
    (* Screen size. *)

    MaxRowNumber, NoOfColumns: CARDINAL;

    (* CurrentRow is the number of the current screen line.             *)

    CurrentRow: CARDINAL;

    (* CurrentColumn is the number of the current screen column.  The   *)
    (* special case CurrentColumn = NoOfColumns means that we have run  *)
    (* off the end of the current row, and must do a WriteLn or         *)
    (* SetCursor before writing a new character.                        *)

    CurrentColumn: CARDINAL;

    (* Physical cursor position. *)

    ScreenPosition, OldPosition: CARDINAL;

(************************************************************************)
(*                             VIO CALLS                                *)
(************************************************************************)

PROCEDURE PutCode (ch: CHAR);

    VAR status: OS2.APIRET16;
        buffer: ARRAY [0..0] OF CHAR;

    BEGIN
        buffer[0] := ch;
        status := OS2.VioWrtTTY (buffer, 1, 0);
    END PutCode;

(************************************************************************)

PROCEDURE SetCursorPosition (row, col: CARDINAL);

    (* Sets the cursor to the specified row and column.      *)

    VAR NewScreenPos: CARDINAL;
        status: OS2.APIRET16;

    BEGIN
        NewScreenPos := NoOfColumns*row + col;
        status := OS2.VioSetCurPos (row, col, 0);
        ScreenPosition := NewScreenPos;
        CurrentRow := row;  CurrentColumn := col;
    END SetCursorPosition;

(************************************************************************)
(*                      SCROLLING AND CURSOR MOVEMENTS                  *)
(************************************************************************)

PROCEDURE SetCursor (row, column: CARDINAL);

    (* Moves the screen cursor to the specified row and column.         *)

    BEGIN
        SetCursorPosition (row, column);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor;

    (* Remembers the current cursor position, for use by a subsequent   *)
    (* call to RestoreCursor.  Note that nesting is not supported, i.e. *)
    (* a call to SaveCursor destroys the information saved by any       *)
    (* earlier call to SaveCursor.                                      *)

    BEGIN
        OldPosition := ScreenPosition;
    END SaveCursor;

(************************************************************************)

PROCEDURE RestoreCursor;

    (* Sets the cursor back to where it was at the time of the last     *)
    (* call to SaveCursor.                                              *)

    BEGIN
        CurrentRow := OldPosition DIV NoOfColumns;
        CurrentColumn := OldPosition MOD NoOfColumns;
        SetCursorPosition (CurrentRow, CurrentColumn);
    END RestoreCursor;

(************************************************************************)

PROCEDURE WriteLn;

    (* Moves the screen cursor to the beginning of the next line,       *)
    (* scrolling if necessary.                                          *)

    BEGIN
        PutCode (CHR(13));  PutCode (CHR(10));
        DEC (ScreenPosition, CurrentColumn);
        CurrentColumn := 0;
        IF CurrentRow < MaxRowNumber THEN
            INC (CurrentRow);  INC (ScreenPosition, NoOfColumns);
        END (*IF*);
    END WriteLn;

(************************************************************************)
(*                      CHARACTER AND STRING OUTPUT                     *)
(************************************************************************)

PROCEDURE WriteChar (ch: CHAR);

    (* Writes one character, and updates the cursor.  This procedure    *)
    (* does not recognise the concept of a control character.  Every    *)
    (* possible value of ch produces something readable on the screen.  *)
    (* If we have run off the end of the current row, wraps to a        *)
    (* new line.                                                        *)

    BEGIN
        IF CurrentColumn = NoOfColumns THEN
            WriteLn;
        END (*IF*);
        PutCode (ch);
        INC (CurrentColumn);
        INC (ScreenPosition);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteString (text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by  *)
    (* the end of the array.                                            *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF ORD (text[j]) = 0 THEN EXIT(*LOOP*)  END (*IF*);
            WriteChar (text[j]);  INC (j);
            IF j > HIGH (text) THEN EXIT(*LOOP*)  END (*IF*);
        END (*LOOP*);
    END WriteString;

(************************************************************************)
(*                      NUMERIC OUTPUT (HEXADECIMAL)                    *)
(************************************************************************)

PROCEDURE WriteHexDigit (number: HexDigit);

    (* Writes a one-digit hexadecimal number.   *)

    BEGIN
        IF number < 10 THEN
            WriteChar (CHR(ORD("0")+ORD(number)))
        ELSE
            WriteChar (CHR(ORD("A")+ORD(number)-10))
        END (*IF*);
    END WriteHexDigit;

(*************************************************************************)

PROCEDURE WriteHexByte (number: CARD8);

    (* Writes its argument as a two-digit hexadecimal number.   *)

    BEGIN

        (* The obscure function names from LowLevel are:        *)
        (*      RSB = right shift       IANDB = logical AND     *)

        WriteHexDigit (RSB(number,4));
        WriteHexDigit (IANDB(number,15));
    END WriteHexByte;

(*************************************************************************)

PROCEDURE WriteHexWord (number: CARDINAL);

    (* Writes its argument as a four-digit hexadecimal number.  *)

    BEGIN
        WriteHexByte (HighByte(number));
        WriteHexByte (LowByte(number));
    END WriteHexWord;

(************************************************************************)

PROCEDURE WriteAddress (addr: ADDRESS);

    (* Writes a segmented address to the screen.        *)

    BEGIN
        WriteHexWord (SEGMENT(addr));  WriteChar (":");
        WriteHexWord (OFFSET(addr));
    END WriteAddress;

(************************************************************************)
(*                      NUMERIC OUTPUT (DECIMAL)                        *)
(************************************************************************)

PROCEDURE WriteLongCard (number: LONGCARD);

    (* Writes a number to the screen.   *)

    VAR remainder: CARDINAL;

    BEGIN
        IF number > 9 THEN
            WriteLongCard (number DIV 10);
        END (*IF*);
        remainder := number MOD 10;
        WriteChar (CHR(remainder + ORD("0")));
    END WriteLongCard;

(************************************************************************)

PROCEDURE WriteCard (number: CARDINAL);

    (* Writes a number to the screen.   *)

    BEGIN
        WriteLongCard (VAL(LONGCARD,number));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteInt (number: INTEGER);

    (* Writes a number to the screen.   *)

    BEGIN
        IF number < 0 THEN
            WriteChar ('-');  number := -number;
        END (*IF*);
        WriteCard (VAL(CARDINAL,number));
    END WriteInt;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

PROCEDURE Initialise;

    VAR row, col: CARD16;
        vioModeInfoPtr: POINTER TO OS2.VIOMODEINFO;

    BEGIN
        (* Work out the screen size. *)

        MaxRowNumber := 24;
        NoOfColumns := 80;
        row := 0;  col := 0;
        IF NotDetached() THEN
            ALLOCATE64 (vioModeInfoPtr, SIZE(OS2.VIOMODEINFO));
            vioModeInfoPtr^.cb := SIZE (OS2.VIOMODEINFO);
            OS2.VioGetMode (vioModeInfoPtr^, 0);
            MaxRowNumber := vioModeInfoPtr^.row - 1;
            NoOfColumns := vioModeInfoPtr^.col;
            OS2.VioGetCurPos (row, col, 0);
            DISPOSE (vioModeInfoPtr);
        END (*IF*);

        (* Set the current row, column, and screen position. *)

        CurrentRow := row;  CurrentColumn := col;
        ScreenPosition := NoOfColumns*row + col;
        OldPosition := ScreenPosition;
    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END GlassTTY.

