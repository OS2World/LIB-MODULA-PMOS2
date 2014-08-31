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

IMPLEMENTATION MODULE MLE;

        (********************************************************)
        (*                                                      *)
        (*                Simple multiline editor               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        3 August 2005                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*    Faults:                                           *)
        (*      1. (Fixed)                                      *)
        (*      2. (Fixed)                                      *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  SetCursor, SaveCursor, CursorLeft, CursorRight, GetKey,
                ColourSwap, EraseLine, WriteChar, WriteString,
                ReadCharWithoutEcho, SetColours,
                NewScrollingRegion, ResetScrollingRegion,
                ScrollUp, ScrollDown;

FROM Keyboard IMPORT
    (* proc *)  PutBack;

FROM SoundEffects IMPORT
    (* proc *)  Beep;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    (*  win          the screen window                                  *)
    (*  r0,c0        top left of edit area relative to window           *)
    (*  row,col      current cursor position relative to edit area      *)
    (*  place        current position within the text array             *)
    (*  offtop       the number of characters that have scrolled off    *)
    (*               the top of the display                             *)
    (*  NumberOfRows number of rows that will fit in the editing area   *)
    (*  CharsPerRow  number of characters per screen row                *)
    (*  NumChars     number of characters in the edit string            *)
    (*  MaxChars     upper bound on NumChars                            *)
    (*  InsertMode   TRUE if we insert rather than overwrite            *)

    MLEstate = POINTER TO
                   RECORD
                       win: Window;
                       r0, c0, row, col: CARDINAL;
                       place: CARDINAL;
                       offtop: CARDINAL;
                       NumberOfRows: CARDINAL;
                       CharsPerRow: CARDINAL;
                       NumChars, MaxChars: CARDINAL;
                       InsertMode: BOOLEAN;
                   END (*RECORD*);

(************************************************************************)

PROCEDURE CreateMLEField (w: Window;  firstrow, firstcol: CARDINAL;
                                          rows, cols: CARDINAL): MLEstate;

    VAR result: MLEstate;

    BEGIN
        NEW (result);
        WITH result^ DO
            win := w;
            r0 := firstrow;
            c0 := firstcol;
            row := 0;
            col := 0;
            place := 0;
            offtop := 0;
            NumberOfRows := rows;
            CharsPerRow := cols;
            NumChars := 0;
            MaxChars := 0;
            InsertMode := FALSE;
        END (*WITH*);
        RETURN result;
    END CreateMLEField;

(************************************************************************)

PROCEDURE DiscardMLEField (VAR (*INOUT*) state: MLEstate);

    BEGIN
        DISPOSE (state);
    END DiscardMLEField;

(************************************************************************)

PROCEDURE WriteARow (MW: MLEstate;  rowno: CARDINAL;
                     VAR (*IN*) text: ARRAY OF CHAR;
                     VAR (*OUT*) lastline: BOOLEAN);

    (* Writes row rowno of the screen display.   *)

    VAR j, start, nextstart, bound: CARDINAL;

    BEGIN
        lastline := FALSE;
        start := MW^.offtop + rowno*MW^.CharsPerRow;
        nextstart := start + MW^.CharsPerRow;
        bound := nextstart;
        IF bound > MW^.NumChars THEN
            bound := MW^.NumChars;
        END (*IF*);
        j := start;
        SetCursor (MW^.win, MW^.r0 + rowno, MW^.c0);
        WHILE j < bound DO
            WriteChar (MW^.win, text[j]);  INC (j);
        END (*WHILE*);
        IF j < nextstart THEN
            EraseLine (MW^.win, 1);
            lastline := TRUE;
        END (*IF*);
        ColourSwap (MW^.win, MW^.r0 + rowno, MW^.c0, MW^.CharsPerRow);
    END WriteARow;

(************************************************************************)

PROCEDURE RefreshFrom (MW: MLEstate;  r: CARDINAL;
                       VAR (*IN*) text: ARRAY OF CHAR);

    (* Refreshes the screen display from row r onwards. *)

    VAR j: CARDINAL;  lastline: BOOLEAN;

    BEGIN
        lastline := FALSE;
        MW^.NumChars := LENGTH(text);
        FOR j := r TO MW^.NumberOfRows-1 DO
            IF NOT lastline THEN
                WriteARow (MW, j, text, lastline);
            END (*IF*);
        END (*FOR*);
    END RefreshFrom;

(************************************************************************)

PROCEDURE RefreshCurrent (MW: MLEstate;  VAR (*IN*) text: ARRAY OF CHAR);

    (* Refreshes the screen display of the current row, and also of     *)
    (* all following rows in the case where MW^.InsertMode is TRUE.     *)

    VAR lastline: BOOLEAN;

    BEGIN
        IF MW^.InsertMode THEN
            RefreshFrom (MW, MW^.row, text);
        ELSE
            WriteARow (MW, MW^.row, text, lastline);
        END (*IF*);
    END RefreshCurrent;

(************************************************************************)

PROCEDURE WriteTopMore (MW: MLEstate);

    (* Writes "more" above the editing area. *)

    VAR r, c: CARDINAL;

    BEGIN
        r := MW^.r0 - 1;
        c := MW^.c0 + MW^.CharsPerRow - 6;
        SetCursor (MW^.win, r, c);
        WriteString (MW^.win, " more ");
        SetColours (MW^.win, r, c, 6, yellow, red);
    END WriteTopMore;

(************************************************************************)

PROCEDURE ClearTopMore (MW: MLEstate);

    (* Removes "more" from above the editing area. *)

    VAR r, c: CARDINAL;

    BEGIN
        r := MW^.r0 - 1;
        c := MW^.c0 + MW^.CharsPerRow - 6;
        SetCursor (MW^.win, r, c);
        WriteString (MW^.win, "      ");
    END ClearTopMore;

(************************************************************************)

PROCEDURE WriteBottomMore (MW: MLEstate);

    (* Writes "more" below the editing area. *)

    VAR r, c: CARDINAL;

    BEGIN
        r := MW^.r0 + MW^.NumberOfRows;
        c := MW^.c0 + MW^.CharsPerRow - 6;
        SetCursor (MW^.win, r, c);
        WriteString (MW^.win, " more ");
        SetColours (MW^.win, r, c, 6, yellow, red);
    END WriteBottomMore;

(************************************************************************)

PROCEDURE ClearBottomMore (MW: MLEstate);

    (* Removes "more" from below the editing area. *)

    VAR r, c: CARDINAL;

    BEGIN
        r := MW^.r0 + MW^.NumberOfRows;
        c := MW^.c0 + MW^.CharsPerRow - 6;
        SetCursor (MW^.win, r, c);
        WriteString (MW^.win, "      ");
    END ClearBottomMore;

(************************************************************************)

PROCEDURE DisplayText (MW: MLEstate;  VAR (*IN*) text: ARRAY OF CHAR);

    (* Refreshes the screen display. *)

    VAR j: CARDINAL;  lastline: BOOLEAN;

    BEGIN
        MW^.NumChars := LENGTH(text);
        FOR j := 0 TO MW^.NumberOfRows-1 DO
            WriteARow (MW, j, text, lastline);
        END (*FOR*);
        ClearTopMore(MW);
        IF MW^.NumChars > MW^.NumberOfRows * MW^.CharsPerRow THEN
            WriteBottomMore(MW);
        ELSE
            ClearBottomMore(MW);
        END (*IF*);
    END DisplayText;

(************************************************************************)

PROCEDURE EditText (MW: MLEstate;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                   MaxCharacters: CARDINAL);

    (* Allows the user to edit the text. *)

    (********************************************************************)

    PROCEDURE UpARow;

        (* The "cursor up" operation. *)

        VAR dummy: BOOLEAN;

        BEGIN
            IF (MW^.row > 0) OR (MW^.offtop > 0) THEN
                IF MW^.row = 0 THEN
                    ScrollDown (MW^.win);
                    DEC (MW^.offtop, MW^.CharsPerRow);
                    IF MW^.NumChars - MW^.offtop
                               > MW^.NumberOfRows * MW^.CharsPerRow THEN
                        WriteBottomMore(MW);
                    END (*IF*);
                    WriteARow (MW, 0, text, dummy);
                    IF MW^.offtop = 0 THEN
                        ClearTopMore(MW);
                    END (*IF*);
                ELSE
                    DEC (MW^.row);
                END (*IF*);
                DEC (MW^.place, MW^.CharsPerRow);
            END (*IF*);
        END UpARow;

    (********************************************************************)

    PROCEDURE DownARow;

        (* The "cursor down" operation. *)

        VAR gap: CARDINAL;  dummy: BOOLEAN;

        BEGIN
            IF (MW^.NumChars DIV MW^.CharsPerRow)
                             > (MW^.place DIV MW^.CharsPerRow) THEN
                gap := MW^.place + MW^.CharsPerRow;
                IF gap > MW^.NumChars THEN
                    DEC (gap, MW^.NumChars);
                ELSE
                    gap := 0;
                END (*IF*);
                IF (MW^.row = MW^.NumberOfRows-1)
                               AND (gap <= MW^.col) THEN
                    ScrollUp (MW^.win);
                    IF MW^.offtop = 0 THEN
                        WriteTopMore(MW);
                    END (*IF*);
                    INC (MW^.offtop, MW^.CharsPerRow);
                    WriteARow (MW, MW^.row, text, dummy);
                ELSIF MW^.row < MW^.NumberOfRows-1 THEN
                    INC (MW^.row);
                END (*IF*);
                INC (MW^.place, MW^.CharsPerRow);
                IF gap > 0 THEN
                    DEC (MW^.col, gap);
                    MW^.place := MW^.NumChars;
                END (*IF*);
                IF MW^.NumChars - MW^.offtop
                           <= MW^.NumberOfRows * MW^.CharsPerRow THEN
                    ClearBottomMore(MW);
                END (*IF*);
            END (*IF*);
        END DownARow;

    (********************************************************************)

    PROCEDURE GotoEndline;

        (* Move to the end of the current row, if possible. *)

        VAR k: CARDINAL;

        BEGIN
            k := MW^.CharsPerRow - MW^.col - 1;
            IF k > 0 THEN
                INC (MW^.place, k);
                MW^.col := MW^.CharsPerRow - 1;
            END (*IF*);
            IF MW^.place > MW^.NumChars THEN
                DEC (MW^.col, MW^.place - MW^.NumChars);
                MW^.place := MW^.NumChars;
            END (*IF*);
        END GotoEndline;

    (********************************************************************)

    PROCEDURE CursorRight;

        (* Move to the next character position. *)

        BEGIN
            IF MW^.place < MW^.NumChars THEN
                IF MW^.col = MW^.CharsPerRow-1 THEN
                    DownARow;
                    IF MW^.col > 0 THEN
                        DEC (MW^.place, MW^.col);
                        MW^.col := 0;
                    END (*IF*);
                ELSE
                    INC (MW^.place);
                    INC (MW^.col);
                END (*IF*);
            END (*IF*);
        END CursorRight;

    (********************************************************************)

    PROCEDURE HandleControlChar(): BOOLEAN;

        (* Called after detecting the CHR(0) which means that a control *)
        (* character has been typed.  Performs the appropriate actions, *)
        (* returns TRUE iff editing is finished.                        *)

        VAR k: CARDINAL;
            ch: CHAR;

        BEGIN
            ch := GetKey (MW^.win);
            IF ch = "K" THEN                            (* cursor left *)
                IF MW^.col = 0 THEN
                    IF MW^.offtop > 0 THEN
                        UpARow;
                        GotoEndline;
                    END (*IF*);
                ELSE
                    DEC (MW^.place);
                    DEC (MW^.col);
                END (*IF*);
            ELSIF ch = "M" THEN                         (* cursor right *)
                CursorRight;
            ELSIF ch = "H" THEN                         (* cursor up *)
                UpARow;
            ELSIF ch = "P" THEN                         (* cursor down *)
                DownARow;
            ELSIF ch = "I" THEN                         (* page up *)
                FOR k := 2 TO MW^.NumberOfRows DO
                    UpARow;
                END (*FOR*);
            ELSIF ch = "Q" THEN                         (* page down *)
                FOR k := 2 TO MW^.NumberOfRows DO
                    DownARow;
                END (*FOR*);
            ELSIF ch = "G" THEN                         (* home *)
                IF MW^.col > 0 THEN
                    DEC (MW^.place, MW^.col);
                    MW^.col := 0;
                END (*IF*);
            ELSIF ch = "O" THEN                         (* end *)
                GotoEndline;
            ELSIF ch = "R" THEN                         (* insert *)
                MW^.InsertMode := NOT MW^.InsertMode;
            ELSIF ch = "S" THEN                         (* delete right *)
                IF MW^.place < MW^.NumChars THEN
                    DEC (MW^.NumChars);
                    IF MW^.NumChars > 0 THEN
                        FOR k := MW^.place TO MW^.NumChars-1 DO
                            text[k] := text[k+1];
                        END (*FOR*);
                    END (*IF*);
                    text[MW^.NumChars] := Nul;
                    RefreshCurrent (MW, text);
                END (*IF*);
            END (*IF*);
            RETURN FALSE;
        END HandleControlChar;

    (********************************************************************)

    CONST Esc = CHR(01BH);  Space = " ";

    VAR k: CARDINAL;
        ch: CHAR;

    BEGIN       (* Body of EditText *)
        WITH MW^ DO
            NewScrollingRegion (win, r0, r0+NumberOfRows-1,
                                     c0, c0+CharsPerRow-1);
            MaxChars := MaxCharacters;
            row := 0;
            col := 0;
            place := 0;
            offtop := 0;
            DisplayText (MW, text);
            (*
            FOR k := 0 TO NumberOfRows-1 DO
                ColourSwap (win, r0+k, c0, CharsPerRow);
            END (*FOR*);
            *)
        END (*WITH*);

        (* Now the main editing loop.   *)

        LOOP
            SetCursor (MW^.win, MW^.row+MW^.r0, MW^.col+MW^.c0);
            ReadCharWithoutEcho (MW^.win, ch);
            IF ORD(ch) = 0 THEN                         (* control char *)
                IF HandleControlChar() THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF (ch = Esc) OR (ORD(ch) = 13) THEN     (* Esc or Return *)
                PutBack(ch);  EXIT(*LOOP*);
            ELSIF ORD(ch) = 8 THEN                      (* delete left *)
                IF MW^.place > 0 THEN
                    IF MW^.col = 0 THEN
                        UpARow;
                        INC (MW^.col, MW^.CharsPerRow);
                        INC (MW^.place, MW^.CharsPerRow);
                    END (*IF*);
                    DEC (MW^.place);
                    DEC (MW^.col);
                    DEC (MW^.NumChars);
                    IF MW^.NumChars > 0 THEN
                        FOR k := MW^.place TO MW^.NumChars-1 DO
                            text[k] := text[k+1];
                        END (*FOR*);
                    END (*IF*);
                    text[MW^.NumChars] := Nul;
                    RefreshCurrent (MW, text);
                END (*IF*);
            ELSIF (MW^.NumChars >= MW^.MaxChars) AND
                      (MW^.InsertMode OR (MW^.place = MW^.NumChars)) THEN
                Beep;                                   (* buffer full *)
            ELSE                                        (* any other char *)
                IF (MW^.InsertMode OR (MW^.place = MW^.NumChars)) THEN
                    FOR k := MW^.NumChars TO MW^.place+1 BY -1 DO
                        text[k] := text[k-1];
                    END (*FOR*);
                    INC (MW^.NumChars);
                    IF MW^.NumChars < MW^.MaxChars THEN
                        text[MW^.NumChars] := Nul;
                    END (*IF*);
                END (*IF*);
                text[MW^.place] := ch;
                RefreshCurrent (MW, text);
                CursorRight;
            END(*IF*);
        END (*LOOP*);
        WITH MW^ DO
            ResetScrollingRegion (MW^.win);
            FOR k := 0 TO NumberOfRows-1 DO
                ColourSwap (win, r0+k, c0, CharsPerRow);
            END (*FOR*);
        END (*WITH*);
    END EditText;

(************************************************************************)

END MLE.

