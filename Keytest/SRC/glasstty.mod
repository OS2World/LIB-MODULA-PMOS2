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
        (*  Last edited:        19 December 1997                *)
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
                SEGMENT, OFFSET, Far, FarCopy, AddOffset;

(************************************************************************)

CONST
    BytesPerChar = 2;                   (* # bytes/char in video buffer *)
    NoOfColumns = 80;
    BytesPerRow = NoOfColumns*BytesPerChar;  (* bytes per screen row    *)
    bottomrow = 24;                     (* row # of last screen row     *)

TYPE
    RowRange = [0..bottomrow];
    HexDigit = CARD8 [0..15];
    LONGCARD = CARDINAL;

(*************************************************************************)

VAR
    (* BlankRow is set up by the initialisation code as a row of space  *)
    (* characters.  (But it's not used in this version, it's just there *)
    (* for possible future expansion.)                                  *)

    BlankRow: ARRAY [0..BytesPerRow-1] OF CHAR;

    (* CurrentRow is the number of the current screen line.             *)

    CurrentRow: RowRange;

    (* CurrentColumn is the number of the current screen column.  The   *)
    (* special case CurrentColumn = NoOfColumns means that we have run  *)
    (* off the end of the current row, and must do a WriteLn or         *)
    (* SetCursor before writing a new character.                        *)

    CurrentColumn: [0..NoOfColumns];

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
        NewScreenPos := BytesPerRow*row + BytesPerChar*col;
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
        CurrentRow := OldPosition DIV BytesPerRow;
        CurrentColumn := (OldPosition MOD BytesPerRow) DIV BytesPerChar;
        SetCursorPosition (CurrentRow, CurrentColumn);
    END RestoreCursor;

(************************************************************************)

PROCEDURE WriteLn;

    (* Moves the screen cursor to the beginning of the next line,       *)
    (* scrolling if necessary.                                          *)

    BEGIN
        PutCode (CHR(13));  PutCode (CHR(10));
        DEC (ScreenPosition, BytesPerChar*CurrentColumn);
        CurrentColumn := 0;
        IF CurrentRow < bottomrow THEN
            INC (CurrentRow);  INC (ScreenPosition, BytesPerRow);
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
        INC (ScreenPosition, BytesPerChar);
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

    VAR j: CARDINAL;  row, col: CARD16;

    BEGIN
        FOR j := 0 TO BytesPerRow-2 BY 2 DO
            BlankRow[j] := " ";  BlankRow[j+1] := CHR(07H);
        END (*FOR*);
        IF NotDetached() THEN
            OS2.VioGetCurPos (row, col, 0);
        ELSE
            row := 0;  col := 0;
        END (*IF*);
        CurrentRow := row;  CurrentColumn := col;
        ScreenPosition := BytesPerRow*row + BytesPerChar*col;
        OldPosition := ScreenPosition;
    END Initialise;

(************************************************************************)

BEGIN
    Initialise;
END GlassTTY.

