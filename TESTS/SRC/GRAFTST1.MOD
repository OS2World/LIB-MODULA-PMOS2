MODULE Graftst1;

        (****************************************************************)
        (*                                                              *)
        (*              Test of Graphics module.                        *)
        (*                                                              *)
        (*      NOTE: This program is not intended as a "final          *)
        (*      product" whose code is static.  It's a collection       *)
        (*      of small tests which can be commented or uncommented    *)
        (*      as needed - or modified, as needed - depending on       *)
        (*      what aspect of the screen graphics is being tested      *)
        (*      at the time.                                            *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        8 November 1996                         *)
        (*  Status:             OK                                      *)
        (*                                                              *)
        (****************************************************************)

FROM ScreenGeometry IMPORT
    (* type *)  Rectangle;

FROM Graphics IMPORT
    (* const *) BLorigin,
    (* proc *)  SetMode, SetDefaultMode, PlotDot, PlotLine, PlotRectangle,
                DrawChar, PlotString, GetScreenShape, Fill, ACopy;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM IO IMPORT
    (* proc *)  RdKey;

FROM Timer IMPORT
    (* proc *)  Sleep;

IMPORT GlassTTY;

(************************************************************************)

VAR Xmax, Ymax, MaxColour: CARDINAL;

(************************************************************************)

PROCEDURE Abort(): BOOLEAN;

    (* Waits until a key is struck on the keyboard, returns TRUE if     *)
    (* the key was the Escape key.                                      *)

    CONST Esc = CHR(01BH);

    BEGIN
        RETURN RdKey() = Esc;
    END Abort;

(************************************************************************)

PROCEDURE CentreCross (x, y, colour: CARDINAL);

    (* Plots a cross at position (x,y), using PlotLine. *)

    BEGIN
        PlotLine (x-5,y,x+5,y,colour);
        PlotLine (x,y-5,x,y+5,colour);
    END CentreCross;

(************************************************************************)

PROCEDURE ShortLines (y, colour: CARDINAL);

    (* Plots some short lines, using point graphics.    *)

    VAR j: CARDINAL;

    BEGIN

        FOR j := 0 TO 7 DO
            PlotDot (0,y-j,colour);
        END (*FOR*);
        FOR j := 0 TO 15 DO
            PlotDot (10,y-j,colour);
        END (*FOR*);
        FOR j := 0 TO 31 DO
            PlotDot (20,y-j,colour);
        END (*FOR*);
        FOR j := 0 TO 63 DO
            PlotDot (30,y-j,colour);
        END (*FOR*);
        FOR j := 0 TO 127 DO
            PlotDot (40,y-j,colour);
        END (*FOR*);
(*
        FOR j := 0 TO 255 DO
            PlotDot (50,y-j,colour);
        END (*FOR*);
        FOR j := 0 TO 479 DO
            PlotDot (60,y-j,colour);
        END (*FOR*);
*)
        FOR j := 0 TO 319 DO
            PlotDot (j,y,colour);
        END (*FOR*);
    END ShortLines;

(************************************************************************)

PROCEDURE DiagonalLine (colour: CARDINAL);

    (* Plots a diagonal line, using point graphics.     *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO 99 DO
            PlotDot (2*j,j,colour);
        END (*FOR*);
    END DiagonalLine;

(************************************************************************)

PROCEDURE PointTest (colour: CARDINAL);

    (* Plots a diagonal line, waits a while, and then                   *)
    (* erases the line.  This test provides a quick verification        *)
    (* that something really does appear on the screen, i.e. that a     *)
    (* suitable mode is set and that PlotDot works.                     *)

    BEGIN
        DiagonalLine (colour);
        Sleep (1000);
        DiagonalLine (0);
    END PointTest;

(************************************************************************)

PROCEDURE Diamond (colour: CARDINAL);

    (* Plots a diamond shape, using PlotLine.   *)

    BEGIN
        PlotLine (15,Ymax DIV 2,Xmax DIV 2,15,colour);
        PlotLine (Xmax DIV 2,15,Xmax-15,Ymax DIV 2,colour);
        PlotLine (Xmax-15,Ymax DIV 2,Xmax DIV 2,Ymax-15,colour);
        PlotLine (Xmax DIV 2,Ymax-15,15,Ymax DIV 2,colour);
    END Diamond;

(************************************************************************)

PROCEDURE LineTest (colour: CARDINAL);

    (* Plots a diamond shape, then erases it.  This is a test that      *)
    (* PlotLine works.                                                  *)

    BEGIN
        Diamond (colour);
        Sleep (600);
        Diamond (0);
    END LineTest;

(************************************************************************)

PROCEDURE TextTest (colour: CARDINAL);

    (* Checks whether DrawChar works.   *)

    VAR dummy: BOOLEAN;

    BEGIN
        DrawChar ("F", 100, 50, colour);
        dummy := Abort();
    END TextTest;

(************************************************************************)

PROCEDURE FillTest (colour: CARDINAL);

    (* Checks whether Fill works.       *)

    BEGIN
        Fill (Xmax DIV 2 - 15, Ymax DIV 2 - 15,
                Xmax DIV 2 + 15, Ymax DIV 2 + 15, colour);
    END FillTest;

(************************************************************************)

PROCEDURE CopyTest (colour1, colour2: CARDINAL);

    (* Fills a region, and then copies most of it. *)

    CONST
        width = 31;  height = 25;
        dx = 0;  dy = 33;

    VAR R: Rectangle;
        x0, y0, x1, y1, temp: CARDINAL;

    BEGIN
        x0 := 7*Xmax DIV 8;  y0 := Ymax DIV 8;
        x1 := x0+width-1;  y1 := y0+height-1;
        Fill (x0+1, y0+1, x1-1, y1-1, colour1);
        Fill (x0+5, y0+5, x1-5, y1-5, colour2);
        IF NOT BLorigin THEN
            temp := y0;  y0 := y1;  y1 := temp;
        END (*IF*);
        WITH R DO
            left := x0;  right := x1;
            bottom := y0;  top := y1;
        END (*WITH*);
        PlotRectangle (R, colour2);
        WITH R DO
            DEC(left,2);  INC(right,2);
            IF BLorigin THEN
                DEC(bottom,2);  INC(top,2);
            ELSE
                INC(bottom,2);  DEC(top,2);
            END (*IF*);
        END (*WITH*);

        PlotRectangle (R, colour2);
        ACopy (x0, y1, width, height, dx, dy);

    END CopyTest;

(************************************************************************)

PROCEDURE ColourDisplay (MaxColour: CARDINAL);

    (* This is useful mainly for the high-colour modes, to give a feel  *)
    (* for what the colour range is.                                    *)

    VAR x, y: CARDINAL;  c: CARDINAL;

    BEGIN
        x := 0;  y := Ymax;  c := 0;
        LOOP
            PlotDot (x, y, c);
            IF x < Xmax THEN INC(x);
            ELSIF y = 0 THEN EXIT(*LOOP*)
            ELSE
                x := 0;  DEC (y);
            END (*IF*);
            IF c = MaxColour THEN c := 0
            ELSE INC(c)
            END (*IF*);
        END (*LOOP*);
    END ColourDisplay;

(************************************************************************)

PROCEDURE Title (mode, messx, messy: CARDINAL; colour: CARDINAL);

    (* Writes a title message for the given mode.  The last three       *)
    (* parameters say where on the screen to put the message and what   *)
    (* colour to write it in.                                           *)

    VAR Message: ARRAY [0..25] OF CHAR;
        StringBuffer: ARRAY [0..4] OF CHAR;
        k: [0..4];  count: [0..26];

    BEGIN
        Message := "MODE xxx   XXXXxYYYYxCCCCC";
        count := 5;
        CardinalToString (mode, StringBuffer, 3);
        FOR k := 0 TO 2 DO
            IF StringBuffer[k] <> " " THEN
                Message[count] := StringBuffer[k];  INC (count);
            END (*IF*);
        END (*FOR*);
        Message[count] := " ";  INC (count);
        Message[count] := " ";  INC (count);
        CardinalToString (Xmax+1, StringBuffer, 4);
        FOR k := 0 TO 3 DO
            IF StringBuffer[k] <> " " THEN
                Message[count] := StringBuffer[k];  INC (count);
            END (*IF*);
        END (*FOR*);
        Message[count] := "x";  INC (count);
        CardinalToString (Ymax+1, StringBuffer, 4);
        FOR k := 0 TO 3 DO
            IF StringBuffer[k] <> " " THEN
                Message[count] := StringBuffer[k];  INC (count);
            END (*IF*);
        END (*FOR*);
        Message[count] := "x";  INC (count);
        CardinalToString (MaxColour+1, StringBuffer, 5);
        FOR k := 0 TO 4 DO
            IF StringBuffer[k] <> " " THEN
                Message[count] := StringBuffer[k];  INC (count);
            END (*IF*);
        END (*FOR*);
        PlotString (Message, messx, messy, count, colour);
    END Title;

(************************************************************************)

PROCEDURE TestMode (m: CARDINAL): BOOLEAN;

    (* Runs us through a sequence of tests on mode m.   *)

    VAR colour1, colour2, CharHeight: CARDINAL;

    BEGIN

        SetMode (3, TRUE);
        GlassTTY.WriteLn;
        GlassTTY.WriteString ("Press any key to test mode ");
        GlassTTY.WriteCard (m);
        IF Abort() THEN RETURN TRUE END(*IF*);

        SetMode (m, TRUE);
        GetScreenShape (Xmax, Ymax, MaxColour, CharHeight);
        colour1 := 2;  colour2 := 1;
        IF MaxColour = 1 THEN
            colour1 := 1;  colour2 := 0;
        ELSIF MaxColour > 255 THEN
            colour1 := MaxColour DIV 2;  colour2 := MaxColour DIV 8;
        END (*IF*);
        Title (m, 30, 15, colour1);
        Title (m, Xmax-210, Ymax-15, colour1);
        ShortLines (Ymax, colour1);
        FillTest (colour1);
        CentreCross (Xmax DIV 2, Ymax DIV 2, 0);
        PointTest (colour1);
        CopyTest (colour1, colour2);
        LineTest (colour1);

        (* TextTest (colour1); *)
        (*ColourDisplay (MaxColour);*)

        RETURN Abort();

    END TestMode;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Runs us through a sequence of tests.     *)


    TYPE ModeSet = SET OF [0..511];

    CONST
        NonGraphicsModes = ModeSet {0..3, 7..12, 264..268};
        BasicModes = ModeSet {4..6, 13..19};
        WorkingVesaModes = ModeSet {256, 257, 259, 272, 273, 275, 276};
        WorkingModes = BasicModes + WorkingVesaModes;
        BadSynchOnTrident = ModeSet {106,258,260};
        ModesWhichMyTridentWontSupport = ModeSet {261,269,270,278,279,281,
                                                282,354};
        ExtraModes = ModeSet {368,369};

        TridentVesaModes = WorkingVesaModes + BadSynchOnTrident + ExtraModes;
        TridentModes = BasicModes + TridentVesaModes;

        ATIVesaModes = ModeSet {106, 256..259, 260};
        ATIExtraModes = ModeSet {83..85, 97..99};
        ATIFailingModes = ModeSet {99, 259};
        ATIModes = BasicModes + ATIExtraModes + ATIVesaModes;

        S3VesaModes = ModeSet {257..261, 272..274, 276, 279};

        (* The list below shows modes which have been dropped from the  *)
        (* tests because they have been found to duplicate the function *)
        (* of modes already in one of the lists above.  (This is for    *)
        (* my Trident adaptor.)                                         *)

        Duplicates = ModeSet {348..351, 362, 372..375};

        Modes16ormore = ModeSet {269..283};
        Modes24 = ModeSet {271, 274, 277, 280, 283};
        Modes16 = Modes16ormore - Modes24;
        ModesToTest = BasicModes;

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j IN ModesToTest) THEN
                IF TestMode(j) THEN EXIT(*LOOP*)
                END(*IF*);
            END (*IF*);
            IF j = 511 THEN EXIT(*LOOP*) END(*IF*);
            INC (j);
        END (*FOR*);
    END RunTheTest;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    RunTheTest;
END Graftst1.

