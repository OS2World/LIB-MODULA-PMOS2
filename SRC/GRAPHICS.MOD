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

IMPLEMENTATION MODULE Graphics;

        (****************************************************************)
        (*                                                              *)
        (*                   Screen graphics output                     *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        8 November 1996                         *)
        (*  Status:             Mostly working                          *)
        (*                      Some functions still missing            *)
        (*                                                              *)
        (*      The procedures in this module assume that the caller    *)
        (*      has control of the entire graphics output region.       *)
        (*      For multi-window graphics, see module GWindows.         *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS, CARD8;

FROM ScreenGeometry IMPORT
    (* type *)  Rectangle;

IMPORT Graph;

FROM Strings IMPORT
    (* proc *)  Extract;

(************************************************************************)

VAR
    GraphicsScreenOpen: BOOLEAN;

    (* Current graphics mode is XM x YM x NumColours. *)

    XM, YM, YM1, NumColours: CARDINAL;

(************************************************************************)

PROCEDURE SetMode (newmode: CARDINAL;  ClearScreen: BOOLEAN);

    (* Sets the video mode. *)

    BEGIN
        IF GraphicsScreenOpen THEN Graph.Exit END(*IF*);

        CASE newmode OF
           |  4, 5, 13, 19, 269, 270, 271:
                    XM := 320;  YM := 200;
           |  6, 14:
                    XM := 640;  YM := 200;
           |  15, 16:
                    XM := 640;  YM := 350;
           |  97, 256:
                    XM := 640;  YM := 400;
           |  17, 18, 98, 257, 272, 273, 274:
                    XM := 640;  YM := 480;
           |  83, 84, 99, 106, 258, 259, 275, 276, 277:
                    XM := 800;  YM := 600;
           |  85, 100, 101, 260, 261, 278, 279, 280:
                    XM := 1024;  YM := 768;
           |  262, 263, 281, 282, 283:
                    XM := 1280;  YM := 1024;
        ELSE
                XM := 320;  YM := 200;
        END (*CASE*);
        YM1 := YM - 1;

        CASE newmode OF
           |  5, 6, 15, 17:
                    NumColours := 2;
           |  4:
                    NumColours := 4;
           |  13, 14, 16, 18, 83, 84, 85, 101, 106, 258, 260, 262:
                    NumColours := 16;
           |  19, 97, 98, 99, 100, 256, 257, 259, 261, 263:
                    NumColours := 256;
           |  269, 272, 275, 278, 281:
                    NumColours := 32768;
           |  270, 273, 276, 279, 282:
                    NumColours := 65536;
           |  271, 274, 277, 280, 283:
                    NumColours := 1000000H;
        ELSE
                    NumColours := 16;
        END (*CASE*);

        GraphicsScreenOpen := Graph.Init (10, 10, XM, YM);
        IF ClearScreen THEN Graph.ClearScreen (0) END(*IF*);

    END SetMode;

(************************************************************************)

PROCEDURE SetDefaultMode;

    (* Sets the video mode to (our opinion of) the "best" graphics mode *)
    (* supported by the hardware.                                       *)

    BEGIN
        SetMode (18, TRUE);
    END SetDefaultMode;

(************************************************************************)

PROCEDURE GraphicsOff (ClearScreen: BOOLEAN);

    (* Sets the video mode to a default text mode. *)

    BEGIN
        SetMode (7, TRUE);
    END GraphicsOff;

(************************************************************************)

PROCEDURE GetScreenShape (VAR (*OUT*) xmax, ymax: CARDINAL;
                                VAR (*OUT*) maxcolour: ColourType;
                                VAR (*OUT*) CharHeight: CARDINAL);

    (* Returns the maximum values permitted by the current mode for     *)
    (* x, y, and colour; and the number of rows in a character.         *)

    BEGIN
        xmax := XM - 1;  ymax := YM1;
        maxcolour := NumColours - 1;
        CharHeight := 16;
    END GetScreenShape;

(************************************************************************)

PROCEDURE SetFont (height, width: CARDINAL;  TablePtr: ADDRESS);

    (* Specifies the font that will be used from now on (until the      *)
    (* next mode setting) for drawing characters.  The first two        *)
    (* parameters are the character size, and TablePtr points to the    *)
    (* bitmap that defines the font.  This procedure is normally        *)
    (* optional, since SetMode sets up a default font.                  *)

    (* Implementation restriction: in the present version the 'width'   *)
    (* parameter is ignored, and all characters are assumed to be       *)
    (* eight pixels wide.                                               *)

    BEGIN
        (* NOT YET IMPLEMENTED *)
    END SetFont;

(************************************************************************)

PROCEDURE SetPaletteColour (Palette_Index, Red, Green, Blue: CARD8);

    (* Sets the colour for one palette register.  Applicable only to    *)
    (* VGA or better.  The three colour codes are 6-bit numbers.        *)

    VAR dummy: CARDINAL;

    BEGIN
        dummy := Graph.RemapPalette (Palette_Index, Red + 32*Green + 1024*Blue);
    END SetPaletteColour;

(************************************************************************)

PROCEDURE PlotDot (x, y: CARDINAL;  colour: ColourType);

    (* Writes a dot at screen position (x, y).  *)

    BEGIN
        IF BLorigin THEN
            Graph.Plot (x, YM1-y, colour);
        ELSE
            Graph.Plot (x, y, colour);
        END (*IF*);
    END PlotDot;

(************************************************************************)

PROCEDURE PlotMark (x, y: CARDINAL;
                        colour: ColourType;  pointtype: CARDINAL);

    (* Writes a mark at screen position (x, y).  Currently, the options *)
    (* for pointtype are:                                               *)
    (*          0       dot                                             *)
    (*          1       X                                               *)
    (*          2       box                                             *)

    BEGIN
        CASE pointtype OF
            1:  PlotLine (x-1,y-1,x+1,y+1, colour);     (*  X   *)
                PlotLine (x+1,y-1,x-1,y+1, colour);
          |
            2:  PlotLine (x-2,y-1,x+2,y-1, colour);     (* box  *)
                PlotLine (x+2,y-1,x+2,y+1, colour);
                PlotLine (x+2,y+1,x-2,y+1, colour);
                PlotLine (x-2,y+1,x-2,y-1, colour);
          |
            ELSE
                PlotDot (x, y, colour);                 (* point *)
        END (*CASE*);
    END PlotMark;

(************************************************************************)

PROCEDURE PlotLine (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* Plots a straight line from (x0,y0) to (x1,y1).  It is the        *)
    (* caller's responsibility to ensure that the coordinates are in    *)
    (* range for the current video mode.                                *)

    BEGIN
        IF BLorigin THEN
            Graph.Line(x0, YM1-y0, x1, YM1-y1, colour);
        ELSE
            Graph.Line(x0, y0, x1, y1, colour);
        END (*IF*);
    END PlotLine;

(************************************************************************)

PROCEDURE PlotRectangle (R: Rectangle;  colour: ColourType);

    (* Plots a rectangle, with clipping if necessary to keep the        *)
    (* points within the screen boundary.                               *)

    BEGIN
        WITH R DO
            IF BLorigin THEN
                Graph.Rectangle (left, YM1-VAL(CARDINAL,top), right,
                                YM1-VAL(CARDINAL,bottom), colour, FALSE);
            ELSE
                Graph.Rectangle (left, top, right, bottom, colour, FALSE);
            END (*IF*);
        END (*WITH*);
    END PlotRectangle;

(************************************************************************)

PROCEDURE ClippedLine (x0, y0, x1, y1: CARDINAL;  colour: ColourType;
                        left, right, ymin, ymax: CARDINAL);

    (* Like PlotLine, but plots only that part of the line which lies   *)
    (* in the rectangle (left <= x <= right), (ymin <= y <= ymax).      *)
    (* The caller is expected to ensure, by appropriate definition of   *)
    (* the rectangle, that all plotted points are in range for the      *)
    (* current video mode.                                              *)

    BEGIN
        IF BLorigin THEN
            Graph.SetClipRgn (left, YM1-ymax, right, YM1-ymin);
            Graph.Line (x0, YM1-y0, x1, YM1-y1, colour);
        ELSE
            Graph.SetClipRgn (left, ymin, right, ymax+1);
            Graph.Line (x0, y0, x1, y1, colour);
        END (*IF*);
        Graph.CancelClipRgn ();
    END ClippedLine;

(************************************************************************)

PROCEDURE Fill (x0, y0, x1, y1: CARDINAL;  colour: ColourType);

    (* Fills a rectangle with the indicated colour.  The rectangle is   *)
    (* specified by giving two opposite corners (x0,y0) and (x1,y1).    *)

    BEGIN
        IF BLorigin THEN
            Graph.Rectangle (x0, YM1-y0, x1, YM1-y1, colour, TRUE);
        ELSE
            Graph.Rectangle (x0, y0, x1, y1, colour, TRUE);
        END (*IF*);
    END Fill;

(************************************************************************)

PROCEDURE ACopy (xs, ys, width, height: CARDINAL;  dx, dy: INTEGER);

    (* Copies a rectangular region by an offset (dx, dy).  The pair     *)
    (* (xs,ys) gives the coordinates of the top left of the source      *)
    (* rectangle.  Restrictions: this procedure is restricted to the    *)
    (* case where distance to move the data is an integral number of    *)
    (* bytes (i.e. if you want it to work for all modes then dx should  *)
    (* be a multiple of 8); and in the case where the source and        *)
    (* destination rectangles overlap then the move has to be upwards   *)
    (* on the screen.  Thus we do not have a completely general "block  *)
    (* copy" operation, but we do have something sufficient to support  *)
    (* "scroll up" and similar operations.                              *)

    VAR handle: Graph.HBITMAP;

    BEGIN
        IF BLorigin THEN
            ys := YM1 - ys;  dy := -dy;
        END (*IF*);
        Graph.GetImage (xs, ys, xs+width-1, ys+height-1, handle);
        Graph.PutImage (VAL(INTEGER,xs)+dx, VAL(INTEGER,ys)+dy, handle, Graph._GPSET);
        Graph.DelImage (handle);
    END ACopy;

(************************************************************************)

PROCEDURE DrawChar (ch: CHAR;  x, y: CARDINAL;  colour: ColourType);

    (* Draws the single character ch.  The coordinates (x,y) are the    *)
    (* location of the bottom left of the character.                    *)

    VAR buffer: ARRAY [0..0] OF CHAR;

    BEGIN
        buffer[0] := ch;
        IF BLorigin THEN
            Graph.RawOutText (x, YM1-y, colour, buffer);
        ELSE
            Graph.RawOutText (x, y, colour, buffer);
        END (*IF*);
    END DrawChar;

(************************************************************************)

PROCEDURE PlotString (VAR (*IN*) text: ARRAY OF CHAR;
                        x, y, length: CARDINAL;  colour: ColourType);

    (* Draws a string of "length" characters starting at location (x,y) *)
    (* It is the caller's responsibility to ensure that the string will *)
    (* not run off the screen edges.                                    *)

    <* m2extensions+ *>
    <* storage+ *>

    VAR bufptr: POINTER TO ARRAY OF CHAR;

    BEGIN
        NEW (bufptr, length);
        Extract (text, 0, length, bufptr^);
        IF BLorigin THEN
            Graph.RawOutText (x, YM-1-y, colour, bufptr^);
        ELSE
            Graph.RawOutText (x, y, colour, bufptr^);
        END (*IF*);
        DISPOSE (bufptr);
    END PlotString;

    <* m2extensions- *>
    <* storage- *>

(************************************************************************)

PROCEDURE ClippedString (VAR (*IN*) text: ARRAY OF CHAR;
                        x, y, length: CARDINAL;  colour: ColourType;
                        left, right, ymin, ymax: CARDINAL);

    (* Like PlotString, but excludes any points which fall outside the  *)
    (* clip rectangle defined by (left,right,ymin,ymax).                *)

    BEGIN
        IF BLorigin THEN
            Graph.SetClipRgn (left, YM1-ymax, right, YM1-ymin);
        ELSE
            Graph.SetClipRgn (left, ymin, right, ymax);
        END (*IF*);
        PlotString (text, x, y, length, colour);
        Graph.CancelClipRgn();
    END ClippedString;

(************************************************************************)

PROCEDURE PlotStringUp (VAR (*IN*) text: ARRAY OF CHAR;
                        x, y, length: CARDINAL;  colour: ColourType);

    (* Like PlotString, but with text written in the +Y direction       *)

    BEGIN
        (* NOT YET IMPLEMENTED *)
    END PlotStringUp;

(************************************************************************)

PROCEDURE ClippedUpString (VAR (*IN*) text: ARRAY OF CHAR;
                        x, y, length: CARDINAL;  colour: ColourType;
                        left, right, ymin, ymax: CARDINAL);

    (* Like ClippedString, but with text written in the +Y direction.   *)

    BEGIN
        IF BLorigin THEN
            Graph.SetClipRgn (left, YM1-ymax, right, YM1-ymin);
        ELSE
            Graph.SetClipRgn (left, ymin, right, ymax);
        END (*IF*);
        PlotStringUp (text, x, y, length, colour);
        Graph.CancelClipRgn();
    END ClippedUpString;

(************************************************************************)

BEGIN
    GraphicsScreenOpen := FALSE;
    XM := 320;  YM := 200;  NumColours := 16;
FINALLY
    IF GraphicsScreenOpen THEN
        Graph.Exit;
    END (*IF*);
END Graphics.

