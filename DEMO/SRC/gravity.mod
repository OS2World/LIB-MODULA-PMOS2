MODULE Gravity;

        (********************************************************)
        (*                                                      *)
        (*              Graphics demo, just for fun             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        7 March 2005                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*     Motion of bodies subject to their mutual         *)
        (*              gravitational attraction                *)
        (*                                                      *)
        (********************************************************)

FROM Graphics IMPORT
    (* type *)  ColourType,
    (* proc *)  GetScreenShape, SetMode, PlotDot;

FROM IO IMPORT
    (* proc *)  KeyPressed;

FROM Random IMPORT
    (* proc *)  RANDOM;

FROM MATHLIB IMPORT
    (* proc *)  Sqrt;

FROM Timer IMPORT
    (* proc *)  Sleep;

(************************************************************************)

CONST
    (* Video mode.  Chose any graphics mode. *)

    VideoMode = 276;

    (* Size of the swarm. *)

    MaxBodyNumber = 50;

    (* SpeedScale controls how fast the swarm moves, and the scaling    *)
    (* factor LeaderAdvantage defines how much faster the leader moves. *)
    (* Attraction controls how tightly the swarm tends to cluster.      *)

    SpeedScale = 0.01;
    LeaderAdvantage = 2.0;
    Attraction = 8.0;

    (* Width of the border region on the screen outside which the       *)
    (* leader will not go.                                              *)

    Border = 20;

TYPE
    BodyNumber = [0..MaxBodyNumber];
    BodyState = RECORD
                   x, y: REAL;
                   sx, sy: REAL;
               END (*RECORD*);

VAR
    (* Present and past state of all the Bodys. *)

    OldBody, Body: ARRAY BodyNumber OF BodyState;

    (* Screen size and max colour.  CharHeight is not used, but is      *)
    (* obtained as a side-effect of getting the other parameters.       *)

    Xmax, Ymax, CharHeight: CARDINAL;
    MaxColour: ColourType;

    (* Maximum speed of all Bodys except the leader.  (We allow the      *)
    (* leader to go faster.)  This is a variable because it depends     *)
    (* on screen resolution.                                            *)

    SpeedLimit: REAL;

    SwarmColour: ColourType;

(************************************************************************)

PROCEDURE Initialise;

    (* Fills the Body array with some suitable random numbers. *)

    VAR i: BodyNumber;

    BEGIN
        FOR i := 0 TO MAX(BodyNumber) DO
            WITH Body[i] DO
                x := FLOAT(Xmax-2*Border)*RANDOM() + FLOAT(Border);
                y := FLOAT(Ymax-2*Border)*RANDOM() + FLOAT(Border);
                sx := 2.0*RANDOM() - 1.0;
                sy := 2.0*RANDOM() - 1.0;
            END (*WITH*);
        END (*FOR*);
    END Initialise;

(************************************************************************)

PROCEDURE Sat (VAR (*INOUT*) x, y: REAL;  limit: REAL);

    (* Limits the magnitude of the (x,y) vector. *)

    VAR d2, scale: REAL;

    BEGIN
        d2 := x*x + y*y;
        IF d2 > limit*limit THEN
            scale := limit/VAL(REAL,Sqrt(VAL(LONGREAL,d2)));
            x := scale*x;  y := scale*y;
        END (*IF*);
    END Sat;

(************************************************************************)

PROCEDURE Move;

    CONST CriticalRS = 5.0;     (* both of these parameters affect how  *)
          K1 = 0.4;             (* tightly the swarm will cluster       *)

    VAR i: BodyNumber;  dx, dy, rsquared, scale: REAL;

    BEGIN
        (* Remember the location of dots to be cleared. *)

        OldBody := Body;

        (* Update the velocity and position of the leader. *)

        WITH Body[0] DO
            sx := sx + RANDOM() - 0.5;
            sy := sy + RANDOM() - 0.5;
            Sat (sx, sy, LeaderAdvantage*SpeedLimit);
            x := x + sx;  y := y + sy;
            IF (TRUNC(x) < Border) OR (TRUNC(x) > Xmax-Border) THEN
                sx := -sx;  x := x + 2.0*sx;
            END (*IF*);
            IF (TRUNC(y) < Border) OR (TRUNC(y) > Ymax-Border) THEN
                sy := -sy;  y := y + 2.0*sy;
            END (*IF*);
        END (*  WITH Body[0] *);

        (* Update the velocity and position of the followers.  The      *)
        (* crucial observation is that the velocity of a Body depends on *)
        (* its distance from the leader (plus a random adjustment).     *)

        FOR i := 1 TO MAX(BodyNumber) DO
            WITH Body[i] DO

                (* Now trying a slightly different law, where each Body  *)
                (* tries to follow the one in front as well as the      *)
                (* leader.                                              *)

                dx := K1*(Body[0].x - x) + (1.0-K1)*(Body[i-1].x - x);
                dy := K1*(Body[0].y - y) + (1.0-K1)*(Body[i-1].y - y);
                rsquared := dx*dx + dy*dy;
                IF rsquared > CriticalRS THEN
                    scale := Attraction*SpeedLimit/rsquared;
                ELSE
                    scale := Attraction*SpeedLimit
                                *rsquared/(CriticalRS*CriticalRS);
                END (*IF*);
                sx := sx + scale*dx + 2.0*(RANDOM() - 0.5);
                sy := sy + scale*dy + 2.0*(RANDOM() - 0.5);
                Sat (sx, sy, SpeedLimit);
                x := x + sx;  y := y + sy;
                IF (x < 0.0) OR (TRUNC(x) > Xmax) THEN
                    sx := -sx;  x := x + 2.0*sx;
                END (*IF*);
                IF (y < 0.0) OR (TRUNC(y) > Ymax) THEN
                    sy := -sy;  y := y + 2.0*sy;
                END (*IF*);
            END (* WITH Body[i] *);
        END (*FOR*);
    END Move;

(************************************************************************)

PROCEDURE Display;

    CONST SwarmColour = 10;  LeaderColour = 12;

    VAR b: BodyNumber;

    BEGIN
        (* A pause, so that we don't update too fast. *)

        (*Sleep (20);*)

        (* Clear out the old positions displayed. *)

(*
        FOR b := 0 TO MAX(BodyNumber) DO
            WITH OldBody[b] DO
                PlotDot (TRUNC(x), TRUNC(y), 0);
            END (*WITH*);
        END (*FOR*);
*)
        WITH OldBody[0] DO
            PlotDot (TRUNC(x), TRUNC(y), 0);
        END (*WITH*);

        (* Plot the followers. *)

        FOR b := 1 TO MAX(BodyNumber) DO
            WITH OldBody[b] DO
                PlotDot (TRUNC(x), TRUNC(y), 0);
            END (*WITH*);
            WITH Body[b] DO
                PlotDot (TRUNC(x), TRUNC(y), SwarmColour);
            END (*WITH*);
        END (*FOR*);

        (* Plot the leader. *)

        WITH Body[0] DO
            PlotDot (TRUNC(x), TRUNC(y), LeaderColour);
        END (*WITH*);
        (*
        IF SwarmColour < MaxColour THEN
            INC (SwarmColour);
        ELSE
            SwarmColour := 1;
        END (*IF*);
        *)
    END Display;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    SetMode (VideoMode, TRUE);
    GetScreenShape (Xmax, Ymax, MaxColour, CharHeight);
    SwarmColour := 1;
    SpeedLimit := SpeedScale*FLOAT(Xmax);
    Initialise;
    REPEAT
        Move;  Display;
    UNTIL KeyPressed();
END Gravity.

