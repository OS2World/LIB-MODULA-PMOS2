MODULE RanTest;

        (********************************************************)
        (*                                                      *)
        (*          Test of random number generator             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        25 June 1996                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM Random IMPORT
    (* proc *)  RANDOM, Randomize;

FROM RandCard IMPORT
    (* var  *)  seed;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn,
                ReadChar;

FROM SWholeIO IMPORT
    (* proc *)  WriteCard;

FROM SRealIO IMPORT
    (* proc *)  WriteReal;

(************************************************************************)
(*                              SPEED TEST                              *)
(************************************************************************)

PROCEDURE SpeedTest (TestSize: CARDINAL);

    (* Generates a lot of random numbers.  Relies on the keyboard user  *)
    (* having a stopwatch, because the timing is not built into this    *)
    (* module.                                                          *)

    VAR j: CARDINAL;  result: REAL;

    BEGIN
        FOR j := 1 TO TestSize DO
            result := RANDOM();
        END (*FOR*);
    END SpeedTest;

(************************************************************************)
(*                          PLAUSIBILITY TESTS                          *)
(************************************************************************)

PROCEDURE TenThousandTest;

    (* Starting with seed = 1, after 10000 calls we should end up with  *)
    (* seed = 1043618065.                                               *)

    VAR j: CARDINAL;  dummy: REAL;

    BEGIN
        Randomize(1);
        FOR j := 1 TO 10000 DO dummy := RANDOM(); END (*FOR*);
        WriteString ("The current value of seed is ");
        WriteCard (seed, 11);
        WriteString (".  It should be 1043618065");
        WriteLn;
    END TenThousandTest;

(************************************************************************)

PROCEDURE DisplayNumbers;

    CONST TestSize = 100;

    VAR j: CARDINAL;  result: REAL;

    BEGIN
        WriteString ("Start of plausibility test");  WriteLn;
        FOR j := 1 TO TestSize DO
            result := RANDOM();
            WriteReal (result, 6);
            IF j MOD 8 <> 0 THEN
                WriteString ("  ");
            ELSE
                WriteLn;
            END (*IF*);
        END (*FOR*);
        WriteLn;
        WriteString ("End of plausibility test");  WriteLn;
    END DisplayNumbers;

(************************************************************************)

PROCEDURE RunTheTests;

    CONST TestSize = 1000000;
    VAR dummy: CHAR;

    BEGIN
        DisplayNumbers;  TenThousandTest;
        WriteString ("Hit <Enter> to start speed test");  WriteLn;
        ReadChar (dummy);
        SpeedTest (TestSize);
        WriteString ("End of speed test, ");
        WriteCard (TestSize, 6);  WriteString (" numbers generated");
        WriteLn;
    END RunTheTests;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    RunTheTests;
END RanTest.

