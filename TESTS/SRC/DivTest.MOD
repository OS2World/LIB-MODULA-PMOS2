MODULE DivTest;

        (********************************************************)
        (*                                                      *)
        (*              Test of CARD64 operations               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        22 October 2001                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM LONGLONG IMPORT
    (* type *)  CARD64,
    (* proc *)  Div10, Mul64;

FROM Conversions IMPORT
    (* proc *)  CardinalToString, Card64ToString;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

(************************************************************************)
(*                           TEST OPERATIONS                            *)
(************************************************************************)

PROCEDURE MulTest (low1, high1, low2, high2: CARDINAL);

    CONST bufsize = 20;

    VAR number1, number2, result: CARD64;
        buffer: ARRAY [0..bufsize-1] OF CHAR;

    BEGIN
        WriteString ("Multiplication");  WriteLn;
        number1 := CARD64{low1, high1};
        number2 := CARD64{low2, high2};
        result := Mul64 (number1, number2);
        WriteString (" First number: ");
        Card64ToString (number1, buffer, bufsize);
        WriteString (buffer);
        WriteLn;
        WriteString ("Second number: ");
        Card64ToString (number2, buffer, bufsize);
        WriteString (buffer);
        WriteLn;
        WriteString ("       Result: ");
        Card64ToString (result, buffer, bufsize);
        WriteString (buffer);
        WriteLn;  WriteLn;
    END MulTest;

(************************************************************************)

PROCEDURE Test (low, high: CARDINAL);

    CONST bufsize = 20;

    VAR number, quotient: CARD64;  remainder: CARDINAL;
        buffer: ARRAY [0..bufsize-1] OF CHAR;

    BEGIN
        number := CARD64{low, high};
        Div10 (number, quotient, remainder);
        WriteString ("The original number: ");
        Card64ToString (number, buffer, bufsize);
        WriteString (buffer);
        WriteLn;
        WriteString ("       The quotient: ");
        Card64ToString (quotient, buffer, bufsize);
        WriteString (buffer);
        WriteLn;
        WriteString ("      The remainder: ");
        CardinalToString (remainder, buffer, bufsize);
        WriteString (buffer);
        WriteLn;  WriteLn;
    END Test;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    Test (MAX(CARDINAL),MAX(CARDINAL));
    Test (42, 55);
    MulTest (100000, 0, 42, 9);
END DivTest.

