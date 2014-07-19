MODULE KMD5test;

        (********************************************************)
        (*                                                      *)
        (*            TEST OF KEYED MD5 ENCRYPTION              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            3 February 2003                 *)
        (*  Last edited:        3 February 2003                 *)
        (*  Status:             Tests passed                    *)
        (*                                                      *)
        (********************************************************)


FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM KeyedMD5 IMPORT
    (* proc *)  HMAC_MD5;

FROM MD5 IMPORT
    (* type *)  DigestType,
    (* proc *)  MD5DigestToString;

(********************************************************************************)

CONST
    Nul = CHR(0);

(********************************************************************************)

PROCEDURE WriteBString (data: ARRAY OF CHAR;  N: CARDINAL);

    (* Writes out an N-byte string. *)

    VAR j: CARDINAL;

    BEGIN
        IF N > 0 THEN
            FOR j := 0 TO N-1 DO
                WriteChar (data[j]);
            END (*FOR*);
        END (*IF*);
    END WriteBString;

(************************************************************************)

PROCEDURE Test (key, data: ARRAY OF CHAR;  KeyLength, DataLength: CARDINAL);

    (* Does a single test. *)

    VAR digest: DigestType;
        result: ARRAY [0..31] OF CHAR;

    BEGIN
        WriteString ("Key:  ");
        WriteBString (key, KeyLength);
        WriteLn;
        WriteString ("Data: ");
        WriteBString (data, DataLength);
        WriteLn;
        HMAC_MD5 (data, DataLength, key, KeyLength, digest);
        MD5DigestToString (digest, result);
        WriteString ("Ans:  ");
        WriteString (result);
        WriteLn;
    END Test;

(************************************************************************)

PROCEDURE RunTheTest;

    (* Does a sequence of conversions, writes out the answer. *)

    VAR key, data: ARRAY [0..511] OF CHAR;
        j: CARDINAL;

    BEGIN
        FOR j := 0 TO 15 DO
            key[j] := CHR(0BH);
        END (*FOR*);
        key[16] := Nul;
        data := "Hi There";
        Test (key, data, 16, 8);

        key := "Jefe";
        data := "what do ya want for nothing?";
        Test (key, data, 4, 28);

        FOR j := 0 TO 15 DO
            key[j] := CHR(0AAH);
        END (*FOR*);
        key[16] := Nul;
        FOR j := 0 TO 49 DO
            data[j] := CHR(0DDH);
        END (*FOR*);
        data[50] := Nul;
        Test (key, data, 16,50);

    END RunTheTest;

(************************************************************************)
(*                          MAIN TEST PROGRAM                           *)
(************************************************************************)

BEGIN
    RunTheTest;
END KMD5test.

