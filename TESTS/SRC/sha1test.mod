MODULE SHA1Test;

        (********************************************************)
        (*                                                      *)
        (*               TEST OF SHA-1 ENCRYPTION               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            15 February 2005                *)
        (*  Last edited:        19 February 2005                *)
        (*  Status:             Testing completed               *)
        (*                                                      *)
        (*  Passed all SHA-1 tests.                             *)
        (*                                                      *)
        (*  We pass the first two HMAC_MD5 tests and then fail  *)
        (*  the third, for no reason that I can discover.  It   *)
        (*  is possible that there is an error in the published *)
        (*  test data.                                          *)
        (*                                                      *)
        (*  HMAC_SHA1 is untested since I have no published     *)
        (*  test results to do a comparison.  However since     *)
        (*  the two HMAC methods have almost identical code,    *)
        (*  I'm confident of this method.                       *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* This test program is based on test data in the SHA-1 standard.       *)
(* The HMAC_MD5 test data come from the HMAC standard.                  *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT CARD8, CARD32;

IMPORT STextIO;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType,
    (* proc *)  MD5DigestToString;

FROM SHA1 IMPORT
    (* type *)  SHA1_CTX, SHA1_DigestType,
    (* proc *)  SHA1Init, SHA1Update, SHA1Final, SHA1DigestToString;

FROM HMAC IMPORT
    (* proc *)  HMAC_MD5, HMAC_SHA1;

FROM TimeConv IMPORT
    (* proc *)  millisecs;

(********************************************************************************)

CONST
    (* Length of test block, number of test blocks. *)

    TEST_BLOCK_LEN = 5000;
    TEST_BLOCK_COUNT = 5000;

(********************************************************************************)
(*                                 OUTPUT                                       *)
(********************************************************************************)

PROCEDURE WriteHex1 (value: CARD8);

    (* Writes a one-digit hex number to standard output. *)

    BEGIN
        IF value < 10 THEN
            STextIO.WriteChar (CHR(ORD('0') + value));
        ELSE
            STextIO.WriteChar (CHR(ORD('A') + value - 10));
        END (*IF*);
    END WriteHex1;

(********************************************************************************)

PROCEDURE WriteCard (value: CARDINAL);

    (* Writes a decimal number to standard output. *)

    BEGIN
        IF value > 9 THEN
            WriteCard (value DIV 10);
        END (*IF*);
        WriteHex1 (value MOD 10);
    END WriteCard;

(********************************************************************************)

PROCEDURE MDPrint (digest: SHA1_DigestType);

    (* Prints a message digest in hexadecimal. *)

    VAR buffer: ARRAY [0..39] OF CHAR;

    BEGIN
        SHA1DigestToString (digest, buffer);
        STextIO.WriteString (buffer);
    END MDPrint;

(********************************************************************************)
(*                           ENCRYPTING A STRING                                *)
(********************************************************************************)

PROCEDURE MDString (string: ARRAY OF CHAR);

    (* Digests a string and prints the result. *)

    VAR context: SHA1_CTX;  len: CARDINAL;
        digest: SHA1_DigestType;

    BEGIN
        context := SHA1Init();
        len := LENGTH(string);
        SHA1Update (context, string, len);
        SHA1Final (context, digest);
        STextIO.WriteString ('SHA1("');  STextIO.WriteString (string);
        STextIO.WriteString ('") = ');
        MDPrint (digest);
        STextIO.WriteLn;
    END MDString;

(********************************************************************************)

PROCEDURE MillionTest;

    (* Digests a string containing a million 'a' characters. *)

    VAR context: SHA1_CTX;  k: CARDINAL;
        digest: SHA1_DigestType;

    BEGIN
        context := SHA1Init();
        FOR k := 1 TO 100000 DO
            SHA1Update (context, "aaaaaaaaaa", 10);
        END (*FOR*);
        SHA1Final (context, digest);
        STextIO.WriteString ('SHA1(a x 1,000,000) = ');
        MDPrint (digest);
        STextIO.WriteLn;
    END MillionTest;

(********************************************************************************)

PROCEDURE SHA1TimeTrial;

    (* Measures the time to digest TEST_BLOCK_COUNT TEST_BLOCK_LEN-byte blocks. *)

    VAR context: SHA1_CTX;
        endTime, startTime: CARDINAL;
        digest: SHA1_DigestType;
        block: ARRAY [0..TEST_BLOCK_LEN] OF CHAR;
        i: CARDINAL;  speed: REAL;

    BEGIN
        STextIO.WriteString ("SHA1 time trial. Digesting ");
        WriteCard (TEST_BLOCK_LEN);
        STextIO.WriteChar (' ');
        WriteCard (TEST_BLOCK_COUNT);
        STextIO.WriteString ("-byte blocks ...");

        (* Initialize block *)

        FOR i := 0 TO TEST_BLOCK_LEN-1 DO
            block[i] := CHR(i MOD 256);
        END (*FOR*);

        (* Start timer *)

        startTime := millisecs();

        (* Digest blocks *)

        context := SHA1Init();
        FOR i := 0 TO TEST_BLOCK_COUNT-1 DO
            SHA1Update (context, block, TEST_BLOCK_LEN);
        END (*FOR*);
        SHA1Final (context, digest);

        (* Stop timer *)

        endTime := millisecs();

        STextIO.WriteString (" done");
        STextIO.WriteLn;
        STextIO.WriteString ("Digest = ");
        MDPrint (digest);
        STextIO.WriteLn;
        STextIO.WriteString ("Time = ");
        WriteCard (endTime-startTime);
        STextIO.WriteString (" milliseconds");
        STextIO.WriteLn;
        STextIO.WriteString ("Speed = ");
        speed := FLOAT(1000*TEST_BLOCK_LEN) * FLOAT(TEST_BLOCK_COUNT)
                                            / FLOAT(endTime-startTime);
        WriteCard (VAL(CARDINAL, speed/1024.0 + 0.5));
        STextIO.WriteString (" Kbytes/second");
        STextIO.WriteLn;

    END SHA1TimeTrial;

(********************************************************************************)

PROCEDURE SHA1TestSuite;

    (* Digests a reference suite of strings and prints the results. *)

    BEGIN
        STextIO.WriteString ("SHA1 test suite:");
        STextIO.WriteLn;

        MDString ("abc");
        MDString ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
        MillionTest;

    END SHA1TestSuite;

(********************************************************************************)

PROCEDURE HMAC_MD5Test;

    (* Digests a reference suite of strings and prints the results. *)

    VAR key: ARRAY [0..15] OF CARD8;
        text: ARRAY [0..49] OF CARD8;
        digest: MD5_DigestType;
        result: ARRAY [0..39] OF CHAR;
        j: CARDINAL;

    BEGIN
        STextIO.WriteString ("HMAC-MD5 tests:");
        STextIO.WriteLn;

        FOR j := 0 TO 15 DO
            key[j] := 0BH;
        END (*FOR*);
        HMAC_MD5 ("Hi There", 8, key, 16, digest);
        MD5DigestToString (digest, result);
        STextIO.WriteString ('HMAC_MD5("Hi There") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

        HMAC_MD5 ("what do ya want for nothing?", 28, "Jefe", 4, digest);
        MD5DigestToString (digest, result);
        STextIO.WriteString ('HMAC_MD5("what do ya want for nothing?") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

        FOR j := 0 TO 15 DO
            key[j] := 0AH;
        END (*FOR*);
        FOR j := 0 TO 49 DO
            text[j] := 0DH;
        END (*FOR*);
        HMAC_MD5 (text, 50, key, 16, digest);
        MD5DigestToString (digest, result);
        STextIO.WriteString ('HMAC_MD5(50x"0DH") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

    END HMAC_MD5Test;

(********************************************************************************)

PROCEDURE HMAC_SHA1Test;

    (* Digests a reference suite of strings and prints the results. *)

    VAR key: ARRAY [0..15] OF CARD8;
        text: ARRAY [0..49] OF CARD8;
        digest: SHA1_DigestType;
        result: ARRAY [0..39] OF CHAR;
        j: CARDINAL;

    BEGIN
        STextIO.WriteString ("HMAC-SHA1 tests:");
        STextIO.WriteLn;

        FOR j := 0 TO 15 DO
            key[j] := 0BH;
        END (*FOR*);
        HMAC_SHA1 ("Hi There", 8, key, 16, digest);
        SHA1DigestToString (digest, result);
        STextIO.WriteString ('HMAC_SHA1("Hi There") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

        HMAC_SHA1 ("what do ya want for nothing?", 28, "Jefe", 4, digest);
        SHA1DigestToString (digest, result);
        STextIO.WriteString ('HMAC_SHA1("what do ya want for nothing?") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

        FOR j := 0 TO 15 DO
            key[j] := 0AH;
        END (*FOR*);
        FOR j := 0 TO 49 DO
            text[j] := 0DH;
        END (*FOR*);
        HMAC_SHA1 (text, 50, key, 16, digest);
        SHA1DigestToString (digest, result);
        STextIO.WriteString ('HMAC_SHA1(50x"0DH") = ');
        STextIO.WriteString (result);
        STextIO.WriteLn;

    END HMAC_SHA1Test;

(************************************************************************)
(*                          MAIN TEST PROGRAM                           *)
(************************************************************************)

BEGIN
    SHA1TestSuite();
    SHA1TimeTrial;
    HMAC_MD5Test;
    HMAC_SHA1Test;
END SHA1Test.

