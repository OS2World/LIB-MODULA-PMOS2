MODULE MDDriver;

        (********************************************************)
        (*                                                      *)
        (*               TEST OF MD5 ENCRYPTION                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 September 1998               *)
        (*  Last edited:        23 May 2005                     *)
        (*  Status:             Working, one option missing     *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* This test program is an adaptation of code in appendix 4 of RFC 1321.*)
(* The original code carries the following copyright notice.            *)
(*                                                                      *)
(* MDDRIVER.C - test driver for MD2, MD4 and MD5                        *)
(*                                                                      *)
(* Copyright (C) 1990-2, RSA Data Security, Inc. Created 1990. All      *)
(* rights reserved.                                                     *)
(*                                                                      *)
(* RSA Data Security, Inc. makes no representations concerning either   *)
(* the merchantability of this software or the suitability of this      *)
(* software for any particular purpose. It is provided "as is"          *)
(* without express or implied warranty of any kind.                     *)
(*                                                                      *)
(* These notices must be retained in any copies of any part of this     *)
(* documentation and/or software.                                       *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT CARD8, ADR;

IMPORT IOChan, TextIO, STextIO, Strings, SeqFile, ChanConsts, IOConsts;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

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

PROCEDURE MDPrint (digest: MD5_DigestType);

    (* Prints a message digest in hexadecimal. *)

    VAR buffer: ARRAY [0..31] OF CHAR;

    BEGIN
        MD5DigestToString (digest, buffer);
        STextIO.WriteString (buffer);
    END MDPrint;

(********************************************************************************)
(*                           ENCRYPTING A STRING                                *)
(********************************************************************************)

PROCEDURE MDString (string: ARRAY OF CHAR);

    (* Digests a string and prints the result. *)

    VAR context: MD5_CTX;  len: CARDINAL;
        digest: MD5_DigestType;

    BEGIN
        context := MD5Init();
        len := LENGTH(string);
        MD5Update (context, string, len);
        MD5Final (context, digest);
        STextIO.WriteString ('MD5("');  STextIO.WriteString (string);
        STextIO.WriteString ('") = ');
        MDPrint (digest);
        STextIO.WriteLn;
    END MDString;

(********************************************************************************)

PROCEDURE MDTimeTrial;

    (* Measures the time to digest TEST_BLOCK_COUNT TEST_BLOCK_LEN-byte blocks. *)

    VAR context: MD5_CTX;
        endTime, startTime: CARDINAL;
        digest: MD5_DigestType;
        block: ARRAY [0..TEST_BLOCK_LEN] OF CHAR;
        i: CARDINAL;  speed: REAL;

    BEGIN
        STextIO.WriteString ("MD5 time trial. Digesting ");
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

        context := MD5Init();
        FOR i := 0 TO TEST_BLOCK_COUNT-1 DO
            MD5Update (context, block, TEST_BLOCK_LEN);
        END (*FOR*);
        MD5Final (context, digest);

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

    END MDTimeTrial;

(********************************************************************************)

PROCEDURE MDTestSuite;

    (* Digests a reference suite of strings and prints the results. *)

    BEGIN
        STextIO.WriteString ("MD5 test suite:");
        STextIO.WriteLn;

        MDString ("");
        MDString ("a");
        MDString ("abc");
        MDString ("message digest");
        MDString ("abcdefghijklmnopqrstuvwxyz");
        MDString
             ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
        MDString ("1234567890123456789012345678901234567890" +
                  "1234567890123456789012345678901234567890");

    END MDTestSuite;

(********************************************************************************)

PROCEDURE MDFile (filename: ARRAY OF CHAR);

    (* Digests a file and prints the result. *)

    VAR file: IOChan.ChanId;  status: ChanConsts.OpenResults;
        context: MD5_CTX;  len: CARDINAL;
        buffer: ARRAY [0..1023] OF CARD8;
        digest: MD5_DigestType;

    BEGIN
        SeqFile.OpenRead (file, filename,
                ChanConsts.read+ChanConsts.raw, status);
        IF status = ChanConsts.opened THEN
            context := MD5Init();
            LOOP
                IOChan.RawRead (file, ADR(buffer), 1024, len);
                IF IOChan.ReadResult(file) <> IOConsts.allRight THEN
                    EXIT(*LOOP*)
                END (*IF*);
                MD5Update (context, buffer, len);
            END (*LOOP*);
            MD5Final (context, digest);
            SeqFile.Close (file);
            STextIO.WriteString ("MD5(");
            STextIO.WriteString (filename);
            STextIO.WriteString (") = ");
            MDPrint (digest);
            STextIO.WriteLn;
        ELSE
            STextIO.WriteString (filename);
            STextIO.WriteString (" can't be opened");
            STextIO.WriteLn;
        END (*IF*);

    END MDFile;

(********************************************************************************)

(*
(* Digests the standard input and prints the result. *)
static void MDFilter ()
{
  MD_CTX context;
  int len;
  unsigned char buffer[16], digest[16];

  MDInit (&context);
  while (len = fread (buffer, 1, 16, stdin))
 MDUpdate (&context, buffer, len);
  MDFinal (digest, &context);

  MDPrint (digest);
  STextIO.WriteLn;
*)

(********************************************************************************)

PROCEDURE GetParameter (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Picks up program argument from the command line. *)

    VAR args: IOChan.ChanId;  j: CARDINAL;

    BEGIN
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, result);
            j := LENGTH (result);
        ELSE
            j := 0;
        END (*IF*);

        (* Strip trailing spaces. *)

        WHILE (j > 0) AND (result[j-1] = ' ') DO
            DEC (j);
        END (*WHILE*);
        result[j] := CHR(0);

    END GetParameter;

(************************************************************************)
(*                          MAIN TEST PROGRAM                           *)
(************************************************************************)

        (*************************************)
        (* Arguments:                        *)
        (*    -sstring - digests string      *)
        (*    -t       - runs time trial     *)
        (*    -x       - runs test script    *)
        (*    filename - digests file        *)
        (*************************************)

VAR arg: ARRAY [0..79] OF CHAR;

BEGIN
    GetParameter (arg);
    IF arg[0] = '-' THEN
        IF arg[1] = 's' THEN
            Strings.Delete (arg, 0, 2);
            MDString (arg);
        ELSIF arg[1] = 't' THEN
            MDTimeTrial ();
        ELSIF arg[1] = 'x' THEN
            MDTestSuite ();
        ELSE
            STextIO.WriteString ("Unknown option ");
            STextIO.WriteChar (arg[1]);  STextIO.WriteLn;
        END (*IF*);
    ELSIF arg[0] <> CHR(0) THEN
        MDFile (arg);
    (*
    ELSE
        MDFilter ();
    *)
    END (*IF*);

END MDDriver.

