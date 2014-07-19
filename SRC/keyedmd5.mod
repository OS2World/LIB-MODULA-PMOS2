(* Obsolete, but should be retained until I have full confidence *)
(* in the replacement module called HMAC.                        *)

IMPLEMENTATION MODULE KeyedMD5;

        (********************************************************)
        (*                                                      *)
        (*                KEYED MD5 ENCRYPTION                  *)
        (*                                                      *)
        (*  Implementation of HMAC_MD5 keyed hashing algorithm  *)
        (*              as defined in RFC 2104                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            3 February 2003                 *)
        (*  Last edited:        18 February 2005                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8,
    (* proc *)  ADR;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType, MD5_CTX,
    (* proc *)  MD5Init, MD5Update, MD5Final;

FROM LowLevel IMPORT
    (* proc *)  IXORB, Copy, BlockFill;

(********************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    Byte64 = ARRAY [0..63] OF CARD8;

(************************************************************************)
(*                      THE HMAC-MD5 ALGORITHM                          *)
(************************************************************************)
(*
PROCEDURE HMAC_MD5 (Text: ARRAY OF LOC;  TextLength: CARDINAL;
                    Key: ARRAY OF LOC;  KeyLength: CARDINAL;
                    VAR (*OUT*) digest: MD5_DigestType);

    (* Inputs are Text and Key, and we have to give their lengths       *)
    (* explicitly because they might contain zero bytes, i.e. it is not *)
    (* good enough to treat them as character arrays.  The encrypted    *)
    (* result is in digest.                                             *)

    VAR context, tctx: MD5_CTX;
        k_ipad, k_opad: Byte64;
        tk: MD5_DigestType;
        j: CARDINAL;

    BEGIN
        (* If Key is longer than 64 bytes, redefine Key := MD5(Key)  *)

        IF KeyLength > 64 THEN
            tctx := MD5Init();
            MD5Update(tctx, Key, KeyLength);
            MD5Final (tctx, tk);
            Copy (ADR(tk), ADR(Key), 16);
            KeyLength := 16;
        END (*IF*);

        (************************************************)
        (* The HMAC_MD5 transform is defined as:        *)
        (*                                              *)
        (* MD5(K XOR opad, MD5(K XOR ipad, text))       *)
        (*                                              *)
        (* where K is an n byte key                     *)
        (* ipad is the byte 0x36 repeated 64 times      *)
        (* opad is the byte 0x5c repeated 64 times      *)
        (* and text is the data being protected         *)
        (************************************************)

        (* Start out by storing key in pads. *)

        BlockFill (ADR(k_ipad), 64, 0);
        BlockFill (ADR(k_opad), 64, 0);
        Copy (ADR(Key), ADR(k_ipad), KeyLength);
        Copy (ADR(Key), ADR(k_opad), KeyLength);

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO 63 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner MD5. *)

        context := MD5Init();
        MD5Update(context, k_ipad, 64);
        MD5Update(context, Text, TextLength);
        MD5Final(context, digest);

        (* Perform outer MD5. *)

        context := MD5Init();
        MD5Update(context, k_opad, 64);
        MD5Update(context, digest, 16);
        MD5Final(context, digest);

    END HMAC_MD5;
*)
(************************************************************************)

END KeyedMD5.

