IMPLEMENTATION MODULE RandCard;

        (********************************************************)
        (*                                                      *)
        (*              Random number generator                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        25 June 1996                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* The algorithm used is Schrage's method, as described in              *)
(*      Stephen K. Park and Keith W. Miller, "Random Number Generators: *)
(*      Good ones are hard to find", CACM 31(10), Oct 1988, 1192-1201.  *)
(* A basic property of this particular implementation is that all       *)
(* intermediate results fit into 32 bits (including sign).              *)
(*                                                                      *)
(************************************************************************)

CONST a = 16807;                  (* 7^5 *)

(************************************************************************)

PROCEDURE RandCardinal (): CARDINAL;

    (* Returns a random number in the range [1..modulus-1],     *)
    (* with a uniform distribution over that range.             *)

    CONST r = modulus MOD a;    (* 2836 *)
          q = modulus DIV a;    (* 127773 *)

    VAR high, low: CARDINAL;  test: INTEGER;

    BEGIN
        high := seed DIV q;  low := seed MOD q;
        test := VAL(INTEGER,a*low) - VAL(INTEGER,r*high);
        IF test > 0 THEN seed := test
        ELSE seed := test + modulus;
        END (*IF*);
        RETURN seed;
    END RandCardinal;

(****************************************************************)

PROCEDURE RandInt (min, max: INTEGER): INTEGER;

    (* Returns a random number in the range [min..max]          *)
    (* (inclusive), with an approximately uniform distribution  *)
    (* over that range.  (In this version I'm not being fussy   *)
    (* about the precise distribution.)                         *)

    BEGIN
        RETURN VAL(INTEGER,RandCardinal()) MOD (max-min) + min + 1;
    END RandInt;

(****************************************************************)
(*                      MODULE INITIALISATION                   *)
(****************************************************************)

BEGIN
    seed := 1;
END RandCard.

