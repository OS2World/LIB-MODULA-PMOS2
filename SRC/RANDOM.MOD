IMPLEMENTATION MODULE Random;

        (********************************************************)
        (*                                                      *)
        (*              Random number generator                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        25 June 1996                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM RandCard IMPORT
    (* const*)  modulus,
    (* var  *)  seed,
    (* proc *)  RandCardinal;

VAR scale: REAL;

(************************************************************************)

PROCEDURE RANDOM(): REAL;

    (* Returns a random number from a uniform (0.0, 1.0) distribution.  *)
    (* This version relies on procedure RandCardinal to do the random   *)
    (* number generation; all that we do is the scaling.                *)

    BEGIN
        RETURN scale*FLOAT(RandCardinal());
    END RANDOM;

(************************************************************************)

PROCEDURE Randomize (newseed: CARDINAL);

    (* Resets the seed of the random number generator.  *)

    BEGIN
        IF newseed = 0 THEN newseed := 1
        ELSIF newseed >= modulus THEN newseed := modulus-1
        END (*IF*);
        seed := newseed;
    END Randomize;

(************************************************************************)

BEGIN
    scale := 1.0/FLOAT(modulus);
END Random.

