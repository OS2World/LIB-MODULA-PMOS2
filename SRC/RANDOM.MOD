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
