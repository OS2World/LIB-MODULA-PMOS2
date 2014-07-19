IMPLEMENTATION MODULE Testbed;

(************************************************************************)
(*                                                                      *)
(*          Miscellaneous procedures for use in testing new code        *)
(*                                                                      *)
(*      Author:         P. Moylan                                       *)
(*      Last edited:    6 August 1996                                   *)
(*      Status:         OK                                              *)
(*                                                                      *)
(************************************************************************)

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

(************************************************************************)
(*                           TEST MESSAGE                               *)
(************************************************************************)

PROCEDURE Checkpoint (message: ARRAY OF CHAR);

    BEGIN
        WriteString (message);
        WriteLn;
        (*PressAnyKey;*)
    END Checkpoint;

(************************************************************************)

END Testbed.

