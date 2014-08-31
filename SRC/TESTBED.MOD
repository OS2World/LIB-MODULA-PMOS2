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

