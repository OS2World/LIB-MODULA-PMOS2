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

IMPLEMENTATION MODULE IOErrorCodes;

    (********************************************************)
    (*                          *)
    (*      I/O subsystem error codes.      *)
    (*                          *)
    (*  This module provides operations on the file system  *)
    (*          error codes.            *)
    (*                          *)
    (*  Programmer:     P. Moylan           *)
    (*  Last edited:    11 May 1994         *)
    (*  Status:     OK              *)
    (*                          *)
    (********************************************************)

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM MiscPMOS IMPORT
    (* proc *)  CopyString;

(************************************************************************)

PROCEDURE TranslateErrorCode (code: ErrorCode;
                VAR (*OUT*) string: ARRAY OF CHAR);

    (* Converts code to its textual representation. *)

    BEGIN
    CASE code OF
        OK:         CopyString ("OK", string);
      |
        OperationAborted:   CopyString ("OperationAborted", string);
      |
        FileNotOpen:    CopyString ("FileNotOpen", string);
      |
        NoSuchDevice:   CopyString ("NoSuchDevice", string);
      |
        NoSuchUnit:     CopyString ("NoSuchUnit", string);
      |
        FeatureNotImplemented: CopyString ("FeatureNotImplemented", string);
      |
        InvalidFileNameString: CopyString ("InvalidFileNameString", string);
      |
        DirectoryNotFound:  CopyString ("DirectoryNotFound", string);
      |
        NotADirectory:  CopyString ("NotADirectory", string);
      |
        NameNotFound:   CopyString ("NameNotFound", string);
      |
        DuplicateFileName:  CopyString ("DuplicateFileName", string);
      |
        DeviceFull:     CopyString ("DeviceFull", string);
      |
        DirectoryFull:  CopyString ("DirectoryFull", string);
      |
        BadDMAAddress:  CopyString ("BadDMAAddress", string);
      |
        IllegalBlockNumber: CopyString ("IllegalBlockNumber", string);
      |
        BadCommand:     CopyString ("BadCommand", string);
      |
        ControllerNotListening: CopyString ("ControllerNotListening", string);
      |
        ControllerOutOfSync: CopyString ("ControllerOutOfSync", string);
      |
        TimeoutError:   CopyString ("TimeoutError", string);
      |
        CalibrationFailure: CopyString ("CalibrationFailure", string);
      |
        SeekFailure:    CopyString ("SeekFailure", string);
      |
        DriveNotReady:  CopyString ("DriveNotReady", string);
      |
        SectorNotFound: CopyString ("SectorNotFound", string);
      |
        BadBlock:       CopyString ("BadBlock", string);
      |
        BadData:        CopyString ("BadData", string);
      |
        WriteFault:     CopyString ("WriteFault", string);
      |
        WriteProtected: CopyString ("WriteProtected", string);
      |
        UndiagnosedFailure: CopyString ("UndiagnosedFailure", string);
      |
      ELSE
        CardinalToString (ORD(code), string, 6);
    END (*CASE*);
    END TranslateErrorCode;

(************************************************************************)

END IOErrorCodes.

