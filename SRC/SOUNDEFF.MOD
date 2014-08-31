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

IMPLEMENTATION MODULE SoundEffects;

        (********************************************************)
        (*                                                      *)
        (*      Procedures to produce audible output.           *)
        (*                                                      *)
        (*      Programmer:     P. Moylan                       *)
        (*      Last edited:    2 July 2001                     *)
        (*      Status:         Seems to be working             *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Queues IMPORT
    (* type *)  Queue,
    (* proc *)  CreateQueue, AddToQueue, TakeFromQueue, Empty;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM OS2 IMPORT
    (* proc *)  DosBeep;

(************************************************************************)

CONST
    OffCode = 0CH;  OnCode = 0FH;

TYPE

    <* m2extensions+ *>
    NoteArrayPointer = POINTER TO ARRAY [0..1000] OF Note;
    <* m2extensions- *>

    SemaphorePointer = POINTER TO Semaphore;

    QueuePointer = POINTER TO QueueElement;

    (* The music waiting to be played is ordered by having a queue of   *)
    (* waiting entries.  Each element of the queue is a record with a   *)
    (* pointer to an array of music data, the last subscript of that    *)
    (* array (the first subscript is always assumed to be zero), and a  *)
    (* pointer to the semaphore on which we will perform a Signal to    *)
    (* indicate that we have finished working on this section of data.  *)
    (* Notice that the queue is not directly a queue of data.  Rather,  *)
    (* it is a queue of pointers to data.                               *)

    QueueElement =  RECORD
                        dataptr: NoteArrayPointer;
                        lastsubscript: CARDINAL;
                        CompletionSemaphoreAddress: SemaphorePointer;
                    END (*RECORD*);

(************************************************************************)

VAR

    (* PlayQueue is the actual queue.   *)

    PlayQueue: Queue;

    (* beep holds the data to be used to produce a "beep" noise.        *)

    beep: ARRAY [0..1] OF Note;

    (* The completion semaphore used when doing the beep.       *)

    BeepSem: Semaphore;

(************************************************************************)

PROCEDURE Play (VAR (*IN*) playdata: ARRAY OF Note;
                        VAR (*INOUT*) done: Semaphore);

    (* Adds the array to the list of music queued up waiting to be      *)
    (* played.  The actual playing is handled by a separate task - see  *)
    (* PlayerTask later in this module.  On return from this procedure, *)
    (* the playing is not necessarily over.  The caller must perform a  *)
    (* Wait(done) to know when the array playdata is no longer in use.  *)

    VAR elementpointer: QueuePointer;

    BEGIN
        NEW (elementpointer);
        WITH elementpointer^ DO
            dataptr := ADR (playdata);  lastsubscript := HIGH (playdata);
            CompletionSemaphoreAddress := ADR(done);
        END (*WITH*);
        AddToQueue (PlayQueue, elementpointer);
    END Play;

(************************************************************************)

PROCEDURE Beep;

    (* Produces a short "beep" noise.   *)

    BEGIN
        Wait (BeepSem);
        Play (beep, BeepSem);
    END Beep;

(************************************************************************)

PROCEDURE PlayerTask;

    (* This is the procedure which does all the real work.  It runs as  *)
    (* a separate task, which typically spends most of its time         *)
    (* blocked while waiting for something to play.                     *)

    (* A duration code of 0 indicates the end of the data, in cases     *)
    (* where the data do not fill the entire array.                     *)
    (* A period code of 1, with a nonzero duration, indicates a rest.   *)

    CONST FreqScale = 1193000;

    VAR arrayptr: NoteArrayPointer;  j, top, Period, Duration: CARDINAL;
        doneaddress: SemaphorePointer;  qptr: QueuePointer;

    BEGIN
        LOOP    (* forever *)

            (* Note that this task will remain blocked inside procedure *)
            (* TakeFromQueue while there is nothing to play.            *)

            qptr := TakeFromQueue (PlayQueue);
            WITH qptr^ DO
                arrayptr := dataptr;
                top := lastsubscript;
                doneaddress := CompletionSemaphoreAddress;
            END (*WITH*);
            DISPOSE (qptr);

            j := 0;
            LOOP
                Period := arrayptr^[j].period;
                Duration := arrayptr^[j].duration;

                (* A duration code of 0 indicates the end of the data.  *)

                IF Duration = 0 THEN
                    EXIT(*LOOP*);
                END (*IF*);

                IF Period <= 1 THEN
                    (* Rest *)
                    Sleep (Duration);
                ELSE
                    (* We have a normal note to play. *)
                    DosBeep (FreqScale DIV Period, Duration);
                END (*IF*);
                IF j = top THEN EXIT(*LOOP*) END(*IF*);
                INC (j);

            END (*LOOP*);

            (* Tell the user task that we have finished with this       *)
            (* buffer-full of data.                                     *)

            Signal (doneaddress^);

        END (*LOOP*);
    END PlayerTask;

(************************************************************************)
(*                      MODULE TERMINATION                              *)
(************************************************************************)

PROCEDURE CloseDown;

    (* Brings the module to an orderly halt.    *)

    VAR qptr: QueuePointer;

    BEGIN
        WHILE NOT Empty(PlayQueue) DO
            qptr := TakeFromQueue (PlayQueue);
            Signal (qptr^.CompletionSemaphoreAddress^);
            DISPOSE (qptr);
        END (*WHILE*);
    END CloseDown;

(************************************************************************)
(*                      MODULE INITIALISATION                           *)
(************************************************************************)

BEGIN

    (* Create an initially empty queue of music to be played.   *)

    CreateQueue (PlayQueue);

    (* Define the parameters of a "beep" noise. *)

    beep[0].period := 1000;
    beep[0].duration := 40;
    beep[1].period := 1;
    beep[1].duration := 20;
    CreateSemaphore (BeepSem, 1);

    (* Create the task which plays the music.   *)

    CreateTask (PlayerTask, 10, "Sound effects");

FINALLY
    CloseDown;
END SoundEffects.

