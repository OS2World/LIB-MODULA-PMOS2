MODULE TaskTest;

(************************************************************************)
(*                                                                      *)
(*                      Test program TaskTest                           *)
(*      Creates several threads, prints some messages from each.        *)
(*                                                                      *)
(*      Purpose: to help in developing and testing the OS/2 version     *)
(*                      of module TaskControl                           *)
(*                                                                      *)
(*      Author:         P. Moylan                                       *)
(*      Last edited:    9 February 1998                                 *)
(*      Status:         Working                                         *)
(*                                                                      *)
(************************************************************************)

FROM Testbed IMPORT
    (* proc *)  Checkpoint;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM SWholeIO IMPORT
    (* proc *)  WriteCard;

(************************************************************************)

CONST NumberOfChildThreads = 3;

VAR ChildDone: Semaphore;
    TaskNum: RECORD
                 access: Semaphore;
                 next: CARDINAL;
             END (*RECORD*);

    ScreenLock: Semaphore;

(************************************************************************)
(*                         CODE FOR ONE THREAD                          *)
(************************************************************************)

PROCEDURE ThreadCode;

    (* One copy of this procedure is run for each child task. *)

    VAR MyID: CARDINAL;  j: CARDINAL;

    BEGIN
        (* Work out which task we are. *)

        WITH TaskNum DO
             Wait (access);
             MyID := next;
             INC (next);
             Signal (access);
        END (*WITH*);
        Wait (ScreenLock);
        WriteString ("Task ");  WriteCard (MyID, 2);
        WriteString (" starting");  WriteLn;
        Signal (ScreenLock);

        (* Do something to prove that we're running. *)

        FOR j := 1 TO 4 DO
            Wait (ScreenLock);
            WriteString ("Message ");  WriteCard (j,2);
            WriteString ("/4 from task ");
            WriteCard (MyID, 2);
            WriteLn;
            Signal (ScreenLock);
            Sleep (2000);
        END (*FOR*);

        (* Tell main task that we're done. *)

        Signal (ChildDone);

    END ThreadCode;

(****************************************************************)
(*        PROCEDURE TO CREATE AND RUN THE CHILD THREADS         *)
(****************************************************************)

PROCEDURE RunTheTest;

    (* On entry, the "count" part of the ThreadInfo array is already    *)
    (* set up.  This procedure creates the necessary semaphores, tells  *)
    (* the child threads to start, and then waits until they have       *)
    (* finished.                                                        *)

    VAR j: CARDINAL;

    BEGIN
        WITH TaskNum DO
            CreateSemaphore (access, 1);  next := 1;
        END (*WITH*);
        FOR j := 1 TO NumberOfChildThreads DO
            Wait (ScreenLock);
            Checkpoint ("RunTheTest, about to call CreateTask");
            Signal (ScreenLock);
            CreateSemaphore (ChildDone, 0);
            CreateTask (ThreadCode, 2, "child");
            Wait (ScreenLock);
            Checkpoint ("RunTheTest, after return from CreateTask");
            Signal (ScreenLock);
        END (*FOR*);
        FOR j := 1 TO NumberOfChildThreads DO
            Wait (ChildDone);
            Checkpoint ("RunTheTest, after Wait(ChildDone)");
        END (*FOR*);
        WriteString ("Done!");  WriteLn;

    END RunTheTest;

(****************************************************************)
(*                          MAIN PROGRAM                        *)
(****************************************************************)

BEGIN
    CreateSemaphore (ScreenLock, 1);
    WriteLn;
    WriteString ("Simple test of creating child threads.");
    WriteLn;
    RunTheTest;
END TaskTest.

