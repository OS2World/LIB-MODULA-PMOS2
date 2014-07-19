IMPLEMENTATION MODULE TaskControl;

        (****************************************************************)
        (*                                                              *)
        (*   Data structures internal to the kernel of the operating    *)
        (*     system; the dispatcher of the operating system; and      *)
        (*                  related procedures.                         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    4 November 1997                         *)
        (*      Status:         Working                                 *)
        (*                                                              *)
        (*      There's so much debugging stuff in this module that     *)
        (*      it's become a bit untidy.  Needs a general clean-up.    *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM OS2 IMPORT
    (* const*)  APIENTRY, SEM_INDEFINITE_WAIT, PRTYS_THREAD, PRTYC_REGULAR, EXIT_THREAD,
                EXIT_PROCESS, ERROR_TIMEOUT,
    (* type *)  PTIB, PPIB, HMTX, HEV,
    (* proc *)  DosCreateThread, DosSetPriority, DosSuspendThread, DosResumeThread,
                DosGetInfoBlocks, DosKillThread, DosExit,
                DosSleep,
                DosCreateMutexSem, DosCloseMutexSem, DosRequestMutexSem, DosReleaseMutexSem,
                DosQueryMutexSem,
                DosCreateEventSem, DosPostEventSem, DosWaitEventSem, DosResetEventSem,
                DosCloseEventSem;

FROM FinalExit IMPORT
    (* proc *)  Crash;

(************************************************************************)

CONST NilProc = CAST(PROC, NIL);

TYPE
    Lock = POINTER TO LockInfo;
    LockInfo = RECORD
                   next: Lock;
                   holder: CARDINAL;
                   mutex: HMTX;
                   LockNumber: CARDINAL;
               END (*RECORD*);

    TaskDescriptor = POINTER TO
                          RECORD
                              previous, next: TaskDescriptor;
                              InternalID: TaskID;
                              UserCode: PROC;
                              UserCode1: PROC1;
                              LockList: Lock;
                              Name: NameString;
                              WakeUp: HEV;
                              suspended, MarkedForTermination: BOOLEAN;
                              HasParameter: BOOLEAN;
                              ParameterValue: ADDRESS;
                          END (*RECORD*);

(************************************************************************)

VAR
    TaskCount: CARDINAL;

    (* The list of all tasks we know about. *)

    MasterTaskList: TaskDescriptor;

    (* Mutual exclusion semaphore.  We must lock this for any access to *)
    (* the task list (including access to individual task descriptors,  *)
    (* if they are on the master task list), and for any access to      *)
    (* TaskCount.                                                       *)

    TaskListAccess: HMTX;
    LockCount: CARDINAL;
    LockCountAccess: HMTX;

(************************************************************************)
(*                KERNEL CRITICAL SECTION PROTECTION                    *)
(************************************************************************)

PROCEDURE LockTaskList;

    BEGIN
        DosRequestMutexSem (TaskListAccess, SEM_INDEFINITE_WAIT);
    END LockTaskList;

(************************************************************************)

PROCEDURE UnlockTaskList;

    BEGIN
        DosReleaseMutexSem (TaskListAccess);
    END UnlockTaskList;

(************************************************************************)
(*                         IDENTIFYING A TASK                           *)
(************************************************************************)

PROCEDURE CurrentTaskID(): TaskID;

    (* Returns the TaskID of the calling task. *)

    VAR ptib: PTIB;  ppib: PPIB;

    BEGIN
        DosGetInfoBlocks (ptib, ppib);
        RETURN ptib^.tib_ptib2^.tib2_ultid;
        END CurrentTaskID;

(************************************************************************)

PROCEDURE DescriptorOf (id: TaskID): TaskDescriptor;

    (* Translates a thread ID to a task descriptor.  The result is NIL  *)
    (* for an unknown task.                                             *)

    VAR result: TaskDescriptor;

    BEGIN
        LockTaskList;
        result := MasterTaskList;
        WHILE (result <> NIL) AND (result^.InternalID <> id) DO
            result := result^.next;
        END (*WHILE*);
        UnlockTaskList;
        RETURN result;
    END DescriptorOf;

(************************************************************************)

PROCEDURE CurrentTask(): TaskDescriptor;

    (* Returns the descriptor of the calling task.  The result is NIL   *)
    (* if the task wasn't created by this module.                       *)

    BEGIN
        RETURN DescriptorOf (CurrentTaskID());
    END CurrentTask;

(************************************************************************)
(*                       DEBUGGING CODE                                 *)
(************************************************************************)

(*
PROCEDURE DumpTaskList;

    VAR current: TaskDescriptor;

    BEGIN
        DumpString ("The current task list is");
        DumpEOL;

        LockTaskList;
        current := MasterTaskList;
        WHILE current <> NIL DO
            DumpCard (current^.InternalID);  DumpString ("  ");
            DumpHex (current^.UserCode);  DumpString ("  ");
            DumpHex (current^.LockList);  DumpString ("  ");
            DumpString (current^.Name);
            IF current^.suspended THEN
                DumpString ("  (suspended)");
            END (*IF*);
            current := current^.next;
            DumpEOL;
        END (*WHILE*);
        UnlockTaskList;

    END DumpTaskList;

(************************************************************************)

PROCEDURE DumpLockList (T: TaskDescriptor);

    VAR current: Lock;

    BEGIN
        DumpString ("Task ");  DumpCard (T^.InternalID);
        DumpString (" now holds locks");

        current := T^.LockList;
        WHILE current <> NIL DO
            DumpString ("  ");  DumpCard (current^.LockNumber);
            current := current^.next;
        END (*WHILE*);
        DumpEOL;

    END DumpLockList;
*)

(************************************************************************)
(*                     OPERATIONS ON THE TASK LIST                      *)
(************************************************************************)

PROCEDURE CreateNewDescriptor(): TaskDescriptor;

    (* Creates a task descriptor, fills in default values for its       *)
    (* fields, but does not yet add it to the task list.                *)

    VAR result: TaskDescriptor;

    BEGIN
        NEW (result);
        WITH result^ DO
            previous := NIL;  next := NIL;
            InternalID := 0;
            UserCode := NilProc;
            LockList := NIL;
            Name := "Unknown";
            DosCreateEventSem (NIL, WakeUp, 0, FALSE);
            suspended := FALSE;  MarkedForTermination := FALSE;
        END (*WITH*);
        RETURN result;
    END CreateNewDescriptor;

(************************************************************************)

PROCEDURE AddToTaskList (T: TaskDescriptor);

    (* Adds T to the master task list. *)

    BEGIN
        LockTaskList;
        IF MasterTaskList <> NIL THEN MasterTaskList^.previous := T END(*IF*);
        T^.next := MasterTaskList;  MasterTaskList := T;
        INC (TaskCount);
        UnlockTaskList;
    END AddToTaskList;

(************************************************************************)

PROCEDURE UnlinkDescriptor (T: TaskDescriptor);

    (* Removes T from the task list, without modifying it except for    *)
    (* clearing the list links.                                         *)

    BEGIN
        LockTaskList;
        IF T^.previous = NIL THEN
            MasterTaskList := T^.next;
        ELSE
            T^.previous^.next := T^.next;
        END (*IF*);
        IF T^.next <> NIL THEN
            T^.next^.previous := T^.previous;
        END (*IF*);
        T^.previous := NIL;  T^.next := NIL;
        DEC (TaskCount);
        UnlockTaskList;
    END UnlinkDescriptor;

(************************************************************************)
(*                     RUNNING THE USER CODE                            *)
(************************************************************************)

<* m2extensions + *>

PROCEDURE [APIENTRY] TaskWrapper (param: CARDINAL);

<* m2extensions - *>

    (* This, as far as OS/2 is concerned, is the actual task.  Its      *)
    (* job is simply to run the user task code.                         *)

    VAR T: TaskDescriptor;

    BEGIN
        T := CAST (TaskDescriptor, param);
        T^.InternalID := CurrentTaskID();
        IF T^.HasParameter THEN
            T^.UserCode1 (T^.ParameterValue);
        ELSE
            T^.UserCode;
        END (*IF*);
        TaskExit;
    END TaskWrapper;

(************************************************************************)
(*                     CREATING A NEW TASK                              *)
(************************************************************************)

PROCEDURE CreateTaskCommon (T: TaskDescriptor;  taskpriority: PriorityLevel;
                     taskname: NameString;  PassParameter: BOOLEAN;  Param: ADDRESS);

    (* Common code for the two task creation procedures. *)

    CONST StackSize = 65536;

    BEGIN
        WITH T^ DO
            Name := taskname;
            HasParameter := PassParameter;
            ParameterValue := Param;
            IF DosCreateThread (InternalID, TaskWrapper, CAST(CARDINAL,T), 0, StackSize) = 0 THEN
                AddToTaskList (T);

                (* Next line commented out for now, because it was making the entire *)
                (* program run terribly slowly.  I haven't yet found out why.        *)

                (*DosSetPriority (PRTYS_THREAD, PRTYC_REGULAR, taskpriority, T^.tid);*)

            ELSE
                Crash ("CreateTask failure.");
            END (*IF*);
        END (*WITH*);

    END CreateTaskCommon;

(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC;  taskpriority: PriorityLevel;
                                                taskname: NameString);

    (* Must be called to introduce a task to the system. The first      *)
    (* parameter, which should be the name of a procedure containing    *)
    (* the task code, gives the starting address.  The second parameter *)
    (* is the task's base priority.  If this task has a higher priority *)
    (* than its creator, it will run immediately.  Otherwise, it        *)
    (* becomes ready.                                                   *)

    VAR T: TaskDescriptor;

    BEGIN
        T := CreateNewDescriptor();
        T^.UserCode := StartAddress;
        CreateTaskCommon (T, taskpriority, taskname, FALSE, NIL);
    END CreateTask;

(************************************************************************)

PROCEDURE CreateTask1 (StartAddress: PROC1;  taskpriority: PriorityLevel;
                                   taskname: NameString;  param: ADDRESS);

    (* Like CreateTask, but allows the passing of a single parameter    *)
    (* "param" to the task.                                             *)

    VAR T: TaskDescriptor;

    BEGIN
        T := CreateNewDescriptor();
        T^.UserCode1 := StartAddress;
        CreateTaskCommon (T, taskpriority, taskname, TRUE, param);
    END CreateTask1;

(************************************************************************)

PROCEDURE TaskExit;

    VAR me: TaskDescriptor;

    BEGIN
        ReleaseAllLocks;
        me := CurrentTask();
        UnlinkDescriptor (me);
        DISPOSE (me);
        DosExit (EXIT_THREAD, 0);
    END TaskExit;

(************************************************************************)
(*                SUSPENDING AND RESUMING A TASK                        *)
(************************************************************************)

PROCEDURE SuspendMe (id: TaskID;  TimeLimit: CARDINAL): BOOLEAN;

    (* Suspends the caller.  A TRUE result indicates that the time      *)
    (* limit expired.                                                   *)

    VAR T: TaskDescriptor;  status: CARDINAL;  PostCount: CARDINAL;
        TimedOut: BOOLEAN;  WakeUpEventSem: HEV;

    BEGIN
        T := DescriptorOf (id);
        LockTaskList;
        T^.suspended := TRUE;
        TimedOut := FALSE;
        WakeUpEventSem := T^.WakeUp;
        UnlockTaskList;
        status := DosWaitEventSem (WakeUpEventSem, TimeLimit);
        LockTaskList;
        IF status = ERROR_TIMEOUT THEN
            TimedOut := TRUE;  T^.suspended := FALSE;
        END (*IF*);
        IF T^.MarkedForTermination THEN
            UnlockTaskList;
            TaskExit;
        END (*IF*);
        IF NOT TimedOut THEN
            status := DosResetEventSem (T^.WakeUp, PostCount);
        END (*IF*);
        UnlockTaskList;
        RETURN TimedOut;
    END SuspendMe;

(************************************************************************)

PROCEDURE ResumeTask (id: TaskID): BOOLEAN;

    (* Resumes a task specified by its thread ID.  The function result  *)
    (* is normally TRUE, but is FALSE if the task couldn't be resumed   *)
    (* (usually because that task no longer exists).                    *)

    VAR T: TaskDescriptor;  status: CARDINAL;

    BEGIN
        T := DescriptorOf (id);
        LockTaskList;
        IF T = NIL THEN
            UnlockTaskList;
            RETURN FALSE;
        END (*IF*);
        T^.suspended := FALSE;
        status := DosPostEventSem (T^.WakeUp);
        UnlockTaskList;
        RETURN TRUE;
    END ResumeTask;

(************************************************************************)
(*                                                                      *)
(*                LOCKS FOR CRITICAL SECTION PROTECTION                 *)
(*                                                                      *)
(*  Note that we distinguish between a Lock and a Semaphore.            *)
(*  A Semaphore is a general semaphore - whose operations are defined   *)
(*  in module Semaphores - which can be used for general inter-task     *)
(*  interlocking.  A Lock is similar to a binary semaphore (with a      *)
(*  more efficient implementation than a Semaphore), but may be used    *)
(*  only in a strictly nested fashion and is therefore useful only      *)
(*  for critical section protection.  No task should perform a          *)
(*  semaphore Wait while it holds a Lock.                               *)
(*                                                                      *)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock. *)

    BEGIN
        NEW (L);
        L^.next := NIL;  L^.holder := 0;
        DosCreateMutexSem (NIL, L^.mutex, 0, FALSE);
        DosRequestMutexSem (LockCountAccess, SEM_INDEFINITE_WAIT);
        INC (LockCount);  L^.LockNumber := LockCount;
        DosReleaseMutexSem (LockCountAccess);
    END CreateLock;

(************************************************************************)

PROCEDURE DestroyLock (VAR (*INOUT*) L: Lock);

    (* Disposes of a lock. *)

    BEGIN
        IF DosCloseMutexSem (L^.mutex) <> 0 THEN
        END (*IF*);
        DISPOSE (L);
    END DestroyLock;

(************************************************************************)

PROCEDURE Obtain (L: Lock);

    (* Obtains lock L, waiting if necessary. *)

    VAR me: TaskDescriptor;

    BEGIN
        me := CurrentTask();
        IF me = NIL THEN
            DosExit (EXIT_PROCESS, 0);
        END (*IF*);
        DosRequestMutexSem (L^.mutex, SEM_INDEFINITE_WAIT);
        L^.next := me^.LockList;  me^.LockList := L;
        L^.holder := me^.InternalID;
    END Obtain;

(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L - which might unblock some other task. *)

    VAR me: TaskDescriptor;  previous, current: Lock;
        status: CARDINAL;

    BEGIN
        me := CurrentTask();
        previous := NIL;  current := me^.LockList;
        LOOP
            IF current = NIL THEN
                Crash ("Releasing a lock we don't hold");
            ELSIF current = L THEN
                IF previous = NIL THEN me^.LockList := current^.next
                ELSE previous^.next := current^.next;
                END (*IF*);
                L^.holder := 0;
                L^.next := NIL;
                status := DosReleaseMutexSem (L^.mutex);
                EXIT (*LOOP*);
            ELSE
                previous := current;  current := current^.next;
            END (*IF*);
        END (*LOOP*);
    END Release;

(************************************************************************)

(*
PROCEDURE DumpLockState (L: Lock);

    (* Writes information about L to the dump file. *)

    VAR pid, tid, pulCount: CARDINAL;

    BEGIN
        DosQueryMutexSem (L^.mutex, pid, tid, pulCount);
        DumpCard (pulCount);
        IF pulCount > 0 THEN
            DumpString (" held by thread ");  DumpCard (tid);
        END (*IF*);
    END DumpLockState;
*)

(************************************************************************)
(*                       TERMINATION PROCESSING                         *)
(************************************************************************)

PROCEDURE ReleaseLocks (T: TaskDescriptor);

    (* Releases all locks held by task T.  (We do this in preparation   *)
    (* for killing task T.)                                             *)

    BEGIN
        WHILE T^.LockList <> NIL DO
            Release (T^.LockList);
        END (*WHILE*);
    END ReleaseLocks;

(************************************************************************)

PROCEDURE ReleaseAllLocks;

    (* Releases all locks held by the current task.  Application-level  *)
    (* tasks normally won't need to call this procedure; it is          *)
    (* provided to support the system shutdown function and for things  *)
    (* like "emergency abort" operations.                               *)

    VAR me: TaskDescriptor;

    BEGIN
        me := CurrentTask();
        IF me <> NIL THEN
            ReleaseLocks (me);
        END (*IF*);
    END ReleaseAllLocks;

(************************************************************************)

PROCEDURE KillTask (T: TaskDescriptor): BOOLEAN;

    (* Kills task T (*and releases all of its locks*).  *)
    (* Assumption: the caller has locked the task list. *)
    (* A FALSE result indicates failure to kill the task.*)

    VAR status: CARDINAL;

    BEGIN
        status := DosKillThread (T^.InternalID);
        RETURN status = 0;
    END KillTask;

(************************************************************************)

PROCEDURE KillAllTasks;

    (* Shuts down all tasks that have been created by this module. *)
    (* Note that this leaves the main thread still running.        *)

    VAR me, current, next: TaskDescriptor;  suicide: BOOLEAN;
        status: CARDINAL;

    BEGIN
        me := CurrentTask();  suicide := FALSE;
        LockTaskList;
        current := MasterTaskList;
        WHILE current <> NIL DO
            next := current^.next;
            IF current^.UserCode = NilProc THEN

                (* Main thread, don't kill it. *)

            ELSIF current = me THEN

                suicide := TRUE;

            ELSIF current^.MarkedForTermination THEN

                (* Do nothing *)

            ELSIF current^.suspended THEN

                current^.MarkedForTermination := TRUE;
                status := DosPostEventSem (current^.WakeUp);

            ELSIF KillTask (current) THEN
                UnlinkDescriptor (current);
            END (*IF*);

            current := next;

        END (*WHILE*);
        UnlockTaskList;

        IF suicide THEN
            TaskExit;
        END (*IF*);

    END KillAllTasks;

(************************************************************************)
(*                       MODULE INITIALISATION                          *)
(************************************************************************)

VAR me: TaskDescriptor;

BEGIN
    DosCreateMutexSem (NIL, TaskListAccess, 0, FALSE);
    DosCreateMutexSem (NIL, LockCountAccess, 0, FALSE);
    LockCount := 0;

    (* Create a task descriptor for the thread that's already running. *)

    NEW (MasterTaskList);
    WITH MasterTaskList^ DO
        next := NIL;  previous := NIL;
        UserCode := NilProc;
        InternalID := CurrentTaskID();
        DosCreateEventSem (NIL, WakeUp, 0, FALSE);
        LockList := NIL;
        Name := "Main thread";
        suspended := FALSE;  MarkedForTermination := FALSE;
    END (*WITH*);
    TaskCount := 1;
    UnlockTaskList;

FINALLY
    me := CurrentTask();
    IF me^.suspended THEN
         me^.suspended := FALSE;
         DosPostEventSem (me^.WakeUp);
         DosSleep (300);
    END (*IF*);
    ReleaseAllLocks;
    KillAllTasks;
    IF TaskCount > 1 THEN
        DosSleep (100);
        KillAllTasks;
    END (*IF*);

END TaskControl.

