IMPLEMENTATION MODULE TaskControl;

        (****************************************************************)
        (*                                                              *)
        (*   Data structures internal to the kernel of the operating    *)
        (*     system; the dispatcher of the operating system; and      *)
        (*                  related procedures.                         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    2 March 1998                            *)
        (*      Status:         New version                             *)
        (*                                                              *)
        (*   PROBLEMS STILL TO BE RESOLVED:                             *)
        (*      Processes.Start or Processes.Create followed by         *)
        (*          Processes.Activate: the new thread never starts.    *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

IMPORT OS2, Processes;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FinalExit IMPORT
    (* proc *)  Crash;

FROM DumpFile IMPORT
    (* proc *)  DumpString, DumpCard, DumpEOL;

(************************************************************************)

TYPE
    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a Lock.  The fields are:                          *)
    (*      next        the next lock held by the same task             *)
    (*      holder      the task holding this lock                      *)
    (*      mutex       critical section protection                     *)
    (*      LockNumber  ID number of this lock                          *)
    (*                                                                  *)
    (* It is possible that the "holder" and "LockNumber" fields are     *)
    (* not needed - I should check this later.                          *)
    (*                                                                  *)
    (********************************************************************)

    Lock = POINTER TO LockInfo;
    LockInfo = RECORD
                   next: Lock;
                   holder: CARDINAL;
                   mutex: OS2.HMTX;
                   LockNumber: CARDINAL;
               END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a task.  The fields have the following meaning.   *)
    (*                                                                  *)
    (*   next        pointer to the next descriptor on the master task  *)
    (*                list                                              *)
    (*   access      critical section protection semaphore, must be     *)
    (*                locked for any access to this record              *)
    (*   WakeUp      event semaphore used in timeout operations         *)
    (*   id          thread identifier                                  *)
    (*   timeout     time (in milliseconds) before we wake up this      *)
    (*                task if it's timing out.                          *)
    (*   suspended   TRUE iff this task is suspended                    *)
    (*   TimedOut    TRUE iff an operation timed out                    *)
    (*   ResumeFlag  set to FALSE when we suspend a task, and to TRUE   *)
    (*                when we activate it again.  This is a workaround  *)
    (*                for what appears to be a bug in the library       *)
    (*                procedure Processes.SuspendMe                     *)
    (*   LockList    head of the list of locks held by this task        *)
    (*                                                                  *)
    (* Note that some of these fields are redundant.  They're included  *)
    (* to simplify the job of debugging.                                *)
    (*                                                                  *)
    (********************************************************************)

    Task = POINTER TO
               RECORD
                   next: Task;
                   access: OS2.HMTX;
                   WakeUp: OS2.HEV;
                   id: TaskID;
                   timeout: CARDINAL;
                   suspended, TimedOut, ResumeFlag: BOOLEAN;
                   LockList: Lock;
               END (*RECORD*);

(************************************************************************)

VAR
    (* Critical section control for kernel access.  We can enter the    *)
    (* protected code only when the CanEnterKernel event semaphore is   *)
    (* posted.  Because of the quirky way that OS/2 semaphores work,    *)
    (* we also need a mutex semaphore to protect operations on the      *)
    (* event semaphore.                                                 *)

    CanEnterKernel: OS2.HEV;
    KernelFlagAccess: OS2.HMTX;

    (* The list of all tasks known to us. *)

    MasterTaskList: Task;

    (* Mutual exclusion semaphore.  We must lock this for any access to *)
    (* the task list, and for any access to TaskCount.                  *)

    TaskListAccess: OS2.HMTX;

    (* Count of the number of Locks created, and a mutual exclusion     *)
    (* semaphore to guard it.                                           *)

    LockCount: CARDINAL;
    LockCountAccess: OS2.HMTX;

    (* Base for task priority calculations. *)

    BasePriority: Processes.Urgency;

(************************************************************************)
(*                 KERNEL CRITICAL SECTION PROTECTION                   *)
(************************************************************************)

PROCEDURE EnterKernel;

    VAR dummy: CARDINAL;

    BEGIN
        OS2.DosRequestMutexSem (KernelFlagAccess, OS2.SEM_INDEFINITE_WAIT);
        OS2.DosWaitEventSem (CanEnterKernel, OS2.SEM_INDEFINITE_WAIT);
        OS2.DosResetEventSem (CanEnterKernel, dummy);
        OS2.DosReleaseMutexSem (KernelFlagAccess);
    END EnterKernel;

(************************************************************************)

PROCEDURE LeaveKernel;

    BEGIN
        OS2.DosRequestMutexSem (KernelFlagAccess, OS2.SEM_INDEFINITE_WAIT);
        OS2.DosPostEventSem (CanEnterKernel);
        OS2.DosReleaseMutexSem (KernelFlagAccess);
    END LeaveKernel;

(************************************************************************)

PROCEDURE LockTaskList;

    BEGIN
        OS2.DosRequestMutexSem (TaskListAccess, OS2.SEM_INDEFINITE_WAIT);
    END LockTaskList;

(************************************************************************)

PROCEDURE UnlockTaskList;

    BEGIN
        OS2.DosReleaseMutexSem (TaskListAccess);
    END UnlockTaskList;

(************************************************************************)

PROCEDURE LockTask (T: Task);

    BEGIN
        LockTaskList;
        OS2.DosRequestMutexSem (T^.access, OS2.SEM_INDEFINITE_WAIT);
        UnlockTaskList;
    END LockTask;

(************************************************************************)

PROCEDURE UnlockTask (T: Task);

    BEGIN
        LockTaskList;
        OS2.DosReleaseMutexSem (T^.access);
        UnlockTaskList;
    END UnlockTask;

(************************************************************************)
(*                            TASK CREATION                             *)
(************************************************************************)

PROCEDURE NewTaskDescriptor(): Task;

    (* Creates a descriptor for a new task, and adds it to the master   *)
    (* task list.  Note that this does not fill in the id field of      *)
    (* the descriptor.                                                  *)

    VAR result: Task;

    BEGIN
        NEW (result);
        WITH result^ DO
            OS2.DosCreateMutexSem (NIL, access, 0, FALSE);
            OS2.DosCreateEventSem (NIL, WakeUp, 0, FALSE);
            timeout := 0;
            suspended := FALSE;
            TimedOut := FALSE;
            ResumeFlag := TRUE;
            LockList := NIL;
        END (*WITH*);
        LockTaskList;
        result^.next := MasterTaskList;
        MasterTaskList := result;
        UnlockTaskList;
        RETURN result;
    END NewTaskDescriptor;

(************************************************************************)

TYPE TaskStartInfo = POINTER TO
                          RECORD
                              Descriptor: Task;
                              CASE HasParameter: BOOLEAN OF
                                 | FALSE:  TaskCode0: PROC;
                                 | TRUE:   TaskCode1: PROC1;
                                           parameter: ADDRESS;
                              END (*CASE*);
                          END;

(************************************************************************)

PROCEDURE TaskWrapper;

    (* This is the task that runs the user's task code. *)

    VAR StartInfo: TaskStartInfo;
        T: Task;  UseParameter: BOOLEAN;
        Proc0: PROC;
        Proc1: PROC1;  param: ADDRESS;

    BEGIN
        StartInfo := Processes.MyParam();
        WITH StartInfo^ DO
            T := Descriptor;
            UseParameter := HasParameter;
            IF UseParameter THEN
                Proc1 := TaskCode1;
                param := parameter;
            ELSE
                Proc0 := TaskCode0;
            END (*IF*);
        END (*WITH*);
        DISPOSE (StartInfo);
        T^.id := Processes.Me();

        (* Call the user's task code. *)

        IF UseParameter THEN
            Proc1 (param);
        ELSE
            Proc0;
        END (*IF*);

        TaskExit;

    END TaskWrapper;

(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC;  taskpriority: PriorityLevel;
                                                taskname: NameString);

    (* Must be called to introduce a task to the system. The first      *)
    (* parameter, which should be the name of a procedure containing    *)
    (* the task code, gives the starting address.  The second parameter *)
    (* is the task's base priority.  If this task has a higher priority *)
    (* than its creator, it will run immediately.  Otherwise, it        *)
    (* becomes ready.                                                   *)

    CONST StackSize = 131072;

    VAR T: Task;  procID: Processes.ProcessId;
        StartInfo: TaskStartInfo;

    BEGIN
        T := NewTaskDescriptor();
        NEW (StartInfo);
        WITH StartInfo^ DO
            Descriptor := T;  HasParameter := FALSE;
            TaskCode0 := StartAddress;
        END (*WITH*);
        Processes.Start (TaskWrapper, StackSize, BasePriority, StartInfo, procID);
    END CreateTask;

(************************************************************************)

PROCEDURE CreateTask1 (StartAddress: PROC1;  taskpriority: PriorityLevel;
                                   taskname: NameString;  param: ADDRESS);

    (* Like CreateTask, but allows the passing of a single parameter    *)
    (* "param" to the task.                                             *)

    CONST StackSize = 131072;

    VAR procID: Processes.ProcessId;
        StartInfo: TaskStartInfo;
        T: Task;

    BEGIN
        T := NewTaskDescriptor();
        NEW (StartInfo);
        WITH StartInfo^ DO
            Descriptor := T;  HasParameter := TRUE;
            TaskCode1 := StartAddress;  parameter := param;
        END (*WITH*);
        Processes.Create (TaskWrapper, StackSize, BasePriority, StartInfo, procID);
        Processes.Activate (procID);
    END CreateTask1;

(************************************************************************)

PROCEDURE TaskExit;

    (* Removes the currently running task from the system, and performs *)
    (* a task switch to the next ready task.  In the present version    *)
    (* we don't bother discarding the task descriptor, because there's  *)
    (* not much overhead in letting it stay in the master list.         *)

    BEGIN
        Processes.StopMe;
    END TaskExit;

(************************************************************************)
(*                        IDENTIFYING A TASK                            *)
(************************************************************************)

PROCEDURE CurrentTaskID(): TaskID;

    (* Returns the TaskID of the calling task. *)

    BEGIN
        RETURN Processes.Me();
    END CurrentTaskID;

(************************************************************************)

PROCEDURE NilTaskID(): TaskID;

    (* Returns an ID meaning "no such task". *)

    BEGIN
        RETURN CAST (TaskID, NIL);
    END NilTaskID;

(************************************************************************)

PROCEDURE DescriptorOf (id: TaskID): Task;

    (* Returns the task descriptor corresponding to the given TaskID. *)

    VAR result: Task;

    BEGIN
        LockTaskList;
        result := MasterTaskList;
        WHILE (result <> NIL) AND (result^.id <> id) DO
            result := result^.next;
        END (*WHILE*);
        UnlockTaskList;
        RETURN result;
    END DescriptorOf;

(************************************************************************)
(*                LOCKS FOR CRITICAL SECTION PROTECTION                 *)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock. *)

    BEGIN
        NEW (L);
        L^.next := NIL;  L^.holder := 0;
        OS2.DosCreateMutexSem (NIL, L^.mutex, 0, FALSE);
        OS2.DosRequestMutexSem (LockCountAccess, OS2.SEM_INDEFINITE_WAIT);
        INC (LockCount);  L^.LockNumber := LockCount;
        OS2.DosReleaseMutexSem (LockCountAccess);
    END CreateLock;

(************************************************************************)

PROCEDURE DestroyLock (VAR (*INOUT*) L: Lock);

    (* Disposes of a lock. *)

    BEGIN
        IF OS2.DosCloseMutexSem (L^.mutex) <> 0 THEN
        END (*IF*);
        DISPOSE (L);
    END DestroyLock;

(************************************************************************)

PROCEDURE Obtain (L: Lock);

    (* Obtains lock L, waiting if necessary. *)

    VAR T: Task;

    BEGIN
        OS2.DosRequestMutexSem (L^.mutex, OS2.SEM_INDEFINITE_WAIT);
        T := DescriptorOf (CurrentTaskID());
        LockTask(T);
        L^.next := T^.LockList;
        T^.LockList := L;
        UnlockTask(T);
    END Obtain;

(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L - which might unblock some other task. *)

    VAR previous, current: Lock;  T: Task;

    BEGIN
        T := DescriptorOf (CurrentTaskID());
        LockTask(T);
        previous := NIL;  current := T^.LockList;
        LOOP
            IF current = NIL THEN
                Crash ("Releasing a lock we don't hold");
            END (*IF*);
            IF current = L THEN
                IF previous = NIL THEN
                    T^.LockList := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                EXIT (*LOOP*);
            END (*IF*);
            previous := current;  current := current^.next;
        END (*LOOP*);
        UnlockTask(T);
        OS2.DosReleaseMutexSem (L^.mutex);
    END Release;

(************************************************************************)

PROCEDURE ReleaseAllLocks;

    (* Releases all locks held by the current task.  Application-level  *)
    (* tasks normally won't need to call this procedure; it is          *)
    (* provided to support the system shutdown function and for things  *)
    (* like "emergency abort" operations.                               *)

    VAR T: Task;

    BEGIN
        T := DescriptorOf (CurrentTaskID());
        LockTask (T);
        WHILE T^.LockList <> NIL DO
            OS2.DosReleaseMutexSem (T^.LockList^.mutex);
            T^.LockList := T^.LockList^.next;
        END (*WHILE*);
        UnlockTask (T);
    END ReleaseAllLocks;

(************************************************************************)

(*
PROCEDURE DumpLockState (L: Lock);

    (* Writes information about L to the dump file. *)
*)

(************************************************************************)
(*                      SUSPEND/RESUME OPERATIONS                       *)
(************************************************************************)

<* m2extensions + *>

PROCEDURE [OS2.APIENTRY] TimeoutChecker (arg: CARDINAL);

<* m2extensions - *>

    (* A separate thread that runs while a task is timing out.  *)

    VAR TimeLimit: CARDINAL;  eventsem: OS2.HEV;  status: CARDINAL;
        OurId: TaskID;  T: Task;

    BEGIN
        T := CAST (Task, arg);
        LockTask (T);
        WITH T^ DO
            TimeLimit := timeout;
            eventsem := WakeUp;
            OurId := id;
        END (*WITH*);
        UnlockTask (T);
        status := OS2.DosWaitEventSem (eventsem, TimeLimit);
        LockTask (T);
        IF T^.suspended AND (status = OS2.ERROR_TIMEOUT) THEN
             T^.TimedOut := TRUE;
             T^.ResumeFlag := TRUE;
            DumpString (">>>TOC before Activate ");  DumpCard (CAST(CARDINAL,OurId));
            DumpEOL;
            Processes.Activate (OurId);
            DumpString (">>>TOC after Activate ");  DumpCard (CAST(CARDINAL,OurId));
            DumpEOL;
        END (*IF*);
        UnlockTask (T);
        OS2.DosExit (OS2.EXIT_THREAD, 0);
    END TimeoutChecker;

(************************************************************************)

PROCEDURE SuspendMe (id: TaskID;  TimeLimit: CARDINAL;  L: Lock): BOOLEAN;

    (* Suspends the caller and releases Lock L.  A TRUE result          *)
    (* indicates that the time limit expired.                           *)

    CONST StackSize = 8192;

    VAR T: Task;  dummy: CARDINAL;  result: BOOLEAN;
        tries: CARDINAL;

    BEGIN
        EnterKernel;
        T := DescriptorOf (id);
        LockTask (T);
        WITH T^ DO
            timeout := TimeLimit;
            suspended := TRUE;
            TimedOut := FALSE;
            ResumeFlag := FALSE;
            OS2.DosResetEventSem (WakeUp, dummy);
        END (*WITH*);
        UnlockTask (T);

        IF TimeLimit <> OS2.SEM_INDEFINITE_WAIT THEN
            OS2.DosCreateThread (dummy, TimeoutChecker, CAST(CARDINAL,T), 0, StackSize);
        END (*IF*);

        Release (L);
        DumpString ("Suspending task ");  DumpCard (CAST(CARDINAL,id));
        DumpEOL;

        (* The loop below is to guard against tasks which re-activate, for      *)
        (* reasons I don't yet understand, even though there has been no call   *)
        (* to Processes.Activate.                                               *)

        tries := 0;
        REPEAT
            DumpString (">>>before SuspendMe ");  DumpCard (CAST(CARDINAL,id));
            DumpEOL;
            LeaveKernel;  Processes.SuspendMe;  EnterKernel;
            INC (tries);
            DumpString (">>>after SuspendMe ");  DumpCard (CAST(CARDINAL,id));
            DumpEOL;
        UNTIL T^.ResumeFlag OR (tries > 255);

        (* Here's where we arrive after being activated again. *)

        IF tries > 1 THEN
            DumpString ("*** SUSPENSION TOOK ");  DumpCard (tries);
            DumpString (" TRIES");  DumpEOL;
        END (*IF*);

        DumpString ("Task ");  DumpCard (CAST(CARDINAL,id));
        DumpString (" has woken up");  DumpEOL;

        LockTask (T);
        WITH T^ DO
            result := TimedOut;
            timeout := 0;
            TimedOut := FALSE;
            suspended := FALSE;
        END (*WITH*);
        UnlockTask (T);
        LeaveKernel;
        RETURN result;

    END SuspendMe;

(************************************************************************)

PROCEDURE ResumeTask (id: TaskID;  L: Lock): BOOLEAN;

    (* Releases lock L, and resumes a task specified by its thread ID.  *)
    (* The function result is normally TRUE, but is FALSE if the task   *)
    (* couldn't be resumed (usually because that task no longer exists).*)

    VAR T: Task;

    BEGIN
        EnterKernel;
        DumpString ("Resuming task ");  DumpCard (CAST(CARDINAL,id));
        DumpEOL;
        T := DescriptorOf (id);
        IF T = NIL THEN
            Release (L);
            LeaveKernel;
            RETURN FALSE;
        END (*IF*);
        LockTask (T);
        IF NOT T^.suspended THEN
            DumpString ("*** Resuming a task that wasn't suspended ***");  DumpEOL;
            Crash ("Resuming a task that wasn't suspended");
        END (*IF*);
        IF NOT T^.TimedOut THEN
            T^.suspended := FALSE;
            IF T^.timeout <> OS2.SEM_INDEFINITE_WAIT THEN
                OS2.DosPostEventSem (T^.WakeUp);
            END (*IF*);
            T^.ResumeFlag := TRUE;
            Release (L);
            DumpString (">>>before Activate ");  DumpCard (CAST(CARDINAL,id));
            DumpEOL;
            Processes.Activate (id);
            DumpString (">>>after Activate ");  DumpCard (CAST(CARDINAL,id));
            DumpEOL;
        ELSE
            Release (L);
        END (*IF*);
        UnlockTask (T);
        LeaveKernel;
        RETURN TRUE;
    END ResumeTask;

(************************************************************************)
(*                     MODULE INITIALISATION                            *)
(************************************************************************)

PROCEDURE CreateMainTaskDescriptor;

    VAR T: Task;

    BEGIN
        T := NewTaskDescriptor();
        T^.id := Processes.Me();
        BasePriority := Processes.UrgencyOf (T^.id);
    END CreateMainTaskDescriptor;

(************************************************************************)

BEGIN
    OS2.DosCreateMutexSem (NIL, KernelFlagAccess, 0, FALSE);
    OS2.DosCreateEventSem (NIL, CanEnterKernel, 0, FALSE);
    OS2.DosCreateMutexSem (NIL, TaskListAccess, 0, FALSE);
    OS2.DosCreateMutexSem (NIL, LockCountAccess, 0, FALSE);
    LockCount := 0;
    MasterTaskList := NIL;
    CreateMainTaskDescriptor;
    OS2.DosPostEventSem (CanEnterKernel);
END TaskControl.

