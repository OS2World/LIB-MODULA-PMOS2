IMPLEMENTATION MODULE TaskControl;

        (****************************************************************)
        (*                                                              *)
        (*   Data structures internal to the kernel of the operating    *)
        (*     system; the dispatcher of the operating system; and      *)
        (*                  related procedures.                         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    8 March 1998                            *)
        (*      Status:         New version                             *)
        (*                                                              *)
        (*   THIS IS THE VERSION THAT USES MODULE Processes.            *)
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

(************************************************************************)

TYPE
    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a Lock.  The fields are:                          *)
    (*      next        the next lock held by the same task             *)
    (*      mutex       critical section protection                     *)
    (*                                                                  *)
    (********************************************************************)

    Lock = POINTER TO LockInfo;
    LockInfo = RECORD
                   next: Lock;
                   mutex: OS2.HMTX;
               END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a task.  The fields have the following meaning.   *)
    (*                                                                  *)
    (*   next        pointer to the next descriptor on the master task  *)
    (*                list                                              *)
    (*   WakeUp      event semaphore used in timeout operations         *)
    (*   id          thread identifier                                  *)
    (*   timeout     time (in milliseconds) before we wake up this      *)
    (*                task if it's timing out.                          *)
    (*   suspended   TRUE iff this task is suspended                    *)
    (*   TimedOut    TRUE iff an operation timed out                    *)
    (*   LockList    head of the list of locks held by this task        *)
    (*                                                                  *)
    (********************************************************************)

    Task = POINTER TO
               RECORD
                   next: Task;
                   WakeUp: OS2.HEV;
                   id: TaskID;
                   timeout: CARDINAL;
                   suspended, TimedOut: BOOLEAN;
                   LockList: Lock;
               END (*RECORD*);

(************************************************************************)

VAR
    (* The list of all tasks known to us. *)

    MasterTaskList: Task;

    (* Mutual exclusion semaphore.  We must lock this for any access to *)
    (* the task list.                                                   *)

    TaskListAccess: OS2.HMTX;

(************************************************************************)
(*                 KERNEL CRITICAL SECTION PROTECTION                   *)
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
            OS2.DosCreateEventSem (NIL, WakeUp, 0, FALSE);
            timeout := 0;
            suspended := FALSE;
            TimedOut := FALSE;
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
            Proc0 := TaskCode0;
            Proc1 := TaskCode1;
            param := parameter;
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

    CONST StackSize = 32768;

    VAR T: Task;  procID: Processes.ProcessId;
        StartInfo: TaskStartInfo;

    BEGIN
        T := NewTaskDescriptor();
        NEW (StartInfo);
        WITH StartInfo^ DO
            Descriptor := T;  HasParameter := FALSE;
            TaskCode0 := StartAddress;
        END (*WITH*);
        Processes.Start (TaskWrapper, StackSize, taskpriority, StartInfo, procID);
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
        Processes.Start (TaskWrapper, StackSize, taskpriority, StartInfo, procID);
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

PROCEDURE DescriptorOf (id: TaskID): Task;

    (* Returns the task descriptor corresponding to the given TaskID. *)
    (* Assumption: the caller has already locked the task list.       *)

    VAR result: Task;

    BEGIN
        result := MasterTaskList;
        WHILE (result <> NIL) AND (result^.id <> id) DO
            result := result^.next;
        END (*WHILE*);
        RETURN result;
    END DescriptorOf;

(************************************************************************)
(*                LOCKS FOR CRITICAL SECTION PROTECTION                 *)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock. *)

    BEGIN
        NEW (L);
        L^.next := NIL;
        OS2.DosCreateMutexSem (NIL, L^.mutex, 0, FALSE);
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
        LockTaskList;
        T := DescriptorOf (Processes.Me());
        L^.next := T^.LockList;
        T^.LockList := L;
        UnlockTaskList;
    END Obtain;

(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L - which might unblock some other task. *)

    VAR previous, current: Lock;  T: Task;

    BEGIN
        LockTaskList;
        T := DescriptorOf (Processes.Me());
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
        UnlockTaskList;
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
        LockTaskList;
        T := DescriptorOf (Processes.Me());
        WHILE T^.LockList <> NIL DO
            OS2.DosReleaseMutexSem (T^.LockList^.mutex);
            T^.LockList := T^.LockList^.next;
        END (*WHILE*);
        UnlockTaskList;
    END ReleaseAllLocks;

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
        LockTaskList;
        WITH T^ DO
            TimeLimit := timeout;
            eventsem := WakeUp;
            OurId := id;
        END (*WITH*);
        UnlockTaskList;
        status := OS2.DosWaitEventSem (eventsem, TimeLimit);
        LockTaskList;
        IF T^.suspended AND (status = OS2.ERROR_TIMEOUT) THEN
            T^.TimedOut := TRUE;
            Processes.Activate (OurId);
        END (*IF*);
        UnlockTaskList;
        OS2.DosExit (OS2.EXIT_THREAD, 0);
    END TimeoutChecker;

(************************************************************************)

PROCEDURE SuspendMe (id: TaskID;  TimeLimit: CARDINAL;  L: Lock): BOOLEAN;

    (* Suspends the caller and releases Lock L.  A TRUE result          *)
    (* indicates that the time limit expired.                           *)

    CONST StackSize = 8192;

    VAR T: Task;  dummy: CARDINAL;  result: BOOLEAN;

    BEGIN
        LockTaskList;
        T := DescriptorOf (id);
        WITH T^ DO
            timeout := TimeLimit;
            suspended := TRUE;
            TimedOut := FALSE;
            OS2.DosResetEventSem (WakeUp, dummy);
        END (*WITH*);
        UnlockTaskList;

        IF TimeLimit <> OS2.SEM_INDEFINITE_WAIT THEN
            OS2.DosCreateThread (dummy, TimeoutChecker, CAST(CARDINAL,T), 0, StackSize);
        END (*IF*);

        Release (L);
        Processes.SuspendMe;

        (* Here's where we arrive after being activated again. *)

        LockTaskList;
        WITH T^ DO
            result := TimedOut;
            timeout := 0;
            TimedOut := FALSE;
            suspended := FALSE;
        END (*WITH*);
        UnlockTaskList;
        RETURN result;

    END SuspendMe;

(************************************************************************)

PROCEDURE ResumeTask (id: TaskID;  L: Lock): BOOLEAN;

    (* Releases lock L, and resumes a task specified by its thread ID.  *)
    (* The function result is normally TRUE, but is FALSE if the task   *)
    (* couldn't be resumed (usually because that task no longer exists).*)

    VAR T: Task;

    BEGIN
        LockTaskList;
        T := DescriptorOf (id);
        IF T = NIL THEN
            UnlockTaskList;
            Release (L);
            RETURN FALSE;
        END (*IF*);
        IF NOT T^.suspended THEN
            Crash ("Resuming a task that wasn't suspended");
        END (*IF*);
        IF NOT T^.TimedOut THEN
            T^.suspended := FALSE;
            IF T^.timeout <> OS2.SEM_INDEFINITE_WAIT THEN
                OS2.DosPostEventSem (T^.WakeUp);
            END (*IF*);
            Release (L);
            Processes.Activate (id);
        ELSE
            Release (L);
        END (*IF*);
        UnlockTaskList;
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
    END CreateMainTaskDescriptor;

(************************************************************************)

BEGIN
    OS2.DosCreateMutexSem (NIL, TaskListAccess, 0, FALSE);
    MasterTaskList := NIL;
    CreateMainTaskDescriptor;
END TaskControl.

