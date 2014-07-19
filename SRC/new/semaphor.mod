IMPLEMENTATION MODULE Semaphores;

        (********************************************************)
        (*                                                      *)
        (*      Implements the Wait and Signal operations on    *)
        (*      semaphores.                                     *)
        (*                                                      *)
        (*      Programmer:     P. Moylan                       *)
        (*      Last edited:    12 February 1998                *)
        (*      Status:         OK                              *)
        (*                   Still contains debugging code      *)
        (*                                                      *)
        (*      Observation: the kernel overheads in semaphore  *)
        (*      and Lock operations are somewhat higher than    *)
        (*      in the original PMOS, largely due to the        *)
        (*      repeated calculation of the current task ID.    *)
        (*      I should look at ways to improve this.          *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* proc *)  CAST;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FinalExit IMPORT
    (* proc *)  Crash;

FROM TaskControl IMPORT
    (* type *)  Lock, TaskID,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                CurrentTaskID, SuspendMe, ResumeTask;

FROM OS2 IMPORT
    (* const *) SEM_INDEFINITE_WAIT;

FROM DumpFile IMPORT
    (* proc *)  DumpString, DumpCard, DumpEOL;

(************************************************************************)

TYPE
    BlockedListPointer = POINTER TO
                             RECORD
                                 ThreadID: TaskID;
                                 next: BlockedListPointer;
                             END;

    Semaphore = POINTER TO
                    RECORD
                        value: INTEGER;
                        access: Lock;
                        holder: CARDINAL;
                        BlockedList: RECORD
                                         head, tail: BlockedListPointer;
                                     END (*RECORD*);
                    END (*RECORD*);

(************************************************************************)

PROCEDURE DumpSemaphoreState (s: Semaphore);

    (* Writes information about s to the dump file. *)

    VAR p: BlockedListPointer;

    BEGIN
        DumpString ("  Semaphore ");  DumpCard (CAST(CARDINAL,s));
        DumpString (", value ");
        IF s^.value < 0 THEN
            DumpString ("-");  DumpCard (-s^.value);
        ELSE
            DumpCard (s^.value);
        END (*IF*);
        IF s^.holder <> 0 THEN
            DumpString (", holder ");  DumpCard (s^.holder);
        END (*IF*);
        p := s^.BlockedList.head;
        IF p <> NIL THEN
            DumpString (", blocked list");
            WHILE p <> NIL DO
                DumpString ("  ");  DumpCard (CAST(CARDINAL,p^.ThreadID));
                p := p^.next;
            END (*WHILE*);
        END (*IF*);
        DumpEOL;
    END DumpSemaphoreState;

(************************************************************************)

PROCEDURE CreateSemaphore (VAR (*OUT*) s: Semaphore;
                                        InitialValue: CARDINAL);

    (* Creates semaphore s, with the given initial value and an empty   *)
    (* queue.                                                           *)

    BEGIN
        NEW(s);
        WITH s^ DO
            CreateLock (access);
            value := InitialValue;
            IF value > 0 THEN holder := 0
            ELSE holder := CAST(CARDINAL,CurrentTaskID());
            END(*IF*);
            WITH BlockedList DO
                head := NIL;  tail := NIL;
            END (*WITH*);
        END (*WITH*);
    END CreateSemaphore;

(************************************************************************)

PROCEDURE DestroySemaphore (VAR (*INOUT*) s: Semaphore);

    (* Reclaims any space used by semaphore s.  Remark:  It is not at   *)
    (* all obvious what should be done with any tasks which happen to   *)
    (* be blocked on this semaphore (should they be unblocked, or       *)
    (* killed?).  At present we take the easy way out and assume that   *)
    (* there are no pending operations on s at the time that it is      *)
    (* destroyed.                                                       *)

    BEGIN
        WITH s^ DO
            DestroyLock (access);
        END (*WITH*);
        DISPOSE (s);
    END DestroySemaphore;

(************************************************************************)

PROCEDURE Wait (s: Semaphore);

    (* Decrements the semaphore value.  If the value goes negative, the *)
    (* calling task is blocked and there is a task switch.              *)

    VAR p: BlockedListPointer;  ThreadID: TaskID;

    BEGIN
        IF s = NIL THEN Crash ("Wait on nonexistent semaphore"); END(*IF*);
        ThreadID := CurrentTaskID();
        WITH s^ DO
            Obtain (access);
            DEC (value);
            IF value < 0 THEN
                DumpString ("Wait: blocking task ");  DumpCard (CAST(CARDINAL,ThreadID));
                DumpEOL;
                DumpSemaphoreState (s);
                NEW (p);
                p^.next := NIL;  p^.ThreadID := ThreadID;
                WITH BlockedList DO
                    IF tail = NIL THEN
                        head := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*WITH*);
                (*Release (access);*)
                IF SuspendMe (ThreadID, SEM_INDEFINITE_WAIT, access) THEN
                    DumpString ("Semaphore Wait failure");  DumpEOL;
                    Crash ("Semaphore Wait failure");
                END (*IF*);

                (* We resume here after some other task unblocks us. *)

                DumpString ("Wait: task ");
                DumpCard (CAST(CARDINAL,ThreadID));
                DumpString (" has woken up");  DumpEOL;
                Obtain (access);
            END (*IF*);
            holder := CAST(CARDINAL,ThreadID);
            Release (access);
        END (*WITH*);
    END Wait;

(************************************************************************)

PROCEDURE TimedWaitT (s: Semaphore;  TimeLimit: INTEGER;
                        VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like procedure Wait, except that it returns with TimedOut TRUE   *)
    (* if the corresponding Signal does not occur within TimeLimit      *)
    (* clock ticks.                                                     *)

    VAR p: BlockedListPointer;  ThreadID: TaskID;

    BEGIN
        IF TimeLimit <= 0 THEN TimeLimit := 1; END(*IF*);

        (* Possible OS/2 bug: I'm not sure that the case TimeLimit=0    *)
        (* works correctly; but changing 0 to 1 should have little      *)
        (* impact on most tasks.                                        *)

        IF s = NIL THEN Crash ("Wait on nonexistent semaphore"); END(*IF*);
        ThreadID := CurrentTaskID();
        WITH s^ DO
            Obtain (access);
            DEC (value);
            IF value < 0 THEN
                NEW (p);
                p^.next := NIL;  p^.ThreadID := ThreadID;
                WITH BlockedList DO
                    IF tail = NIL THEN
                        head := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*WITH*);
                (*Release (access);*)

                DumpString ("TimedWaitT: blocking task ");  DumpCard (CAST(CARDINAL,ThreadID));
                DumpEOL;
                DumpSemaphoreState (s);

                TimedOut := SuspendMe (ThreadID, TimeLimit, access);

                DumpString ("TimedWaitT: task ");
                DumpCard (CAST(CARDINAL,ThreadID));
                IF TimedOut THEN
                    DumpString (" has timed out");
                ELSE
                    DumpString (" has woken up");
                END (*IF*);
                DumpEOL;

                Obtain (access);
                IF TimedOut THEN
                    INC(value);
                ELSE
                    holder := CAST(CARDINAL,ThreadID);
                END (*IF*);
            ELSE                     (* value >= 0 *)
                TimedOut := FALSE;
                holder := CAST(CARDINAL,ThreadID);
            END (*IF*);
            Release (access);
        END (*WITH*);
    END TimedWaitT;

(************************************************************************)

PROCEDURE Signal (s: Semaphore);

    (* Increments the semaphore value.  Unblocks one task, if there was *)
    (* one waiting on this semaphore.                                   *)

    VAR p: BlockedListPointer;  ThreadToUnblock: TaskID;

    BEGIN
        IF s = NIL THEN Crash ("Signal on nonexistent semaphore"); END(*IF*);
        WITH s^ DO
            Obtain (access);
            INC (value);
            holder := 0;
            IF value <= 0 THEN
                DumpString ("Signal: current task ");  DumpCard (CAST(CARDINAL,CurrentTaskID()));
                DumpEOL;
                DumpSemaphoreState (s);
                WITH BlockedList DO
                    p := head;  head := p^.next;
                    IF head = NIL THEN tail := NIL END(*IF*);
                    ThreadToUnblock := p^.ThreadID;
                    DISPOSE (p);
                END (*WITH*);

                (* The recursion below is to handle the possibility     *)
                (* that we're trying to resume a task that no longer    *)
                (* exists.  This is not a common situation, but it      *)
                (* does occur during program shutdown.                  *)

                IF NOT ResumeTask (ThreadToUnblock, access) THEN
                    Signal (s);
                END (*IF*);
            ELSE
                Release (access);
            END (*IF*);
        END (*WITH*);
    END Signal;

(************************************************************************)

PROCEDURE SemaphoreHolder (s: Semaphore): TaskID;

    (* Returns the Task ID of the current holder of s.  The result is   *)
    (* 0 if there is no current holder.                                 *)

    BEGIN
        RETURN CAST(TaskID,s^.holder);
    END SemaphoreHolder;

END Semaphores.

