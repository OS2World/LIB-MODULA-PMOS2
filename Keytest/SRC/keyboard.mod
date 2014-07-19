IMPLEMENTATION MODULE Keyboard;

        (****************************************************************)
        (*                                                              *)
        (*                      Keyboard Input                          *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        19 December 1997                        *)
        (*  Status:             Working                                 *)
        (*                                                              *)
        (****************************************************************)

(************************************************************************)
(*                                                                      *)
(*  To allow the keyboard user to type ahead, this module contains a    *)
(*  task which puts characters into a circular buffer, where they are   *)
(*  kept until picked up by a call to InKey.  (There are already        *)
(*  type-ahead facilities in the operating system, and also in the      *)
(*  keyboard hardware itself; but doing things this way makes it        *)
(*  easier to provide a "hot key" facility.)                            *)
(*                                                                      *)
(*  As a protection against deadlock, there is a timeout on the         *)
(*  "circular buffer full" condition.  If the buffer remains full for   *)
(*  too long, the oldest character in the buffer is discarded to make   *)
(*  room for the newest character.                                      *)
(*                                                                      *)
(************************************************************************)

IMPORT OS2;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM CircularBuffers IMPORT
    (* type *)  CircularBuffer,
    (* proc *)  CreateBuffer, PutBufferImpatient, GetBuffer, BufferEmpty;

(************************************************************************)
(*                               GLOBALS                                *)
(************************************************************************)

CONST

    (* Codes to specify the keyboard indicator lights.  *)

    ScrollLockLED = 1;
    NumLockLED = 2;
    CapsLockLED = 4;

(************************************************************************)
(*                      THE 'HOT KEY' TABLES                            *)
(************************************************************************)

TYPE CharSet = SET OF CHAR;

VAR HotKeys, HotFunctionKeys: CharSet;
    HotKeySemaphore: ARRAY CHAR OF Semaphore;
    HotFunctionKeySemaphore: ARRAY CHAR OF Semaphore;

(************************************************************************)
(*                      THE CHARACTER BUFFER                            *)
(************************************************************************)

CONST
    CharBufferSize = 8;

VAR
    (* CharBuffer is a circular buffer holding characters.      *)

    CharBuffer: CircularBuffer;

    (* The state of the three "lock" conditions.  *)

    CapsLock, NumLock, ScrollLock: BOOLEAN;

    (* A variable that is set unless the process is detached. *)

    ProcessIsNotDetached: BOOLEAN;

(************************************************************************)
(*         PUTTING KEYBOARD CHARACTERS INTO THE CIRCULAR BUFFER         *)
(************************************************************************)

PROCEDURE PutCode (FunctionKey: BOOLEAN;  code: CHAR);

    (* Puts a code into CharBuffer - unless it is a hot key, in which   *)
    (* case it is dealt with immediately.                               *)

    BEGIN
        IF FunctionKey THEN
            IF code IN HotFunctionKeys THEN
                Signal (HotFunctionKeySemaphore[code]);
            ELSE
                PutBufferImpatient (CharBuffer, CHR(0), 2000);
                PutBufferImpatient (CharBuffer, code, 2000);
            END (*IF*);
        ELSE
            IF code IN HotKeys THEN
                Signal (HotKeySemaphore[code]);
            ELSE
                PutBufferImpatient (CharBuffer, code, 2000);
            END (*IF*);
        END (*IF*);
    END PutCode;

(************************************************************************)

PROCEDURE InputTask;

    (* This procedure, which is run as a separate task, picks up the    *)
    (* keyboard input and stores it in CharBuffer.                      *)

    VAR KeyData: OS2.KBDKEYINFO;
        result: CHAR;

    BEGIN
        LOOP
            OS2.KbdCharIn (KeyData, 0, 0);
            result := KeyData.chChar;
            IF (result = CHR(0)) OR (result = CHR(224)) THEN
                PutCode (TRUE, KeyData.chScan);
            ELSE
                PutCode (FALSE, result);
            END (*IF*);
        END (*LOOP*);
    END InputTask;

(************************************************************************)
(*                          THE PUTBACK BUFFER                          *)
(************************************************************************)

MODULE PutBackBuffer;

    (* Implementation of the PutBack procedure poses some awkward       *)
    (* problems, to the point where it would not be worth implementing  *)
    (* if it were not such a useful operation.  The obvious solution,   *)
    (* of stuffing characters back into the character buffer, creates   *)
    (* deadlock if we try to avoid losing characters, and creates some  *)
    (* critical section problems even if we accept the risk of losing   *)
    (* characters.  The critical section problems can easily be solved, *)
    (* but only at the cost of making input less efficient, and this    *)
    (* is hard to justify given that PutBack operations will typically  *)
    (* be infrequent.  (That is, it is undesirable to cripple the       *)
    (* "normal" case just for the sake of supporting a special case     *)
    (* which accounts for just a small proportion of total operations). *)
    (* The solution adopted in this version is to have a separate data  *)
    (* structure to hold the characters which are put back.  These      *)
    (* characters are held in a "lossy stack" - we discard the oldest   *)
    (* datum whenever the stack is going to overflow.                   *)

    EXPORT
        (* var  *)  SomeCharsSaved,
        (* proc *)  Push, Pop;

    CONST
        stacksize = 8;

    VAR
        SomeCharsSaved: BOOLEAN;
        stackptr: [0..stacksize];
        stack: ARRAY [1..stacksize] OF CHAR;

    (********************************************************************)

    PROCEDURE Push (ch: CHAR);

        (* Pushes ch onto the stack.  If the stack is already full, the *)
        (* character at the bottom of the stack is lost.                *)

        VAR j: [1..stacksize];

        BEGIN
            IF stackptr = stacksize THEN
                FOR j := 1 TO stacksize-1 DO
                    stack[j] := stack[j+1];
                END (*FOR*);
                stack[stacksize] := ch;
            ELSE
                INC (stackptr);  stack[stackptr] := ch;
                SomeCharsSaved := TRUE;
            END (*IF*);
        END Push;

    (********************************************************************)

    PROCEDURE Pop(): CHAR;

        (* Returns the character from the top of the stack.     *)

        VAR result: CHAR;

        BEGIN
            result := stack[stackptr];  DEC(stackptr);
            SomeCharsSaved := stackptr > 0;
            RETURN result;
        END Pop;

    (********************************************************************)

    BEGIN
        SomeCharsSaved := FALSE;
        stackptr := 0;
    END PutBackBuffer;

(************************************************************************)
(*              THE EXTERNALLY CALLABLE INPUT PROCEDURES                *)
(************************************************************************)

PROCEDURE KeyPressed(): BOOLEAN;

    (* Returns TRUE iff a character is available. *)

    BEGIN
        RETURN SomeCharsSaved OR NOT BufferEmpty(CharBuffer);
    END KeyPressed;

(************************************************************************)

PROCEDURE InKey(): CHAR;

    (* Reads one key from the keyboard, or from the putback      *)
    (* buffer if any characters have been put back.              *)

    BEGIN
        IF SomeCharsSaved THEN
            RETURN Pop()
        ELSE
            RETURN GetBuffer (CharBuffer);
        END (*IF*);
    END InKey;

(************************************************************************)

PROCEDURE PutBack (ch: CHAR);

    (* This is an "un-read" operation, i.e. the character ch will       *)
    (* re-appear on the next call to InKey.  This facility is provided  *)
    (* for the use of software which can overshoot by one character     *)
    (* when reading its input - a situation which can often occur.      *)

    BEGIN
        Push (ch);
    END PutBack;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer (ch: CHAR);

    (* Stores ch as if it had come from the keyboard, so that a         *)
    (* subsequent InKey() will pick it up.                              *)

    BEGIN
        PutCode (FALSE, ch);
    END StuffKeyboardBuffer;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer2 (ch: CHAR);

    (* Like StuffKeyboardBuffer, but stores a two-byte sequence: Nul    *)
    (* followed by ch.                                                  *)

    BEGIN
        PutCode (TRUE, ch);
    END StuffKeyboardBuffer2;

(************************************************************************)

PROCEDURE SetLocks (code: CARDINAL);

    (* Set/clear the caps lock, num lock, and scroll lock conditions.   *)
    (* The code is defined in KBDRIVER.DEF.                             *)

    BEGIN
        CapsLock := ORD(IAND (code, CapsLockLED)) <> 0;
        NumLock := ORD(IAND (code, NumLockLED)) <> 0;
        ScrollLock := ORD(IAND (code, ScrollLockLED)) <> 0;
        (*
        ClearLED (CapsLockLED+NumLockLED+ScrollLockLED);
        ToggleLED (BYTE(code));
        *)
    END SetLocks;

(************************************************************************)

PROCEDURE LockStatus (): CARDINAL;

    (* Returns the current state of the caps lock, num lock, and scroll *)
    (* lock conditions, using the code defined in KBDRIVER.DEF.         *)

    (* NOT YET IMPLEMENTED *)

    VAR result: CARDINAL;

    BEGIN
        result := 0;
        IF CapsLock THEN result := CapsLockLED END(*IF*);
        IF NumLock THEN INC (result, NumLockLED) END(*IF*);
        IF ScrollLock THEN INC (result, ScrollLockLED) END(*IF*);
        RETURN result;
    END LockStatus;

(************************************************************************)

PROCEDURE HotKey (FunctionKey: BOOLEAN;  code: CHAR;  S: Semaphore);

    (* After this procedure is called, typing the key combination for   *)
    (* 'code' will cause a Signal(S).  Set FunctionKey=TRUE to trap one *)
    (* of the two-character special function keys, and FALSE otherwise. *)
    (* The character is consumed; if it should be passed on, then the   *)
    (* user's hot key handler can do a PutBack().  Note: there is no    *)
    (* provision for having multiple hot key handlers for the same key; *)
    (* any existing hot key mapping will be overridden.                 *)

    BEGIN
        IF FunctionKey THEN
            INCL (HotFunctionKeys, code);
            HotFunctionKeySemaphore[code] := S;
        ELSE
            INCL (HotKeys, code);
            HotKeySemaphore[code] := S;
        END (*IF*);
    END HotKey;

(************************************************************************)
(*                     CHECK FOR DETACHED MODE                          *)
(************************************************************************)

PROCEDURE NotDetached(): BOOLEAN;

    (* Returns TRUE unless called by a process running detached.        *)
    (* (A detached process may not do keyboard, screen, or mouse I/O.)  *)

    BEGIN
        RETURN ProcessIsNotDetached;
    END NotDetached;

(************************************************************************)

PROCEDURE DetachCheck;

    (* Sets the variable ProcessIsNotDetached. *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        ProcessIsNotDetached := pPib^.pib_ultype <> 4;
    END DetachCheck;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

BEGIN
    HotKeys := CharSet{};
    HotFunctionKeys := CharSet{};
    CreateBuffer (CharBuffer, CharBufferSize);
    DetachCheck;
    IF ProcessIsNotDetached THEN
        (*SetLocks (0);*)
        CreateTask (InputTask, 8, "Keyboard main");
    END (*IF*);
END Keyboard.

