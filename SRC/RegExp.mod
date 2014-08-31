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

IMPLEMENTATION MODULE RegExp;

        (********************************************************)
        (*                                                      *)
        (*       String matching with regular expression        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 August 2014                   *)
        (*  Last edited:        20 August 2014                  *)
        (*  Status:             Appears to be working           *)
        (*                                                      *)
        (********************************************************)


FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)
(*                                                                      *)
(*                                THE RULES                             *)
(*                                                                      *)
(* SPECIAL ASSUMPTIONS:                                                 *)
(*                                                                      *)
(*  1.  We are not looking at multiline inputs, therefore we do not     *)
(*      expect to find any line termination characters.                 *)
(*  2.  We are looking for complete string matches, not substring       *)
(*      matches, therefore the ^ and $ metacharacters (to specify start *)
(*      and end of the string) are not implemented.  If we find either  *)
(*      of those characters in a template, we ignore it.                *)
(*                                                                      *)
(* THE METACHARACTERS:                                                  *)
(*                                                                      *)
(*  .   matches any single character                                    *)
(*  []  matches any single character inside the brackets. I am assuming *)
(*         that these brackets cannot be nested.                        *)
(*  [^] matches any single character that is _not_ inside the brackets. *)
(*  ()  matches the regular expression inside the parentheses.          *)
(*                                                                      *)
(*      ----------------------------------------------------------      *)
(*      I am assuming (the public documentation is unclear on this      *)
(*      point) that the above four cases define an "element" for        *)
(*      the purposes of the next four definitions.                      *)
(*      ----------------------------------------------------------      *)
(*                                                                      *)
(*  *   matches the preceding element zero or more times.               *)
(*  ?   matches the preceding element zero or one times.                *)
(*  +   matches the preceding element one or more times.                *)
(*  {m,n}  matches the preceding element at least m but not more than   *)
(*      n times.                                                        *)
(*                                                                      *)
(*      ----------------------------------------------------------      *)
(*      Next, I assume that a "term" is any linear string of            *)
(*      elements, with each element optionally qualified by one         *)
(*      (but no more) of the qualifiers * ? + {m,n}.                    *)
(*      ----------------------------------------------------------      *)
(*                                                                      *)
(*  |   matches the term before the operator or the regular             *)
(*      expression after the operator.                                  *)
(*                                                                      *)
(* ESCAPED CHARACTERS:                                                  *)
(*                                                                      *)
(*  \c  where c is any single character, matches c even if it happens   *)
(*      to be a metacharacter.  This is not quite the same as the       *)
(*      PCRE or Posix ERE rule, but it seems to me to be just as        *)
(*      useful and easier to understand.                                *)
(*                                                                      *)
(* All of these rules appear straightforward. The tricky part lies in   *)
(* knowing how to back up when a rule fails, because the effect of the  *)
(* above rules is to specify a nondeterministic automaton.              *)
(*                                                                      *)
(* Theoretically, on can convert a nondeterministic finite automaton to *)
(* a deterministic one, but the explosion in the number of states means *)
(* that I'd rather not go down that path.  In fact my implementation    *)
(* does not even use a nondeterministic finite automaton, although it   *)
(* is logically equivalent to one.                                      *)
(*                                                                      *)
(************************************************************************)


TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    EmptySet = CharSet{};
    AllChars = CharSet{CHR(0)..CHR(255)};
    Digits = CharSet{'0'..'9'};

TYPE
    NodeKind = (normal, repeatnode, ORnode);

    (* An RElist is basically a linear list of elements, although   *)
    (* some of these elements have sublists.  A normal node just    *)
    (* specifies a set of characters to match.  An OR node has two  *)
    (* regular expressions (which may of course include more OR     *)
    (* nodes) hanging off it, to represent an alternative.  The     *)
    (* found1 flag in an OR node is set if we have matched the      *)
    (* first alternative but not yet tried the second alternative.  *)
    (* That flag is of course cleared if we have to back up to      *)
    (* an earlier node.                                             *)

    (* The repeat node is the most complicated.  It represents      *)
    (* something that can be repeated between minrpt and maxrpt     *)
    (* times.  Each time we successfully process this node, we      *)
    (* set nextcount to be one less than the number of matches we   *)
    (* found this time.  (So we are using a "longest match first"   *)
    (* strategy.)  The previpos value is the position in the input  *)
    (* string where we last did this match.  If that has changed,   *)
    (* we go back to looking for the longest possible match.  The   *)
    (* nextcount field is also reset if our search has to back up   *)
    (* to a point earlier in the template than this node.           *)

    RElist = POINTER TO
                    RECORD
                        next: RElist;
                        CASE kind: NodeKind OF

                            normal:     chars: CharSet;
                           |
                            repeatnode:
                                        minrpt, maxrpt: CARDINAL;
                                        previpos, nextcount: CARDINAL;
                                        string: RElist;
                           |
                            ORnode:
                                        part1, part2: RElist;
                                        found1: BOOLEAN;

                        END (*CASE*);
                    END (*RECORD*);

(************************************************************************)
(*                         ERROR MESSAGES ETC.                          *)
(************************************************************************)

(*
PROCEDURE NYI (procname: ARRAY OF CHAR);

    (* Writes a "not yet implemented" message. *)

    BEGIN
        WriteString (procname);
        WriteString (" is not yet implemented.");
        WriteLn;
    END NYI;
*)

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes N in decimal to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;

(************************************************************************)

PROCEDURE TemplateError (msg: ARRAY OF CHAR;  tpos: CARDINAL);

    (* Reports a malformed regular expression. *)

    BEGIN
        WriteString ("ERROR: ");
        WriteString (msg);
        WriteString (" at character position ");
        WriteCard (tpos);  WriteLn;
    END TemplateError;

(************************************************************************)

PROCEDURE ReadCard (VAR (*IN*) str: ARRAY OF CHAR;
                    VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Reads a decimal number starting at str[pos], updates pos.    *)

    VAR result: CARDINAL;

    BEGIN
        result := 0;
        WHILE str[pos] IN Digits DO
            result := 10*result + ORD(str[pos]) - ORD('0');
            INC (pos);
        END (*WHILE*);
        RETURN result;
    END ReadCard;

(************************************************************************)
(*       CONVERTING A REGULAR EXPRESSION TO A DECISION STRUCTURE        *)
(************************************************************************)

PROCEDURE MakeStruct (VAR (*IN*) template: ARRAY OF CHAR;
                        VAR (*INOUT*) tpos: CARDINAL): RElist;  FORWARD;

    (* The general case, including repetitions. *)

(************************************************************************)

PROCEDURE Bracketed (VAR (*IN*) template: ARRAY OF CHAR;
                              VAR (*INOUT*) tpos: CARDINAL): RElist;

    (* Called in the case where template[tpos] = '['.   *)
    (* Returns a single node.                           *)

    VAR node: RElist;  set: CharSet;
        prevch, ch: CHAR;
        invert: BOOLEAN;

    BEGIN
        invert := FALSE;
        prevch := Nul;
        set := EmptySet;
        IF template[tpos] = '^' THEN
            invert := TRUE;  INC (tpos);
        END (*IF*);
        WHILE (tpos <= HIGH(template)) AND (template[tpos] <> Nul)
                                        AND (template[tpos] <> ']') DO
            ch := template[tpos];  INC(tpos);
            IF ch = '-' THEN
                IF (prevch = Nul) OR (template[tpos] = ']') THEN

                    (* Interpret the '-' literally. *)

                    INCL (set, ch);
                ELSE
                    (* We have a range. *)

                    ch := template[tpos];  INC(tpos);
                    set := set + CharSet{prevch..ch};
                END (*IF*);
            ELSE
                INCL (set, ch);
            END (*IF*);
            prevch := ch;
        END (*WHILE*);
        IF template[tpos] = ']' THEN
            INC (tpos);
        ELSE
            TemplateError ("missing ']'", tpos);
        END (*IF*);
        IF invert THEN
            set := AllChars - set;
        END (*IF*);

        (* Create the new node. *)

        NEW (node);
        node^.next := NIL;
        node^.kind := normal;
        node^.chars := set;

        RETURN node;

    END Bracketed;

(************************************************************************)

PROCEDURE MakeBasic (VAR (*IN*) template: ARRAY OF CHAR;
                                VAR (*INOUT*) tpos: CARDINAL): RElist;

    (* Processes template starting at template[tpos], finishes when     *)
    (* meeting an unmatched ')' or end of string.  Updates tpos.        *)
    (* Returns the head and the tail of the resulting structure.        *)

    (* This is the version that handles only a single character         *)
    (* expression, or a [] expression, or a () subexpression.           *)
    (* Repetition specifications like * are explicitly excluded; we     *)
    (* handle those in the next procedure.                              *)

    VAR ch: CHAR;
        head: RElist;

    BEGIN
        head := NIL;
        IF (tpos <= HIGH(template)) AND (template[tpos] <> Nul)
                                        AND (template[tpos] <> ')') THEN
            ch := template[tpos];
            INC (tpos);
            IF (ch = '^') OR (ch = '$') THEN

                (* Do nothing; ignore this character. *)

            ELSIF ch = '(' THEN

                (* Parenthesised subexpression. *)

                head := MakeStruct (template, tpos);
                IF template[tpos] = ')' THEN
                    INC (tpos);
                ELSE
                    TemplateError ("')' expected", tpos);
                END (*IF*);

            ELSIF ch = '[' THEN

                head := Bracketed (template, tpos);

            ELSIF ch = '\' THEN

                (* Escape code denoting single character. *)

                ch := template[tpos];
                IF ch = Nul THEN
                    TemplateError ("'\' as last character of template", tpos);
                ELSE
                    INC (tpos);
                    NEW (head);
                    head^.next := NIL;
                    head^.kind := normal;
                    head^.chars := CharSet{ch};
                END (*IF*);

            ELSE
                (* Character or set of characters. *)

                NEW (head);
                head^.next := NIL;
                head^.kind := normal;
                IF ch = '.' THEN
                    head^.chars := AllChars;
                ELSE
                    head^.chars := CharSet{ch};
                END (*IF*);

            END (*IF*);

        END (*WHILE*);

        RETURN head;

    END MakeBasic;

(************************************************************************)

PROCEDURE MakeRepeatSection (substring: RElist; min, max: CARDINAL): RElist;

    (* Makes a new node representing a "repeat this" pattern. *)

    VAR node: RElist;

    BEGIN
        NEW (node);
        node^.next := NIL;
        node^.kind := repeatnode;
        node^.minrpt := min;
        node^.maxrpt := max;
        node^.previpos := 0;
        node^.nextcount := max;
        node^.string := substring;
        RETURN node;
    END MakeRepeatSection;

(************************************************************************)

PROCEDURE MakeTermStruct (VAR (*IN*) template: ARRAY OF CHAR;
                                VAR (*INOUT*) tpos: CARDINAL): RElist;

    (* Processes template starting at template[tpos], finishes when     *)
    (* meeting an '|' or an unmatched ')' or end of string.  Updates    *)
    (* tpos.  Returns the head of the resulting structure.              *)

    VAR head, tail: RElist;

    (********************************************************************)

    PROCEDURE AppendNew (newlist: RElist);

        (* Appends the newlist list to the main list. *)

        BEGIN
            IF newlist <> NIL THEN
                IF head = NIL THEN
                    head := newlist;
                ELSE
                    tail^.next := newlist;
                END (*IF*);
                tail := newlist;
                WHILE tail^.next <> NIL DO
                    tail := tail^.next;
                END (*WHILE*)
            END (*IF*);
        END AppendNew;

    (********************************************************************)

    CONST stoppers = CharSet {Nul, '|', ')'};

    VAR newlist: RElist;
        m, n: CARDINAL;

    BEGIN
        head := NIL;
        tail := NIL;
        WHILE (tpos <= HIGH(template))
                            AND NOT (template[tpos] IN stoppers) DO

            newlist := MakeBasic (template, tpos);
            IF template[tpos] = '*' THEN
                INC (tpos);
                AppendNew (MakeRepeatSection (newlist, 0, MAX(CARDINAL)));
            ELSIF template[tpos] = '+' THEN
                INC (tpos);
                AppendNew (MakeRepeatSection (newlist, 1, MAX(CARDINAL)));
            ELSIF template[tpos] = '?' THEN
                INC (tpos);
                AppendNew (MakeRepeatSection (newlist, 0, 1));
            ELSIF template[tpos] = '{' THEN
                INC (tpos);
                m := ReadCard (template, tpos);
                IF template[tpos] = ',' THEN INC(tpos) END(*IF*);
                n := ReadCard (template, tpos);
                IF template[tpos] = '}' THEN INC(tpos) END(*IF*);
                AppendNew (MakeRepeatSection (newlist, m, n));
            ELSE
                AppendNew (newlist);
            END (*IF*);
        END (*WHILE*);

        RETURN head;

    END MakeTermStruct;

(************************************************************************)

PROCEDURE MakeORnode (part1, part2: RElist): RElist;

    (* Makes a new node representing a "part1 | part2" pattern. *)

    VAR node: RElist;

    BEGIN
        NEW (node);
        node^.next := NIL;
        node^.part1 := part1;
        node^.kind := ORnode;
        node^.part2 := part2;
        node^.found1 := FALSE;
        RETURN node;
    END MakeORnode;

(************************************************************************)

PROCEDURE MakeStruct (VAR (*IN*) template: ARRAY OF CHAR;
                             VAR (*INOUT*) tpos: CARDINAL): RElist;

    (* Processes template starting at template[tpos], finishes when     *)
    (* meeting an unmatched ')' or end of string.  Updates tpos.        *)
    (* Returns the head of the resulting structure.                     *)

    VAR head, head2: RElist;

    BEGIN
        head := MakeTermStruct (template, tpos);
        WHILE template[tpos] = '|' DO
            INC (tpos);
            head2 := MakeTermStruct (template, tpos);
            head := MakeORnode (head, head2);
        END (*WHILE*);
        RETURN head;
    END MakeStruct;

(************************************************************************)

PROCEDURE MakeDS (VAR (*IN*) template: ARRAY OF CHAR;
                        VAR (*INOUT*) tpos: CARDINAL): RElist;

    (* Processes template starting at template[tpos], finishes when     *)
    (* meeting an unmatched ')' or end of string.  Updates tpos.        *)

    VAR head: RElist;

    BEGIN
        head := MakeStruct (template, tpos);
        IF (tpos <= HIGH(template)) AND (template[tpos] <> Nul) THEN
            TemplateError ("premature termination of regular expression", tpos);
        END (*IF*);
        RETURN head;
    END MakeDS;

(************************************************************************)
(*                   DISCARDING A DECISION STRUCTURE                    *)
(************************************************************************)

PROCEDURE Discard (VAR (*INOUT*) struct: RElist);

    (* Returns struct to the heap. *)

    VAR next: RElist;

    BEGIN
        WHILE struct <> NIL DO
            next := struct^.next;
            IF struct^.kind = ORnode THEN
                Discard (struct^.part1);
                Discard (struct^.part2);
            ELSIF struct^.kind = repeatnode THEN
                Discard (struct^.string);
            END (*IF*);
            DISPOSE (struct);
            struct := next;
        END (*WHILE*);
    END Discard;

(************************************************************************)
(*           CHECKING INPUT STRING AGAINST DECISION STRUCTURE           *)
(************************************************************************)

PROCEDURE ResetCounts (RE: RElist);

    (* Resets all the repeatnode counts to their maximum values, and    *)
    (* clears the found1 flag in OR nodes.                              *)

    VAR p: RElist;

    BEGIN
        p := RE;
        WHILE p <> NIL DO
            IF p^.kind = repeatnode THEN
                p^.nextcount := p^.maxrpt;
                ResetCounts (p^.string);
            ELSIF p^.kind = ORnode THEN
                p^.found1 := FALSE;
                ResetCounts (p^.part1);
                ResetCounts (p^.part2);
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
    END ResetCounts;

(************************************************************************)
(*                                                                      *)
(* The following procedures have an output parameter CanTryAgain.  If   *)
(* we return with CanTryAgain = TRUE, this means that there are still   *)
(* possible matches that have not yet been checked: if the caller's     *)
(* match failes at a higher level then it is still worth calling this   *)
(* procedure again.  When that parameter is labelled as an INOUT        *)
(* parameter, this means that that parameter might set CanTryAgain but  *)
(* will never clear it, so that the caller can accumulate the logical   *)
(* OR of all such results.                                              *)
(*                                                                      *)
(* The ipos parameter is the position in the input string.  It is       *)
(* updated when we find a match, but not updated on failure.            *)
(*                                                                      *)
(************************************************************************)

PROCEDURE DoMatch (VAR (*IN*) input: ARRAY OF CHAR;
                    RE: RElist;
                    VAR (*INOUT*) ipos: CARDINAL;
                    VAR (*OUT*) remainder: RElist;
                    VAR (*OUT*) CanTryAgain: BOOLEAN): BOOLEAN;
                                                                FORWARD;

    (* The general case. *)

(************************************************************************)

PROCEDURE ORMatch (VAR (*IN*) input: ARRAY OF CHAR;
                    RE: RElist;
                    VAR (*INOUT*) ipos: CARDINAL;
                    VAR (*OUT*) remainder: RElist;
                    VAR (*INOUT*) CanTryAgain: BOOLEAN): BOOLEAN;

    (* Handles the case where RE^.kind = ORnode.  *)

    VAR startpos: CARDINAL;

    BEGIN
        startpos := ipos;

        (* Note: we skip checking the first alternative if the  *)
        (* found1 flag is still set.                            *)

        IF (NOT RE^.found1) AND DoMatch (input, RE^.part1, ipos,
                            remainder, CanTryAgain) THEN
            RE^.found1 := TRUE;
            CanTryAgain := TRUE;
            RETURN TRUE;
        END (*IF*);

        (* If the first try failed or was skipped, try the  *)
        (* second alternative.                              *)

        RE^.found1 := FALSE;
        ipos := startpos;
        IF DoMatch (input, RE^.part2, ipos, remainder, CanTryAgain) THEN
            RETURN TRUE;
        ELSE
            ipos := startpos;
            RETURN FALSE;
        END (*IF*);

    END ORMatch;

(************************************************************************)

PROCEDURE RepeatMatch (VAR (*IN*) input: ARRAY OF CHAR;
                    RE: RElist;
                    VAR (*INOUT*) ipos: CARDINAL;
                    VAR (*OUT*) remainder: RElist;
                    VAR (*INOUT*) CanTryAgain: BOOLEAN): BOOLEAN;

    (* Handles the case where RE^.kind = repeatnode.                    *)
    (* If we return with CanTryAgain = TRUE, this means that there are  *)
    (* still possible matches that have not yet been checked, i.e. if   *)
    (* the caller's match failes at a higher level then it is still     *)
    (* worth calling this procedure again.  (Or, since this is an INOUT *)
    (* parameter, it might have already been set by the caller.)        *)

    VAR count, maxcount, savepos: CARDINAL;
        success: BOOLEAN;

    BEGIN
        (* Reset the downstream "try again" status. *)

        ResetCounts (RE^.next);
        ResetCounts (RE^.string);

        (* Check for one pathological case. *)

        IF RE^.maxrpt < RE^.minrpt THEN
            RETURN FALSE;
        END (*IF*);

        (* Set the maximum number of repeats to one less than   *)
        (* the last time we processed this node.  If, however,  *)
        (* we are at a different point in the input string,     *)
        (* restart at the maximum possible count.               *)

        IF ipos = RE^.previpos THEN
            maxcount := RE^.nextcount;
        ELSE
            RE^.previpos := ipos;
            maxcount := RE^.maxrpt;
        END (*IF*);
        count := 0;

        IF maxcount = 0 THEN

            (* Special case: if maxcount = 0 then our only  *)
            (* option is to match an empty string.          *)

            RETURN TRUE;

        END (*IF*);

        (* Try to match the pattern as often as possible, up to *)
        (* a maximum of maxcount times.                         *)

        LOOP
            savepos := ipos;
            IF DoMatch (input, RE^.string, ipos,
                                    remainder, CanTryAgain) THEN
                INC (count);
                IF count = maxcount THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSE
                ipos := savepos;
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        success := count >= RE^.minrpt;
        IF count > RE^.minrpt THEN
            DEC (count);
            CanTryAgain := TRUE;
        END (*IF*);
        RE^.nextcount := count;

        RETURN success;

    END RepeatMatch;

(************************************************************************)

PROCEDURE TryMatch (VAR (*IN*) input: ARRAY OF CHAR;
                    RE: RElist;
                    VAR (*INOUT*) ipos: CARDINAL;
                    VAR (*OUT*) remainder: RElist;
                    VAR (*OUT*) CanTryAgain: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff a string starting at input[ipos] matches a      *)
    (* leading part of the regular expression defined by RE.            *)
    (* If we have a match then ipos is updated and remainder points     *)
    (* to the part as yet unmatched.                                    *)

    VAR p: RElist;
        startpos: CARDINAL;

    BEGIN
        p := RE;
        startpos := ipos;
        CanTryAgain := FALSE;
        WHILE (p <> NIL) AND (ipos <= HIGH(input)) AND (input[ipos] <> Nul) DO
            CASE p^.kind OF
                normal:
                    (* Want to match current character. *)

                    IF input[ipos] IN p^.chars THEN
                        INC (ipos);
                    ELSE
                        ipos := startpos;
                        RETURN FALSE;
                    END (*IF*);
               |
                repeatnode:
                    IF NOT RepeatMatch (input, p, ipos, remainder, CanTryAgain) THEN
                        ipos := startpos;
                        RETURN FALSE;
                    END (*IF*);
               |
                ORnode:
                    IF NOT ORMatch (input, p, ipos, remainder, CanTryAgain) THEN
                        ipos := startpos;
                        RETURN FALSE;
                    END (*IF*);

            END (*CASE*);
            p := p^.next;
        END (*WHILE*);
        remainder := p;
        IF p = NIL THEN
            RETURN TRUE;
        ELSE
            ipos := startpos;
            RETURN FALSE;
        END (*IF*);
    END TryMatch;

(************************************************************************)

PROCEDURE DoMatch (VAR (*IN*) input: ARRAY OF CHAR;
                    RE: RElist;
                    VAR (*INOUT*) ipos: CARDINAL;
                    VAR (*OUT*) remainder: RElist;
                    VAR (*OUT*) CanTryAgain: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff a string starting at input[ipos] matches a      *)
    (* leading part of the regular expression defined by RE.            *)
    (* If we have a match then ipos is updated and remainder points     *)
    (* to the part as yet unmatched.                                    *)

    BEGIN
        CanTryAgain := FALSE;
        LOOP
            IF TryMatch (input, RE, ipos, remainder, CanTryAgain) THEN
                RETURN TRUE;
            ELSIF NOT CanTryAgain THEN
                RETURN FALSE;
            END (*IF*);
        END (*LOOP*);
    END DoMatch;

(************************************************************************)
(*                       THE USER-CALLABLE ROUTINE                      *)
(************************************************************************)

PROCEDURE EREMatch (VAR (*IN*) input, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff input matches the regular expression in         *)
    (* template, according to the regular expression rules of POSIX     *)
    (* Extended Regular Syntax.                                         *)

    VAR RE, remainder: RElist;
        txtpos: CARDINAL;  CanTryAgain, success: BOOLEAN;

    BEGIN
        txtpos := 0;
        RE := MakeDS (template, txtpos);
        txtpos := 0;
        success := DoMatch (input, RE, txtpos, remainder, CanTryAgain)
                    AND (remainder = NIL)
                    AND ((txtpos > HIGH(input)) OR (input[txtpos] = Nul));
        Discard (RE);
        RETURN success;
    END EREMatch;

(************************************************************************)

END RegExp.

