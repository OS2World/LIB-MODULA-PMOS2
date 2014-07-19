MODULE WCTest;

        (********************************************************)
        (*                                                      *)
        (*             Test of the WildCard module              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            10 June 1999                    *)
        (*  Last edited:        28 March 2003                   *)
        (*  Status:             Apparently working              *)
        (*                                                      *)
        (*     Now doing a test of WildMatchS                   *)
        (*                                                      *)
        (********************************************************)

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM WildCard IMPORT
    (* type *)  CharSet,
    (* proc *)  WildMatch, WildMatchS;

(************************************************************************)
(*                          TEST CODE                                   *)
(************************************************************************)

PROCEDURE TestPair (input, template: ARRAY OF CHAR);

    CONST Stoppers = CharSet{'/'};

    BEGIN
        WriteString (input);
        IF WildMatchS (input, template, Stoppers) THEN
            WriteString (" matches ");
        ELSE
            WriteString (" doesn't match ");
        END (*IF*);
        WriteString ("template ");  WriteString (template);
        WriteLn;
    END TestPair;

(************************************************************************)

PROCEDURE RunTheTest;

    BEGIN
        WriteString ("Testing wildcard matches");  WriteLn;
        TestPair ("a", "*a");
        TestPair ("a", "A");
        TestPair ("a", "b*");
        TestPair ("a", "*");
        TestPair ("ab", "*b*");
        TestPair ("a", "**");
        TestPair ("a", "**b");
        TestPair ("xyabuvdefabcmmdefmmm", "**abc*def*");
        TestPair ("xyz\abc", "%\ABC");
        TestPair ("xyz\abc", "%\%");
        TestPair ("xyz/abc", "%/ABC");
        TestPair ("xyz/abc", "x%/%c");
        TestPair ("xyz", "x%/%z");
        TestPair ("a/b/c", "%/%/%");
    END RunTheTest;

(************************************************************************)
(*                            MAIN PROGRAM                              *)
(************************************************************************)

BEGIN
    RunTheTest;
END WCTest.

