MODULE ERETest;

        (********************************************************)
        (*                                                      *)
        (*              Test of the RegExp module               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 August 2014                   *)
        (*  Last edited:        20 August 2014                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn, ReadString, SkipLine;

FROM RegExp IMPORT
    (* proc *)  EREMatch;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

(************************************************************************)
(*                            TEST CODE                                 *)
(************************************************************************)

PROCEDURE TestPair (input, template: ARRAY OF CHAR);

    CONST Stoppers = CharSet{'/'};

    BEGIN
        IF EREMatch (input, template) THEN
            WriteString (input);
            WriteString (" matches ");
        ELSE
            WriteString (input);
            WriteString (" doesn't match ");
        END (*IF*);
        WriteString ("template ");  WriteString (template);
        WriteLn;
    END TestPair;

(************************************************************************)

PROCEDURE RunSimpleTests;

    BEGIN
        WriteString ("Testing regular expression matches");  WriteLn;
        TestPair ("a", "a");
        TestPair ("a", "A");
        TestPair ("abc", "a.c");
        TestPair ("xyz", "x[uvw]z");
        TestPair ("xyz", "x[uvwxyz]z");
        TestPair ("xyz", "x[u-z]z");
        TestPair ("xyz", "x[^u-z]z");
        TestPair ("xyz", "x[^a-c]z");
        TestPair ("xyz", "abc|xy.");
        TestPair ("aaaaa", "a*a");
        TestPair ("a", "b*a");
        TestPair ("a", "a*a");
        TestPair ("a", "a*a{1,3}");
        TestPair ("aefg", "a(bcd|ef)g");
        TestPair ("abcdef", "a(bcdef|bcd)ef");
        TestPair ("a(b)c", ".\(b\)c?");
        TestPair ("The 42 quick brown foxes", "[ A-Za-z0-9]+");
    END RunSimpleTests;

(************************************************************************)

PROCEDURE UseUserInput;

    CONST Nul = CHR(0);

    VAR template, string, newinput: ARRAY [0..2047] OF CHAR;

    BEGIN
        template := "";
        string := "";
        WriteLn;
        WriteString ("Now you can enter your own templates and test strings.");
        WriteLn;
        WriteString ("Enter a null string to reuse the previous template or string.");
        WriteLn;
        WriteString ("Type CTRL/C to exit.");  WriteLn;

        LOOP        (* forever *)
            WriteString ("template: ");
            ReadString (newinput);  SkipLine;
            IF newinput[0] <> Nul THEN
                template := newinput;
            END (*IF*);
            WriteString ("test string: ");
            ReadString (newinput);  SkipLine;
            IF newinput[0] <> Nul THEN
                string := newinput;
            END (*IF*);
            TestPair (string, template);
        END (*LOOP*);

    END UseUserInput;

(************************************************************************)
(*                            MAIN PROGRAM                              *)
(************************************************************************)

BEGIN
    RunSimpleTests;
    UseUserInput;
END ERETest.

