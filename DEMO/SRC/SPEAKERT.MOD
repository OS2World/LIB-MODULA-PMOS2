MODULE SpeakerTest;

        (********************************************************)
        (*                                                      *)
        (*           Test of "PC speaker" music                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        14 October 1996                 *)
        (*  Status:             Just started                    *)
        (*                                                      *)
        (********************************************************)

FROM MusicDemonstration IMPORT
    (* proc *)  WaitForEndOfMusic;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

BEGIN
    WaitForEndOfMusic;
END SpeakerTest.

