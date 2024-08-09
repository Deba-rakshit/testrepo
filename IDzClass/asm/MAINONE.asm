MAIN1   CEEENTRY PPA=MAINPPA,AUTO=WORKSIZE,MAIN=YES,BASE=R10   @ZV!CHNG
        USING    WORKAREA,R13                                  @ZV!CHNG
*
*       Call CEEMOUT to announce calling the subroutine
*       New File
*
        LA       R2,CALLING_MSG                                @ZV!CHNG
        LA       R3,DEST                                       @ZV!CHNG
        LA       R4,FBCODE                                     @ZV!CHNG
        STM      R2,R4,PLIST                                   @ZV!CHNG
*
        LA       R1,PLIST                                      @ZV!CHNG
        L        R15,MOUT                                      @ZV!CHNG
        BALR     R14,R15                                       @ZV!CHNG
*
*       Invoke the subroutine
*
        LA       R1,0               No parameters              @ZV!CHNG
        L        R15,SUBROUTINE                                @ZV!CHNG
        BALR     R14,R15            Just Do It.                @ZV!CHNG

*
*       Call CEEMOUT to announce the return
*
        LA       R2,RETURN_MSG                                 @ZV!CHNG
        LA       R3,DEST                                       @ZV!CHNG
        LA       R4,FBCODE                                     @ZV!CHNG
        STM      R2,R4,PLIST                                   @ZV!CHNG
*
        LA       R1,PLIST                                      @ZV!CHNG
        L        R15,MOUT                                      @ZV!CHNG
        BALR     R14,R15                                       @ZV!CHNG
*
*       Go away
*
        CEETERM  RC=0                                          @ZV!CHNG
* =========================================================
*      Constants and workarea definitions
* =========================================================
MOUT          DC     V(CEEMOUT)                                @ZV!CHNG
SUBROUTINE    DC     V(SUB1)                                   @ZV!CHNG
*
* Messages for CEEMOUT
*
CALLING_MSG   DS     0F                                        @ZV!CHNG
              DC     AL2(CALLING_END-CALLING_START)            @ZV!CHNG
CALLING_START DC     C'Calling the subroutine now....'         @ZV!CHNG
CALLING_END   EQU    *                                         @ZV!CHNG
*
RETURN_MSG    DS     0F                                        @ZV!CHNG
              DC     AL2(RETURN_END-RETURN_START)              @ZV!CHNG
RETURN_START  DC     C'Successfully returned from the subroutine.'
RETURN_END    EQU    *                                         @ZV!CHNG
*
DEST       DC     F'2'            Destination is the MSGFILE   @ZV!CHNG
*
MAINPPA    CEEPPA                                              @ZV!CHNG
*
WORKAREA   DSECT                                               @ZV!CHNG
           ORG    *+CEEDSASZ             Leave space for DSA Fixed area
PLIST      DS     0D                                           @ZV!CHNG
PARM1      DS     A                                            @ZV!CHNG
PARM2      DS     A                                            @ZV!CHNG
PARM3      DS     A                                            @ZV!CHNG
PARM4      DS     A                                            @ZV!CHNG
PARM5      DS     A                                            @ZV!CHNG
           DS     0D                                           @ZV!CHNG
FBCODE     DS     3F                                           @ZV!CHNG
*
           DS     0D                                           @ZV!CHNG
WORKSIZE   EQU    *-WORKAREA                                   @ZV!CHNG
           CEEDSA SECTYPE=ALL                                  @ZV!CHNG
           CEECAA ,                                            @ZV!CHNG
*
           REGEQU ,                                            @ZV!CHNG
*
           END    MAIN1                                        @ZV!CHNG