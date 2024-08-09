SUB1    CEEENTRY PPA=MAINPPA,AUTO=WORKSIZE,MAIN=NO,BASE=R10    @ZV!CHNG
        USING    WORKAREA,R13                                  @ZV!CHNG
*
        LA       R2,HERE_MSG                                   @ZV!CHNG
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
        CEETERM RC=0                                           @ZV!CHNG
DFHEISTG DSECT
OUTAREA  DS    CL200               DATA OUTPUT AREA
*
EIASM    CSECT ,
         MVC  OUTAREA(40),MSG1
         MVC  OUTAREA(4),EIBTRMID
         EXEC CICS SEND TEXT FROM(OUTAREA) LENGTH(43) FREEKB ERASE
         EXEC CICS RECEIVE
         MVC  OUTAREA(13),MSG2
         EXEC CICS SEND TEXT FROM(OUTAREA) LENGTH(13) FREEKB ERASE
         EXEC CICS RETURN
         EXEC SQL UPDATE DSN8910.DEPT                                  X
                  SET MGRNO = :MGRNUM                                  X
                  WHERE DEPTNO = :INTDEPT
*
MSG1     DC   C'xxxx: ASM program invoked. ENTER TO END.'
MSG2     DC   C'PROGRAM ENDED'
* =========================================================
*      Constants and workarea definitions
* =========================================================
MOUT       DC     V(CEEMOUT)                                   @ZV!CHNG
*
HERE_MSG   DS     0F                                           @ZV!CHNG
           DC     AL2(HERE_END-HERE_START)                     @ZV!CHNG
HERE_START DC     C'>>>>>>>>>In SUB1 now.'                     @ZV!CHNG
HERE_END   EQU    *                                            @ZV!CHNG
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
           CEECAA                                              @ZV!CHNG
*
R0         EQU    0                                            @ZV!CHNG
R1         EQU    1                                            @ZV!CHNG
R2         EQU    2                                            @ZV!CHNG
R3         EQU    3                                            @ZV!CHNG
R4         EQU    4                                            @ZV!CHNG
R5         EQU    5                                            @ZV!CHNG
R6         EQU    6                                            @ZV!CHNG
R7         EQU    7                                            @ZV!CHNG
R8         EQU    8                                            @ZV!CHNG
R9         EQU    9                                            @ZV!CHNG
R10        EQU    10                                           @ZV!CHNG
R11        EQU    11                                           @ZV!CHNG
R12        EQU    12                                           @ZV!CHNG
R13        EQU    13                                           @ZV!CHNG
R14        EQU    14                                           @ZV!CHNG
R15        EQU    15                                           @ZV!CHNG
*
           END    SUB1                                         @ZV!CHNG 