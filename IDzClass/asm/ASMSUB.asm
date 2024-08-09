ASMSUB  CEEENTRY PPA=MAINPPA,AUTO=WORKSIZE,MAIN=NO,BASE=R10
        USING    WORKAREA,R13
*
*       Obtain the single inbound parm
*
        L        R2,0(,R1)             R2 points to the parm
        L        R3,0(,R2)             R3 now has the parm
        SLA      R3,1(0)               Shift left 1 - multiply by 2
        ST       R3,0(,R2)             Save it back
*
        LA       R2,HERE_MSG
        LA       R3,DEST
        LA       R4,FBCODE
        STM      R2,R4,PLIST
*
        LA       R1,PLIST
        L        R15,MOUT
        BALR     R14,R15
*
*       Go away
*
        CEETERM RC=0
* =========================================================
*      Constants and workarea definitions
* =========================================================
MOUT       DC     V(CEEMOUT)
*
           DC     0F
HERE_MSG   DC     AL2(HERE_END-HERE_START)
HERE_START DC     C'>>>>>>>>>In ASMSUB now. Time to go.'
HERE_END   EQU    *
*
DEST       DC     F'2'            Destination is the MSGFILE
*
MAINPPA    CEEPPA
*
WORKAREA   DSECT
           ORG    *+CEEDSASZ             Leave space for DSA Fixed area
PLIST      DS     0D
PARM1      DS     A
PARM2      DS     A
PARM3      DS     A
PARM4      DS     A
PARM5      DS     A
           DS     0D
FBCODE     DS     3F
*
           DS     0D
WORKSIZE   EQU    *-WORKAREA
           CEEDSA SECTYPE=ALL
           CEECAA
*
*          REGEQU
R0         EQU    0
R1         EQU    1
R2         EQU    2
R3         EQU    3
R4         EQU    4
R5         EQU    5
R6         EQU    6
R7         EQU    7
R8         EQU    8
R9         EQU    9
R10        EQU    10
R11        EQU    11
R12        EQU    12
R13        EQU    13
R14        EQU    14
R15        EQU    15
           END