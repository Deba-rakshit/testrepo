DBGMAIN  CEEENTRY PPA=MAINPPA,AUTO=WORKSIZE,MAIN=YES
         USING    WORKAREA,R13
*                             EASY TO WRITE REENTRANT CODE
*                             EASIER TO CALL GETMAIN/FREEMAIN
*                             EASIER FOR REENTRANT CODE



         LA   R2,STRT_MSG
         LA   R3,DEST
         LA   R4,FBCODE
         STM  R2,R4,PLIST
         LA   R1,PLIST
         L    R15,MOUT
         BALR R14,R15
*
         PACK PCKA,ZNA
         PACK PCKB,ZNB
         PACK PCKC,ZNC
         ZAP  PCKSUM,PCKA
         AP   PCKSUM,PCKB
         AP   PCKSUM,PCKC
         MVC  OUTSUM,SUMMSK
         ED   OUTSUM,PCKSUM
         MVC  SUMMSG+1(8),OUTSUM
         MVC  LINE_ST,SUMMSG
*
         WTO   'Hello IDz - from the worlds simplest ALC program!'
*
         LA     R2,LINE_MSG
         LA     R3,DEST
         LA     R4,FBCODE
         STM  R2,R4,PLIST
         LA     R1,PLIST
         L        R15,MOUT
         BALR R14,R15
*
         LA     R2,DONE_MSG
         LA     R3,DEST
         LA     R4,FBCODE
         STM  R2,R4,PLIST
         LA     R1,PLIST
         L        R15,MOUT
         BALR R14,R15
*
         CEETERM  RC=0
* ==============================================================
*  Constants and Variables
* ==============================================================
ZLEN     EQU 5
PLEN     EQU ZLEN/2+1
*
SUMMSG   DC  C'(xxxxxxxx) -- The sum    '
SUMMSK   DC  X'4020202020202120'
ZNA           DC  ZL5'100'
ZNB           DC  ZL5'150'
ZNC           DC  ZL5'50'
*
PCKA        DS  PL(PLEN)
PCKB        DS  PL(PLEN)
PCKC        DS  PL(PLEN)
PCKSUM   DS  PL(PLEN+1)
OUTSUM   DS  CL(L'SUMMSK)
*
MOUT     DC     V(CEEMOUT)        The CEL Message service
*
LINE_MSG DS     0F
              DC     AL2(LINE_END-LINE_ST)
LINE_ST   DS       CL25
LINE_END EQU    *
*
STRT_MSG DS     0F
                DC     AL2(STRT_END-STRT_ST)
STRT_ST  DC       C'Starting the program.'
STRT_END EQU    *
*
DONE_MSG DS     0F
                DC     AL2(DONE_END-DONE_ST)
DONE_ST  DC       C'Terminating the program.'
DONE_END EQU    *
*
DEST     DC     F'2'              The destination is the MSGFILE
*
MAINPPA  CEEPPA
* ===================================================================
*  The Workarea and DSA
* ===================================================================
WORKAREA DSECT
         ORG    *+CEEDSASZ
PLIST    DS     0D
PARM1    DS     A
PARM2    DS     A
PARM3    DS     A
PARM4    DS     A
PARM5    DS     A
*
FBCODE   DS     3F
*
         DS         0D
WORKSIZE EQU    *-WORKAREA
         CEEDSA                   Mapping of the Dynamic Save Area
         CEECAA                   Mapping of the Common Anchor Area
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10     EQU   10
R11     EQU   11
R12     EQU   12
R13     EQU   13
R14     EQU   14
R15     EQU   15
           END   DBGMAIN            Nominate DBGMAIN as the entry point