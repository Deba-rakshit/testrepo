**************************************************************
*                                                            *
*  NAME:  DISPARM                                            *
*                                                            *
*        Shows an assembler subroutine that displays inbound *
*        parameters and returns.                             *
*                                                            *
**************************************************************
DISPARM  CEEENTRY PPA=PARMPPA,AUTO=WORKSIZE,MAIN=NO
         USING WORKAREA,R13
* Invoke CEE3PRM to retrieve the command parameters for us
         SLR   R0,R0
         ST    R0,COUNTER
         CALL  CEE3PRM,(CHARPARM,FBCODE),VL,MF=(E,CALL3PRM)      CALL2
* Check the feedback code from CEE3PRM to see if everything worked.
         CLC   FBCODE(8),CEE000
         BE    GOT_PARM
* Invoke CEEMOUT to issue the error message for us
         CALL  CEEMOUT,(BADFBC,DEST,FBCODE),VL,MF=(E,CALLMOUT)
         B     GO_HOME                 Time to go....
GOT_PARM DS    0H
* See if the parm string is blank.
         LA    R1,1
SAVECTR  ST    R1,COUNTER
         CL    R1,=F'5'                    BUMPCTR
         BH    LOOPEND
         LA    R1,1(,R1)
         B     SAVECTR
LOOPEND  DS    0H
         CLC   CHARPARM(80),=CL80' '   Is the parm empty?
         BNE   DISPLAY_PARM            No. Print it out.
* Invoke CEEMOUT to issue the error message for us
         CALL  CEEMOUT,(NOPARM,DEST,FBCODE),VL,MF=(E,CALLMOUT)
         B     GO_TEST                 Time to go....

DISPLAY_PARM   DS  0H
* Set up the plist to CEEMOUT to display the parm.
         LA    R0,2
         ST    R0,COUNTER
         LA    R02,80        Get the size of the string
         STH   R02,BUFFSIZE  Save it for the len-prefixed string
* Invoke CEEMOUT to display the parm string for us
         CALL  CEEMOUT,(BUFFSIZE,DEST,FBCODE),VL,MF=(E,CALLMOUT)
*        AMODE Testing
GO_TEST  DS  0H
         L     R15,INAMODE24@
         BSM   R14,R15
InAMode24 Equ  *
         LA    R1,DEST
         O     R1,=X'FF000000'
         L     R15,0(,R1)
         LA    R15,2(,R15)
         ST    R15,0(,R1)
         L     R15,INAMODE31@
         BSM   R14,R15
InAMode31 Equ  *
* Return to the caller
GO_HOME  DS    0H
         LA    R0,3
         ST    R0,COUNTER
         CEETERM  RC=0

*        CONSTANTS
DEST     DC    F'2'               Destination is the LE message file
CEE000   DS    3F'0'              Success feedback code
InAMode24@ DC  A(InAMode24)
InAMode31@ DC  A(InAMode31+X'80000000')
BADFBC   DC    Y(BADFBEND-BADFBSTR)
BADFBSTR DC    C'Feedback code from CEE3PRM was nonzero.'
BADFBEND EQU   *
NOPARM   DC    Y(NOPRMEND-NOPRMSTR)
NOPRMSTR DC    C'No user parm was passed to the application.'
NOPRMEND EQU   *
PARMPPA  CEEPPA ,                 Constants describing the code block
* ===================================================================
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
CALL3PRM CALL  ,(,),VL,MF=L       2-argument parameter list
CALLMOUT CALL  ,(,,),VL,MF=L      3-argument parameter list
FBCODE   DS    3F                 Space for a 12-byte feedback code
COUNTER  DS    F
BUFFSIZE DS    H                  Halfword prefix for following string
CHARPARM DS    CL255              80-byte buffer
         DS    0D
WORKSIZE EQU   *-WORKAREA
         PRINT NOGEN
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
MYDATA   DSECT   ,
MYF      DS      F
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
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
R02      EQU   2
         END