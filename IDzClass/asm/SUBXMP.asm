**************************************************************
*                                                            *
*  NAME: SUBXMP                                              *
*                                                            *
*        A simple main assembler routine that brings up      *
*        Language Environment, calls a subroutine, and       *
*        returns with a return code of 0.                    *
*                                                            *
**************************************************************
SUBXMP   CEEENTRY PPA=XMPPPA,AUTO=WORKSIZE
         USING WORKAREA,R13
* Invoke CEEMOUT to issue the greeting message
         CALL  CEEMOUT,(HELLOMSG,DEST,FBCODE),VL,MF=(E,CALLMOUT)
* No plist to DISPARM, so zero R1. Then call it.
         SLR   R0,R0
         ST    R0,COUNTER
         LA    R0,HELLOMSG
         SR    R01,R01 ssue a message
         CALL  DISPARM                CALL1
* Invoke CEEMOUT to issue the farewell message
         CALL  CEEMOUT,(BYEMSG,DEST,FBCODE),VL,MF=(E,CALLMOUT)
* Terminate Language Environment and return to the caller
         CEETERM  RC=0

*        CONSTANTS
HELLOMSG DC    Y(HELLOEND-HELLOSTR)
HELLOSTR DC    C'Hello from the sub example.'
HELLOEND EQU   *

BYEMSG   DC    Y(BYEEND-BYESTART)
BYESTART DC    C'Terminating the sub example.'
BYEEND   EQU   *
DEST     DC    F'2'               Destination is the LE message file
COUNTER  DC    F'-1'

XMPPPA   CEEPPA  ,                Constants describing the code block
*        The Workarea and DSA
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
CALLMOUT CALL  ,(,,),VL,MF=L      3-argument parameter list
FBCODE   DS    3F                 Space for a 12-byte feedback code
         DS    0D
WORKSIZE EQU   *-WORKAREA
         PRINT NOGEN
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
R0       EQU   0
R01      EQU   1
R13      EQU   13
         END   SUBXMP             Nominate SUBXMP as the entry point