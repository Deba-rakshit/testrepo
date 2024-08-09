IAPUT1   CEEENTRY PPA=PARMPPA,AUTO=WORKSIZE,MAIN=NO
         USING    WORKAREA,R13
*
         MVC   CHARPARM(30),0(R01)
         LA    R02,30
         STH   R02,BUFFSIZE
*
* Invoke CEEMOUT to display the parm string for us
*
         CALL  CEEMOUT,(BUFFSIZE,DEST,FBCODE),VL,MF=(E,CALLMOUT)
*
* Return to the caller
*
         CEETERM  RC=0
* =================================================================
*              CONSTANTS
* =================================================================
*
DEST     DC    F'2'               Destination is the LE message file
*
*   CEEPPA                   Constants describing the code block
* ==============================================================
PARMPPA  CEEPPA ,                 Constants describing the code blo
* =================================================================
*        The Workarea and DSA
* =================================================================
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed par
*
CALLMOUT CALL  ,(,,),VL,MF=L      3-argument parameter list
FBCODE   DS    3F                 Space for a 12-byte feedback code
*
BUFFSIZE DS    H                  Halfword prefix for following string
CHARPARM DS    CL30               30-byte buffer
*
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
*
R01      EQU   1
R02      EQU   2
R03      EQU   3
R04      EQU   4
R05      EQU   5
R06      EQU   6
R07      EQU   7
R08      EQU   8
R09      EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         END   IAPUT1             Nominate IAPUT1 as the entry point
