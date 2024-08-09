* ****************************************************************
*
*   Program Name:  TAED002A
*   Langjage:      Assembler
*   Description:   Loaded and called by TAED002 via SVC LINK
*
* ****************************************************************
TAED002A CEEENTRY PPA=XMPPPA,AUTO=WORKSIZE,MAIN=YES,ENCLAVE=YES
         USING WORKAREA,R13
*
         ST   R11,SAVER11      SAVE BASE
         L    R14,4(R13)       REG14 ADRESSES NOW CEEBTOR RSA
         L    R14,4(R14)       REG14 ADRESSES NOW REAL CALLERS DSA
         LM   R0,R12,20(R14)
         L    R11,SAVER11      RELOAD BASE
         L    R12,=V(CEEARLU)
         BALR R14,R12
*
         CALL TAED002B         CALL TAED002B
*
* TERMINATE LANGUAGE ENVIRONMENT AND RETURN TO THE CALLER
* PASS REG15 AS RETURN REGISTER
*
         CEETERM  RC=(R15)
* ====================================================================
*              CONSTANTS
* =================================================================
*
*
XMPPPA   CEEPPA  ,                CONSTANTS DESCRIBING THE CODE BLOCK
* ====================================================================
*        THE WORKAREA AND DSA
* =================================================================
WORKAREA DSECT
         ORG   *+CEEDSASZ         LEAVE SPACE FOR THE DSA FIXED PART
*
FBCODE   DS    3F                 SPACE FOR A 12-BYTE FEEDBACK CODE
*
SAVER11  DS    1F
*
*
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                MAPPING OF THE DYNAMIC SAVE AREA
         CEECAA  ,                MAPPING OF THE COMMON ANCHOR AREA
*
R0       EQU   0
R01      EQU   1
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         END   TAED002A           NOMINATE TAED002A AS THE ENTRY POINT