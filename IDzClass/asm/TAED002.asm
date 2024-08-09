* ****************************************************************
*
*   Program Name:  TAED002
*   Language:      Assembler
*   Description:   TAED002 uses SVC LINK to load and call TAED002A
*
*       file:///c:\IDzwksp\bnch.png
*
*   Updated: 2005/10/18
* ****************************************************************
TAED002  CEEENTRY PPA=XMPPPA,AUTO=WORKSIZE,MAIN=YES,ENCLAVE=YES
         USING WORKAREA,R13
*
         COPY TCOPY
* Initialize TFLAG, SFLAG
         LA     R5,TFLAG
         MVI    0(R5),C'X'
         LA     R5,SFLAG
         MVI    0(R5),C'X'

* Set R5 to first char in TSTSTR
* Set R6 to last  char in TSTSTR
         SLR    R5,R5
         LA     R5,TSTSTR2
         SLR    R6,R6
         LA     R6,L'TSTSTR2(R5)
         AHI    R6,-1
* Look for a Q
QLOOP    CLI    0(R5),C'Q'
         BE     FOUNDQ
         LA     R5,1(R5)
         CR     R5,R6
         BL     QLOOP
* No Q found
NOQ      EQU    *
         LA     R7,TFLAG
         MVI    0(R7),C'N'
         B      LNK2PGM
* Found a Q
FOUNDQ   EQU    *
         LA     R7,TFLAG
         MVI    0(R7),C'Y'
         B      LNK2PGM
*
*
*  LINK to TAED002A and create a new enclave.
*
LNK2PGM  EQU    *
         LINK EP=TAED002A,Param=(ParmStr),VL=1
*
* Initialize TFLAG, SFLAG
         LA     R5,TFLAG
         MVI    0(R5),C'X'
         LA     R5,SFLAG
         MVI    0(R5),C'X'

* Set R5 to first char in TSTSTR
* Set R6 to last  char in TSTSTR
         SLR    R5,R5
         LA     R5,TSTSTR
         SLR    R6,R6
         LA     R6,L'TSTSTR(R5)
         AHI    R6,-1
* Look for a J
JLOOP    CLI    0(R5),C'J'
         BE     FOUNDJ
         LA     R5,1(R5)
         CR     R5,R6
         BL     JLOOP
* No J found
NOJ      EQU    *
         LA     R7,TFLAG
         MVI    0(R7),C'N'
         B      ENDPGM
* Found a J
FOUNDJ   EQU    *
         LA     R7,TFLAG
         MVI    0(R7),C'Y'
         B      ENDPGM
*
* TERMINATE LANGUAGE ENVIRONMENT AND RETURN TO THE CALLER
* PASS REG15 AS RETURN REGISTER
*
ENDPGM   EQU    *
         CEETERM  RC=0
* ====================================================================
*     CONSTANTS and variables
* =================================================================
*
* ** Start general work area
*
TSTSTR     DC     CL20'ABCDQFGHIJKLMNOPQRST'
TFLAG      DS     CL1
SFLAG      DS     CL1
*
ParmStr  DC    Y(0)
*
* ** End general work area
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