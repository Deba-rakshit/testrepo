* ****************************************************************
* ****************************************************************
*
*   Program Name:  TAND004
*   Language:      Assembler
*   Description:   Simple assembler program with Unicode constants
*
*   Updated: 2009/08/05
* ****************************************************************
TAND004  CEEENTRY PPA=XMPPPA,AUTO=WORKSIZE,MAIN=YES,ENCLAVE=YES
         USING WORKAREA,R13
*
         J     SKIPIT
         DS    0D
*
* Write message
SKIPIT   CLI   CHARPARM+6,C'F'
         BE    PUTMSG
         LA    R5,CHARPARM
         MVI   5(R5),C'F'
         B     PUTMSG
*
PUTMSG   EQU   *
         LA    R2,30
         STH   R2,BUFFSIZE
         CALL  CEEMOUT,(BUFFSIZE,DEST,FBCODE),VL,MF=(E,CALLMOUT)
*
* Move Unicode fields to character fields
         SLR    R6,R6
         SLR    R7,R7
         SLR    R8,R8
         SLR    R9,R9
* Move UA to CHARUA
         LA     R6,UA             * address of sending field UA
         LA     R8,CHARUA         * address of receiving field CHARUA
         L      R7,LEN12          * length of receiving field
         L      R9,LEN12          * length of receiving field
         MVCL   R8,R6
*
         SLR    R6,R6
         SLR    R7,R7
         SLR    R8,R8
         SLR    R9,R9
* Move UALTRS to CHARLTRS
         LA     R6,UALTRS         * address of sending field UALTRS
         LA     R8,CHARLTRS       * address of receiving field CHARLTRS
         L      R7,LEN104         * length of receiving field
         L      R9,LEN104         * length of receiving field
         MVCL   R8,R6
*
         SLR    R6,R6
         SLR    R7,R7
         SLR    R8,R8
         SLR    R9,R9
* Move XMLDOC to CHARXML
         LA     R6,XMLDOC         * address of sending field XMLDOC
         LA     R8,CHARXML        * address of receiving field CHARXML
         L      R7,LEN380         * length of receiving field
         L      R9,LEN380         * length of receiving field
         MVCL   R8,R6
*
         SLR    R6,R6
         SLR    R7,R7
         SLR    R8,R8
         SLR    R9,R9
* Move XMLDOC2 to CHARXML2
         LA     R6,XMLDOC2        * address of sending field XMLDOC2
         LA     R8,CHARXML2       * address of receiving field CHARXML2
         L      R7,LEN380         * length of receiving field
         L      R9,LEN380         * length of receiving field
         MVCL   R8,R6
*
         SLR    R5,R5
         SLR    R6,R6
         SLR    R7,R7
         SLR    R8,R8
         SLR    R9,R9
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
         B      NXTLOOK
* Found a Q
FOUNDQ   EQU    *
         LA     R7,TFLAG
         MVI    0(R7),C'Y'
         B      NXTLOOK
*
* Initialize TFLAG, SFLAG
NXTLOOK  LA     R5,TFLAG
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
DEST     DC    F'2'               Destination is the LE message file
*
TSTSTR     DC     CL20'ABCDQFGHIJKLMNOPQRST'
TFLAG      DS     CL1
SFLAG      DS     CL1
*
ParmStr  DC    Y(0)
*
CALLMOUT CALL  ,(,,),VL,MF=L      3-argument parameter list
*
BUFFSIZE DS    H
CHARPARM DC    CL30'ABCDEZGHIJKLMNOPQRSTUVWXYZ1234'
*
LEN12    DC    F'12'
LEN104   DC    F'104'
LEN380   DC    F'380'
*
CHARUA   DC    CL12' '
CHARLTRS DC    CL104' '
CHARXML  DC    CL200' '
         DC    CL100' '
         DC    CL80' '
CHARFILL DC    CL100' '
CHARXML2 DC    CL200' '
         DC    CL100' '
         DC    CL80' '
CHARFIL2 DC    CL100' '
XMLEBCDC DC    CL21'<?xml version="1.0"?>'
         DC    CL27'<EBCDCLevel  Version="1.0">'
         DC    CL13'<Data-Levels>'
         DC    CL41'<Level01>01 EBCDIC Level Element Level 01'
         DC    CL41'<Level02>02 EBCDIC Level Element Level 02'
         DC    CL10'</Level02>'
         DC    CL10'</Level01>'
         DC    CL14'</Data-Levels>'
         DC    CL13'</EBCDCLevel>'
* XML doc with an error (/Level00 should be /Level01)
XMLEBCD2 DC    CL21'<?xml version="1.0"?>'
         DC    CL27'<EBCDCLvlEr  Version="1.0">'
         DC    CL13'<Data-Levels>'
         DC    CL41'<Level01>01 EBCDIC LvlEr Element Level 01'
         DC    CL41'<Level02>02 EBCDIC LvlEr Element Level 02'
         DC    CL10'</Level02>'
         DC    CL10'</Level00>'
         DC    CL14'</Data-Levels>'
         DC    CL13'</EBCDCLvlEr>'
*
* ** Unicode constants
UA       DC    CU'UTF-16'
UALTRS   DC    CU'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
XMLDOC   DC    CU'<?xml version="1.0"?>'
         DC    CU'<UCodeLevel  Version="1.0">'
         DC    CU'<Data-Levels>'
         DC    CU'<Level01>01 UniCodeLevel Element Level 01'
         DC    CU'<Level02>02 UniCodeLevel Element Level 02'
         DC    CU'</Level02>'
         DC    CU'</Level01>'
         DC    CU'</Data-Levels>'
         DC    CU'</UCodeLevel>'
* XML doc with an error (/Level00 should be /Level01)
XMLDOC2  DC    CU'<?xml version="1.0"?>'
         DC    CU'<UCodeLvlEr  Version="1.0">'
         DC    CU'<Data-Levels>'
         DC    CU'<Level01>01 UniCodeLvlEr Element Level 01'
         DC    CU'<Level02>02 UniCodeLvlEr Element Level 02'
         DC    CU'</Level02>'
         DC    CU'</Level00>'
         DC    CU'</Data-Levels>'
         DC    CU'</UCodeLvlEr>'
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
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                MAPPING OF THE DYNAMIC SAVE AREA
         CEECAA  ,                MAPPING OF THE COMMON ANCHOR AREA
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
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         END   TAND004