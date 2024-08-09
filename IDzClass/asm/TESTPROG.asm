***********************************************************************
*---------------------------------------------------------------------*
*                                                                     *
*        THIS SUB-PROGRAM WILL RETURN THE DESCRIPTION ASSOCIATED WITH *
*        A PARTICULAR EIBRESP VALUE.                                  *
*                                                                     *
*        EXAMPLE COMMAREA -                                           *
*                                                                     *
*        03  WS-GETDESC-COMMAREA.                                     *
*            05  WS-GETDESC-RESP-CODE        PIC  9(04) BINARY.       *
*            05  WS-GETDESC-DESCRIPTION-LGTH PIC  9(04) BINARY.       *
*            05  WS-GETDESC-DESCRIPTION      PIC  X(12).              *
*        03  WS-GETDESC-COMMAREA-LGTH        PIC  9(04) BINARY.       *
*                                                                     *
*        MOVE EIBRESP                  TO WS-GETDESC-RESP-CODE.       *
*        MOVE LENGTH OF WS-GETDESC-COMMAREA                           *
*                                      TO WS-GETDESC-COMMAREA-LGTH.   *
*                                                                     *
*        EXEC CICS LINK                                               *
*                  PROGRAM ('GETDESC')                                *
*                  COMMAREA(WS-GETDESC-COMMAREA)                      *
*                  LENGTH  (WS-GETDESC-COMMAREA-LGTH)                 *
*        END-EXEC.                                                    *
*                                                                     *
*        UPON RETURN, 'WS-GETDESC-DESCRIPTION' WILL CONTAIN THE       *
*        DECRIPTION-TEXT ASSOCIATED WITH THE PASSED 'EIBRESP'. ALSO,  *
*        'WS-GETDESC-DESCRIPTION-LGTH' WILL CONTAIN THE ACTUAL        *
*        LENGTH OF THE DESCRIPTION. FOR EXAMPLE, THE 'EIBRESP' VALUE  *
*        THAT WAS PASSED WAS 19 AND ITS ASSOCIATED DESCRIPTION WILL   *
*        BE 'NOTOPEN'. THE DESCRIPTION-LGTH WILL EQUAL 7.             *
*                                                                     *
*        THE RETURN-CODE IS RETURNED IN R15 TO THE CALLER, OTHERWISE  *
*        KNOWN AS THE RETURN-CODE SPECIAL-REGISTER FOR COBOL-CALLERS. *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
         PRINT NOGEN                   SUPPRESS MACRO-EXPANSION
         CEECAA                        LE 'COMMON ANCHOR AREA' DSECT
         USING CEECAA,R12              R12 IS 'OFF LIMITS'
         PRINT GEN                     ACTIVATE MACRO-EXPANSION
COMDSECT DSECT                         COMMAREA DSECT (R7)
COMMAREA EQU   *                       BEGIN COMMAREA
COMMRCDE DS    XL2                     RESP-CODE (UNALIGNED HWORD)
COMMDLEN DS    XL2                     DESCRIPTN-LGTH (UNALIGNED HWORD)
COMMDESC DS    CL12                    DESCRIPTN-TEXT
COMMLGTH EQU   *-COMMAREA              COMMAREA-LGTH
TBLDSECT DSECT                         DFHEITAB DSECT (R9)
TBLENTRY EQU   *                       BEGIN ENTRY
TBLDESC  DS    CL12                    DESCRIPTION (MATCHES DFHRESP)
TBLEYECT DS    XL2                     EYECATCHER (X'50CO')
         DS    XL4                     NOT USED
TBLRSPCD DS    XL2                     RESP-CODE (UNALIGNED HWORD)
         DS    XL2                     NOT USED
TBLLGTH  EQU   *-TBLENTRY              LENGTH OF TBL-DSECT
DFHEISTG DSECT                         DYNAMIC-STG DSECT (R13)
RETNCODE DS    H                       RETURN-CODE HWORD
MAXLOOP  DS    F                       SANITY-MAX FWORD
DWORD    DS    D                       DOUBLEWORD WORK-AREA
GETDESC  DFHEIENT CODEREG=R3,DATAREG=R13,EIBREG=R11
         LA    R3,0(,R3)               CLEAR TOP-BIT
         B     BEGIN                   BEGIN EXECUTION
         DFHREGS
GETDESC  AMODE 31
GETDESC  RMODE ANY
EYECTCHR DS    0CL48
         DC    PL2'407'
         DC    CL10'EYECATCHER'
         DC    PL1'7'
         DC    CL6' ===>'
PROGNAME DC    CL8'GETDESC'
         DC    CL2','
         DC    CL8'&SYSDATC'
         DC    CL2','
         DC    CL9'&SYSTIME..00 '
         CNOP  0,4                     ENSURE ALIGNMENT
BEGIN    EQU   *
         LA    R15,4095                LOAD WITH F'4095'
         STH   R15,RETNCODE            STORE IN HWORD
         LH    R15,EIBCALEN            LOAD COMMAREA-LGTH
         CHI   R15,COMMLGTH            MINIMUM-LGTH?
         BL    CICSRETN                NO, RETURN TO CALLER
         XC    DFHEIUSR(DFHEIEND-DFHEIUSR),DFHEIUSR
         L     R7,DFHEICAP             COMMAREA ADDRESSABILITY
         LA    R7,0(,R7)               CLEAR TOP-BIT
         USING COMDSECT,R7             INFORM ASSEMBLER
         LA    R9,0                    ENSURE X'00'S
         STCM  R9,B'0011',COMMDLEN     SAME
         MVI   COMMDESC,C' '           ENSURE ALL SPACES
         MVC   COMMDESC+1(L'COMMDESC-1),COMMDESC
         OC    DWORD(2),COMMRCDE       NON-ZERO RESP-CODE?
         MVC   DWORD(2),DWORD+2        ENSURE X'00'S
         BNZ   LOADPGM                 YES, LOAD PROGRAM
         MVC   COMMDESC(6),=C'NORMAL'  INSERT 'NORMAL'
         MVI   COMMDLEN+1,X'06'        SET DESCRIPTION-LGTH
         MVI   RETNCODE+1,X'04'        SET TO H'4'
         B     CICSRETN                RETURN TO CALLER
LOADPGM  EQU   *
*
         EXEC  CICS LOAD,              ESTABLISH ADDRESSABILITY TO THE X
               PROGRAM('DFHEITAB'),    CICS IN-CORE TABLE              X
               SET    (R9),                                            X
               FLENGTH(DWORD),                                         X
               NOHANDLE,
*
         LA    R9,0(,R9)               CLEAR TOP-BIT
         USING TBLDSECT,R9             INFORM ASSEMBLER
         ICM   R15,B'1111',EIBRESP     TABLE LOAD OK?
         BZ    GOODLOAD                YES, FIND ENTRY-START
         MVI   RETNCODE+1,X'10'        SET TO H'16'
         MVC   COMMDESC,=C'@LDERROR000@'
         CVD   R15,DWORD               MAKE IT DECIMAL
         OI    DWORD+L'DWORD-1,X'0F'   ENSURE 'F' SIGN-NIBBLE
         UNPK  COMMDESC+8(3),DWORD     UNPACK RESP-CODE
         MVI   COMMDLEN+1,L'COMMDESC   SET DESCRIPTION-LGTH
         B     CICSRETN                RETURN TO CALLER
GOODLOAD EQU   *
         L     R15,DWORD               LOAD FOR VALIDITY?
         CHI   R15,2                   LGTH LESS THAN F'2'?
         BCTR  R15,0                   REDUCE BY F'1'
         ST    R15,MAXLOOP             STORE IN FWORD
         LA    R15,0                   ENSURE X'00'S
         BNL   FINDEYE                 NO, FIND THE EYECATCHER
         B     NORESPCD                SHOULD NEVER HAPPEN
FINDEYE  EQU   *
         CLC   TBLENTRY(2),=X'50C0'    EYECATCHER FOUND?
         BNE   BUMP4EYE                NO, CHECK 'NEXT' BYTE
         AHI   R9,-L'TBLDESC           REPOSITION AT ENTRY-START
         B     LCTEDESC                LOCATE DESCRIPTION
BUMP4EYE EQU   *
         LA    R9,1(,R9)               BUMP TO 'NEXT' BYTE
         LA    R15,1(,R15)             BUMP 'SANITY' KTR
         C     R15,MAXLOOP             EXCEEDED 'SANITY' MAX?
         BH    NORESPCD                YES, TABLE-OVERRUN
         B     FINDEYE                 CONTINUE LOOP
LCTEDESC EQU   *
         CLC   TBLEYECT,=X'50C0'       ANY MORE ENTRIES?
         BNE   NORESPCD                NO, RESPONSE-CODE NOT FOUND
         CLC   TBLRSPCD,COMMRCDE       RESPONSE-CODES MATCH?
         BNE   LCTEBUMP                NO, CHECK 'NEXT' ENTRY
         MVC   COMMDESC,TBLDESC        POPULATE DESCRIPTION
         LA    R14,COMMDESC+L'COMMDESC-1
         CLI   0(R14),C' '             LAST BYTE EXCEEDS SPACE?
         BH    CICSRETN                YES, RETURN TO CALLER
         LA    R15,L'COMMDESC-1        MAX-LGTH MINUS F'1'
         BCTR  R14,0                   DECREMENT ADDRESS
         B     CALCLGTH                CALCULATE DESCRIPTION-LGTH
LCTEBUMP EQU   *
         AHI   R9,TBLLGTH              BUMP TO 'NEXT' TBL-ENTRY
         B     LCTEDESC                CHECK FOR END OF ENTRIES
CALCLGTH EQU   *
         CLI   0(R14),C' '             EXCEEDS SPACE?
         BNH   CALCDECR                NO, DECREMENT AND CONTINUE
         STC   R15,COMMDLEN+1          SET ACTUAL LGTH
         B     CICSRETN                RETURN TO CALLER
CALCDECR EQU   *
         MVI   0(R14),C' '             ENSURE IT'S A SPACE
         BCTR  R14,0                   DECREMENT ADDRESS
         BCT   R15,CALCLGTH            BRANCH WHEN NON-ZERO
NORESPCD EQU   *
         MVI   RETNCODE+1,X'␍'        SET TO H'08'
         MVI   COMMDLEN+1,L'COMMDESC   SET DESCRIPTION-LGTH
         MVC   COMMDESC,=C'@@@NOTFND@@@'
CICSRETN EQU   *
         LH    R15,RETNCODE            LOAD RETURN-CODE
*
         DFHEIRET RCREG=R15            RETURN TO CALLER
*
         LTORG ,                       BEGIN LITERAL-ORG
*
         END   ,                       END 'GETDESC'