* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Program for Date Validation.                                        *
* This progam will read a 8 byte date in DDMMYYYY format and will     *
* check if it is a valid date.                                        *
* Program Input : 8 Byte String(Date) in DDMMYYYY format.             *
* Program Output: 4 Byte string +0 for VALID and -1 for INVALID.      *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Sample COBOL invocation :                                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* WORKING-STORAGE SECTION.                                            *
* 77  STRING1 PIC 9(8).                                            *
* 77  INBETWEEN PIC X(8).                                            *
* 77  RESULT PIC S9(4).                                            *
* PROCEDURE DIVISION.                                                 *
* PARA1.                                                              *
*      MOVE '01122010' TO STRING1.                                    *
*      CALL 'DATEVAL' USING STRING1, RESULT.                          *
*      DISPLAY RESULT.                                                *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'PROGRAM FOR DATE VALIDATION'
DATEVAL  CSECT
         STM   14,12,12(13)                  HOUSE KEEPING
         BALR  12,0                          LOAD BASER WITH PGM ENTRY
         USING *,12                          ESTABLISH BASER
         LR    R11,R13                       OLD SAVE AREA
         LA    R13,SAVE                      NEW SAVE AREA
         ST    R11,4(R13)                    BACKWARD CHAIN
         ST    R13,8(R11)                    FWD CHAIN
         L     R7,0(R1)                      GET ADDR OF PARM1
         L     R9,4(R1)                      GET ADDR OF PARM2
         USING WORKINGS,R7                   DSECT IT
         CLC   WSDATE(8),=XL8'0000000000000000'
         BE    EMPTY
         CLC   WSDATE(8),=XL8'FFFFFFFFFFFFFFFF'
         BE    EMPTY
         CLC   WSDATE(8),=CL8' '
         BE    EMPTY
         B     CHKNEXT
EMPTY    DS    0H
         OI    FLAG,X'40'
         TM    FLAG,INVALID
         BO    EXIT1
CHKNEXT  DS    0H
         NC    FLAG,NULL1                    CLEAR FLAGS
         NC    FLAGF,NULL1                   CLEAR FLAGS
         BAL   R14,YYYYCHK                   BRANCH TO YEAR VALIDATION
         TM    FLAG,INVALID                  TEST FLAG FOR INVALID DATE
         BO    EXIT1                         YES ? EXIT
         SR    R14,R14                       CLEAR R14
         BAL   R14,MMCHK                     BRANCH TO MONTH VALIDATION
         TM    FLAG,INVALID                  TEST FLAG FOR INVALID DATE
         BO    EXIT1                         YES ? EXIT
         SR    R14,R14                       CLEAR R14
         BAL   R14,DDCHK                     BRANCH TO DAY VALIDATION
         TM    FLAG,INVALID                  TEST FLAG FOR INVALID DATE
         BO    EXIT1                         YES ? EXIT
         B     NOREXIT                       NO ? BRANCH AROUND EXIT1
EXIT1    DS    0H
         MVC   STATUS(4),=X'F0F0F0D1'        SET STATUS AS INVALID (-1)
         B     RETURN
NOREXIT  DS    0H
         MVC   STATUS(4),=X'F0F0F0C0'        SET STATUS AS VALID (+0)
RETURN   DS    0H
         MVC   0(4,R9),STATUS
         L     R13,SAVE+4                    LOAD R13 WITH SAVEAREA
         LM    14,12,12(13)                  RESTORE REGISTERS
         LA    15,0(0,0)                     SET RC
         BR    14                            RETURN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Routine to check for a leap year and a year between 1990 - 2050     *
* Routine Input : 4 Byte String(YYYY).                                *
* Routine Output: Will set the FLAG or FLAGF mask indicating          *
*                 invalid year and/or leap year.                      *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
YYYYCHK  DS    0H
         ST    R14,ADDR1                     SAVE RETURN ADDR
         XC    FLAGF,FLAGF                   CLEAR FLAG
         PACK  YYYY,WSYY                     PACK INPUT YEAR
         PACK  DIVIDENT,WSYY                 PACK YEAR TO DIVISOR
         PACK  DIVISOR,=CL1'4'               PACK 4
         DP    DIVIDENT,DIVISOR              DIVIDE
         CP    REMAIN,=PL8'0'                COMPARE REMAINDER AND 0
         BE    LEAPSET                       EQUAL ? BRANCH TO LEAPSET
         PACK  DIVIDENT,WSYY                 PACK DIVIDENT
         PACK  DIVISOR,=CL3'400'             PACK 400
         DP    DIVIDENT,DIVISOR              DIVIDE
         CP    REMAIN,=PL8'0'                COMPARE REMAINDER AND 0
         BE    LEAPSET                       EQUAL ? BRANCH TO LEAPSET
         B     NOLEAP                        NOT EQUAL ? GOTO NOLEAP
LEAPSET  DS    0H
         OI    FLAGF,X'20'                   SET FLAGF TO LEAP
         B     NEXT1                         BRANCH TO NEXT1
NOLEAP   DS    0H
*        WTO   'NOT A LEAP YEAR'
         OI    FLAGF,X'10'                   SET FLAGF TO NOLEAP
NEXT1    DS    0H
         PACK  COMP1,=CL4'1990'              PACK 1990
         CP    YYYY,COMP1                    COMPARE YEAR WITH 1990
         BC    4,SETM                        YEAR < 1990
SCHECK   DS    0H                            SET INVALID FLAG
         PACK  COMP1,=CL4'2050'              PACK 2050
         CP    YYYY,COMP1                    COMPARE YEAR WITH 2050
         BC    2,SETM                        YEAR > 2050 SET INVLID
         B     AROUND                        BRANCH AROUND
SETM     DS    0H
*        WTO   'INPUT YEAR INVALID'
         OI    FLAG,X'40'                    SET INVALID FLAG
AROUND   DS    0H
         L     R14,ADDR1                     LOAD RETURN ADDR
         BR    R14                           BRANCH
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Routine to check for a valid month and check 28/29 day for Feb.     *
* Routine Input : 2 Byte Input Month and 2 Byte Input Date.           *
* Routine Output: Will set the FLAG mask indicating                   *
*                 invalid input.                                      *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MMCHK    DS    0H
         ST    R14,ADDR1                     STORE RETURN ADDR
         PACK  MM,WSMM                       PACK MONTH TO MM
         PACK  MMCP,=CL2'01'                 PACK 01 TO MMCP
         CP    MM,MMCP                       COMPARE INPUT MONTH AND 01
         BC    4,SETM1                       IF INPUT MM<0  SET INVALID
         PACK  MMCP,=CL2'12'                 PACK 12 TO MMCP
         CP    MM,MMCP                       COMPARE INPUT  MONTH AND12
         BC    2,SETM1                       IF INPUT MM>12 SET INVALID
         LA    R5,MON31END                   LOAD NO OF TABLE ENTRIES
         LA    R4,MON31ONE                   LOAD LENGTH OF ONE ENTRY
         LA    R6,MON31                      LOAD TABLE ADDRESS
         PACK  DEC,WSMM                      PACK INPUT MM TO DWORD
         CVB   R8,DEC                        CONVERT IT TO BINARY
LOOP1    DS    0H
         C     R8,0(R6)                      CHECK TABLE FOR MONTH
         BE    CHK31                         FOUND ? BRANCH TO CHK31
         LA    R6,4(R6)                      BUMP TABLE
         BCT   R5,LOOP1                      DECREMENT COUNTER AND LOOP
         B     LOOP2                         ELSE GOTO LOOP2
CHK31    DS    0H
         PACK  DD,WSDD                       PACK INPUT DATE
         PACK  DDCP,=CL2'31'                 PACK 31
         CP    DD,DDCP                       COMPARE INPUT DATE WITH 31
         BE    CHKDONE                       EQUAL ? GOTO LOOP3
         BC    4,LOOP2                       INPUT DATE < 31 GOTO LOOP2
LOOP2    DS    0H
         SR    R4,R4                         CLEAR R4
         SR    R5,R5                         CLEAR R5
         SR    R6,R6                         CLEAR R6
         LA    R5,MON30END                   LOAD 30MONTH NO OF ENTRIES
         LA    R4,MON30ONE                   LAOD 30MONTH ONE ENTRY LEN
         LA    R6,MON30                      LOAD 30MONTH TABLE ADDR
SUBLOOP  DS    0H
         C     R8,0(R6)                      COMPARE INP DATE WITH TAB
         BE    CHK30                         EQUAL ? BRANCH TO CHK30
         LA    R6,4(R6)                      BUMP TABLE
         BCT   R5,SUBLOOP                    DECREMENT R5 AND LOOP
         B     LOOP3                         GOTO LOOP3
CHK30    DS    0H
         PACK  DD,WSDD                       PACK INPUT DATE
         PACK  DDCP,=CL2'30'                 PACK 30
         CP    DD,DDCP                       COMPARE INP DATE WITH 30
         BE    CHKDONE                       EQUAL ? BRANCH TO LOOP3
         BC    4,LOOP3                       INPUT DATE < 30 GOTO LOOP3
         B     SETM1                         SET INVALID
LOOP3    DS    0H
         PACK  DEC,WSDD                      PACK INPUT DATE INTO DWORD
         CVB   R8,DEC                        CONVERT TO BINARY
         PACK  MM,=CL2'02'                   PACK 2
         PACK  MMCP,WSMM                     PACK INPUT MONTH
         CP    MM,MMCP                       COMPARE INP MONTH WITH 2
         BE    CHK28                         EQUAL ? BRANCH TO CHK28
         B     CHKDONE                       BRANCH TO CHKDONE
CHK28    DS    0H
         TM    FLAGF,LEAPF                   TEST LEAP FLAG
         BO    LEAPCHK2                      SET ? GOTO LEAPCHK2
         B     LEAPCHK3                      NO SET ? GOTO LEAPCHK3
LEAPCHK2 DS    0H
         C     R8,=F'29'                     COMPARE INP DATE WITH 29
         BE    CHKDONE                       EQUAL ? GOTO CHKDONE
         BC    4,CHKDONE                     INPUT DATE < 29 GOTO CHKD
         B     SETM1                         SET INVALID
LEAPCHK3 DS    0H
         C     R8,=F'28'                     COMPARE INPUT DATE WITH 28
         BE    CHKDONE                       EQUAL ? BRANCH TO CHKDONE
         BC    4,CHKDONE                     INPUT DATE < 28 ? B TO CHK
         B     SETM1                         SET INVALID
CHKDONE  DS    0H
         B     AROUND                        BRANCH AROUND INVALID
SETM1    DS    0H
*        WTO   'INPUT MONTH/DATE INVALID'
         OI    FLAG,X'40'
AROUND1  DS    0H
         L     R14,ADDR1                     LOAD RETUN ADDRESS
         BR    R14                           BRANCH
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Routine to check for a valid day.                                   *
* Routine Input : 2 Byte String(DD).                                  *
* Routine Output: Will set the FLAG mask indicating valid/            *
*                 invalid input.                                      *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DDCHK    DS    0H
         ST    R14,ADDR1                     STORE RETURN ADDR
         PACK  DD,WSDD
         PACK  DDCP,=CL2'01'                 PACK 01 TO DDCP
         CP    DD,DDCP                       COMPARE INPUT AND 01
         BC    4,SETM2                       IS INPUT < 01 ?Y SET INV
         PACK  DDCP,=CL2'31'                 PACK 31 TO DDCP
         CP    DD,DDCP                       IS INPUT > 31 ?
         BC    2,SETM2                       YES BRANCH SET INVALID
         B     OUT                           NO ? BRANCH OUT
SETM2    DS    0H
         OI    FLAG,X'40'                    SET INVALID FLAG
OUT      DS    0H
         L     R14,ADDR1                     LOAD RETURN ADDRESS
         BR    R14                           RETURN
* Program variables/constants/equates
SAVE     DS    18F
DEC      DS    D
ADDR1    DS    A
YYYY     DS    PL4
MM       DS    PL2
MMCP     DS    PL2
DD       DS    PL2
DDCP     DS    PL2
COMP1    DS    PL4
DIVID1   DS    CL3
DIVIDENT DS    0PL16
QUO      DS    PL8
REMAIN   DS    PL8
DIVISOR  DS    PL8
VALID    EQU   X'80'
INVALID  EQU   X'40'
FLAG     DC    X'00'
LEAPF    EQU   X'20'
NOLEAPF  EQU   X'10'
FLAGF    DC    X'00'
NULL1    DC    X'00'
MON31    DS    0F
         DC    F'01'
MON31ONE EQU   *-MON31
         DC    F'03'
         DC    F'05'
         DC    F'07'
         DC    F'08'
         DC    F'10'
         DC    F'12'
MON31END EQU   (*-MON31)/MON31ONE
MON30    DS    0F
         DC    F'04'
MON30ONE EQU   *-MON30
         DC    F'06'
         DC    F'09'
         DC    F'11'
MON30END EQU   (*-MON30)/MON30ONE
STATUS   DS    CL4
         YREGS
*Program DSECTS
WORKINGS DSECT
WSDATE   DS    0CL8
WSMM     DS    CL2
WSDD     DS    CL2
WSYY     DS    CL4
         END   DATEVAL