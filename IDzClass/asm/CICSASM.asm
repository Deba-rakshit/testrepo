*ASM XOPTS(LEASM)
         TITLE 'CICSASM - REFORMAT BIRTHDAY PASSED VIA COMMAREA'
* --------------------------------------------------------------------
*
* PROGRAM NAME   : CICSASM
* OBJECTIVE      : REFORMAT BIRTHDAY (DATE) USING LE CALLABLE SERVICES
*                : DATE IS PASSED FROM ADA01 VIA COMMAREA
* PROGRAMMER     : JANICE WINCHELL - DEBUG TOOL SPECIALIST
* NOTES          : BE SURE TO INCLUDE EQADCCXT IF YOU ARE USING DTCN
*
* --------------------------------------------------------------------
*
DFHEISTG DSECT
COMMAREA DS    0CL218                        COMMAREA
         DS    0D
DAYDIF1  DS    PL8
DAYDIF2  DS    PL4
DATEFORM DS    CL29                          FORMATTED DATE
RETIREIN DS    CL1                           RETIREMENT INDICATOR
RETIREDT DS    CL29                          RETIREMENT DATE FORMATTED
DATERG   DS    0CL8                          DATE FROM INPUT MAP
DATEYYYY DS    CL4
DATEMM   DS    CL2
DATEDD   DS    CL2
DATERET  DS    CL4
FC       DS    CL12
LILIAN   DS    F
SECS     DS    6D
GREGOR   DS    CL17
TODAYLIL DS    F
DAYDIFF  DS    F                             NO OF DAYS FROM BIRTHDAY
COUNTER  DS    PL1
CURRYYP  DS    PL3'0'
DATEYYP  DS    PL3'0'
DATEMMP  DS    PL2'0'
DATEDDP  DS    PL2'0'
PDATE    DS    CL5
FLAG     DS    CL1
RESPONSE DS    F
PROCESS  DS    CL1                           WHAT TO DO INDICATOR
DATEIND  DS    CL1                           VALID DATE SWITCH
DATAOUT  DS    CL20
LENOFDT  EQU   *-COMMAREA
         COPY  DFHBMSCA                      COPY STD BMS ATTR
         COPY  DFHAID                        COPY AID KEYS DEFNS
         COPY  ADMA0                         COPY MAP FOR BIRTHDAY PROG
DATEPARM DS    0F
PARM1    DS    A
PARM2    DS    A
PARM3    DS    A
PARM4    DS    A
DATEPASS DS    0F
DATELTH  DS    H
DATEVAL  DS    CL8
OUTDATE  DS    CL12
DATEOUT  DS    CL80
ADA01    CSECT
         CLC   EIBCALEN,=H'0'              IS THIS FIRST TIME THROUGH?
         BNE   RECEIVE                    .. NO READ MAP FROM SCREEN
         B     SENDMAP
RECEIVE  EQU   *
         CLI   EIBAID,DFHENTER                       WAS ENTER PRESSED?
         BE    RECMAP                                YES, RECEIVE MAP
         CLI   EIBAID,DFHPF3                         WAS PF3 PRESSED?
         BE    TRANEND                               YES, END TRANS
         CLI   EIBAID,DFHPF12                        WAS PF12 PRESSED?
         BE    TRANEND                               YES, END TRANS
         CLI   EIBAID,DFHCLEAR                       WAS CLEAR PRESSED?
         BE    TRANEND                               YES, END TRANS
* ANY OTHER KEY WILL CAUSE AN ERROR MESSAGE AND THE SCREEN WILL BE
* REDISPLAYED
*
         MVC   ADBDAYMO,BADSEL
         MVI   ADBDAYMA,DFHBMASB
**
**  Add Send Map statement here... or below as indicated by comments

         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
               LENGTH(218)
TRANEND  EQU  *
         EXEC CICS SEND TEXT FROM(ENDTRAN) LENGTH(44) ERASE FREEKB
         DFHEIRET
RECMAP   EQU   *
*        WTO  'ABOUT TO RECEIVE MAP',ROUTCDE=11
         EXEC CICS RECEIVE MAP('ADMENU') MAPSET('ADMA0')               X
              RESP(RESPONSE) INTO(ADMENUS)
         CLC  RESPONSE,DFHRESP(MAPFAIL)               NULL MAP?
         BE   MAPFAIL                                 YES,REDISPLAY MAP
         CLC  RESPONSE,DFHRESP(NORMAL)                NORMAL RESPONSE?
         BNE  ERRORS                                  NO, TERMINATE
         CLC  ADBDAYL,=X'0000'                      WAS A DATE ENTERED?
         BE   NONNUM                                NO, ISSUE ERROR MSG
         CLI  ADBDAYII,C'1'                           GO VERIFY DATE
         BE   DATECHK
         CLI  ADBDAYII,C'2'                           CALC RETIREMENT
         BE   RETIRE
INPUTERR EQU  *
         XC   ADBDAYMO,ADBDAYMO                     CLEAR MSG
         MVC  ADBDAYMO,BADSEL                       MOVE IN NEW MSG
         MVI  ADBDAYMA,DFHBMASB                     SET ATTRIBUTE
         MVI  ADBDAYA,DFHBMFSE                      SET ON MDT
**
**  Add Send Map statement here... or above as indicated by comments

         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         x
               LENGTH(218)
*                                                     RE-ENTER FIELDS
DATECHK  EQU  *
*********************************************************************
* CHECK REFORMAT INPUT DATE WITH LE CALLABLE ROUTINES               *
*********************************************************************
         MVC  DATERG,ADBDAYI                          MOVE IN DATE
         BAS  R5,CEELOCT                              INPUT DATE CHECK
         EXEC CICS LINK PROGRAM(ADA02) COMMAREA(COMMAREA)              x
              LENGTH (218)
         MVI  ADBDFMTA,DFHBMASB                   TURN ON DATE INFO
         MVI  ADBDFM1A,DFHBMASB                   TURN ON DATE VERBAGE
         MVI  ADBDFM2A,DFHBMASB                   MAKE FORMATTED DATE
         MVC  ADBDFM2O,DATEFORM                   MOVE IN NEW DATE
         MVI  ADDIFF1A,DFHBMASB                   TURN ON TEXT
         MVC  OUTDATE,EDDATE                      MOVE IN EDIT PATTERN
         ED   OUTDATE,DAYDIF2                     AND EDIT DATE
         MVC  ADDIFF2O,OUTDATE                     MOVE IN NO  OF DAYS
         MVI  ADDIFF2A,DFHBMASB                   TURN ON DATA ATTR
         MVI  ADDIFF3A,DFHBMASB                   TURN ON TEXT 'DAYS'
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           x
               RESP(RESPONSE) FREEKB
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
               LENGTH(218)
RETIRE   EQU  *
*********************************************************************
* CALCULATE RETIREMENT DATE IF YOU RETIRE AT AGE 65                 *
*********************************************************************
         MVC  DATERG,ADBDAYI                          MOVE IN DATE
         BAS  R5,CEELOCT                              INPUT DATE CHECK
         EXEC CICS LINK PROGRAM(ADA03) COMMAREA(COMMAREA)              X
              LENGTH (218)
         MVI  ADBRETA,DFHBMASB                    TURN ON DATE INFO
         MVI  ADRET1A,DFHBMASB                   TURN ON DATE VERBAGE
         MVI  ADRET2A,DFHBMASB                   TURN ON OUTPUT FOR DT
         XC   ADRET2O,ADRET2O                     CLEAR OUT RETIRE DATE
         MVC  ADRET2O(L'RETIREDT),RETIREDT
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
              LENGTH(218)
ERRORS   EQU  *
         XC   MSGERRO,MSGERRO
         MVC  MSGERRO(L'DTERROR),DTERROR
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB MAPONLY
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
              LENGTH(218)
MAPFAIL  EQU  *
ENDA     EQU  *
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
               LENGTH(218)
SENDMAP  EQU   *
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB MAPONLY
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
               LENGTH(218)
***********************************************************************
*  CHECK DATE RANGE YEAR BETWEEN 1580 AND 1000(CANNOT ENTER HIGHER    *
*  THAN 9999 AND FOR BIRTHDAY EVEN 2008 IS CONSIDERED INVALID         *
*  SUBROUTINE ENTERED VIA BAS R5                                      *
***********************************************************************
CEELOCT  EQU  *
         LOAD  EP=CEELOCT                LOAD ENTRY OF CEELOCT
         LR    R15,R0                    LOAD ENTRY POINT ADDRESS
         LA    R1,TODAYLIL               SET UP ADDRESS FOR RET LILIAN
         ST    R1,PARM1                  SAVE AS 1ST PARM
         LA    R1,SECS                   SET UP ADDRESS FOR SECS
         ST    R1,PARM2                  SAVE AS 2ND PARM OUT
         LA    R1,GREGOR                 SET UP ADDRESS FOR GREGORIAN
         ST    R1,PARM3                  SAVE AS 3RD PARM OUT
         LA    R1,FC                     SET UP ADDRESS FOR FEEDBACK CD
         ST    R1,PARM4                  SAVE AS 4TH PARAMETER BACK
         LA    R1,DATEPARM
         BALR  R14,R15                   AND BRANCH TO CEELOCT
         PACK  DATEYYP,DATEYYYY         PACK INPUT DATE FOR TEST
         CP    DATEYYP,P1580            INPUT < 1580?
         BL    DTERRS                   YES, ERROR
         PACK  CURRYYP,GREGOR(4)
         CP    DATEYYP,CURRYYP           INPUT >=CURRENT YEAR?
         BNL   DTERRS                    INVALID BIRTHDAY IF YES
CHKMM    EQU  *
         PACK DATEMMP,DATEMM                     PACK MONTH FOR TEST
         PACK DATEDDP,DATEDD                     PACK DAY
CHKMM1   EQU  *
         CP   DATEMMP,P01                        LESS THAN 01?
         BL   DTERRS1                            YES, MONTH IN ERROR
         CP   DATEMMP,P12                        MONTH > 12?
         BH   DTERRS1                            YES, MONTH IN ERROR
CHKDAY01 EQU  *
         CP   DATEMMP,P01                        IS THIS JANUARY?
         BNE  CHKDAY02                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKLEAP  EQU  *
         CP   DATEYYP,P2000                      IS THIS A LEAP YEAR?
         BNE  CHKDAY02                           NO
         CP   CHKMM,P29                          29 DAYS IS OK
         BNH DTEXIT                              ANYTHING UNDER 29 ok
CHKDAY02 EQU  *
         CP   DATEMMP,P02                        IS THIS FEBRUARY?
         BNE  CHKDAY03                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P28                        28 DAYS?
         BNH  DTEXIT                             MONTH/DAY ok - EXIT
         B    DTERRS1
CHKDAY03 EQU  *
         CP   DATEMMP,P03                        IS THIS MARCH?
         BNE  CHKDAY04                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY04 EQU  *
         CP   DATEMMP,P04                        IS THIS APRIL?
         BNE  CHKDAY05                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P30                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY05 EQU  *
         CP   DATEMMP,P05                        IS THIS MAY?
         BNE  CHKDAY06                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY06 EQU  *
         CP   DATEMMP,P06                        IS THIS JUNE?
         BNE  CHKDAY07                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P30                        30 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY07 EQU  *
         CP   DATEMMP,P07                        IS THIS JULY?
         BNE  CHKDAY08                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY08 EQU  *
         CP   DATEMMP,P08                        IS THIS AUGUST?
         BNE  CHKDAY09                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY09 EQU  *
         CP   DATEMMP,P09                        IS THIS SEPTEMBER?
         BNE  CHKDAY10                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY10 EQU  *
         CP   DATEMMP,P10                        IS THIS OCTOBER?
         BNE  CHKDAY11                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY11 EQU  *
         CP   DATEMMP,P11                        IS THIS NOVEMBER?
         BNE  CHKDAY12                           NO, CHECK NEXT MONTH
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             MONTH/DAY OK - EXIT
         B    DTERRS1
CHKDAY12 EQU  *
         CP   DATEMMP,P12                        IS THIS DECEMBER?
         BNE  DTERRS1                            NO, ERROR
         CP   DATEDDP,P31                        31 DAYS?
         BNH  DTEXIT                             NO, ERROR IN DAYS
DTERRS   EQU  *                                  ERROR MESSAGE
         XC   MSGERRO,MSGERRO                    CLEAR MESSAGE AREA
         MVC  MSGERRO(L'DTERROR),DTERROR
         MVI  MSGERRA,DFHBMASB                   MSG=BRIGHT
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
              LENGTH(218)
DTERRS1  EQU  * ERROR MESSAGE
         XC   MSGERRO,MSGERRO                    CLEAR MESSAGE AREA
         MVC  MSGERRO(L'DTERROR1),DTERROR1
         MVI  MSGERRA,DFHBMASB                   MSG=BRIGHT
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
              LENGTH(218)
NONNUM   EQU  *                                  NON-DATE ERR MESSAGE
         MVI  ADBDAYMA,DFHBMASB                  TURN ON MESSAGE IN MAP
         EXEC  CICS SEND MAP('ADMENU') MAPSET('ADMA0') ERASE           X
               RESP(RESPONSE) FREEKB
         EXEC CICS RETURN TRANSID(EIBTRNID) COMMAREA(COMMAREA)         X
              LENGTH(218)
DTEXIT   EQU  *
         BR   R5
*********************************************************************
* END OF INTERNAL CHECK FOR VALID DATE RANGE                        *
*********************************************************************
         DS     0D
         LTORG
         DFHEIRET
ADAT     DC   CL4'ADAT'
BADSEL   DC   CL44'PLEASE RE-ENTER, EITHER 1 OR 2 TO PROCESS    '
ENDTRAN  DC   CL44'THE ADAT TRANSACTION IS NOW ENDED. THANK YOU'
DTERROR  DC   CL44'INVALID YEAR ENTERED, MUST BE > 1580 < TODAY'
DTERROR1 DC   CL44'EITHER MONTH IS NOT 01-12 OR DAY IS INVALID '
DTERROR3 DC   CL44'PLEASE ENTER A VALID DATE AS YYYYMMDD       '
ERRMSG1  DC   CL44'INVALID KEY PRESSED, HIT ENTER OR CLEAR KEY '
ERRMSG2  DC   CL44'PLEASE ENTER DATE AND OPTION 1 OR 2, + ENTER'
EDDATE   DC   X'40206B2020216B202020'
P1580    DC   PL3'+1580'
P10000   DC   PL3'+10000'
P2000    DC   PL3'+2000'
P2008    DC   PL3'+2008'
H1       DC   H'-1'
P1       DC   PL1'+1'
P01      DC   PL2'+01'
P02      DC   PL2'+02'
P03      DC   PL2'+03'
P04      DC   PL2'+04'
P05      DC   PL2'+05'
P06      DC   PL2'+06'
P07      DC   PL2'+07'
P08      DC   PL2'+08'
P09      DC   PL2'+09'
P10      DC   PL2'+10'
P11      DC   PL2'+11'
P12      DC   PL2'+12'
P28      DC   PL2'+28'
P29      DC   PL2'+29'
P30      DC   PL2'+30'
P31      DC   PL2'+31'
ADA02    DC   CL8'ADA02   '
ADA03    DC   CL8'ADA03   '
         DFHREGS
         LTORG
         END