*ASM XOPTS(LEASM)
         TITLE 'ADA03 - REFORMATE BIRTHDAY PASSED VIA COMMAREA'
* --------------------------------------------------------------------
*
* PROGRAM NAME   : ADA03
* OBJECTIVE      : CALCULATE POSSIBLE RETIREMENT DATE
*                : ADD 65 TO BIRTHDATE PASSED FROM ADA01
* PROGRAMMER     : J WINCHELL ADTOOLS ADVOCATE TEAM
*
* --------------------------------------------------------------------
*
DFHEISTG DSECT
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
COMMDS   DSECT
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
*
ADA03    CSECT ,
*        L     R6,DFHEICAP             GET ADDRESS OF PASSED COMMAREA
         EXEC CICS ADDRESS COMMAREA(R6)
         USING COMMDS,R6               AND MAP TO IT
ADA0310  EQU   *
         BAS   R5,RETIRE
         BAS   R5,REFORM
         EXEC CICS RETURN
RETIRE   EQU   *
* LOAD UP PARAMETERS TO CONVERT BD + 65 TO LILIAN VALUE         *
* USING CEEDAYS TO CONVERT DATE TO LILIAN DATE INTEGER
*****************************************************************
         LOAD  EP=CEEDAYS                LOAD ENTRY OF CEEDAYS
         LR    R15,R0                    LOAD ENTRY POINT ADDRESS
         PACK  DATEYYP,DATEYYYY          TAKE INPUT BIRTHDAY
         AP    DATEYYP,=P'+65'           ADD 65 YEARS
         UNPK  DATEYYYY,DATEYYP          UNPACK NEW DATE
         OI    DATEYYYY+L'DATEYYYY-1,X'F0'
         MVC   DATEVAL,DATERG            MOVE DATE TO PASS TO CEEDAYS
         MVC   DATELTH,H8                LENGTH OF PATTERN
         LA    R1,DATEPASS               SET UP ADDRESS FOR INPUT DATE
         ST    R1,PARM1                  SAVE AS 1ST PARM
         LA    R1,PICINP                 SET UP ADDRESS FOR PIC STR
         ST    R1,PARM2                  SAVE AS 2ND PARM OUT
         LA    R1,LILIAN                 SET UP ADDRESS FOR LILIAN OUt
         ST    R1,PARM3                  SAVE AS 3RD PARM OUT
         LA    R1,FC                     SET UP ADDRESS FOR FEEDBACK CD
         ST    R1,PARM4                  SAVE AS 4TH PARAMETER BACK
         LA    R1,DATEPARM
* CALL  CEEdays
         BALR  R14,R15                   AND BRANCH TO CEEDAYS
         BR    R5
*****************************************************************
*****************************************************************
* LOAD UP PARAMETERS TO REFORMAT INPUT BIRTHDAY +65             *
* USE CEEDATE PLUS PATTERN WITH DATEOUT CONTAINING FINAL FORM   *
*****************************************************************
*****************************************************************
REFORM   EQU   *
         LOAD  EP=CEEDATE                LOAD ENTRY OF CEEDATE
         LR    R15,R0                    LOAD ENTRY POINT ADDRESS
         LA    R1,LILIAN                 SET UP ADDRESS FOR INPT LILIAN
         ST    R1,PARM1                  SAVE AS 1ST PARM
         LA    R1,PICSTR1                SET UP ADDRESS FOR PIC STR
         ST    R1,PARM2                  SAVE AS 2ND PARM OUT
         LA    R1,DATEOUT                SET UP ADDRESS FOR OUTPUT
         ST    R1,PARM3                  SAVE AS 3RD PARAMETER BACK
         LA    R1,FC                     SET UP ADDRESS FOR FEEDBACK
         ST    R1,PARM4                  SAVE AS 4TH PARM OUT
         LA    R1,DATEPARM               LOAD ADDRESS OF PARMS
* CALL  CEEDATE
         BALR  R14,R15                   AND BRANCH TO CEEDATE
         MVC   DATEFORM,DATEOUT          MOVE TO COMMAREA FOR ADA01
         MVC   RETIREDT,DATEOUT          MOVE TO COMMAREA FOR ADA01
         BR    R5
*****************************************************************
*****************************************************************
         DS    0D
PICINP   DS    0CL10
PICSTR   DC    H'08'                     LENGTH OF PATTERN
         DC    C'YYYYMMDD'
PICINP1  DS    0CL31
PICSTR1  DC    H'29'                     LENGTH OF PATTERN
         DC    C'Wwwwwwwwwz DD Mmmmmmmmmz YYYY'
H8       DC    H'8'                      LENGTH OF DATE PATTERN
H29      DC    H'29'                     LENGTH OF DATE PATTERN
         DFHREGS
         LTORG
         END