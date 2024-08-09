*ASM XOPTS(LEASM)
         TITLE 'ADA02 - REFORMATE BIRTHDAY PASSED VIA COMMAREA'
* --------------------------------------------------------------------
*
* PROGRAM NAME   : ADA02
* OBJECTIVE      : REFORMAT BIRTHDAY (DATE) USING LE CALLABLE SERVICES
*                : DATE IS PASSED FROM ADA01 VIA COMMAREA
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
ADA02    CSECT ,
*        L     R6,DFHEICAP             GET ADDRESS OF PASSED COMMAREA
         EXEC CICS ADDRESS COMMAREA(R6)
         USING COMMDS,R6               AND MAP TO IT
ADA0210  EQU   *
         BAS   R5,DATECHEK
         BAS   R5,DATEDIFF
         BAS   R5,DATEREFM
         EXEC CICS RETURN
DATECHEK EQU   *
*****************************************************************
* GET TODAY'S DATE USING CICS SERVICES                          *
*                                                               *
*****************************************************************
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
* CALL  CEELOCT
         BALR  R14,R15                   AND BRANCH TO CEELOCT
*        CALL  CEELOCT
         BR    R5
*****************************************************************
*****************************************************************
DATEDIFF EQU   *
*****************************************************************
* LOAD UP PARAMETERS TO CALCULATE DIFFERENCE BT BIRTH/TODAY     *
* USING CEEDAYS TO CONVERT DATE TO LILIAN DATE INTEGER
*****************************************************************
         LOAD  EP=CEEDAYS                LOAD ENTRY OF CEEDAYS
         LR    R15,R0                    LOAD ENTRY POINT ADDRESS
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
         L     R9,TODAYLIL               LOAD TODAY LILIAN
         L     R8,LILIAN                 LOAD BIRTHDAY IN LILIAN
         SR    R9,R8                     CALCULATE DIFFERENCE
         CVD   R9,DAYDIF1                CONVERT BINAR TO DEC
         MVC   DAYDIF2,DAYDIF1+4         MOVE PART OF FIELD ONLY
         BR    R5
*****************************************************************
*****************************************************************
DATEREFM EQU   *
         LOAD  EP=CEEDATE                LOAD ENTRY OF CEEDATE
         LR    R15,R0                    LOAD ENTRY POINT ADDRESS
         MVC   DATEVAL,DATERG            MOVE DATE TO PASS TO CEEDAYS
         MVC   DATELTH,H29               LENGTH OF PATTERN
         LA    R1,LILIAN                 SET UP ADDRESS FOR INPT LILIAN
         ST    R1,PARM1                  SAVE AS 1ST PARM
         LA    R1,PICSTR1                SET UP ADDRESS FOR PIC STR
         ST    R1,PARM2                  SAVE AS 2ND PARM OUT
         LA    R1,DATEFORM               SET UP ADDRESS FOR OUTPUT
         ST    R1,PARM3                  SAVE AS 3RD PARAMETER BACK
         LA    R1,FC                     SET UP ADDRESS FOR FEEDBACK
         ST    R1,PARM4                  SAVE AS 4TH PARM OUT
         LA    R1,DATEPARM               LOAD ADDRESS OF PARMS
* CALL  CEEDATE
         BALR  R14,R15                   AND BRANCH TO CEEDATE
*****************************************************************
* LOAD UP PARAMETERS TO REFORMAT INPUT BIRTHDAY USING LE CEEDAYS*
* USE CEEDATE PLUS PATTERN                                      *
*****************************************************************
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