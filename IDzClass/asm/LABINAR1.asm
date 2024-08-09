         PRINT   NOGEN
LABINARS CEEENTRY PPA=MAINPPA,AUTO=WORKSIZE,MAIN=NO,BASE=R10
         USING    WORKAREA,R13
*LABINARS AMODE   31                                                    00000901
*LABINARS RMODE   24                                                    00000901
**********************************************************************  00001000
*       FUNCTION - PRACTICE BINARY ARITHMETIC                           00002001
*                - USE BINARY AND SUM DIGITS FROM 1 TO 10               00002101
*                  SET UP REGISTER 5 WITH VALUE 1 (USE THIS TO ADD..)   00002201
*                  SET UP REGISTER 6 AS A COUNTER THAT                  00002301
*                    INCREMENTS BY 1 FOR EACH LOOP                      00002401
*                  SET UP REGISTER 7 AS SUM HOLDER                      00002501
*                  ADD REG6 AND COUNTER TO REG7                         00002601
*                  CONVERT THE ANSWER TO DECIMAL AND DISPLAY IT         00002701
*                  **CORRECT ANSWER IS 55!                              00002801
*       INPUT(S) - NONE                                                 00003001
*      OUTPUT(S) - SYSPRINT                                             00004000
*          ENTRY - LABINARS                                             00005001
*          PARMS - NONE                                                 00006000
* NORMAL EXIT(S) - RETURN TO CALLER                                     00007000
* ERROR  EXIT(S) - NONE                                                 00008000
*    MACROS USED - SAVE, RETURN                                         00009000
* EXTERNAL CALLS - NONE                                                 00010000
**********************************************************************  00020000
* STANDARD LINKAGE FOR A REUSABLE MVS CSECT                             00030000
**********************************************************************  00040000
*        SAVE  (14,12)                SAVE CALLER'S REGS                00050000
*        BASR  R12,0                  ESTABLISH                         00060000
*        USING *,R12                  ADDRESSABILITY                    00070000
*        LA    R2,SAVEAREA            POINT TO MY LOWER-LEVEL SA        00080000
*        ST    R2,8(,R13)             FORWARD-CHAIN MINE FROM CALLER'S  00090000
*        ST    R13,SAVEAREA+4         BACK-CHAIN CALLER'S FROM MINE     00100000
*        LR    R13,R2                 SET 13 FOR MY SUBROUTINE CALLS    00110000
**********************  BEGIN LOGIC  *********************************  00120000
A10BEG   EQU   *                                                        00121001
         OPEN  (PRTOUT,(OUTPUT))      OPEN PRINT FILE                   00122002
         LTR   R15,R15                CHECK RETURN CODE                 00123001
         BNZ   RETURN                 GET OUT IF NO GOOD                00124001
         LH    R5,H1                  LOAD R5 WITH 1 FOR INCREMENTING   00130001
         SR    R6,R6                  CLEAR COUNTER REGISTER            00140001
         SR    R7,R7                  CLEAR SUM REGISTER                00150001
BINLOOP  EQU   *                                                        00160001
         AR    R6,R5                  ADD 1 TO REG 6                    00161001
         AR    R7,R6                  ACCUMULATE IN REG 7               00162001
         CH    R6,H10                 HAVE WE DONE 10?                  00163001
         BNE   BINLOOP                NOT YET                           00164001
         CVD   R7,ANSWER              CONVERT TO PACKED                 00164101
         UNPK  PRTANS,ANSWER          UNPACK FOR PRINTING               00164205
         OI    PRTANS+L'PRTANS-1,X'F0' ENSURE CORRECT SIGN              00164401
         PUT   PRTOUT,PRTAREA         WRITE PRINT AREA                  00164502
         CLOSE PRTOUT                                                   00164702
**********************  END LOGIC    *********************************  00164800
RETURN   DS    0H                     BRANCH TO HERE FOR NORMAL RETURN  00164900
         CEETERM RC=0
*        L     R13,SAVEAREA+4         POINT TO CALLER'S SAVE AREA       00165000
*        RETURN (14,12),RC=0          RESTORE CALLER'S REGS & RETURN    00165100
*                                     WITH ZERO IN REGISTER 15          00166000
**********************  REG SAVEAREA *********************************  00169401
         DS    0D
SAVEAREA DC    18F'0'                 SAVE AREA IN MY PROGRAM           00169501
**********************  REG EQUATES  *********************************  00170001
R0       EQU   0                                                        00221800
R1       EQU   1                                                        00221900
R2       EQU   2                                                        00222000
R3       EQU   3                                                        00222100
R4       EQU   4                                                        00222200
R5       EQU   5                                                        00223000
R6       EQU   6                                                        00224000
R7       EQU   7                                                        00225000
R8       EQU   8                                                        00226000
R9       EQU   9                                                        00227000
R10      EQU   10                                                       00228000
R11      EQU   11                                                       00229000
R12      EQU   12                                                       00230000
R13      EQU   13                                                       00240000
R14      EQU   14                                                       00250000
R15      EQU   15                                                       00260000
         LTORG                                                          00261000
**********************  DATA AREAS   *********************************  00167000
H1       DC    H'1'                   HALFWORD VALUE 1                  00167101
H10      DC    H'10'                  HALFWORD VALUE 10                 00167202
         DS    0D                     ENSURE DOUBLEWORD ALIGNMENT       00167302
ANSWER   DC    PL8'0'                 ANSWER PACKED HERE                00167401
PRTAREA  DS    0CL121                 PRINT AREA                        00167601
         DS    CL1                    RESERVE FOR C/C                   00167701
         DS    CL10                                                     00167806
         DC    CL10'ANSWER =  '                                         00167901
PRTANS   DS    CL10                   PRINT ANSWER VALUE HERE           00168006
         DS    CL90                   REST OF PRINT AREA UNUSED         00168106
**********************  DCB DEFINITIONS ******************************  00168206
PRTOUT   DCB   DDNAME=SYSPRINT,       DCB FOR PRINTOUT                 C00168301
               DSORG=PS,                                               C00168401
               LRECL=121,                                              C00168501
               RECFM=FBA,                                              C00168601
               MACRF=(PM)                                               00168701
SNAP     DCB   DDNAME=SNAPOUT,        DCB FOR SNAP DUMP                C00168806
               DSORG=PS,                                               C00168906
               LRECL=125,                                              C00169006
               BLKSIZE=1632,                                           C00169106
               RECFM=VBA,                                              C00169206
               MACRF=(W)                                                00169306
*
MAINPPA    CEEPPA
*
WORKAREA   DSECT
           ORG    *+CEEDSASZ             Leave space for DSA Fixed area
*
           DS     0D
WORKSIZE   EQU    *-WORKAREA
           CEEDSA SECTYPE=ALL
           CEECAA
         END   LABINARS                                                 00270000