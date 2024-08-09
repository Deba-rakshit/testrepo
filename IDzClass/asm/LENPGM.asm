* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **  00010000
* This program read a 255 byte input string, reverses it, fills low- *  00010100
* values with spaces and return the length of the string to the      *  00010200
* caller.                                                            *  00010300
* Program Input : 255 byte input string                              *  00010400
* Program Output: Length of Input String                             *  00010500
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **  00010600
* Sample COBOL invocation :                                          *  00010700
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **  00010800
*   WORKING-STORAGE SECTION.                                         *  00010900
*   01 WORK1.                                                        *  00011000
*     02 STRING1 PIC X(255).                                         *  00011100
*   01 WORK2.                                                        *  00011200
*     02 LEN1   PIC S9(4).                                           *  00011300
*   PROCEDURE DIVISION.                                              *  00011400
*   PROGRAM-PARA.                                                    *  00011500
*       MOVE 'INPUT STRING            ' TO STRING1.                  *  00011600
*       CALL 'LENPGM' USING WORK1, WORK2                            *   00011700
*String length will be returned in variable LEN1.                    *  00011800
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **  00011900
LENPGM   CSECT                                                          00012000
         STM   14,12,12(13)      HOUSEKEEPING                           00012100
         LR    12,15             LOAD BASE REGISTER WITH PGM ENTRY      00012200
         USING LENPGM,12         ESTABLISH BASE REGISTER                00012300
         LR    11,13             LOAD OLD SAVEA TO R11                  00012400
         LA    13,SAVE           LOAD NEW SAVEA TO R13                  00012500
         ST    11,4(13)          BACKWARD CHAIN                         00012600
         ST    13,8(11)          FWD CHAIN                              00012700
*                                                                       00012800
         L     R7,0(R1)          LOAD PARM ADDRESS TO R7(STRING VAR)    00012900
         L     R8,4(R1)          LOAD PARM ADDRESS TO R8(LENGTH VAR)    00013000
         MVC   STR1,0(R7)        MOVE INPUT STRING TO STR1              00013100
*                                                                       00013200
*                                                                       00013300
         LA    R2,STR1           ADDRESS STR1                           00013400
         LA    R1,STR2           ADDRESS STR2                           00013500
         MVCIN 0(255,1),254(2)   REVERSE INPUT STR1                     00013600
*                                                                       00013700
         LA    R5,STR2           LOAD STR2(REVERSED STRING)             00013800
         LA    R4,255            LOAD MAX VALUE                         00013900
         SR    R10,R10           CLEAR R10                              00014000
LOOP1    DS    0H                LOOP1 ROUTINE                          00014100
         CLI   0(R5),X'00'       CHECK FOR NULL                         00014200
         BE    MLOWVAL           FOUND ? BRANCH TO MLOWVAL              00014300
         B     AROUND1           NO ? BRANCH TO AROUND1                 00014400
MLOWVAL  DS    0H                                                       00014500
         MVI   0(R5),C' '        CONVERT LOW VALUES TO SPACES           00014600
AROUND1  DS    0H                                                       00014700
         LA    R5,1(R5)          BUMP R5                                00014800
         BCT   R4,LOOP1          DECREMENT R4 AND GOTO LOOP1            00014900
*                                                                       00015000
         LA    R5,STR2           ADDRESS STR2                           00015100
         LA    R4,255            LOAD MAX VALUE OF I/P TO R4            00015200
LOOP2    DS    0H                LOOP2 ROUTINE START                    00015300
         CLI   0(R5),C' '        COMPARE STR2 FOR SPACE                 00015400
         BNE   OUTSIDE           NOT FOUND? GET OUT OF LOOP2            00015500
         LA    R10,1(R10)        SPACE FOUND - BUMP SPACE COUNT R10     00015600
         LA    R5,1(R5)          BUMP R5 - TO NEXT BYTE OF STR2         00015700
         BCT   R4,LOOP2          DECREMENT R4 AND GOTO LOOP2            00015800
OUTSIDE  DS    0H                                                       00015900
         LR    R10,R4            LEN OF STRING IN R4 GOES TO R10        00016000
*                                                                       00016100
         CVD   R10,DEC1          CONVERT R10 TO PACKED                  00016200
         UNPK  LEN1,DEC1         UNPACK IT                              00016300
         MVC   0(4,R8),LEN1      MOVE LENGTH TO OUTPUT AREA             00016400
*                                                                       00016500
AROUND   DS    0H                                                       00016600
         L     R13,SAVE+4                                               00016700
         LM    14,12,12(13)                                             00016800
         LA    15,0                                                     00016900
         BR    14                                                       00017000
*                                                                       00017100
*  PROGRAM VARIABLES/CONSTANTS                                          00017200
*                                                                       00017300
SAVE     DS    18F                                                      00017400
STR1     DS    CL255                                                    00017500
STR2     DS    CL255                                                    00017600
DEC1     DS    D                                                        00017700
LEN1     DS    CL4                                                      00017800
         YREGS                                                          00017900
         END   LENPGM                                                   00018000