ASSEM123 CSECT
        STM    R14,R12,12(R13)         SAVE REGS
        BALR   R12,0                   ESTABLISH BASE REGISTER
        USING  *,R12
        ST     R13,WKAREA+4
        LA     R13,WKAREA
        SPACE  3
        L      R6,0(R1)                ESTABLISH ADDR TO BEG OF W-S
        USING  WSSTRT,R6
        L      R5,4(R1)                ESTABLISH ADDR TO END OF W-S
        USING  WSEND,R5
        L      R8,8(R1)                ESTABLISH ADDR TO W-S SAVE AREA
        USING  WSSAVE,R8
        L      R4,12(R1)               ESTABLISH ADDR TO W-S PARM LIST
        USING  WSPARM,R4
        MVI    DONEFLG,NOTDONE         SET FLAG TO NOT DONE
        LR     R7,R5                       PREPARE FOR SUBTRACTION
        SR     R7,R6                       DETERMINE LENGTH OF W-S

        AH     R7,ONE                 FOR CORRECT LENGTH, SET UP 1
        CH     R7,FULLREC        BIGGER THAN THE 1K SAVE REC
        BH     MRECS                  NEED TO STORE MULTIPLE SAVE RECS
        LR     R9,R7                      SET UP FOR 1 MOVE AND SAVE
        STH    R7,0(R8)                SET LENGTH IN SAVE REC
        LA     R8,2(R8)                BUMP TGT PTR PAST LENGTH 1/2WORD
        MVCL   R8,R6                  MOVE WS TO WS SAVE REC
        MVI    DONEFLG,DONE            SET FLAG TO DONE
        B      FINISH                    EXIT
MRECS   DS     0H
        CLC    SAVER6,=F'00'           FIRST TIME THROUGH?
        BE     FIRST
        L      R6,SAVER6
        LR     R7,R5
        SR     R7,R6
        AH     R7,ONE                      SET UP 1 FOR PROPER LENGTH
        CH     R7,FULLREC             REMAINING WS <= SAVE REC SIZE?
        BNH    CONT1                     YES...MOVE ACTUAL WS LENGTH
FIRST   LH     R7,FULLREC        NO...LOAD MAX REC SIZE
CONT1   LR     R9,R7                   LOAD ODD RECS WIH MOVE LENGTH
        STH    R7,0(R8)                    SET LENGTH IN SAVE REC
        LA     R8,2(R8)                     BUMP TGT PTR PAST LENGTH
        MVCL   R8,R6                   MOVE WS TO WS SAVE REC
        AR     R6,R7                   BUMP SOURCE PTR BY MOVE LENGTH
        ST     R6,SAVER6               SAVE FOR NEXT CALL
        CR     R6,R5                   MOVED ALL WS?
        BL     FINISH                  NOT YET...EXIT
        MVI    DONEFLG,DONE            SET FLAGE TO DONE
        B      FINISH                  AND EXIT
        EJECT

FINISH  DS     0H
        XR     R15,R15                 ZERO TO RETURN CODE REG
        L      R13,WKAREA+4            RESTORE BACK PTR
        LM     R14,R12,12(R13)         RESTORE REGS
        BR     R14                     RETURN TO CALLER
        SPACE  3

* Defining all the variables used in the assembler routine

WKAREA  DS     18F                     REG SAVE AREA
FULLREC DC     H'0998'                 MAX REX SIZE FOR WS SAVE REC
ONE     DC     H'0001'                 MAX REX SIZE FOR WS SAVE REC
        SPACE  1
NEWSLEN DC     F'00000000'             NEW WS LENGTH SAVE AREA
WSACUM  DC     F'00000000'             OLD WS ACCUMULATOR ON RESTORE
WTOMSG  WTO     'U0001-** RSAM ABEND - CHANGE IN WORKING STORAGE SIZE',X
               ROUTCDE=11,MF=L
        SPACE  1

         IF (CLC,KEY,L,=F'10')
               BRAS R12,SMALL
         ELSEIF (CLC,KEY,L,=F'50')
               BRAS R12,MEDIUM
         ELSE
               BRAS R12,BIG
         ENDIF

         DO UNTIL=(LTR,R1,Z,R1)
               LR R2,R1
               L R1,NEXT(R1)
         ENDDO

         BRAS R12,GETREC
         DO WHILE (CLI,EOF,NE,TRUE)
               BRAS R12,PROREC
               BRAS R12,GETREC
         ENDDO

        IF  (10),OR,                                                   X
               (AR,R2,R3,NZ),AND,                                      X
               (ICM,R1,M3,B2(D2),4)
               BRAS R12,SMALL
        ELSE
               BRAS R12,BIG
        ENDIF

OUTER  DO WHILE=2
          DO WHILE=4
            MVC A,D
            IF (CLC,A,EQ,B)
               ITERATE OUTER
            ELSE
                ITERATE
            ENDIF
          ENDDO
        ENDDO

LOOP   DO WHILE=2
          DO WHILE=4
            MVC A,D
            IF (CLC,A,EQ,B)
               ASMLEAVE LOOP
            ELSE
               ASMLEAVE
            ENDIF
          ENDDO
        ENDDO

        SELECT CLI,0(R6),EQ           Defines the comparison
          WHEN  (X'20')
               BRAS R12,A
          WHEN  (1,5,13)
               BRAS R12,B
          WHEN  (3,7,15)
               BRAS R12,C
        ENDSEL



*/Here 4 DSECTS are given that correspond to 4 parameters
*/sent to the corresponding COBOL code
*/(these are captured in the LINKAGE SECTION of the COBOL code)

WSSTRT  DSECT                          WORKING-STORAGE START ADDR
        DS     F
WSEND   DSECT                          WORKING-STORAGE END ADDR
        DS     F
WSSAVE  DSECT                          WORKING-STORAGE SAVE RECORD
        DS     F
WSPARM  DSECT                          WORKING-STORAGE SAVE MODE
MODE    DS     CL1
SAVER6  DS     F              R6 SAVE AREA
DONEFLG DS     CL1            TELLS IF ALL OF WS WAS MOVED


DONE    EQU    C'D'                    DONE MOVE
NOTDONE EQU    C'N'                    NOT DONE MOVE
ERROR   EQU    C'E'                    WS LENGTH HAS CHANGED ON RESTART
        SPACE  1
R1      EQU    1                       PARAMETER LIST ADDRESS
R3      EQU    3                       WORK REGISTER
R4      EQU    4                       PARAMETER LIST ADDRESS
R5      EQU    5                       PARAMETER LIST ADDRESS
R6      EQU    6                       W-S PARM ADDR
R7      EQU    7                       W-S START ADDR
R8      EQU    8                       W-S END ADDR
R9      EQU    9                       W-S SAVE RECORD ADDR
R10     EQU    10                      SAVE REG FOR W-S START ADDR
R11     EQU    11
R12     EQU    12                      MODULE BASE REG
R13     EQU    13
R14     EQU    14                      LINK REGISTER
R15     EQU    15                      BRANCH REGISTER
        SPACE  3
        END