******************************************************************
*                                                                *
******************************************************************
ASM01B   CSECT ,                                                01S0001
@MAINENT DS    0H                                               01S0001
         USING *,@15                                            01S0001
         B     @PROLOG                                          01S0001
         DC    AL1(16)                                          01S0001
         DC    C'ASM01B    97.295'                              01S0001
         DROP  @15
@PROLOG  STM   @14,@12,12(@13)                                  01S0001
         LR    @12,@15                                          01S0001
@PSTART  EQU   ASM01B                                           01S0001
         USING @PSTART,@12                                      01S0001
         ST    @13,@SA00001+4                                   01S0001
         LA    @14,@SA00001                                     01S0001
         ST    @14,8(,@13)                                      01S0001
         LR    @13,@14                                          01S0001
         MVC   @PC00001(4),0(@01)                               01S0001
*   EXPARM2 = 2;                                                01S0007
         LA    @02,2                                            01S0007
         ST    @02,EXPARM2                                      01S0007
*   IF (PARM2 = 2) THEN             /* THIS BRANCH NEVER TAKEN       */
         L     @03,@PA00063                                     01S0008
         C     @02,PARM2(,@03)                                  01S0008
         BNE   @RF00008                                         01S0008
*     CALL PROC1(EXPARM2);          /* PROC1 ALWAYS CALLED           */
         LA    @01,@AL00002                                     01S0009
         BAL   @14,PROC1                                        01S0009
*   ELSE                            /*                               */
*     CALL ASM01D;                  /* ASM01D NEVER CALLED           */
         B     @RC00008                                         01S0010
@RF00008 L     @15,@CV00065                                     01S0010
         BALR  @14,@15                                          01S0010
*   DO WHILE(EXPARM1>0);            /* DO LOOP EXECUTED 10 TIMES     */
         B     @DE00011                                         01S0011
@DL00011 DS    0H                                               01S0012
*     EXPARM1 = EXPARM1 - 1;                                    01S0012
         L     @00,EXPARM1                                      01S0012
         BCTR  @00,0                                            01S0012
         ST    @00,EXPARM1                                      01S0012
*     CALL ASM01C;                  /* ASM01C CALLED 10 TIMES        */
         L     @15,@CV00064                                     01S0013
         BALR  @14,@15                                          01S0013
*   END;                                                        01S0014
@DE00011 L     @00,EXPARM1                                      01S0014
         LTR   @00,@00                                          01S0014
         BP    @DL00011                                         01S0014
*   DO WHILE(EXPARM2>10);           /* DO LOOP NEVER EXECUTED        */
         B     @DE00015                                         01S0015
@DL00015 DS    0H                                               01S0016
*     EXPARM2 = EXPARM2 - 1;        /* THIS STATEMENT NEVER EXECUTED */
         L     @01,EXPARM2                                      01S0016
         BCTR  @01,0                                            01S0016
         ST    @01,EXPARM2                                      01S0016
*   END;                            /* END OF MAIN PROCEDURE         */
@DE00015 L     @02,EXPARM2                                      01S0017
         LA    @03,10                                           01S0017
         CR    @02,@03                                          01S0017
         BH    @DL00015                                         01S0017
*   END ASM01B;                                                 01S0022
@EL00001 L     @13,4(,@13)                                      01S0022
@EF00001 DS    0H                                               01S0022
@ER00001 LM    @14,@12,12(@13)                                  01S0022
         BR    @14                                              01S0022
*PROC1:                                                         01S0018
*   PROCEDURE(P1PARM1);             /* THIS PROCEDURE ALWAYS EXECUTED*/
PROC1    STM   @14,@12,12(@13)                                  01S0018
         MVC   @PC00002(4),0(@01)                               01S0018
*   P1PARM1 = 10;                                               01S0020
         L     @04,@PA00066                                     01S0020
         LA    @05,10                                           01S0020
         ST    @05,P1PARM1(,@04)                                01S0020
*   END PROC1;                                                  01S0021
@EL00002 DS    0H                                               01S0021
@EF00002 DS    0H                                               01S0021
@ER00002 LM    @14,@12,12(@13)                                  01S0021
         BR    @14                                              01S0021
@DATA    DS    0H
         DS    0F
@AL00002 DS    0A
         DC    A(EXPARM2)
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@PC00002 DS    1F
         DS    0F
@CV00064 DC    V(ASM01C)
@CV00065 DC    V(ASM01D)
         LTORG
         DS    0D
EXPARM1  DC    F'10'
EXPARM2  DC    F'2'
@DYNSIZE EQU   0
@00      EQU   0
@01      EQU   1
@02      EQU   2
@03      EQU   3
@04      EQU   4
@05      EQU   5
@06      EQU   6
@07      EQU   7
@08      EQU   8
@09      EQU   9
@10      EQU   10
@11      EQU   11
@12      EQU   12
@13      EQU   13
@14      EQU   14
@15      EQU   15
PARM2    EQU   0,4,C'F'
P1PARM1  EQU   0,4,C'F'
@PA00066 EQU   @PC00002,4,C'F'
@PA00063 EQU   @PC00001,4,C'F'
@RC00008 EQU   @DE00011
         DS    0D
@ENDDATA EQU   *
@MODLEN  EQU   @ENDDATA-ASM01B
         END   ,