******************************************************************
*                                                                *
******************************************************************
ASM01C   CSECT ,                                                01S0001
@MAINENT DS    0H                                               01S0001
         USING *,@15                                            01S0001
         B     @PROLOG                                          01S0001
         DC    AL1(16)                                          01S0001
         DC    C'ASM01C    97.295'                              01S0001
         DROP  @15
@PROLOG  STM   @14,@12,12(@13)                                  01S0001
         LR    @12,@15                                          01S0001
@PSTART  EQU   ASM01C                                           01S0001
         USING @PSTART,@12                                      01S0001
         ST    @13,@SA00001+4                                   01S0001
         LA    @14,@SA00001                                     01S0001
         ST    @14,8(,@13)                                      01S0001
         LR    @13,@14                                          01S0001
*   IF (EXPARM1 = 0) THEN           /* THIS BRANCH ALWAYS TAKEN      */
         L     @01,EXPARM1                                      01S0005
         LTR   @01,@01                                          01S0005
         BNZ   @RF00005                                         01S0005
*     DO;                                                       01S0006
*       CALL ASM01D;                /* CALL TO ASM01D NEVER MADE     */
         L     @15,@CV00064                                     01S0007
         BALR  @14,@15                                          01S0007
*     END;                                                      01S0008
*   IF (EXPARM2 = 2) THEN           /* THIS BRANCH NEVER TAKEN       */
@RF00005 CLC   EXPARM2(4),@CF00020                              01S0009
         BNE   @RF00009                                         01S0009
*     CALL PROC1(EXPARM2);          /* PROC1 ALWAYS CALLED           */
         LA    @01,@AL00002                                     01S0010
         BAL   @14,PROC1                                        01S0010
*   DO WHILE(EXPARM2>100);          /* THIS DO LOOP NEVER EXECUTED   */
         B     @DE00011                                         01S0011
@DL00011 DS    0H                                               01S0012
*     EXPARM2 = EXPARM2 - 1;        /* THIS STATEMENT NEVER EXECUTED */
         L     @00,EXPARM2                                      01S0012
         BCTR  @00,0                                            01S0012
         ST    @00,EXPARM2                                      01S0012
*   END;                            /* END OF MAIN PROCEDURE         */
@DE00011 L     @01,EXPARM2                                      01S0013
         LA    @00,100                                          01S0013
         CR    @01,@00                                          01S0013
         BH    @DL00011                                         01S0013
*   END ASM01C;                                                 01S0022
@EL00001 L     @13,4(,@13)                                      01S0022
@EF00001 DS    0H                                               01S0022
@ER00001 LM    @14,@12,12(@13)                                  01S0022
         BR    @14                                              01S0022
*PROC1:                                                         01S0014
*   PROCEDURE(P1PARM1);             /* THIS PROCEDURE ALWAYS EXECUTED*/
PROC1    STM   @14,@12,@SA00002                                 01S0014
         MVC   @PC00002(4),0(@01)                               01S0014
*   P1PARM1 = 10;                                               01S0016
         L     @02,@PA00065                                     01S0016
         LA    @03,10                                           01S0016
         ST    @03,P1PARM1(,@02)                                01S0016
*   IF (EXPARM1 = 0) THEN           /* THIS BRANCH ALWAYS TAKEN      */
         L     @04,EXPARM1                                      01S0017
         LTR   @04,@04                                          01S0017
         BNZ   @RF00017                                         01S0017
*     DO;                                                       01S0018
*       CALL ASM01D;                /* CALL TO ASM01D NEVER MADE     */
         L     @15,@CV00064                                     01S0019
         BALR  @14,@15                                          01S0019
*     END;                                                      01S0020
*   END PROC1;                                                  01S0021
@RF00017 DS    0H                                               01S0021
@EL00002 DS    0H                                               01S0021
@EF00002 DS    0H                                               01S0021
@ER00002 LM    @14,@12,@SA00002                                 01S0021
         BR    @14                                              01S0021
@DATA    DS    0H
         DS    0F
@AL00002 DS    0A
         DC    A(EXPARM2)
         DS    0F
@SA00001 DS    18F
@SA00002 DS    0F
@PC00002 EQU   @SA00002+60,,C'A'
         DS    16F
         DS    0F
@CF00020 DC    F'2'
@CV00064 DC    V(ASM01D)
         LTORG
         DS    0D
EXPARM1  DC    F'1'
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
P1PARM1  EQU   0,4,C'F'
@PA00065 EQU   @PC00002,4,C'F'
@RF00009 EQU   @DE00011
         DS    0D
@ENDDATA EQU   *
@MODLEN  EQU   @ENDDATA-ASM01C
         END   ,