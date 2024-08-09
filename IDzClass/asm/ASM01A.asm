*
******************************************************************
*                                                                *
******************************************************************
ASM01A   CSECT ,                                                01S0001
@MAINENT DS    0H                                               01S0001
         USING *,@15                                            01S0001
         B     @PROLOG                                          01S0001
         DC    AL1(16)                                          01S0001
         DC    C'ASM01A     97.295'                             01S0001
         DROP  @15
@PROLOG  STM   @14,@12,12(@13)                                  01S0001
         LR    @12,@15                                          01S0001
@PSTART  EQU   ASM01A                                           01S0001
         USING @PSTART,@12                                      01S0001
         ST    @13,@SA00001+4                                   01S0001
         LA    @14,@SA00001                                     01S0001
         ST    @14,8(,@13)                                      01S0001
         LR    @13,@14                                          01S0001
*   DO WHILE(EXPARM1>0);            /* THIS DO LOOP EXECUTED 5 TIMES */
         B     @DE00006                                         01S0006
@DL00006 DS    0H                                               01S0007
*     EXPARM1 = EXPARM1 - 1;        /*                               */
         L     @01,EXPARM1                                      01S0007
         BCTR  @01,0                                            01S0007
         ST    @01,EXPARM1                                      01S0007
*     CALL ASM01B(PARM2);          /* ASM01B CALLED 5 TIMES        */
         L     @15,@CV00063                                     01S0008
         LA    @01,@AL00002                                     01S0008
         BALR  @14,@15                                          01S0008
*   END;                                                        01S0009
@DE00006 L     @00,EXPARM1                                      01S0009
         LTR   @00,@00                                          01S0009
         BP    @DL00006                                         01S0009
*   IF (EXPARM2 = 0) THEN           /* THIS BRANCH ALWAYS TAKEN      */
         L     @01,EXPARM2                                      01S0010
         LTR   @01,@01                                          01S0010
         BNZ   @RF00010                                         01S0010
*     CALL PROC1(EXPARM2);          /* PROC1 NEVER CALLED            */
         LA    @01,@AL00003                                     01S0011
         BAL   @14,PROC1                                        01S0011
*   DO WHILE(EXPARM2>0);            /* DO LOOP EXECUTED TWICE        */
         B     @DE00012                                         01S0012
@DL00012 DS    0H                                               01S0013
*     EXPARM2 = EXPARM2 - 1;                                    01S0013
         L     @02,EXPARM2                                      01S0013
         BCTR  @02,0                                            01S0013
         ST    @02,EXPARM2                                      01S0013
*   END;                                                        01S0014
@DE00012 L     @03,EXPARM2                                      01S0014
         LTR   @03,@03                                          01S0014
         BP    @DL00012                                         01S0014
*   RETURN CODE(0);                                             01S0015
         SLR   @15,@15                                          01S0015
         L     @13,4(,@13)                                      01S0015
         L     @14,12(,@13)                                     01S0015
         LM    @00,@12,20(@13)                                  01S0015
         BR    @14                                              01S0015
*   END ASM01A;                                                 01S0020
*PROC1:                                                         01S0016
*   PROCEDURE(P1PARM1);             /* THIS PROCEDURE NEVER EXECUTED */
PROC1    STM   @14,@12,12(@13)                                  01S0016
         MVC   @PC00002(4),0(@01)                               01S0016
*   P1PARM1 = 10;                                               01S0018
         L     @02,@PA00064                                     01S0018
         LA    @03,10                                           01S0018
         ST    @03,P1PARM1(,@02)                                01S0018
*   END PROC1;                                                  01S0019
@EL00002 DS    0H                                               01S0019
@EF00002 DS    0H                                               01S0019
@ER00002 LM    @14,@12,12(@13)                                  01S0019
         BR    @14                                              01S0019
@DATA    DS    0H
         DS    0F
@AL00002 DS    0A
         DC    A(PARM2)
@AL00003 DS    0A
         DC    A(EXPARM2)
         DS    0F
@SA00001 DS    18F
@PC00002 DS    1F
         DS    0F
@CV00063 DC    V(ASM01B)
         LTORG
         DS    0D
EXPARM1  DC    F'5'
EXPARM2  DC    F'2'
PARM2    DC    F'2'
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
@PA00064 EQU   @PC00002,4,C'F'
@RF00010 EQU   @DE00012
         DS    0D
@ENDDATA EQU   *
@MODLEN  EQU   @ENDDATA-ASM01A
         END   ,