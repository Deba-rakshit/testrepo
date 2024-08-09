******************************************************************
*                                                                *
******************************************************************
ASM01D   CSECT ,                                                01S0001
@MAINENT DS    0H                                               01S0001
         USING *,@15                                            01S0001
         B     @PROLOG                                          01S0001
         DC    AL1(16)                                          01S0001
         DC    C'ASM01D    97.295'                              01S0001
         DROP  @15
@PROLOG  STM   @14,@12,12(@13)                                  01S0001
         LR    @12,@15                                          01S0001
@PSTART  EQU   ASM01D                                           01S0001
         USING @PSTART,@12                                      01S0001
         ST    @13,@SA00001+4                                   01S0001
         LA    @14,@SA00001                                     01S0001
         ST    @14,8(,@13)                                      01S0001
         LR    @13,@14                                          01S0001
*                                                               01S0003
*/*    ASM01D IS NEVER CALLED                                        */
*                                                               01S0003
*   IF (EXPARM2 = 2) THEN                                       01S0003
         CLC   EXPARM2(4),@CF00020                              01S0003
         BNE   @RF00003                                         01S0003
*     CALL PROC1(EXPARM2);                                      01S0004
         LA    @01,@AL00002                                     01S0004
         BAL   @14,PROC1                                        01S0004
@RF00003 DS    0H                                               01S0013
*   END ASM01D;                                                 01S0013
@EL00001 L     @13,4(,@13)                                      01S0013
@EF00001 DS    0H                                               01S0013
@ER00001 LM    @14,@12,12(@13)                                  01S0013
         BR    @14                                              01S0013
*PROC1:                                                         01S0005
*   PROCEDURE(P1PARM1);                                         01S0005
PROC1    STM   @14,@12,12(@13)                                  01S0005
         MVC   @PC00002(4),0(@01)                               01S0005
*   P1PARM1 = 10;                                               01S0007
         LA    @01,10                                           01S0007
         L     @02,@PA00059                                     01S0007
         ST    @01,P1PARM1(,@02)                                01S0007
*   IF (P1PARM1 = 0) THEN                                       01S0008
         LTR   @01,@01                                          01S0008
         BNZ   @RF00008                                         01S0008
*     DO;                                                       01S0009
*       EXPARM2 = 0;                                            01S0010
         SLR   @03,@03                                          01S0010
         ST    @03,EXPARM2                                      01S0010
*     END;                                                      01S0011
*   END PROC1;                                                  01S0012
@RF00008 DS    0H                                               01S0012
@EL00002 DS    0H                                               01S0012
@EF00002 DS    0H                                               01S0012
@ER00002 LM    @14,@12,12(@13)                                  01S0012
         BR    @14                                              01S0012
@DATA    DS    0H
         DS    0F
@AL00002 DS    0A
         DC    A(EXPARM2)
         DS    0F
@SA00001 DS    18F
@PC00002 DS    1F
         DS    0F
@CF00020 DC    F'2'
         LTORG
         DS    0D
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
@PA00059 EQU   @PC00002,4,C'F'
         DS    0D
@ENDDATA EQU   *
@MODLEN  EQU   @ENDDATA-ASM01D
         END   ,