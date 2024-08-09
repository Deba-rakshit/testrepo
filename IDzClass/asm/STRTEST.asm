**********************************************************************  08320000
**********************************************************************  08330000
********* TEST PROGRAM FOR THE 'STRING' MACRO ************************  08340000
**********************************************************************  08350000
**********************************************************************  08360000
         MACRO                                            JDATE MACRO   08370001
        @JDATE &DATE                                      JDATE MACRO   08380001
         LA    R1,=P'&DATE'                               JDATE MACRO   08390001
*** STRING ((R1),P),2X,((R1),P),INTO=XXX                  JDATE MACRO   08400001
         STRING ((R1),P),2X,((R1),P,YYMMDD),INTO=XXX,     JDATE MACRO  X08410001
               3X,((R1),P,YY/MM/DD),                      JDATE MACRO  X08420001
               3X,((R1),P,DD/MM/YY),                      JDATE MACRO  X08430001
               3X,((R1),P,MM/DD/YY),                      JDATE MACRO  X08440001
               3X,((R1),P,YYYYMMDD),                      JDATE MACRO  X08450001
               3X,((R1),P,YYYY-MM-DD)                     JDATE MACRO   08460001
         PUT   SYSPRINT,XXX                               JDATE MACRO   08470001
         MEND                                             JDATE MACRO   08480001
**********************************************************************  08490001
TESTPGM  START X'015000'                                       @ZV!CHNG 08530001
         BALR  R12,0                                           @ZV!CHNG 08540000
         USING *,R12                                           @ZV!CHNG 08550000
*LOAD EP=SYSDEBUG                                                       08560001
*LR R15,R0                                                              08570001
*BASSM R14,R15                                                          08580001
*STRING 1X,INTO=XXX                                                     08590000
*RC8     STRING ((R1),,R**B),((R1),,R22Z),((R1),,R16B),INTO=XXX         08600000
         OPEN  (SYSPRINT,OUTPUT)                               @ZV!CHNG 08610000
         STRING 'Assembler is &SYSVER, DATE is &SYSDATC',INTO=XXX       08620000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 08630000
         STRING 'AMPERSAND=&& AND APOSTROPHE='' ',INTO=XXX     @ZV!CHNG 08640000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 08650000
RBPREFIX EQU   *                                               @ZV!CHNG 08660000
RBINTCOD EQU   *+6,2,C'H'                                      @ZV!CHNG 08670000
         L     R1,PSATOLD-PSA(0,0)                             @ZV!CHNG 08680000
         L     R1,0(,R1)               TCBRBP                  @ZV!CHNG 08690000
         L     R2,PSAAOLD-PSA(0,0)     ASCB                    @ZV!CHNG 08700000
         STRING 'SVC',(RBINTCOD-RBPREFIX(R1),H,R3Z),                   X08710000
               1X,(WWWW,,T),' - ',     VV.MM OF SVC RTNE               X08720000
               ((R8),,X),1X,           COM-REG ADDR                    X08730000
               'ASID=',(ASCBASID-ASCB(R2),,X),1X,                      X08740001
               PARM1,1X,               MAIN PGM NAME                   X08750000
               INTO=XXX                                        @ZV!CHNG 08760000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 08770000
*                                                                       08780000
         LA    R2,XXX                                          @ZV!CHNG 08790000
         STRING 1X,INTO=((R2),8)                               @ZV!CHNG 08800000
        @JDATE 90058                                           @ZV!CHNG 08810000
        @JDATE 91059                                           @ZV!CHNG 08820000
        @JDATE 93060                                           @ZV!CHNG 08830000
        @JDATE 94365                                           @ZV!CHNG 08840000
        @JDATE 80058                                           @ZV!CHNG 08850000
        @JDATE 84059                                           @ZV!CHNG 08860000
        @JDATE 88060                                           @ZV!CHNG 08870000
        @JDATE 92061                                           @ZV!CHNG 08880000
        @JDATE 00366                                           @ZV!CHNG 08890000
         LA    R2,1234                                         @ZV!CHNG 08900000
         STRING 'CVTPTR=X''',(CVTPTR,4,X),'''',INTO=XXX,               X08910000
               ' 1234=',((R2),,R4Z)                            @ZV!CHNG 08920000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 08930000
         L     R1,CVTPTR(0,0)                                  @ZV!CHNG 08940000
         STRING 'CVTDATE=',(56(R1),P,YYMMDD),INTO=XXX          @ZV!CHNG 08950000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 08960000
         LA    R0,1000                                         @ZV!CHNG 08970000
         LA    R3,0033                                         @ZV!CHNG 08980000
         STRING 'D1=/',D1,'/,WWWW=/',WWWW,'/',                         X08990000
               ((R3),,L),'/',((R3),,X),'/',((R0),,L),'/',              X09000000
               ((R3),,R9B),'/',INTO=XXX                        @ZV!CHNG 09010000
         LR    R4,R15                   LENGTH USED            @ZV!CHNG 09020000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09030000
         STRING WWWW,                                                  X09040000
               (4(R13),4,X),'''',(4(R13),F),'''',                      X09050000
               (4(R13),F,L),'''',                                      X09060000
               (4(R13),F,L11),'''',                                    X09070000
               (4(R13),F,Z9),'''',                                     X09080000
               8X,'R4=',((R4),,L),      LENGTH USED                    X09090000
               INTO=XXX                                        @ZV!CHNG 09100000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09110000
         STRING %TIME,D1,'B12345678B',5X,(CTR1,P),1X,PARM1,1X,PARM2,   X09120000
               INTO=XXX                                        @ZV!CHNG 09130000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09140000
         LA    R3,17                                           @ZV!CHNG 09150001
         STRING INTO=XXX,'CCC1234A',(D1,(R3)),'.',(CTR1,P,R7Z) @ZV!CHNG 09160000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09170000
         STRING C'DDN2(',(D1,,T),')',X'40C1C2,C3C4',                   +09180000
               ' PSATOLD=',(PSATOLD-PSA,,X),                           +09190000
               INTO=XXX                                        @ZV!CHNG 09200000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09210000
         BALR  R0,0                                            @ZV!CHNG 09220000
         STRING 'R0=',((R0),,X),'   16(R0)=',(16(R0),4,X),INTO=XXX      09230000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09240000
         STRING 'R0=',((R0),,X),'   CTR1=',(CTR1,P,R5B),INTO=XXX        09250000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09260000
         SLR   R0,R0                                           @ZV!CHNG 09270000
         STRING 'R0=',((R0),,X),'   CTR1=',(CTR1,P,R5B),INTO=XXX        09280000
         PUT   SYSPRINT,XXX                                    @ZV!CHNG 09290000
*                                                                       09300000
EXIT     SLR   R15,R15                                         @ZV!CHNG 09310000
         SVC   3                       GOBACK                  @ZV!CHNG 09320000
D1       DC    C'D1-----D1    '                                @ZV!CHNG 09330000
WWWW     DC    C'WWWW'                                         @ZV!CHNG 09340000
CTR1     DC    P'1'                                            @ZV!CHNG 09350000
PARM1    DC    C'<-PARM1->'                                    @ZV!CHNG 09360000
PARM2    DC    C'<-PARM2->'                                    @ZV!CHNG 09370000
XXX      DS    CL132                                           @ZV!CHNG 09380000
CVTPTR   EQU   0016,4,C'A'                                     @ZV!CHNG 09390000
SYSPRINT DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=PM,RECFM=FB,LRECL=121     09400000
         STRING GENERATE                                       @ZV!CHNG 09410000
R0       EQU   0                                               @ZV!CHNG 09420001
R1       EQU   1                                               @ZV!CHNG 09430001
R2       EQU   2                                               @ZV!CHNG 09440001
R3       EQU   3                                               @ZV!CHNG 09450001
R4       EQU   4                                               @ZV!CHNG 09460001
R5       EQU   5                                               @ZV!CHNG 09470001
R6       EQU   6                                               @ZV!CHNG 09480001
R7       EQU   7                                               @ZV!CHNG 09490001
R8       EQU   8                                               @ZV!CHNG 09500001
R9       EQU   9                                               @ZV!CHNG 09510001
R10      EQU   10                                              @ZV!CHNG 09520001
R11      EQU   11                                              @ZV!CHNG 09530001
R12      EQU   12                                              @ZV!CHNG 09540001
R13      EQU   13                                              @ZV!CHNG 09550001
R14      EQU   14                                              @ZV!CHNG 09560001
R15      EQU   15                                              @ZV!CHNG 09570001
PSA      DSECT                                                 @ZV!CHNG 09580000
PSATOLD  EQU   *+X'21C',4,C'A'                                 @ZV!CHNG 09590000
PSAAOLD  EQU   *+X'224',4,C'A'                                 @ZV!CHNG 09600000
ASCB     DSECT                                                 @ZV!CHNG 09610000
ASCBASID EQU   *+36,2,C'X'                                     @ZV!CHNG 09620000
         END