IASBU10A CEEENTRY PPA=PARMPPA,AUTO=WORKSIZE,MAIN=NO                     IAS00010
         USING    WORKAREA,R13                                          IAS00020
*                                                                       IAS00030
         PACK   DWORK,NUMB1                                             IAS00040
         CVB    R2,DWORK                                                IAS00050
         ST     R2,FWORK                                                IAS00060
         PACK   DWORK,NUMB2                                             IAS00070
         CVB    R4,DWORK                                                IAS00080
*                                                                       IAS00090
LOOP1    A      R2,FWORK                                                IAS00100
         ST     R2,SUMWORK                                              IAS00110
         CVD    R2,DWORK                                                IAS00120
         MVC    OUTSUM,SUMMSK                                           IAS00130
         ED     OUTSUM,DWORK+L'DWORK-SUMLEN/2                           IAS00140
         MVC    SUMMSG+1(8),OUTSUM                                      IAS00150
         LA     R1,SUMMSG                                               IAS00160
         CALL   IAPUT1                                                  IAS00170
         BCT    R4,LOOP1                                                IAS00180
*                                                                       IAS00190
         CVD    R2,DWORK                                                IAS00200
         MVC    OUTSUM,SUMMSK                                           IAS00210
         ED     OUTSUM,DWORK+L'DWORK-SUMLEN/2                           IAS00220
         MVC    SUMFIN+1(8),OUTSUM                                      IAS00230
         LA     R1,SUMFIN                                               IAS00240
         CALL   IAPUT1                                                  IAS00250
*                                                                       IAS00260
*   Terminate the CEL environment and return to the caller              IAS00270
*                                                                       IAS00280
         CEETERM  RC=0                                                  IAS00290
* ==============================================================        IAS00300
*                                                                       IAS00310
SUMLEN   EQU    8                                                       IAS00320
SUMMSG   DC     C'(xxxxxxxx) -- The sum         '                       IAS00330
SUMFIN   DC     C'(xxxxxxxx) -- The final sum   '                       IAS00340
SUMMSK   DC     X'4020202020202120'                                     IAS00350
NUMB1    DC     ZL5'100'                                                IAS00360
NUMB2    DC     ZL5'10'                                                 IAS00370
*                                                                       IAS00380
DWORK    DS     D                                                       IAS00390
FWORK    DS     F                                                       IAS00400
SUMWORK  DS     F                                                       IAS00410
OUTSUM   DS     CL(L'SUMMSK)                                            IAS00420
*                                                                       IAS00430
PARMPPA  CEEPPA                   Constants descibing the code block    IAS00440
* ===================================================================   IAS00450
*        The Workarea and DSA                                           IAS00460
* ===================================================================   IAS00470
WORKAREA DSECT                                                          IAS00480
         ORG    *+CEEDSASZ        Leave space for the DSA fixed part    IAS00490
*                                                                       IAS00500
PLIST    DS     0D                                                      IAS00510
PARM1    DS     A                                                       IAS00520
PARM2    DS     A                                                       IAS00530
PARM3    DS     A                                                       IAS00540
PARM4    DS     A                                                       IAS00550
PARM5    DS     A                                                       IAS00560
*                                                                       IAS00570
         DS     0D                                                      IAS00580
WORKSIZE EQU    *-WORKAREA                                              IAS00590
         CEEDSA                   Mapping of the Dynamic Save Area      IAS00600
         CEECAA                   Mapping of the Common Anchor Area     IAS00610
*                                                                       IAS00620
R0       EQU    0                                                       IAS00630
R1       EQU    1                                                       IAS00640
R2       EQU    2                                                       IAS00650
R3       EQU    3                                                       IAS00660
R4       EQU    4                                                       IAS00670
R5       EQU    5                                                       IAS00680
R6       EQU    6                                                       IAS00690
R7       EQU    7                                                       IAS00700
R8       EQU    8                                                       IAS00710
R9       EQU    9                                                       IAS00720
R10      EQU    10                                                      IAS00730
R11      EQU    11                                                      IAS00740
R12      EQU    12                                                      IAS00750
R13      EQU    13                                                      IAS00760
R14      EQU    14                                                      IAS00770
R15      EQU    15                                                      IAS00780
*                                                                       IAS00790
         END    IASBU10A                                                IAS00800
