DYNQUERY  TITLE 'Test the MVS DYNALLOC feature'
DYNQUERY  CSECT
DYNQUERY  AMODE ANY
DYNQUERY  RMODE ANY
***************************************************************
* This program will dynamically query the specified DDNAME    *
* that is passed to the program and return the DSNAME for     *
* the specified DDNAME, including member name if applicable.  *
* RC 1080 means DDNAME not found.                             *
*                                                             *
* Syntax from COBOL:                                          *
*   01 DDNAME PIC X(8).                                       *
*   01 DSNAME PIC X(54).                                      *
*                                                             *
*   CALL DYNQUERY USING DDNAME, DSNAME.                       *
*                                                             *
* Author:                                                     *
*   IBM                                                       *
***************************************************************
*
*        Standard Prolog
*
START    STM   R14,R12,12(R13)     Save registers
         LR    R12,R15             Get base address
         USING DYNQUERY,R12        Establish addressability
         ST    R13,SAVE+4          Forward link of save areas
         LA    R10,SAVE            Address of our save area
         ST    R10,8(,R13)         Backward link of save areas
         LR    R13,R10             Our save area is current one
*
         L     R2,0(,R1)           Get addr of DDNAME parm
         L     R3,4(,R1)           Get addr of full DSNAME parm
         MVC   DDNAME,0(R2)        Copy DDNAME
         MVC DSNAMEM(DSNAMEL),BLANKS Clear to blanks
         MVC DSNAME(L'DSNAME),BLANKS Clear to blanks
         MVC MEMNAME(L'MEMNAME),BLANKS Clear to blanks
*
         L     R1,RBPTR
         LA    R1,0(,R1)
         USING S99RB,R1
         XC    S99RB(RBLEN),S99RB
         MVI   S99RBLN,RBLEN
         MVI   S99VERB,S99VRBIN
         OI    S99FLG11,S99NOCNV
         LA    R6,TUPL
         ST    R6,S99TXTPP
*******************************
* Set up the DDNAME parameter *
*******************************
         LA    R5,TUDDN1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINDDNAM)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DDNAME)
         MVC   S99TUPAR(L'DDNAME),DDNAME
         DROP  R5
*******************************
* Set up the DSNAME parameter *
*******************************
         LA    R5,TUDSN1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRTDSN)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSNAME)
         MVC   S99TUPAR(L'DSNAME),DSNAME
         DROP  R5
*********************************
* Set up the MEMBER parameter   *
*********************************
         LA    R5,TUMEM1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRTMEM)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'MEMNAME)
         MVC   S99TUPAR(L'MEMNAME),MEMNAME
         DROP  R5
*********************************
* Set up the DSORG parameter    *
*********************************
         LA    R5,TUORG1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRTORG)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSORG)
         MVC   S99TUPAR(L'DSORG),DSORG
         DROP  R5
*********************************
* Set up the DSTYPE parameter   *
*********************************
         LA    R5,TUTYP1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRTTYP)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSTYP)
         MVC   S99TUPAR(L'DSTYP),DSTYP
         DROP  R5
*********************************
* Set up the STATUS parameter   *
*********************************
         LA    R5,TUSTAT1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRTSTA)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSSTAT)
         MVC   S99TUPAR(L'DSSTAT),DSSTAT
         DROP  R5
*********************************
* Set up the DSTYPE parameter   *
*********************************
         LA    R5,TUDSNT1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRDSNT)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSDSNT)
         MVC   S99TUPAR(L'DSDSNT),DSDSNT
         DROP  R5
*********************************
* Set up the REC ORG parameter  *
*********************************
         LA    R5,TURECO1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRRECO)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSRECO)
         MVC   S99TUPAR(L'DSRECO),DSRECO
         DROP  R5
*********************************
* Set up the PATH parameter     *
*********************************
         LA    R5,TUPATH1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRPATH)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSPATH)
         MVC   S99TUPAR(L'DSPATH),DSPATH
         DROP  R5
*********************************
* Set up the PATHOPTS parameter *
*********************************
         LA    R5,TUPOPT1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRPOPT)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSPOPT)
         MVC   S99TUPAR(L'DSPOPT),DSPOPT
         DROP  R5
*********************************
* Set up the PATHMODE parameter *
*********************************
         LA    R5,TUPMODE1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRPMDE)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSPMODE)
         MVC   S99TUPAR(L'DSPMODE),DSPMODE
         DROP  R5
*********************************
* Set up the FILEDATA parameter *
*********************************
         LA    R5,TUFDAT1
         USING S99TUNIT,R5
         MVC   S99TUKEY,=AL2(DINRFDAT)
         MVC   S99TUNUM,=X'0001'
         MVC   S99TULNG,=AL2(L'DSFDAT)
         MVC   S99TUPAR(L'DSFDAT),DSFDAT
         DROP  R5
*******************************
* Set up the extension block  *
*******************************
         LA    R5,RBX
         ST    R5,S99S99X
         USING S99RBX,R5
         XC    S99RBX(RBXLEN),S99RBX  Clear the RB extension            02140000
         MVC   S99EID,=CL6'S99RBX'
         MVI   S99EVER,S99RBXVR
*        MVI   S99EOPTS,S99EIMSG+S99EWTP
         MVI   S99EMGSV,S99XINFO
         DROP  R5
*
         DROP  R1
         LA    R1,RBPTR
         DYNALLOC
         LTR   R15,R15
         BZ    OK
*
*         DYNQUERY failed
*
         LA    R1,RB
         USING S99RB,R1
         SR    R15,R15           Clear R15
         ICM   R15,3,S99ERROR    Get the error from DYNALLOC
         DROP  R1
         B     EXIT
*
OK       DS    0H                DYNQUERY successful
*******************************
* Get the DSNAME value        *
*******************************
         LA    R5,TUDSN1
         USING S99TUNIT,R5
         MVC   DSNAME,S99TUPAR
         DROP  R5
*******************************
* Get the MEMBER value        *
*******************************
         LA    R5,TUMEM1
         USING S99TUNIT,R5
         MVC   MEMNAME,S99TUPAR
         DROP  R5
*******************************
* Get the DSORG value         *
*******************************
         LA    R5,TUORG1
         USING S99TUNIT,R5
         MVC   DSORG,S99TUPAR
         DROP  R5
*******************************
* Get the DSTYP value         *
*******************************
         LA    R5,TUTYP1
         USING S99TUNIT,R5
         MVC   DSTYP,S99TUPAR
         DROP  R5
*******************************
* Get the STATUS value        *
*******************************
         LA    R5,TUSTAT1
         USING S99TUNIT,R5
         MVC   DSSTAT,S99TUPAR
         DROP  R5
*******************************
* Get the DSNTYPE value       *
*******************************
         LA    R5,TUDSNT1
         USING S99TUNIT,R5
         MVC   DSDSNT,S99TUPAR
         DROP  R5
*******************************
* Get the REC ORG value       *
*******************************
         LA    R5,TURECO1
         USING S99TUNIT,R5
         MVC   DSRECO,S99TUPAR
         DROP  R5
*******************************
* Get the PATH value          *
*******************************
         LA    R5,TUPATH1
         USING S99TUNIT,R5
         MVC   DSPATH,S99TUPAR
         DROP  R5
*******************************
* Get the PATHOPTS value      *
*******************************
         LA    R5,TUPOPT1
         USING S99TUNIT,R5
         MVC   DSPOPT,S99TUPAR
         DROP  R5
*******************************
* Get the PATHMODE value      *
*******************************
         LA    R5,TUPMODE1
         USING S99TUNIT,R5
         MVC   DSPMODE,S99TUPAR
         DROP  R5
*******************************
* Get the FILEDATA value      *
*******************************
         LA    R5,TUFDAT1
         USING S99TUNIT,R5
         MVC   DSFDAT,S99TUPAR
         DROP  R5
*******************************
* Check the values            *
*******************************
         SR    R15,R15
         CLI   DSNAME,C' '
         BE    CHKMEM
*
*        DSNAME is found
*
         MVC   DSNAMEM,DSNAME
CHKMEM   CLI   MEMNAME,C' '
         BE    DONE
*
*        MEMBER is found
*
         MVI   PARENO,C'('
         MVC   MBRNAME,MEMNAME
         MVI   PARENC,C')'
*
DONE     DS    0H
         MVC   0(DSNAMEL,R3),DSNAMEM
*
*        Standard Epilog
*
EXIT     L     R13,4(,R13)         Restore previous save area
         L     R14,12(,R13)        Restore reg 14
         LM    R0,R12,20(R13)      Restore remaining regs
         BR    R14                 Return to invoker
*
SAVE     DS    18F                 Save area
*
RBPTR    DC    A(X'80000000'+RB)
         DS    0F
RB       DC    XL(RBLEN)'00'
RBLEN    EQU   S99RBEND-S99RB
         DS    0F
RBX      DC    XL(RBXLEN)'00'
RBXLEN   EQU   S99ERSN-S99RBX+L'S99ERSN
TUPL     DC    A(TUDDN1)
         DC    A(TUDSN1)
         DC    A(TUMEM1)
         DC    A(TUORG1)
         DC    A(TUTYP1)
         DC    A(TUSTAT1)
         DC    A(TUDSNT1)
         DC    A(TURECO1)
         DC    A(TUPATH1)
         DC    A(TUPOPT1)
         DC    A(TUPMODE1)
         DC    A(X'80000000'+TUFDAT1)
*
TUDDN1   DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+8)'00'
TUDSN1   DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+44)'00'
TUMEM1   DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+8)'00'
TUORG1   DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+2)'00'
TUTYP1   DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+1)'00'
TUSTAT1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+2)'00'
TUDSNT1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+2)'00'
TURECO1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+2)'00'
TUPATH1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+128)'00'
TUPOPT1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+4)'00'
TUPMODE1 DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+4)'00'
TUFDAT1  DC    XL(L'S99TUKEY+L'S99TUNUM+L'S99TULNG+1)'00'
*
         DS    0F
         DC    CL4'DDN'
DDNAME   DC    CL8' '
         DS    0F
         DC    CL4'DSN'
DSNAME   DC    CL44' '
         DS    0F
         DC    CL4'MEM'
MEMNAME  DC    CL8' '
         DS    0F
         DC    CL4'ORG'
DSORG    DC    XL2'0000'
         DS    0F
         DC    CL4'TYP'
DSTYP    DC    X'00'
         DS    0F
         DC    CL4'STA'
DSSTAT   DC    X'00'
         DS    0F
         DC    CL4'DTY'
DSDSNT   DC    X'00'
         DS    0F
         DC    CL4'ROR'
DSRECO   DC    X'00'
         DS    0F
         DC    CL4'PTH'
DSPATH   DC    CL128' '
         DS    0F
         DC    CL4'POP'
DSPOPT   DC    XL4'00'
         DS    0F
         DC    CL4'PMD'
DSPMODE  DC    XL4'00'
         DS    0F
         DC    CL4'FDT'
DSFDAT   DC    X'00'
*
DSNAMEM  DC    CL44' '
PARENO   DC    C' '
MBRNAME  DC    CL8' '
PARENC   DC    C' '
DSNAMEL  EQU   *-DSNAMEM
BLANKS   DC    CL(DSNAMEL)' '
*
R0       EQU   0                                                        MIN00280
R1       EQU   1                                                        MIN00280
R2       EQU   2                                                        MIN00280
R3       EQU   3                                                        MIN00280
R4       EQU   4                                                        MIN00280
R5       EQU   5                                                        MIN00280
R6       EQU   6                                                        MIN00280
R7       EQU   7                                                        MIN00280
R8       EQU   8                                                        MIN00280
R9       EQU   9                                                        MIN00280
R10      EQU   10                                                       MIN00280
R11      EQU   11                                                       MIN00280
R12      EQU   12                                                       MIN00280
R13      EQU   13                                                       MIN00280
R14      EQU   14                                                       MIN00280
R15      EQU   15                                                       MIN00280
*
         IEFZB4D0
         IEFZB4D2
*
         END