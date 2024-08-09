DSKTOPRT START 0
BEGIN    SAVE (14,12)    SAVE REGISTERS
         BALR 3,0         LOAD BASE REGISTER
         USING *,3
         USING SCOURSE,4
         ST  13,SAVE+4
         LA  13,SAVE
         OPEN (STUDENT,INPUT,CRSELST,OUTPUT)
READSTUD GET  STUDENT,STUDREC
         MVC  LSID,SID
         MVC  LSNAME,SNAME
         MVC  LSHOURS,HRSEDIT
         ED   LSHOURS,SHOURS
         LA   4,SSEGAREA
         LH   5,SLGTH
         S    5,=F'37'
NXTCRSE  MVC  LCNBR,SCNBR
         MVC  LCNAME,SCNAME
         MVC  LCHRS,SCHRS
         ED   LCHRS,SCHRS
         PUT  CRSELST,LLINE
         MVI  LLINE,X'40'
         MVC  LLINE+1(131),LLINE
         S    5,=F'31'
         C    5,=F'0'
         BNH  READSTUD
         LA   4,31(4)
         B    NXTCRSE
EOFDISK  CLOSE (STUDENT,,CRSELST)
         L 13,SAVE+4
         RETURN (14,12)
* ==============================================================
*  Constants and Variables
* ==============================================================
STUDENT  DCB DSORG=PS,                                                 X
               RECFM=VB,                                               X
               MACRF=GM,                                               X
               BLKSIZE=704,                                            X
               LRECL=374,                                              X
               DDNAME=STUDENT,                                         X
               EODAD=EOFDISK
*
CRSELST  DCB DSORG=PS,                                                 X
               RECFM=F,                                                X
               MACRF=PM,                                               X
               BLKSIZE=132,                                            X
               LRECL=132,                                              X
               DDNAME=CRSELST
*
SAVE     DS   18F
STUDREC  DS   0CL347
SLGTH    DS   H
         DS   CL2
SID      DS   CL6
SNAME    DS   CL25
SHOURS   DS   PL2
SSEGAREA DS   CL310
SCOURSE  DSECT
SCNBR    DS   CL4
SCNAME   DS   CL25
SCHRS    DS   PL2
*
DSKTOPRT CSECT
LLINE    DS  0CL132
         DC  C' '
LSID     DS  CL6
         DC  3C' '
LSNAME   DS  CL25
         DC  C' '
LSHOURS  DS  CL5
         DC  3C' '
LCNBR    DS  CL4
         DC  3C' '
LCNAME   DS  CL25
LCHRS    DS  CL5
         DC  51C' '
HRSEDIT  DC  X'4020214B21'
         END BEGIN