*********                                 *********
*********  Remote Syntax Check Workshop 1 *********
*********                                 *********
*
DEBUG1   START 0
BEGIN    SAVE (14,12)    SAVE REGISTERS
         BALR 3,0         LOAD BASE REGISTER
         USING *,3
         ST  13,SAVE+4
         LA  13,SAVE
         OPEN (INFILE,INPUT,OUTFILE,OUTPUT)
READ     MVC  OUTAREA,SPACES
         GET  CARDFILE,RECORD
         MVC  LNAME,LAST
         MVC  FNAME,FIRST
         MVC  SALOUT,SALARY
         PUT  OUTFILE,OUTAREA
         B    READRTN
EOF      CLOSE (INFILE,OUTFILE)
*
RECORD   DS   0CL80
SPACES   DC   CL1' '
OUTAREA  DS   0CL132
         DS   CL5
LNAME    DS   CL15
         DS   CL20
FNAME    DS   CL10
         DS   CL20
SALOUT   DS   CL5
BLANK    DS   CL50
SAVEAREA DS   18F
* ==============================================================
*  Constants and Variables
* ==============================================================
INFILE   DCB DSORG=PS,                                                 X
               RECFM=FB,                                               X
               MACRF=GM,                                               X
               BLKSIZE=80,                                             X
               LRECL=80,                                               X
               DDNAME=INFILE,                                          X
               EODAD=EOF
*
PRTFILE  DCB DSORG=PS,                                                 X
               RECFM=FBA,                                              X
               MACRF=PM,                                               X
               BLKSIZE=132,                                            X
               LRECL=132,                                              X
               DDNAME=OUTFILE
*