ORDUPD   START 0
BEGIN    SAVE (14,12)    SAVE REGISTERS
         BALR 3,0         LOAD BASE REGISTER
         USING *,3
         USING ORDMASK,4
         ST  13,SAVE+4
         LA  13,SAVE
         OPEN (SHIPTR,INPUT,ORDERS,UPDAT,RPTTR,OUTPUT,ERRLIST,OUTPUT)
READSHIP GET  SHIPTR,SHIPWRK
READORD  GET  ORDERS
         LR   4,1
TEST     CLC  ORDCTL,SHCTL
         BL   READORD
         BE   MATCH
         MVC  ERRTR,SHIPWRK
         PUT  ERRLIST,ERRLINE
         GET  SHIPTR,SHIPWRK
         B    TEST
MATCH    MVC  ORPRODSH,SHPROD
         MVC  ORQTYSH,SHQTY
         MVC  ORDATESH,SHDATE
         PUTX ORDERS
         PUT  RPTTR
         MVC  0(80,1),0(4)
         B    READSHIP
EOFSHIP  CLOSE (SHIPTR,,ORDERS,,RPTTR,,ERRLIST)
         L 13,SAVE+4
         RETURN (14,12)
EOFORD  MVC  ERRTR,SHIPWRK
        PUT  ERRLIST,ERRLINE
        GET  SHIPTR,SHIPWRK
        B    EOFORD
* ==============================================================
*  Constants and Variables
* ==============================================================
SHIPTR   DCB DSORG=PS,                                                 X
               RECFM=FB,                                               X
               MACRF=GM,                                               X
               BLKSIZE=240,                                            X
               LRECL=24,                                               X
               DDNAME=SHIPTR,                                          X
               EODAD=EOFSHIP
*
ORDERS   DCB DSORG=PS,                                                 X
               RECFM=FB,                                               X
               MACRF=(GL,PM),                                          X
               BLKSIZE=400,                                            X
               LRECL=80,                                               X
               DDNAME=ORDERS,                                          X
               EODAD=EOFORD
*
RPTTR    DCB DSORG=PS,                                                 X
               RECFM=FB,                                               X
               MACRF=PL,                                               X
               BLKSIZE=400,                                            X
               LRECL=80,                                               X
               DDNAME=RPTTR
*
ERRLIST  DCB DSORG=PS,                                                 X
               RECFM=F,                                                X
               MACRF=PM,                                               X
               BLKSIZE=132,                                            X
               LRECL=132,                                              X
               DDNAME=RPTTR
*
SAVE     DS   18F
SHIPWRK  DS   0CL24
SHTRCODE DS   CL2
SHCTL    DS   0CL10
SHORDNBR DS   CL4
SHPROD   DS   CL6
SHQTY    DS   CL4
SHDATE   DS   CL8
ORDMASK  DSECT
         DS  CL24
ORDCTL   DS  0CL10
ORDNBR   DS  CL4
ORPRODOR DS  CL6
ORQTYOR  DS  CL4
ORDATEOR DS  CL8
ORPRODSH DS  CL6
ORQTYSH  DS  CL4
ORDATESH DS  CL8
         DS  CL16
ORDUPD   CSECT
ERRLINE  DS   0CL132
         DC   20C' '
ERRTR    DS   CL24
         DC   88C'          UNMATCHED SHIPMENT TRANSACTION'
*
         END BEGIN