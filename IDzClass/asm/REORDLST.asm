REORDLST START 0
BEGIN    SAVE (14,12)    SAVE REGISTERS
         BALR 3,0         LOAD BASE REGISTER
         USING *,3
         ST  13,SAVE+4
         LA  13,SAVE
         OPEN (INVMAST,INPUT,PRTOUT,OUTPUT)
READINV  GET  INVMAST,INVWRKA
         AP   COUNT,=P'1'
         PACK WRKAVAIL,INVONHND
         PACK WRKONORD,INVONORD
         AP   WRKAVAIL,WRKONORD
         PACK WRKORDPT,INVORDPT
         CP   WRKAVAIL,WRKORDPT
         BNL  READINV
         PACK PACKAREA,INVITNBR
         MVC  PRTITNBR,PATTERN1
         ED   PRTITNBR,PACKAREA
         MVC  PRTITDES,INVITDES
         PACK PACKAREA,INVPRICE
         MVC  PRTPRICE,PATTERN2
         ED   PRTPRICE,PACKAREA
         MVC  PRTAVAIL,PATTERN1
         ED   PRTAVAIL,WRKAVAIL
         MVC  PRTORDPT,PATTERN1
         ED   PRTORDPT,WRKORDPT
         CP   LINECNT,=P'50'
         BL   PRTDET
         PUT  PRTOUT,HDGLINE1
         PUT  PRTOUT,HDGLINE2
         PUT  PRTOUT,HDGLINE3
         ZAP  LINECNT,=P'0'
         MVI  PRTDCTL,C'0'
*
PRTDET   PUT  PRTOUT,PRTDETL
         AP   LINECNT,=P'1'
         MVI  PRTDCTL,C' '
         B    READINV
INVEOF   ED   CNTPATRN,COUNT
         PUT  PRTOUT,CNTLINE
         CLOSE (INVMAST,,PRTOUT)
         L 13,SAVE+4
         RETURN (14,12)
* ==============================================================
*  Constants and Variables
* ==============================================================
INVMAST  DCB DSORG=PS,                                                 X
               RECFM=FB,                                               X
               MACRF=GM,                                               X
               BLKSIZE=500,                                            X
               LRECL=50,                                               X
               DDNAME=INVMAST,                                         X
               EODAD=INVEOF
*
PRTOUT   DCB DSORG=PS,                                                 X
               RECFM=FBA,                                              X
               MACRF=PM,                                               X
               BLKSIZE=1330,                                           X
               LRECL=133,                                              X
               DDNAME=REPORT
*
INVWRKA  DS   0CL50
INVITNBR DS   CL5
INVITDES DS   CL20
         DS   CL5
INVPRICE DS   CL5
INVORDPT DS   CL5
INVONHND DS   CL5
INVONORD DS   CL5
         DS   CL30
*
HDGLINE1 DS   0CL133
         DC   C'1'
         DC   24C' '
         DC   C'REORDER LISTING'
         DC   93C' '
*
HDGLINE2 DS   0CL133
         DC   C'0'
         DC   24C' '
         DC   C'ITEM       ITEM              UNIT                      X
                    REORDER'
         DC   69C' '
*
HDGLINE3 DS   0CL133
         DC   C' '
         DC   24C' '
         DC   C'NO.        DESCRIPTION       PRICE        AVAILABLE    X
                    POINT'
         DC   70C' '
*
PRTDETL  DS  0CL133
PRTDCTL  DS  CL1
PRTITNBR DS  CL6
         DC   5C' '
PRTITDES DS  CL20
         DC   4C' '
PRTPRICE DS  CL7
         DC   4C' '
PRTAVAIL DS  CL6
         DC   4C' '
PRTORDPT DS  CL6
         DC  70C' '
* DATA STUFF
CNTLINE  DS  0CL133
         DC  C'-'
CNTPATRN DC  X'4020206B202020'
         DC  C' RECORDS IN THE INPUT FILE'
         DC  99C' '
* DATA STUFF
SAVE     DS  18F
PATTERN1 DC  X'402020202020'
PATTERN2 DC  X'4020206B202020'
WRKAVAIL DS  PL3
WRKONORD DS  PL3
WRKORDPT DS  PL3
PACKAREA DS  PL3
COUNT    DC  PL3'0'
LINECNT  DC  P'50'
*
         END BEGIN