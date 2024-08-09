hlasmex CSECT
***********************************************************************
*   HLASM Sample Program
***********************************************************************
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
***********************************************************************
         SAVE  (14,12)
         BALR  12,0
         USING *,12
**********************************************************************
         ST    R1,SAVER1
         L     R8,0(,R1)
**********************************************************************
         CLC   0(8,R8),GET
         BE    IORTN
         CLC   0(8,R8),OPEN
         BE    OPEN
**********************************************************************
         CLC   0(8,R8),CLOSE
         BE    CLOSFILE
**********************************************************************
         LA    R15,15
         B     RETURN
***********************************************************************
IORTN    EQU   *
         OPEN  (TRXNIN,(INPUT))
         LTR   R15,R15
         BNZ   BADOPEN
         TM    TRXNIN+48,X'10'
         BZ    BADOPEN
         ST    R15,8(,R8)
         LA    R9,TRXNIN
         MVC   12(2,R8),82(R9)
         B     RETURN
*
***********************************************************************
GETFILE  EQU   *
         L     R1,SAVER1
         L     R3,0(R1)
         LA    R3,14(,R3)
         GET   TRXNIN,(R3)
         LTR   R15,R15
         BNZ   BADGET
         ST    R15,8(,R8)
         LA    R9,TRXNIN
         MVC   12(2,R8),82(R9)
         B     RETURN
*
***********************************************************************
CLOSFILE EQU   *
         CLOSE (TRXNIN)
         ST    R15,8(,R8)
*
***********************************************************************
RETURN   EQU   *
         SR    15,15
         RETURN (14,12),RC=(15)
*
ERROR1   EQU   *
         WTO   '* HLASMEX, TRXNIN - GENERAL I/O ERROR'
         RETURN (14,12),,RC=1
*
BADOPEN  EQU   *
         ST    R15,8(,R8)
         WTO   '* HLASMEX - FILE OPEN FAIL'
         L     R15,8(,R8)
         RETURN (14,12),RC=(15)
*
BADGET   EQU   *
         ST    R15,8(,R8)
         WTO   '* HLASMEX - FILE GET FAIL'
         L     R15,8(,R8)
         RETURN (14,12),RC=(15)
*
EODRTN   EQU   *
         WTO   '* HLASMEX EOF'
         LA    R15,16
         ST    R15,8(,R8)
         RETURN (14,12),RC=(15)
*
***********************************************************************
QSAMFILE DCB   MACRF=G,EODAD=EODRTN,SYNAD=ERROR1,                      X
               DDNAME=TRXNIN
*
***********************************************************************
SAVER1   DC    F'0'
GET      DC    CL8'GET     '
OPEN     DC    CL8'OPEN    '
CLOSE    DC    CL8'CLOSE   '
*
         END