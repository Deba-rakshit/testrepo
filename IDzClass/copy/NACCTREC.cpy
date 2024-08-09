000100*--------------------------------------------------------------*
000110*                                                              *
000120*               @BANNER_START@                                 *
000130*      nacctrec.cpy                                            *
000131*      (C) Copyright IBM Corp. 2000. All Rights Reserved.      *
000132*                                                              *
000133* Element of Designing and Programming CICS Applications book  *
000194*               @BANNER_END@                                   *
000195*                                                              *
000196*--------------------------------------------------------------*
000201*
000210* The description of the Account record is placed in this copy book.
000300* Its fields are described beginning at the '20' level in order
000400* to allow it to be used within other structures.
000500*
000510* This is the linkage commarea version of the Account record layout.
000520*
000600     20  ACCTDO               PIC X(5).
000700     20  SNAMEDO              PIC X(18).
000800     20  FNAMEDO              PIC X(12).
000900     20  MIDO                 PIC X.
001000     20  TTLDO                PIC X(4).
001100     20  TELDO                PIC X(10).
001200     20  ADDR1DO              PIC X(24).
001300     20  ADDR2DO              PIC X(24).
001400     20  ADDR3DO              PIC X(24).
001500     20  AUTH1DO              PIC X(32).
001600     20  AUTH2DO              PIC X(32).
001700     20  AUTH3DO              PIC X(32).
001800     20  AUTH4DO              PIC X(32).
001900     20  CARDSDO              PIC X.
002000     20  IMODO                PIC X(2).
002100     20  IDAYDO               PIC X(2).
002200     20  IYRDO                PIC X(2).
002300     20  RSNDO                PIC X.
002400     20  CCODEDO              PIC X.
002500     20  APPRDO               PIC X(3).
002600     20  SCODE1DO             PIC X.
002700     20  SCODE2DO             PIC X.
002800     20  SCODE3DO             PIC X.
002900     20  STATDO               PIC X(2).
003000     20  LIMITDO              PIC X(8).
003100     20  PAY-HIST OCCURS 3.
003200         25  BAL              PIC X(8).
003300         25  BMO              PIC 9(2).
003400         25  BDAY             PIC 9(2).
003500         25  BYR              PIC 9(2).
003600         25  BAMT             PIC X(8).
003700         25  PMO              PIC 9(2).
003800         25  PDAY             PIC 9(2).
003900         25  PYR              PIC 9(2).
004000         25  PAMT             PIC X(8).
