000100*--------------------------------------------------------------*
000101*                                                              *
000102*               @BANNER_START@                                 *
000130*      nacwtrec.cpy                                            *
000131*      (C) Copyright IBM Corp. 2000. All Rights Reserved.      *
000132*                                                              *
000133* Element of Designing and Programming CICS Applications book  *
00113 *               @BANNER_END@                                   *
000114*                                                              *
000115*--------------------------------------------------------------*
000116*
000200* The description of the Account record is placed in this
000300* copy book.
000310* Its fields are described beginning at the '20' level in order
000400* to allow it to be used within other structures.
000500*
000520* This is the working storage version of the Account record layout.
000530*
000600     20  ACCTDO               PIC X(5)  VALUE SPACES.
000700     20  SNAMEDO              PIC X(18) VALUE SPACES.
000800     20  FNAMEDO              PIC X(12) VALUE SPACES.
000900     20  MIDO                 PIC X     VALUE SPACES.
001000     20  TTLDO                PIC X(4)  VALUE SPACES.
001100     20  TELDO                PIC X(10) VALUE SPACES.
001200     20  ADDR1DO              PIC X(24) VALUE SPACES.
001300     20  ADDR2DO              PIC X(24) VALUE SPACES.
001400     20  ADDR3DO              PIC X(24) VALUE SPACES.
001500     20  AUTH1DO              PIC X(32) VALUE SPACES.
001600     20  AUTH2DO              PIC X(32) VALUE SPACES.
001700     20  AUTH3DO              PIC X(32) VALUE SPACES.
001800     20  AUTH4DO              PIC X(32) VALUE SPACES.
001900     20  CARDSDO              PIC X     VALUE SPACES.
002000     20  IMODO                PIC X(2)  VALUE SPACES.
002100     20  IDAYDO               PIC X(2)  VALUE SPACES.
002200     20  IYRDO                PIC X(2)  VALUE SPACES.
002300     20  RSNDO                PIC X     VALUE SPACES.
002400     20  CCODEDO              PIC X     VALUE SPACES.
002500     20  APPRDO               PIC X(3)  VALUE SPACES.
002600     20  SCODE1DO             PIC X     VALUE SPACES.
002700     20  SCODE2DO             PIC X     VALUE SPACES.
002800     20  SCODE3DO             PIC X     VALUE SPACES.
002900     20  STATDO               PIC X(2)  VALUE SPACES.
003000     20  LIMITDO              PIC X(8)  VALUE SPACES.
003100     20  PAY-HIST OCCURS 3.
003200         25  BAL              PIC X(8)  VALUE SPACES.
003300         25  BMO              PIC 9(2)  VALUE ZERO.
003400         25  BDAY             PIC 9(2)  VALUE ZERO.
003500         25  BYR              PIC 9(2)  VALUE ZERO.
003600         25  BAMT             PIC X(8)  VALUE SPACES.
003700         25  PMO              PIC 9(2)  VALUE ZERO.
003800         25  PDAY             PIC 9(2)  VALUE ZERO.
003900         25  PYR              PIC 9(2)  VALUE ZERO.
004000         25  PAMT             PIC X(8)  VALUE SPACES.
