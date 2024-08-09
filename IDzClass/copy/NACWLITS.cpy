000100*--------------------------------------------------------------*
000110*                                                              *
000120*               @BANNER_START@                                 *
000130*      nacwlits.cpy                                            *
000131*      (C) Copyright IBM Corp. 2000. All Rights Reserved.      *
000132*                                                              *
000133* Element of Designing and Programming CICS Applications book  *
000194*               @BANNER_END@                                   *
000195*                                                              *
000196*--------------------------------------------------------------*
000197*
000210* Various values which you might wish to modify are placed in one
000300* copy book in order to make those sorts of changes more easily.
000400* The transaction names are designed to be any three characters
000500* you wish followed by a particular value to indicate the
000600* function. As supplied the three characters used are 'NAC' so
000700* that the BMS front-end transaction is 'NACT', the print
000800* transaction is 'NACP', the IIOP interface transaction is 'NACO'
000900* and the MQ interface transaction is 'NACM'. In this manner you
001000* can change the names to suit your conventions.
001100*
001200     05  WS-LITS-TRANSACTIONS.
001300         10  WS-LITS-TRANS-BMS-IF        PIC X(1)
001400                                         VALUE 'T'.
001500         10  WS-LITS-TRANS-PRINT         PIC X(1)
001600                                         VALUE 'P'.
001700         10  WS-LITS-TRANS-IIOP-IF       PIC X(1)
001800                                         VALUE 'O'.
001900         10  WS-LITS-TRANS-MQ-IF         PIC X(1)
002000                                         VALUE 'M'.
002100*
002200* The program names are designed to be any four characters
002300* you wish followed by a particular value to indicate the
002400* function. As supplied the four characters used are 'NACT' so
002500* that the BMS interface program is 'NACT01', the
002600* Create/Read/Update/Delete logic program is 'NACT02', the print
002700* program is 'NACT03', the Error Handling program is 'NACT04',
002800* the name Browsing program is 'NACT05'.
003000* In this manner you can change the names to suit your
003100* conventions if this is desired.
003200*
003300     05  WS-LITS-PROGRAMS.
003400         10  WS-LITS-PROG-BMS-IF         PIC X(2)
003500                                         VALUE '01'.
003600         10  WS-LITS-PROG-CRUD           PIC X(2)
003700                                         VALUE '02'.
003800         10  WS-LITS-PROG-PRINT          PIC X(2)
003900                                         VALUE '03'.
004000         10  WS-LITS-PROG-ERROR-HANDLER  PIC X(2)
004100                                         VALUE '04'.
004200         10  WS-LITS-PROG-BROWSE         PIC X(2)
004300                                         VALUE '05'.
004400         10  WS-LITS-PROG-IIOP-IF        PIC X(2)
004500                                         VALUE '06'.
004600         10  WS-LITS-PROG-MQ-IF          PIC X(2)
004700                                         VALUE '07'.
004800*
004900* The file names as delivered are shown (respectively) for the
005000* main Accounts file, the Lock file and the Alternate Index Path
005100* to the Accounts file via Name. Thus you can change the names to
005200* suit your conventions if this is necessary.
005300*
005400     05  WS-LITS-FILES.
005500         10  WS-LITS-FILES-ACCOUNT       PIC X(8)
005600                                         VALUE 'ACCTFIL'.
005700         10  WS-LITS-FILES-LOCKING       PIC X(8)
005800                                         VALUE 'ACINUSE'.
005900         10  WS-LITS-FILES-NAME          PIC X(8)
006000                                         VALUE 'ACCTNAM'.
006100*
006200* The system traps errors and ABENDs wherever possible. However
006300* there are situations where errors cannot be handled elegantly.
006400* In these cases the program ABENDs. These are (respectively)
006500* when the Error Handler itself is not available, the interface
006600* to the Create/Read/Update/Delete program is incorrect, the
006700* interface to the name Browsing program is incorrect and the
006800* interface to the Error Handling program is incorrect. Only
006900* the first case should result in an unhandled ABEND.
007000*
007100     05  WS-LITS-ABENDS.
007200         10  WS-LITS-ABEND-ERROR-ABEND   PIC X(4)
007300                                         VALUE 'NERR'.
007400         10  WS-LITS-ABEND-CRUD-IF       PIC X(4)
007500                                         VALUE 'NIEC'.
007600         10  WS-LITS-ABEND-BRWS-IF       PIC X(4)
007700                                         VALUE 'NIEB'.
007800         10  WS-LITS-ABEND-ERRH-IF       PIC X(4)
007900                                         VALUE 'NERH'.
008000*
008100* The Error Handler issues messages to both the operator console
008200* and a TD queue as well as requests a dump. The messages have a
008300* prefix in order to enable implementation of automated operations
008400* alerts, etc.
008500*
008600     05  WS-LITS-OTHERS.
008700*
008800* The prefix takes the form 'XXXEHnnn' where 'XXX' can be any
008900* suitable set of characters (delivered as 'NAC'), 'EH' is an
009000* Error Handler literal and the 'nnn' is a sequence number.
009100*
009200         10  WS-LITS-ERROR-PREFIX        PIC X(3)
009300                                         VALUE 'NAC'.
009400*
009500* When ABENDs and errors are trapped a dump is requested.
009600* In order to ensure that no dump is misinterpreted as being
009700* issued by CICS logic, all dumps are preceded by a suitable
009800* character as specified in WS-LITS-DUMP-PREFIX (delivered as 'N').
009900*
010000         10  WS-LITS-DUMP-PREFIX         PIC X(1)
010100                                         VALUE 'N'.
010200*
010300* The TD queue to which error messages are directed is delivered
010400* as the standard CICS one, CSSL, but can be specified to be any
010500* other in WS-LITS-ERROR-QUEUE.
010600*
010700         10  WS-LITS-ERROR-QUEUE         PIC X(4)
010800                                         VALUE 'CSSL'.
010900*
011000* The Error Handler logic needs to propagate ABENDS up to the
011100* highest logical level of the transaction. In order to do this
011200* it requires a special code which it can recognise for this
011300* condition. As delivered this code is 'SPCL' and should not
011400* conflict with any other legitimate ABEND/DUMP code. If it does,
011500* then WS-LITS-SPECIAL value can be changed.
011600*
011700         10  WS-LITS-SPECIAL             PIC X(4)
011800                                         VALUE 'SPCL'.
011900*
012000* The time limit for use of a record is set at 10 minutes.
012100* This can be amended if desired.
012200*
012300         10  WS-LITS-USE-LIMIT           PIC S9(7) COMP-3
012400                                         VALUE +1000.
012500*
012600* The BMS MAPSET name is delivered as 'NACTSET' but may be
012700* amended if necessary. Note that the source code will
012800* still expect to COPY that name in the programs using it.
012900*
013000         10  WS-LITS-MAPSET              PIC X(7)
013100                                         VALUE 'NACTSET'.
013200*
013300* The maximum number of name matches deliverable by the browsing
013400* function is specified here. It may be desireable to reduce this.
013500*
013600         10  WS-LITS-MAX-MATCHES         PIC 9(4)
013700                                         VALUE 80.
