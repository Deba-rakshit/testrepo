000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.   NACT01.
000301 AUTHOR.       A PROGRAMMER.
000302 INSTALLATION. IBM HURSLEY.
000303 DATE-WRITTEN. AUGUST 1999.
000304 DATE-COMPILED.
000316*
000319*-------------------------------------------------------------*
000320*                                                             *
000321*               @BANNER_START@                                *
000322*      nact01.cbl                                             *
000323*      (C) Copyright IBM Corp. 2000. All Rights Reserved.     *
000324*                                                             *
000325* Element of Designing and Programming CICS Applications book *
000326*               @BANNER_END@                                  *
000327*                                                             *
000328*-------------------------------------------------------------*
000329*
000336***************************************************************
000338*
000339*    DESCRIPTION
000340*
000341* This program provides the front-end interface (presentation
000342* logic) for 3270 terminals for the CICS Application Design and
000342* programming book sample application. It employs Basic Mapping
000345* Support (BMS) services to perform its function.
000346*
000348* NACT01 is the first program that is executed when the
000349* transaction NACT is entered.  It handles the processing flow
000350* of the new Account application, performing the following
000351* functions -
000352*
000353*   -   display the menu screen
000354*   -   validate the user input
000355*   -   determine the action required such as displaying
000356*       further screens, starting the print transaction etc
000357*   -   validate any input data before calling the CRUD or
000358*       Browse program
000359*
000360***************************************************************
000361*     AMENDMENT HISTORY
000370*
000380*      DATE         AUTHOR          DESCRIPTION
000390*
000391*
000392***************************************************************
000393*     FILES
000394*
000395***************************************************************
000396*     CICS RESOURCES
000397*
000398***************************************************************
000399*     UTILITIES
000400*
000401***************************************************************
000402*     COPYBOOKS
000403*
000404*     NACWCRUD  - Working storage layout of the data passed
000405*                 to the CRUD program.
000406*     NACWBRWS  - Working storage layout of the data passed
000407*                 to the Browse program.
000408*     NACWERRH  - Working storage layout of the data passed
000409*                 to the Error Handler program.
000410*     NACWLITS  - Common working storage.
000413*     NACTSET   - The mapsets.
000420*     DFHAID    - Standard attention identifier constants
000421*                 list supplied by CICS.  It contains a list
000422*                 of hexidecimal values that correspond to
000423*                 the various attention keys and is used to
000424*                 interpret user input.
000425*     DFHBMSCA  - Defines all of the commonly used values for
000426*                 all attributes and assigns meaningful names
000427*                 to each combination.  Supplied by CICS.
000428*
000429***************************************************************
000430*
000800*
000900 ENVIRONMENT DIVISION.
001000 DATA DIVISION.
001010*
001100 WORKING-STORAGE SECTION.
001200*
001201*    Store eye catcher details to aid dump reading
001202*
001203 01  WS-DEBUG-DETAILS.
001204     05  FILLER                        PIC X(32)
001205           VALUE 'NACT01-------WORKING STORAGE  '.
001206     05  DEBUG-EYE.
001207         10  DEBUG-TRANID              PIC X(4) VALUE SPACES.
001208         10  DEBUG-TERMID              PIC X(4) VALUE SPACES.
001209         10  DEBUG-TASKNO              PIC 9(7) VALUE ZERO.
001210     05  FILLER                        PIC X    VALUE SPACE.
001211     05  DEBUG-COMMAREA-ADDR           USAGE IS POINTER.
001225*
001230 01  FILLER.
001300*
001400* These fields are used for interfacing with other
001500* programs in the suite.
001600*
002010     05  WS-PROGRAM-NAME               PIC X(8) VALUE SPACES.
002020     05  CRUD-PROGRAM.
002030         10  WS-CRUD-PROGRAM-PREFIX    PIC X(4) VALUE SPACES.
002040         10  FILLER                    PIC X(4) VALUE '02  '.
002041     05  PRINT-PROGRAM.
002042         10  WS-PRINT-PROGRAM-PREFIX   PIC X(4) VALUE SPACES.
002043         10  FILLER                    PIC X(4) VALUE '03  '.
002044     05  ABEND-PROGRAM.
002045         10  WS-ABEND-PROGRAM-PREFIX   PIC X(4) VALUE SPACES.
002046         10  FILLER                    PIC X(4) VALUE '04  '.
002047     05  BROWSE-PROGRAM.
002048         10  WS-BROWSE-PROGRAM-PREFIX  PIC X(4) VALUE SPACES.
002049         10  FILLER                    PIC X(4) VALUE '05  '.
002100*
002200* The program generally uses techniques to prevent name
002300* dependencies. This field is used for STARTing printing.
002400*
002500     05  PRINT-TRANSID                 PIC X(4) VALUE SPACES.
002510*
002520* Store of EIBRESP and EIBRESP2 set up in each EXEC CICS statement
002540*
002560     05  RESPONSE                      PIC S9(8) COMP-4 VALUE 0.
002570     05  REASON-CODE                   PIC S9(8) COMP-4 VALUE 0.
002600*
002700* The EIBFN is defined as character but the error interface
002800* expects a numeric value.
002900*
003000     05  WORK-FN                       PIC 9(4) COMP VALUE ZERO.
003100     05  WORK-FN-X REDEFINES WORK-FN   PIC X(2).
003200*
003300* General indexes.
003400*
003500     05  IX1                           PIC 9(4) COMP VALUE ZERO.
003600     05  IX2                           PIC 9(4) COMP VALUE ZERO.
003700*
003800* The program provides look up via account or name
003900*
004000     05  NAME-INDICATOR                PIC X VALUE SPACES.
004100         88  GOT-NAME                  VALUE 'Y'.
004200         88  NEED-ACCT                 VALUE 'N'.
004300*
004400* The STARS will be shown on the ? screen after validation.
004401* They will be used to highlight mandatory fill fields that
004402* the user has left blank
004500*
004600     05  ERROR-STARS                   PIC X     VALUE '*'.
004800*
004900* Each line satisfying the name look up is built here
004900* in SUM-LINE following the title line SUM-TITLE-LINE
005000*
005100     05  SUM-TITLE-LINE.
005200         10  FILLER                    PIC X(40)
005200             VALUE 'ACCT    SURNAME       FIRST   MI  TTL   '.
005200         10  FILLER                    PIC X(39)
005200             VALUE 'ADDRESS                   ST      LIMIT'.
005100     05  SUM-LINE.
005200         10  ACCTDO                    PIC X(5)  VALUE SPACES.
005300         10  FILLER                    PIC X(3)  VALUE SPACES.
005400         10  SNAMEDO                   PIC X(12) VALUE SPACES.
005500         10  FILLER                    PIC X(2)  VALUE SPACES.
005600         10  FNAMEDO                   PIC X(7)  VALUE SPACES.
005700         10  FILLER                    PIC X(2)  VALUE SPACES.
005800         10  MIDO                      PIC X(1)  VALUE SPACES.
005900         10  FILLER                    PIC X(2)  VALUE SPACES.
006000         10  TTLDO                     PIC X(4)  VALUE SPACES.
006100         10  FILLER                    PIC X(2)  VALUE SPACES.
006200         10  ADDR1DO                   PIC X(24) VALUE SPACES.
006300         10  FILLER                    PIC X(2)  VALUE SPACES.
006400         10  STATDO                    PIC X(2)  VALUE SPACES.
006500         10  FILLER                    PIC X(3)  VALUE SPACES.
006600         10  LIMITDO                   PIC X(8)  VALUE SPACES.
008710*
008720* An array containing the number of days in each month to be
008730* used to validate the date issued input field.
008731*
008740     05  DAYS-IN-MONTH.
008750         10  JANUARY                   PIC 99  VALUE 31.
008760         10  FEBRUARY                  PIC 99  VALUE 29.
008770         10  MARCH                     PIC 99  VALUE 31.
008780         10  APRIL                     PIC 99  VALUE 30.
008790         10  MAY                       PIC 99  VALUE 31.
008791         10  JUNE                      PIC 99  VALUE 30.
008792         10  JULY                      PIC 99  VALUE 31.
008793         10  AUGUST                    PIC 99  VALUE 31.
008794         10  SEPTEMBER                 PIC 99  VALUE 30.
008795         10  OCTOBER                   PIC 99  VALUE 31.
008796         10  NOVEMBER                  PIC 99  VALUE 30.
008797         10  DECEMBER                  PIC 99  VALUE 31.
008798     05 FILLER REDEFINES DAYS-IN-MONTH.
008799         10  MAX-DAYS-IN-MONTH         PIC 99  OCCURS 12.
008800*
008802* Various numeric fields to be used to determine the validity
008803* of the date issued field
008804*
008805     05  NUMERIC-CA-DAY                PIC 99  VALUE ZERO.
008806     05  NUMERIC-CA-MONTH              PIC 99  VALUE ZERO.
008807     05  NUMERIC-CA-YEAR               PIC 99  VALUE ZERO.
008808     05  DATE-CHECK-REMAINDER          PIC 99  VALUE ZERO.
008809     05  DATE-CHECK-ANSWER             PIC 99  VALUE ZERO.
008810*
008900* There are several messages which can be output. They are
009000* placed in an array here for ease of changing without
009100* having multiple literals in the program code itself.
009200*
009300 01  MSG-LIST.
009400     05  FILLER                        PIC X(60) VALUE
009500         'NAMES MUST BE ALPHABETIC AND SURNAME IS REQUIRED.'.
009600     05  FILLER                        PIC X(60) VALUE
014100     'MISMATCH BETWEEN INPUT OF REQUEST TYPE AND PRINTER ID'.
009800     05  FILLER                        PIC X(60) VALUE
009900     'REQUEST TYPE REQUIRED; MUST BE D, P, A, M OR X'.
010000     05  FILLER                        PIC X(60) VALUE
010100         'PRINTER NAME REQUIRED ON PRINT REQUESTS'.
010200     05  FILLER                        PIC X(60) VALUE
010300         'ACCOUNT NUMBER REQUIRED (BETWEEN 10000 AND 79999)'.
010400     05  FILLER                        PIC X(60) VALUE
010500         'ACCOUNT NO. MUST BE NUMERIC AND FROM 10000 TO 79999'.
010600     05  FILLER                        PIC X(60) VALUE
010700         'NO NAMES ON FILE MATCHING YOUR REQUEST'.
010800     05  FILLER                        PIC X(60) VALUE
010900         'ENTER EITHER NAME OR A REQUEST TYPE AND ACCOUNT NUMBER'.
011000     05  FILLER                        PIC X(60) VALUE
011100         'THIS ACCOUNT NUMBER ALREADY EXISTS'.
011200     05  FILLER                        PIC X(60) VALUE
011300         'NO RECORD OF THIS ACCOUNT NUMBER'.
011400     05  FILLER                        PIC X(60) VALUE
011500         'THIS ACCOUNT NUMBER ALREADY IN USE'.
011600     05  FILLER                        PIC X(60) VALUE
011700         'PRINT REQUEST SCHEDULED'.
011800     05  FILLER                        PIC X(60) VALUE
011900         'PRINTER NAME NOT RECOGNIZED'.
012000     05  FILLER                        PIC X(60) VALUE
012100     'INVALID KEY PRESSED - USE ONLY "CLEAR" OR "ENTER" KEY'.
012200     05  FILLER                        PIC X(60) VALUE
012300        'THERE ARE MORE MATCHING NAMES. PRESS PF8 TO CONTINUE.'.
012400     05  FILLER                        PIC X(60) VALUE
012500         'PREVIOUS REQUEST CANCELLED AS REQUESTED'.
012600     05  FILLER                        PIC X(15) VALUE
012700         'ACCOUNT NUMBER '.
012600     05  ADD-ACCOUNT-NO                PIC X(05) VALUE SPACES.
012600     05  FILLER                        PIC X(40) VALUE
012700         ' ADDED'.
012600     05  FILLER                        PIC X(15) VALUE
012700         'ACCOUNT NUMBER '.
012600     05  MOD-ACCOUNT-NO                PIC X(05) VALUE SPACES.
012600     05  FILLER                        PIC X(40) VALUE
012700         ' MODIFIED'.
012600     05  FILLER                        PIC X(15) VALUE
012700         'ACCOUNT NUMBER '.
012600     05  DEL-ACCOUNT-NO                PIC X(05) VALUE SPACES.
012600     05  FILLER                        PIC X(40) VALUE
012700         ' DELETED'.
013200     05  FILLER                        PIC X(60) VALUE
013300         'EITHER ENTER Y TO CONFIRM, OR "CLEAR" TO CANCEL'.
013400     05  FILLER                        PIC X(60) VALUE
013500         'YOUR REQUEST WAS INTERRUPTED; PLEASE RETRY'.
013600     05  FILLER                        PIC X(60) VALUE
013700         'CORRECT HIGHLIGHTED ITEMS (STAR MEANS ITEM REQUIRED)'.
013800     05  FILLER                        PIC X(60) VALUE
013900         'USE ONLY "ENTER" (TO PROCEED) OR "CLEAR" (TO CANCEL)'.
014000     05  FILLER                        PIC X(60) VALUE
014100     'MAKE SOME ENTRIES AND PRESS "ENTER", OR "CLEAR" TO CANCEL'.
014200 01  FILLER REDEFINES MSG-LIST.
014300     05  MSG-TEXT                      PIC X(60) OCCURS 24.
014400*
014500* The messages are referred to by a type to make the program
014600* more 'readable'.
014700*
014800 01  FILLER.
014900     05  MSG-NO                        PIC S9(4) COMP VALUE +0.
015000         88  MSG-ALL-OK                               VALUE  0.
015100         88  MSG-NAMES                                VALUE  1.
015200         88  MSG-PRTR-MISMATCH                        VALUE  2.
015300         88  MSG-REQUEST                              VALUE  3.
015400         88  MSG-PRTR-REQ                             VALUE  4.
015500         88  MSG-ACCT-REQ                             VALUE  5.
015600         88  MSG-ACCT-NUM                             VALUE  6.
015700         88  MSG-NOMATCH                              VALUE  7.
015800         88  MSG-CONFLICT                             VALUE  8.
015900         88  MSG-DUPLICATE                            VALUE  9.
016000         88  MSG-NO-REC                               VALUE 10.
016100         88  MSG-IN-USE                               VALUE 11.
016200         88  MSG-PRINT-SCHED                          VALUE 12.
016300         88  MSG-PRTR-UNKNOWN                         VALUE 13.
016400         88  MSG-BAD-KEY                              VALUE 14.
016500         88  MSG-MORE                                 VALUE 15.
016600         88  MSG-REQ-CANC                             VALUE 16.
016700         88  MSG-ADD-DONE                             VALUE 17.
016800         88  MSG-MOD-DONE                             VALUE 18.
016900         88  MSG-DEL-DONE                             VALUE 19.
017000         88  MSG-TO-CANCEL                            VALUE 20.
017100         88  MSG-REQ-INT                              VALUE 21.
017200         88  MSG-CORRECT                              VALUE 22.
017300         88  MSG-ACT-KEY                              VALUE 23.
017400         88  MSG-NEED-DATA                            VALUE 24.
017500*
017510 01  FILLER.
017520     05  WS-BB-ERROR                   PIC X VALUE SPACES.
017521         88  WS-BB-ERROR-PRESENT       VALUE 'Y'.
017530*
017600* The interfaces to the CRUD, Browse and error handler programs
017700* are described in copy books in order to ensure consistency.
017800*
017810 01  FILLER .
017811     05  FILLER                        PIC X(36) VALUE
017820         '********  NACWCRUD COPYBOOK  *******'.
018000     COPY NACWCRUD.
018001*
018010 01  FILLER.
018011     05  FILLER                        PIC X(36) VALUE
018020         '********  NACWBRWS COPYBOOK  *******'.
018022     COPY NACWBRWS.
018023*
018030 01  FILLER.
018040     05  FILLER                        PIC X(36) VALUE
018050         '********  NACWERRH COPYBOOK  *******'.
018200     COPY NACWERRH.
018300*
018400* Various values which you might wish to modify are placed in one
018500* copy book in order to make those sorts of changes more easily.
018600*
018610*
018620 01  FILLER.
018630     05  FILLER                        PIC X(36) VALUE
018640         '********  NACWLITS COPYBOOK  *******'.
018700     COPY NACWLITS.
018800*
018900* The generated symbolic map must be included in the program.
019000*
019010 01  FILLER.
019020     05  FILLER                        PIC X(36) VALUE
019030         '********  NACTSET COPYBOOK  ********'.
019100 COPY NACTSET.
019200*
019300* The CICS supplied values for interfacing with a 3270 must
019400* be available for testing and setting keys and attributes.
019500*
019510 01  FILLER.
019520     05  FILLER                        PIC X(36) VALUE
019530         '********  DFHAID  COPYBOOK  ********'.
019600 COPY DFHAID.
019610*
019620 01  FILLER.
019630     05  FILLER                        PIC X(36) VALUE
019640         '********  DFHBMSCA COPYBOOK  *******'.
019700 COPY DFHBMSCA.
019800*
019900* There is one desired value (ASKIP DARK) which the
020000* standard copy book does not provide.
020100*
020200     02  DFHBMASD                      PIC X VALUE '@'.
020400*
020402 01  FILLER.
020403     05  FILLER                        PIC X(36) VALUE
020404         '********  LINKAGE SECTION   ********'.
020405*
020410 EJECT.
020420*
020500* Between pseudo-conversational tasks various data needs to
020600* be saved. This area describes that data.
020700*
020800 LINKAGE SECTION.
020900 01  DFHCOMMAREA.
021000     05  CA-LAST-MAP                     PIC X.
021100         88  CA-SENT-MENU                VALUE 'M'.
021200         88  CA-SENT-DETAIL              VALUE 'D'.
021300         88  CA-SENT-NAMES               VALUE 'N'.
021400*
021500* This field only has meaning when CA-SENT-DETAIL is true.
021600*
021700     05  CA-MODE                         PIC X.
021800         88  CA-DISPLAYING               VALUE 'D'.
021900         88  CA-MODIFYING                VALUE 'M'.
022000         88  CA-DELETING                 VALUE 'X'.
022100         88  CA-ADDING                   VALUE 'A'.
022200     05  CA-COMMON-DATA.
022300         10  CA-ACCOUNT                  PIC X(5).
022400         10  CA-SURNAME                  PIC X(18).
022500         10  CA-FIRSTNAME                PIC X(12).
022600     05  CA-MENU-DATA.
022700         10  CA-REQUEST-TYPE             PIC X.
022800             88  CA-REQUEST-DISPLAY      VALUE 'D'.
022900             88  CA-REQUEST-MODIFY       VALUE 'M'.
023000             88  CA-REQUEST-ADD          VALUE 'A'.
023100             88  CA-REQUEST-DELETE       VALUE 'X'.
023200             88  CA-REQUEST-PRINT        VALUE 'P'.
023300             88  CA-REQUEST-TYPE-OK      VALUE 'D' 'M' 'A'
023400                                               'X' 'P'.
023500         10  CA-PRINTER                  PIC X(4).
023600         10  CA-CONT-SURNAME             PIC X(18).
023700         10  CA-CONT-FIRSTNAME           PIC X(12).
023800         10  CA-CONT-MORE                PIC 9(4).
023900         10  FILLER                      PIC X(191).
024000     05  CA-DETAIL-DATA REDEFINES CA-MENU-DATA.
024100         10  CA-MIDDLE-INITIAL           PIC X.
024200         10  CA-TITLE                    PIC X(4).
024300         10  CA-TELEPHONE-NO             PIC X(10).
024400         10  CA-ADDRESS-LINE1            PIC X(24).
024500         10  CA-ADDRESS-LINE2            PIC X(24).
024600         10  CA-ADDRESS-LINE3            PIC X(24).
024700         10  CA-AUTH1D                   PIC X(32).
024800         10  CA-AUTH2D                   PIC X(32).
024900         10  CA-AUTH3D                   PIC X(32).
025000         10  CA-AUTH4D                   PIC X(32).
025100         10  CA-NUMBER-CARDS-ISSUED      PIC X.
025200         10  CA-MONTH-ISSUED             PIC X(2).
025300         10  CA-DAY-ISSUED               PIC X(2).
025400         10  CA-YEAR-ISSUED              PIC X(2).
025500         10  CA-REASON-CODE              PIC X.
025600         10  CA-CARD-CODE                PIC X.
025700         10  CA-APPROVED                 PIC X(3).
025800         10  CA-SPECIAL-CODE1            PIC X.
025900         10  CA-SPECIAL-CODE2            PIC X.
026000         10  CA-SPECIAL-CODE3            PIC X.
026100*
026110 EJECT.
026120*
026800*
026900 PROCEDURE DIVISION.
026901*
026910 NACT01-MAIN SECTION.
026920*
026921* First we establish the ABEND handler in case unexpected
026922* errors arise, such as program interrupts. The technique
026923* used here assumes a naming convention where the variable
026924* part of the program names is in the 5th and 6th positions.
026925* This allows for changes to the names in case the provided
026926* names conflict with existing applications.
026930*
026933* The logic flow is then determined
026940*
026950 NACT01-010.
026960*
026970*  Assign returns system information to the program.
026971*  This example returns the name of the current program to
026972*  WS-PROGRAM-NAME.
026980*
027000     EXEC CICS ASSIGN
027100               PROGRAM(WS-PROGRAM-NAME)
027200               NOHANDLE
027300     END-EXEC.
027310*
027330 NACT01-020.
027340*
027350*  Set the up the names of the programs that are linked and
027380*  xctl'd.
027390*
027400     MOVE WS-PROGRAM-NAME TO WS-CRUD-PROGRAM-PREFIX
027401                             WS-PRINT-PROGRAM-PREFIX
027402                             WS-ABEND-PROGRAM-PREFIX
027403                             WS-BROWSE-PROGRAM-PREFIX.
027404*
027405 NACT01-030.
027406*
027407*  Establish the abend handler program.
027408*
027600     EXEC CICS HANDLE ABEND
027700               PROGRAM(ABEND-PROGRAM)
027810               RESP(RESPONSE)
027820               RESP2(REASON-CODE)
027900     END-EXEC.
027910*
028000     IF  RESPONSE NOT = DFHRESP(NORMAL)
028100         EXEC CICS ABEND
028200                   ABCODE(WS-LITS-ABEND-ERROR-ABEND)
028300         END-EXEC
028400     END-IF.
028401*
028402 NACT01-040.
028403*
028404*  Set up values in the eye-catcher
028405*
028406     MOVE EIBTRNID TO DEBUG-TRANID.
028407     MOVE EIBTRMID TO DEBUG-TERMID.
028408     MOVE EIBTASKN TO DEBUG-TASKNO.
028409*
028410*  Set up the commarea address
028411*
028412     SET DEBUG-COMMAREA-ADDR TO ADDRESS OF DFHCOMMAREA.
028413*
028414 NACT01-050.
028415*
028416*  Initialise all maps
028420*
028500     PERFORM A-INITIALIZE-MENU-SCREEN.
029900*
029910 NACT01-060.
029920*
030000* The first time we need to obtain an area for saving data
030100* between pseudo-conversational tasks as well as send the
030200* initial menu map.
030210* The logic must now take action depending on what happened last.
030300*
030400     IF  EIBCALEN = 0
030500         EXEC CICS GETMAIN
030600                   LENGTH(LENGTH OF DFHCOMMAREA)
030700                   SET   (ADDRESS OF DFHCOMMAREA)
030800         END-EXEC
030900         PERFORM U-SEND-EMPTY-MENU
030910     ELSE
030920         EVALUATE TRUE
030930            WHEN CA-SENT-MENU
030940                 PERFORM B-GET-MENU-DATA
030950            WHEN CA-SENT-NAMES
030960                 EVALUATE EIBAID
030961                     WHEN DFHPF8
030962                     WHEN DFHPA2
030963                          PERFORM C-GO-FORWARD
030964                     WHEN OTHER
030965                          SET CA-SENT-MENU TO TRUE
030966                          PERFORM B-GET-MENU-DATA
030967                 END-EVALUATE
030970            WHEN CA-SENT-DETAIL
030980                 PERFORM D-GET-DETAIL-DATA
030981            WHEN OTHER
030983                 PERFORM Y-UNEXPECTED-ERROR
030984         END-EVALUATE
031000     END-IF.
031010*
031020 NACT01-070.
031030*
031040* End the task without saving any data.
031100*
031200     EXEC CICS RETURN
031300     END-EXEC.
031400*
032110 END-DO-MAIN-LOGIC.
032200     EXIT.
032210     EJECT.
032211*
032212 A-INITIALIZE-MENU-SCREEN SECTION.
032213*
032214* Initialize acctmnu, the menu output map.
032216* Initialize msgmo, the output error line on the map
032217* to remove all previously displayed error messages.
032218* Initialize the summary title and lines to remove
032218* all previously displayed summary information.
032221* Initialize the acctdtl, the detail output map.
032222*
032223 A-010.
032224     MOVE LOW-VALUES TO ACCTMNUO
032225     MOVE SPACES     TO MSGMO.
032226*
032225     MOVE SPACES     TO SUMTTLMO.
032228     PERFORM TEST BEFORE VARYING IX1 FROM 1 BY 1 UNTIL IX1 > 6
032229         MOVE SPACES TO SUMLNMO (IX1)
032230     END-PERFORM.
032231*
032232     MOVE LOW-VALUES TO ACCTDTLO.
032233*
032234 END-A-INITIALIZE-MENU-SCREEN.
032235     EXIT.
032236     EJECT.
032240*
032600 B-GET-MENU-DATA SECTION.
032610*
032620* Processing is dependant upon the key that was pressed and
032621* field EIBAID contains this information.  EIBAID is set to
032622* dfhpf3 when function key 3 has been pressed, DFHCLEAR for
032623* the clear/pause key and DFHENTER for the enter key.
032624* key testing is necessary to determine the action required.
032630*
032640 B-010.
032700     EVALUATE EIBAID
032800         WHEN DFHPF3
032900         WHEN DFHCLEAR
033000             PERFORM BA-SEND-CONTROL
033200         WHEN DFHENTER
033300             MOVE SPACE TO WS-BB-ERROR
033301             PERFORM BB-MENU-DATA-ENTERED
033310             IF WS-BB-ERROR-PRESENT
033320                PERFORM BD-MENU-ERROR-OUT
033321                SET CA-SENT-MENU TO TRUE
033322                PERFORM X-RETURN-COM-AREA
033330             ELSE
033340                PERFORM BE-FALL-THROUGH
033341                SET CA-SENT-DETAIL TO TRUE
033342                PERFORM X-RETURN-COM-AREA
033350             END-IF
033400         WHEN OTHER
033500             SET MSG-BAD-KEY TO TRUE
033600             PERFORM BC-MENU-ERR-OUT-WTH-CURSOR
033610             SET CA-SENT-MENU TO TRUE
033620             PERFORM X-RETURN-COM-AREA
033700     END-EVALUATE.
062301*
062310 END-B-GET-MENU-DATA.
062400     EXIT.
062500     EJECT.
062501*
062506 BA-SEND-CONTROL SECTION.
062507*
062508* End the pseudo-conversational series of tasks.
062509* Free the user's keyboard and end without saving
062510* any data.
062511*
062512 BA-010.
062513     EXEC CICS SEND CONTROL
062514               FREEKB
062515               ERASE
062517               RESP(RESPONSE)
062518               RESP2(REASON-CODE)
062519     END-EXEC.
062520*
062521     IF  RESPONSE NOT = DFHRESP(NORMAL)
062522         PERFORM Y-UNEXPECTED-ERROR
062523     END-IF.
062524*
062525 END-BA-SEND-CONTROL.
062526     EXIT.
062527     EJECT.
062528*
062529 BB-MENU-DATA-ENTERED SECTION.
062530*
062531* Obtain the input (if any) from the user.
062532*
062533 BB-010.
062534     MOVE LOW-VALUES TO ACCTMNUI.
062535*
062536     EXEC CICS RECEIVE
062537               MAP('ACCTMNU')
062538               MAPSET(WS-LITS-MAPSET)
062540               RESP(RESPONSE)
062541               RESP2(REASON-CODE)
062542     END-EXEC.
062543*
062544     IF  (RESPONSE NOT = DFHRESP(NORMAL) )
062545     AND (RESPONSE NOT = DFHRESP(MAPFAIL))
062546          PERFORM Y-UNEXPECTED-ERROR
062547     END-IF.
062548*
062549 BB-020.
062556*
062557* This routine examines each possible input field from the menu
062558* map to see if it has been entered or erased. The Length suffix
062559* field will be non-zero if the field has been entered. The Flag
062560* suffixed field will contain a special value if the field has
062561* been entirely erased. In both cases we can move the Input
062562* suffixed field to the area where we save data between pseudo-
062563* conversational tasks since it will contain the input or
062564* LOW-VALUES (nulls).
062565*
062567     IF  SNAMEML NOT = 0
062568         MOVE SNAMEMI TO CA-SURNAME
062569     ELSE
062570         IF  SNAMEMF = DFHBMEOF
062571             MOVE SPACES TO CA-SURNAME
062572         END-IF
062573     END-IF.
062574*
062576     IF  FNAMEML NOT = 0
062577         MOVE FNAMEMI TO CA-FIRSTNAME
062578     ELSE
062579         IF  FNAMEMF = DFHBMEOF
062580             MOVE SPACES TO CA-FIRSTNAME
062581         END-IF
062582     END-IF.
062583*
062584     IF  REQMF = DFHBMEOF
062585         MOVE LOW-VALUE TO CA-REQUEST-TYPE
062586     ELSE
062587         IF  REQML NOT = 0
062588             MOVE REQMI TO CA-REQUEST-TYPE
062589             IF CA-REQUEST-TYPE = SPACE
062590                 MOVE LOW-VALUE TO CA-REQUEST-TYPE
062591             END-IF
062593         END-IF
062594     END-IF.
062595*
062596     IF  ACCTMF = DFHBMEOF
062597         MOVE LOW-VALUES TO CA-ACCOUNT
062598     ELSE
062599         IF  ACCTML NOT = 0
062600             MOVE ACCTMI TO CA-ACCOUNT
062601             IF  CA-ACCOUNT = SPACES
062602                 MOVE LOW-VALUES TO CA-ACCOUNT
062603             END-IF
062605         END-IF
062606     END-IF.
062607*
062596     IF  PRTRMF = DFHBMEOF
062597         MOVE LOW-VALUES TO CA-PRINTER
062598     ELSE
062599         IF  PRTRML NOT = 0
062600             MOVE PRTRMI TO CA-PRINTER
062601             IF  CA-PRINTER = SPACES
062602                 MOVE LOW-VALUES TO CA-PRINTER
062603             END-IF
062605         END-IF
062606     END-IF.
062615*
062616 BB-030.
062617     MOVE LOW-VALUES TO ACCTMNUO.
062618     MOVE SPACES     TO MSGMO.
062619*
032225     MOVE SPACES     TO SUMTTLMO.
032228     PERFORM TEST BEFORE VARYING IX1 FROM 1 BY 1 UNTIL IX1 > 6
032229         MOVE SPACES TO SUMLNMO (IX1)
062622     END-PERFORM.
062623*
062624 BB-040.
062625*
062626* The validation rules for input are applied here. The technique
062627* sets attributes in the output map area. If the field is in
062628* error, it is highlighted and a request to put the cursor at
062629* the beginning of that field is made. (Setting the
062630* input Length field to a special value of -1 tells BMS of our
062631* desire to place the cursor on the screen at the beginning of
062632* that field. BMS ignores all but the first such request.) If
062633* the field is valid, its attribute is set to normal intensity
062634* in case it had been in error (highlighted) previously.
062635*
062636*
062637* Initialise the msg-all-ok flag and replace all low values
062638* in both the first and surname with spaces.
062639*
062640     SET MSG-ALL-OK TO TRUE.
062641*
062642     INSPECT CA-SURNAME REPLACING ALL LOW-VALUES BY SPACES.
062643     INSPECT CA-FIRSTNAME REPLACING ALL LOW-VALUES BY SPACES.
062644*
062645 BB-050.
062646*
062647* The forename must contain alphabetic characters only.
062648*
062649     IF  CA-FIRSTNAME NOT ALPHABETIC
062650         SET MSG-NAMES TO TRUE
062651         MOVE -1 TO FNAMEML
062652         MOVE DFHBMBRY TO FNAMEMA
062653     ELSE
062654         MOVE DFHBMUNP TO FNAMEMA
062655     END-IF.
062656*
062659* The surname must contain alphabetic characters only.
062660*
062661     IF  CA-SURNAME NOT ALPHABETIC
062662         SET MSG-NAMES TO TRUE
062663         MOVE -1 TO SNAMEML
062664         MOVE DFHBMBRY TO SNAMEMA
062665     ELSE
062666         MOVE DFHBMUNP TO SNAMEMA
062667     END-IF.
062670*
062671* Determine if a name search or a specific function
062672* has been requested.
062673*
062674     IF  CA-SURNAME = SPACES
062675     AND CA-FIRSTNAME = SPACES
062676         SET NEED-ACCT TO TRUE
062677     ELSE
062678         SET GOT-NAME  TO TRUE
062679*
062680* Something must be entered into the surname for a name search.
062681*
062682         IF  CA-SURNAME = SPACES
062683             SET MSG-NAMES TO TRUE
062684             MOVE -1 TO SNAMEML
062685             MOVE DFHBMBRY TO SNAMEMA
062686         END-IF
062687     END-IF.
062688*
062689     IF  MSG-ALL-OK
062690*
062691* If a specific function was requested, then an account must
062692* be present and in the acceptable range of '10000' to '79999',
062692* as well as a valid request type of D, A, M, X or P.
062693*
062694         IF  NEED-ACCT
062695             IF  CA-ACCOUNT = LOW-VALUES
062696                 SET MSG-ACCT-REQ TO TRUE
062697                 MOVE -1 TO ACCTML
062698                 MOVE DFHBMBRY TO ACCTMA
062699             ELSE
062700                 IF (CA-ACCOUNT NOT NUMERIC)
062701                 OR  CA-ACCOUNT < '10000'
062702                 OR  CA-ACCOUNT > '79999'
062703                     SET MSG-ACCT-NUM TO TRUE
062704                     MOVE -1 TO ACCTML
062705                     MOVE DFHBMBRY TO ACCTMA
062706                 ELSE
062707                     MOVE DFHBMUNP TO ACCTMA
062708                 END-IF
062709             END-IF
062728             IF  CA-REQUEST-TYPE-OK
062731                 MOVE DFHBMUNP TO REQMA
062732             ELSE
062733                 SET MSG-REQUEST TO TRUE
062734                 MOVE -1 TO REQML
062735                 MOVE DFHBMBRY TO REQMA
062736             END-IF
062690*
062691* Also, a check is made between request type and printer id
062692* that if request-type is P for PRINT, a printer id is input
062692* and conversely, if request-type is NOT P, that a printer id
062693* has not been input.
062710*
062728             IF  (CA-REQUEST-PRINT AND CA-PRINTER = LOW-VALUES)
062728                               OR (CA-PRINTER NOT = LOW-VALUES
062728                                   AND NOT CA-REQUEST-PRINT)
062733                 SET MSG-PRTR-MISMATCH TO TRUE
062734                 MOVE -1 TO REQML
062735                 MOVE DFHBMBRY TO REQMA
062732             ELSE
062731                 MOVE DFHBMUNP TO PRTRMA
062736             END-IF
062710*
062711* If a name search was requested, then neither an account,
062711* nor request type, nor printer id should be present.
062713*
062714         ELSE
062715             IF  CA-PRINTER = LOW-VALUES
062716                 MOVE DFHBMUNP TO PRTRMA
062717             ELSE
062718                 SET MSG-CONFLICT TO TRUE
062719                 MOVE -1 TO PRTRML
062720                 MOVE DFHBMBRY TO PRTRMA
062721             END-IF
062715             IF  CA-ACCOUNT = LOW-VALUES
062716                 MOVE DFHBMUNP TO ACCTMA
062717             ELSE
062718                 SET MSG-CONFLICT TO TRUE
062719                 MOVE -1 TO ACCTML
062720                 MOVE DFHBMBRY TO ACCTMA
062721             END-IF
062715             IF  CA-REQUEST-TYPE = LOW-VALUES
062716                 MOVE DFHBMUNP TO REQMA
062717             ELSE
062718                 SET MSG-CONFLICT TO TRUE
062719                 MOVE -1 TO REQML
062720                 MOVE DFHBMBRY TO REQMA
062721             END-IF
062722         END-IF
062737     END-IF.
062738*
062739 BB-060.
062740*
062741* When the input data is valid, further processing is required
062742* else set the error flag to true and return to B-GET-MENU-DATA
062743* to prepare the menu screen highlighting the error.
062744*
062745     IF  MSG-ALL-OK
062746         PERFORM BBA-PROCESS-VALID-MENU
062747     ELSE
062750         SET WS-BB-ERROR-PRESENT TO TRUE
062800     END-IF.
062811*
062812 END-BB-MENU-DATA-ENTERED.
062813     EXIT.
062814     EJECT.
062850*
078020 BBA-PROCESS-VALID-MENU SECTION.
078021*
078030* When the input data valid, we need to process the request.
078040*
078041 BBA-010.
078042*
078043* If a name search is requested, then the menu must be changed.
078044*
078050     MOVE LOW-VALUES TO ACCTDTLO.
078060*
078063     IF  GOT-NAME
078064         MOVE LOW-VALUES     TO ACCTMNUO
078065         MOVE SPACES         TO MSGINVMO
078065         MOVE SPACES         TO MSGMO
078065         MOVE DFHBMUNP       TO REQMA
078065         MOVE DFHBMUNP       TO ACCTMA
078065         MOVE DFHBMUNP       TO PRTRMA
078066         MOVE SUM-TITLE-LINE TO SUMTTLMO
078067         PERFORM BBAA-SEARCH-NAMES
078068     END-IF.
078069*
078070 BBA-020.
078071*
078072* The action taken when an account is entered depends on the
078073* function requested. All of these call the back-end CRUD
078074* program. Various errors may arise, and those must be handled.
078075*
078076     EVALUATE TRUE
078077*
078078* A simple Display only requires obtaining the data and
078079* setting up the map to prevent modification.
078080*
078081         WHEN CA-REQUEST-DISPLAY
078082             MOVE LOW-VALUES TO NACTREC-DATA
078083             MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
078084             SET WS-CRUD-REQ-ENQUIRE TO TRUE
078085             PERFORM T-CALL-CRUD
078086             IF  WS-CRUD-NO-ERROR
078087                 PERFORM BBAB-MOVE-TO-DETAIL-MAP
078088                 MOVE -1 TO SNAMEDL
078089                 MOVE 'PRESS "CLEAR" OR "ENTER" TO RETURN TO THE M
078089-                     'ENU WHEN FINISHED' TO MSGDO
078091                 PERFORM BBAC-PROTECT-DETAILS
078092                 SET CA-DISPLAYING TO TRUE
078093*
078094* The user must be told when no such account exists.
078095*
078096             ELSE
078097                 SET MSG-NO-REC TO TRUE
078098                 MOVE -1       TO ACCTML
078099                 MOVE DFHBMBRY TO ACCTMA
078100                 SET WS-BB-ERROR-PRESENT TO TRUE
078101             END-IF
078102*
078103* A Modification requires locking the account as well as
078104* obtaining the data.
078105*
078106         WHEN CA-REQUEST-MODIFY
078107             MOVE LOW-VALUES TO NACTREC-DATA
078108             MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
078109             SET WS-CRUD-REQ-READ TO TRUE
078110             PERFORM T-CALL-CRUD
078111             EVALUATE TRUE
078112                 WHEN WS-CRUD-NO-ERROR
078113                     PERFORM BBAB-MOVE-TO-DETAIL-MAP
078114                     PERFORM BBAD-SAVE-DETAILS
078115                     SET CA-MODIFYING TO TRUE
078116*
078117* The user must be told when the account is already in use.
078118*
078119                 WHEN WS-CRUD-BAD-LOCK
078120                     SET MSG-IN-USE TO TRUE
078121                     MOVE -1       TO ACCTML
078122                     MOVE DFHBMBRY TO ACCTMA
078123                     SET WS-BB-ERROR-PRESENT TO TRUE
078124*
078125* The user must be told when no such account exists.
078126*
078127                 WHEN OTHER
078128                     SET MSG-NO-REC TO TRUE
078129                     MOVE -1       TO ACCTML
078130                     MOVE DFHBMBRY TO ACCTMA
078131                     SET WS-BB-ERROR-PRESENT TO TRUE
078132             END-EVALUATE
078133*
078134* An Add requires ensuring the record does not exists as well as
078135* locking the account.
078136*
078137         WHEN CA-REQUEST-ADD
078138             MOVE LOW-VALUES TO NACTREC-DATA
078139             MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
078140             SET WS-CRUD-REQ-ENQUIRE TO TRUE
078141             PERFORM T-CALL-CRUD
078142             EVALUATE TRUE
078143*
078144* When the account already exists, the user must be informed.
078145*
078146                 WHEN WS-CRUD-NO-ERROR
078147                     SET MSG-DUPLICATE TO TRUE
078148                     MOVE -1       TO ACCTML
078149                     MOVE DFHBMBRY TO ACCTMA
078150                     SET WS-BB-ERROR-PRESENT TO TRUE
078151                 WHEN OTHER
078152                     SET WS-CRUD-REQ-LOCK TO TRUE
078153                     PERFORM T-CALL-CRUD
078154*
078155* If the account is already in use, the user must be informed.
078156*
078157                     IF  WS-CRUD-BAD-LOCK
078158                         SET MSG-DUPLICATE TO TRUE
078159                         MOVE -1       TO ACCTML
078160                         MOVE DFHBMBRY TO ACCTMA
078161                         SET WS-BB-ERROR-PRESENT TO TRUE
078162                     END-IF
078163*
078164* Otherwise the output map must allow for entry of all of
078165* the required data.
078166*
078167                     MOVE -1 TO SNAMEDL
078168                     MOVE '       ADD' TO TITLEDO
078172                     MOVE CA-ACCOUNT   TO ACCTDO IN ACCTDTLO
078172*                    MOVE LOW-VALUES   TO VFYTLDO IN ACCTDTLO
078173                     MOVE
078174                'FILL IN AND PRESS "ENTER," OR "CLEAR" TO CANCEL'
078175                      TO  MSGDO
078176                     SET CA-ADDING TO TRUE
078177             END-EVALUATE
078178*
078179* A Delete requires locking the account as well as
078180* obtaining the data.
078181*
078182         WHEN CA-REQUEST-DELETE
078183             MOVE LOW-VALUES TO NACTREC-DATA
078184             MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
078185             SET WS-CRUD-REQ-READ TO TRUE
078186             PERFORM T-CALL-CRUD
078187             EVALUATE TRUE
078188*
078189* The data must all be protected but confirmation of the
078190* delete must be solicited from the user.
078191*
078192                 WHEN WS-CRUD-NO-ERROR
078193                     PERFORM BBAB-MOVE-TO-DETAIL-MAP
078194                     MOVE '    DELETE' TO TITLEDO
078198                     MOVE 'ENTER Y TO CONFIRM DELETE:' TO VFYTLDO
078195                     MOVE -1           TO VFYDL
078196                     MOVE DFHBMUNP     TO VFYDA
078197                     MOVE 'CONFIRM DELETION AND PRESS "ENTER", OR
078198-                         '"CLEAR" TO CANCEL' TO MSGDO
078200                     PERFORM BBAC-PROTECT-DETAILS
078201                     SET CA-DELETING TO TRUE
078202*
078203* The user must be told when the account is already in use.
078204*
078205                 WHEN WS-CRUD-BAD-LOCK
078206                     SET MSG-IN-USE TO TRUE
078207                     MOVE -1       TO ACCTML
078208                     MOVE DFHBMBRY TO ACCTMA
078209                     SET WS-BB-ERROR-PRESENT TO TRUE
078210*
078211* The user must be told when no such account exists.
078212*
078213                 WHEN OTHER
078214                     SET MSG-NO-REC TO TRUE
078215                     MOVE -1       TO ACCTML
078216                     MOVE DFHBMBRY TO ACCTMA
078217                     SET WS-BB-ERROR-PRESENT TO TRUE
078218             END-EVALUATE
078219*
078220* A Print request must ensure a terminal identifer was entered.
078221*
078222         WHEN CA-REQUEST-PRINT
078224             IF  CA-PRINTER = LOW-VALUES
078225                 SET MSG-PRTR-REQ TO TRUE
078226                 MOVE -1 TO PRTRML
078227                 MOVE DFHBMBRY TO PRTRMA
078228                 SET WS-BB-ERROR-PRESENT TO TRUE
078229*
078230* We must obtain the data and initiate the printer transaction.
078231*
078232             ELSE
078233                 MOVE LOW-VALUES TO NACTREC-DATA
078234                 MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
078235                 SET WS-CRUD-REQ-ENQUIRE TO TRUE
078236                 PERFORM T-CALL-CRUD
078237                 IF  WS-CRUD-NO-ERROR
078238                     PERFORM BBAB-MOVE-TO-DETAIL-MAP
078239                     MOVE EIBTRNID TO PRINT-TRANSID
078240                     MOVE WS-LITS-TRANS-PRINT
078241                           TO PRINT-TRANSID(4:1)
078242                     EXEC CICS START
078243                               TRANSID(PRINT-TRANSID)
078244                               TERMID(CA-PRINTER)
078245                               FROM(ACCTDTLO)
078247                               RESP(RESPONSE)
078248                               RESP2(REASON-CODE)
078249                     END-EXEC
078250                     EVALUATE RESPONSE
078251*
078252* We must inform the user when printing
078253* has been successfully scheduled.
078254*
078255                         WHEN DFHRESP(NORMAL)
078256                             SET MSG-PRINT-SCHED TO TRUE
078257                             MOVE -1 TO SNAMEML
078258                             SET WS-BB-ERROR-PRESENT TO TRUE
078259*
078260* We must inform the user when the terminal
078261* used is not known to CICS.
078262*
078263                         WHEN DFHRESP(TERMIDERR)
078264                             SET MSG-PRTR-UNKNOWN TO TRUE
078265                             MOVE -1 TO SNAMEML
078266                             SET WS-BB-ERROR-PRESENT TO TRUE
078267*
078268* Unexpected conditions must always be anticipated.
078269*
078270                         WHEN OTHER
078271                             PERFORM Y-UNEXPECTED-ERROR
078272                     END-EVALUATE
078273*
078274* The user must be told when no such account exists.
078275*
078276                 ELSE
078277                     SET MSG-NO-REC TO TRUE
078278                     MOVE -1       TO ACCTML
078279                     MOVE DFHBMBRY TO ACCTMA
078280                     SET WS-BB-ERROR-PRESENT TO TRUE
078281                 END-IF
078282             END-IF
078283     END-EVALUATE.
078284*
078285 END-BBA-PROCESS-VALID-MENU.
078286     EXIT.
078287     EJECT.
078288*
078289 BBAA-SEARCH-NAMES SECTION.
078290*
078291* This routine initiates a Browse process.
078292*
078293 BBAA-010.
078294     PERFORM V-SET-UP-FOR-BROWSE.
078295*
078296     SET WS-BRWS-REQ-BROWSE TO TRUE.
078297     MOVE CA-SURNAME   TO SNAMEDO IN WS-BRWS-ENTRY (1).
078298     MOVE CA-FIRSTNAME TO FNAMEDO IN WS-BRWS-ENTRY (1).
078299*
078300     PERFORM W-LINK-FOR-BROWSE.
078301*
078302 END-BBAA-SEARCH-NAMES.
078303     EXIT.
078304     EJECT.
078305*
078306 BBAB-MOVE-TO-DETAIL-MAP SECTION.
078310*
078320* This routine populates the detail map from the data obtained.
078330*
078340 BBAB-010.
078400     MOVE ACCTDO   IN NACTREC-DATA TO ACCTDO   IN ACCTDTLO.
078500     MOVE SNAMEDO  IN NACTREC-DATA TO SNAMEDO  IN ACCTDTLO.
078600     MOVE FNAMEDO  IN NACTREC-DATA TO FNAMEDO  IN ACCTDTLO.
078700     MOVE MIDO     IN NACTREC-DATA TO MIDO     IN ACCTDTLO.
078800     MOVE TTLDO    IN NACTREC-DATA TO TTLDO    IN ACCTDTLO.
078900     MOVE TELDO    IN NACTREC-DATA TO TELDO    IN ACCTDTLO.
079000     MOVE ADDR1DO  IN NACTREC-DATA TO ADDR1DO  IN ACCTDTLO.
079100     MOVE ADDR2DO  IN NACTREC-DATA TO ADDR2DO  IN ACCTDTLO.
079200     MOVE ADDR3DO  IN NACTREC-DATA TO ADDR3DO  IN ACCTDTLO.
079300     MOVE AUTH1DO  IN NACTREC-DATA TO AUTH1DO  IN ACCTDTLO.
079400     MOVE AUTH2DO  IN NACTREC-DATA TO AUTH2DO  IN ACCTDTLO.
079500     MOVE AUTH3DO  IN NACTREC-DATA TO AUTH3DO  IN ACCTDTLO.
079600     MOVE AUTH4DO  IN NACTREC-DATA TO AUTH4DO  IN ACCTDTLO.
079700     MOVE CARDSDO  IN NACTREC-DATA TO CARDSDO  IN ACCTDTLO.
079800     MOVE IMODO    IN NACTREC-DATA TO IMODO    IN ACCTDTLO.
079900     MOVE IDAYDO   IN NACTREC-DATA TO IDAYDO   IN ACCTDTLO.
080000     MOVE IYRDO    IN NACTREC-DATA TO IYRDO    IN ACCTDTLO.
080100     MOVE RSNDO    IN NACTREC-DATA TO RSNDO    IN ACCTDTLO.
080200     MOVE CCODEDO  IN NACTREC-DATA TO CCODEDO  IN ACCTDTLO.
080300     MOVE APPRDO   IN NACTREC-DATA TO APPRDO   IN ACCTDTLO.
080400     MOVE SCODE1DO IN NACTREC-DATA TO SCODE1DO IN ACCTDTLO.
080500     MOVE SCODE2DO IN NACTREC-DATA TO SCODE2DO IN ACCTDTLO.
080600     MOVE SCODE3DO IN NACTREC-DATA TO SCODE3DO IN ACCTDTLO.
080700     MOVE STATDO   IN NACTREC-DATA TO STATDO   IN ACCTDTLO.
080800     MOVE LIMITDO  IN NACTREC-DATA TO LIMITDO  IN ACCTDTLO.
081801*
081810 END-BBAB-MOVE-TO-DETAIL-MAP.
081900     EXIT.
081910     EJECT.
081920*
082200 BBAC-PROTECT-DETAILS SECTION.
082210*
082220* This routine protects the detail data fields from modification.
082230*
082240 BBAC-010.
082300     MOVE DFHBMASK TO SNAMEDA
082400                      FNAMEDA
082500                      MIDA
082600                      TTLDA
082700                      TELDA
082800                      ADDR1DA
082900                      ADDR2DA
083000                      ADDR3DA
083100                      AUTH1DA
083200                      AUTH2DA
083300                      AUTH3DA
083400                      AUTH4DA
083500                      CARDSDA
083600                      IMODA
083700                      IDAYDA
083800                      IYRDA
083900                      RSNDA
084000                      CCODEDA
084100                      APPRDA
084200                      SCODE1DA
084300                      SCODE2DA
084400                      SCODE3DA.
084501*
084510 END-BBAC-PROTECT-DETAILS.
084600     EXIT.
084610     EJECT.
084611*
084612 BBAD-SAVE-DETAILS SECTION.
084613*
084614* This routine saves the Account details in the commarea
084615* before an modify takes place.
084616*
084617 BBAD-010.
084618     MOVE SNAMEDO  IN NACTREC-DATA TO CA-SURNAME.
084619     MOVE FNAMEDO  IN NACTREC-DATA TO CA-FIRSTNAME.
084620     MOVE MIDO     IN NACTREC-DATA TO CA-MIDDLE-INITIAL.
084621     MOVE TTLDO    IN NACTREC-DATA TO CA-TITLE.
084622     MOVE TELDO    IN NACTREC-DATA TO CA-TELEPHONE-NO.
084623     MOVE ADDR1DO  IN NACTREC-DATA TO CA-ADDRESS-LINE1.
084624     MOVE ADDR2DO  IN NACTREC-DATA TO CA-ADDRESS-LINE2.
084625     MOVE ADDR3DO  IN NACTREC-DATA TO CA-ADDRESS-LINE3.
084626     MOVE AUTH1DO  IN NACTREC-DATA TO CA-AUTH1D.
084627     MOVE AUTH2DO  IN NACTREC-DATA TO CA-AUTH2D.
084628     MOVE AUTH3DO  IN NACTREC-DATA TO CA-AUTH3D.
084629     MOVE AUTH4DO  IN NACTREC-DATA TO CA-AUTH4D.
084630     MOVE CARDSDO  IN NACTREC-DATA TO CA-NUMBER-CARDS-ISSUED.
084631     MOVE IMODO    IN NACTREC-DATA TO CA-MONTH-ISSUED.
084632     MOVE IDAYDO   IN NACTREC-DATA TO CA-DAY-ISSUED.
084633     MOVE IYRDO    IN NACTREC-DATA TO CA-YEAR-ISSUED.
084634     MOVE RSNDO    IN NACTREC-DATA TO CA-REASON-CODE.
084635     MOVE CCODEDO  IN NACTREC-DATA TO CA-CARD-CODE.
084636     MOVE APPRDO   IN NACTREC-DATA TO CA-APPROVED.
084637     MOVE SCODE1DO IN NACTREC-DATA TO CA-SPECIAL-CODE1.
084638     MOVE SCODE2DO IN NACTREC-DATA TO CA-SPECIAL-CODE2.
084639     MOVE SCODE3DO IN NACTREC-DATA TO CA-SPECIAL-CODE3.
084640*
084641     MOVE '    MODIFY'     TO TITLEDO
084642     MOVE -1 TO SNAMEDL
084643     MOVE 'MAKE CHANGES AND PRESS "ENTER", OR "CLEAR" TO CANCEL'
084644           TO  MSGDO.
084645*
084646 END-BBAD-SAVE-DETAILS.
084647     EXIT.
084648     EJECT.
084649*
084650 BC-MENU-ERR-OUT-WTH-CURSOR SECTION.
084651*
084652* This routine simply completes the menu map with a
084653* message and sends it to the user's terminal but
084654* retains the current cursor position on the screen.
084655*
084656 BC-010.
084657     MOVE MSG-TEXT (MSG-NO) TO MSGMO.
084658*
084659     EXEC CICS SEND
084660               MAP('ACCTMNU')
084661               MAPSET(WS-LITS-MAPSET)
084662               CURSOR(EIBCPOSN)
084663               DATAONLY
084664               FRSET
084665               FREEKB
084667               RESP(RESPONSE)
084668               RESP2(REASON-CODE)
084669     END-EXEC.
084670*
084671     IF  RESPONSE NOT = DFHRESP(NORMAL)
084672         PERFORM Y-UNEXPECTED-ERROR
084673     END-IF.
084674*
084677 END-BC-MENU-ERR-OUT-WTH-CURSOR.
084678     EXIT.
084679     EJECT.
084680*
084681 BD-MENU-ERROR-OUT SECTION.
084682*
084683* Complete the menu map with a message and send it
084684* to the user's terminal.
084685*
084686 BD-010.
084687     MOVE 'HIGHLIGHTED FIELDS INVALID - CAN BE MORE ERRORS THAN LI
084687-         'STED BELOW' TO MSGINVMO.
084687     MOVE MSG-TEXT (MSG-NO) TO MSGMO.
084688*
084689     EXEC CICS SEND
084690               MAP('ACCTMNU')
084691               MAPSET(WS-LITS-MAPSET)
084692               CURSOR
084693               DATAONLY
084694               FRSET
084695               FREEKB
084697               RESP(RESPONSE)
084698               RESP2(REASON-CODE)
084699     END-EXEC.
084700*
084701     IF  RESPONSE NOT = DFHRESP(NORMAL)
084702         PERFORM Y-UNEXPECTED-ERROR
084703     END-IF.
084704*
084705 END-BD-MENU-ERROR-OUT.
084706     EXIT.
084707     EJECT.
084708*
084709 BE-FALL-THROUGH SECTION.
084710*
084711* The results of our processing must be shown to the user
084712* and the transaction terminated.
084713*
084714 BE-010.
084715     EXEC CICS SEND
084716               MAP('ACCTDTL')
084717               MAPSET(WS-LITS-MAPSET)
084718               ERASE
084719               FREEKB
084720               CURSOR
084722               RESP(RESPONSE)
084723               RESP2(REASON-CODE)
084724     END-EXEC.
084725*
084726     IF  RESPONSE NOT = DFHRESP(NORMAL)
084727         PERFORM Y-UNEXPECTED-ERROR
084728     END-IF.
084730*
084731 END-BE-FALL-THROUGH.
084732     EXIT.
084733     EJECT.
084734*
084735 C-GO-FORWARD SECTION.
084736*
084737* This routine handles the case when
084738* 6 name matches have been displayed.
084740*
084741* The user may decide to view the next list of surnames.
084742* The maximum number of surnames output is 6.  The call to the
084743* the browse program is set up for a continuation request.
084744*
084745 C-010.
084746     PERFORM V-SET-UP-FOR-BROWSE.
084747*
084748     SET WS-BRWS-REQ-CONTINUE TO TRUE.
084749     MOVE CA-CONT-SURNAME   TO CA-SURNAME
084750                        SNAMEDO IN WS-BRWS-ENTRY (1).
084751     MOVE CA-CONT-FIRSTNAME TO CA-FIRSTNAME
084752                        FNAMEDO IN WS-BRWS-ENTRY (1).
084753     MOVE CA-CONT-MORE      TO WS-BRWS-MORE.
084754*
084755     PERFORM W-LINK-FOR-BROWSE.
084756*
084757 END-C-GO-FORWARD.
084758     EXIT.
084760     EJECT.
084800*
084900 D-GET-DETAIL-DATA SECTION.
084910*
084920* This routine is invoked after a detail map has been displayed.
084930*
084940 D-010.
085000     EVALUATE TRUE
085100*
085200* If the previous request was to Display an account,
085300* then we simply go back to an empty menu.
085400*
085500         WHEN CA-DISPLAYING
085600             PERFORM U-SEND-EMPTY-MENU
085700*
085800* If the previous request was to Modify an account, then
085900* we need to check which key was pressed.
086000*
086100         WHEN CA-MODIFYING
086200             EVALUATE EIBAID
086300*
086400* We need to get the changes and validate the input.
086500*
086600                 WHEN DFHENTER
086700                     PERFORM DA-RECEIVE-DETAIL
086800                     IF  NOT MSG-ALL-OK
086900                         MOVE -1 TO SNAMEDL
087000                         PERFORM DB-SEND-DETAIL-WTH-MESSAGE
087100                     END-IF
087200                     PERFORM DC-VALIDATE-DETAIL
087300*
087400* If the data is valid, we need to update the account.
087500*
087600                     IF  MSG-ALL-OK
087700                         PERFORM DD-MOVE-DETAIL-MAP-TO-REC
087800                         SET WS-CRUD-REQ-UPDATE TO TRUE
087900                         PERFORM T-CALL-CRUD
088000*
088100* There is a possibility that the user took too much time
088200* and someone else obtained the lock in the meantime.
088300*
088400                         IF  WS-CRUD-BAD-LOCK
088500                             SET MSG-REQ-INT TO TRUE
088600                             PERFORM DE-SEND-MENU-WITH-MESSAGE
088700*
088800* If all is OK then we need to inform the user it was done.
088900*
089000                         ELSE
089100                             MOVE CA-ACCOUNT TO MOD-ACCOUNT-NO
089100                             SET MSG-MOD-DONE TO TRUE
089200                             PERFORM DE-SEND-MENU-WITH-MESSAGE
089300                         END-IF
089400*
089500* If the data is not valid, we need to tell the user.
089600*
089700                     ELSE
089800                         PERFORM DB-SEND-DETAIL-WTH-MESSAGE
089900                     END-IF
090000*
090100* Pressing <Clear> causes the request to be cancelled.
090200* This requires that the previous lock be released.
090300*
090400                 WHEN DFHCLEAR
090500                     SET WS-CRUD-REQ-FREE TO TRUE
090600                     MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
090700                     PERFORM T-CALL-CRUD
090800                     SET MSG-REQ-CANC TO TRUE
090900                     PERFORM DE-SEND-MENU-WITH-MESSAGE
091000*
091100* Other keys are invalid.
091200*
091300                 WHEN OTHER
091400                     SET MSG-ACT-KEY TO TRUE
091500                     PERFORM DB-SEND-DETAIL-WTH-MESSAGE
091600             END-EVALUATE
091700*
091800* If the previous request was to Delete an account, then
091900* we need to check which key was pressed.
092000*
092100         WHEN CA-DELETING
092110             MOVE LOW-VALUES TO NACTREC-DATA
092120             MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
092200             EVALUATE EIBAID
092300*
092400* We need to see if the user has correctly confirmed the delete.
092500*
092600                 WHEN DFHENTER
092700                     PERFORM DA-RECEIVE-DETAIL
092800                     IF  VFYDI = 'Y'
092900                         SET WS-CRUD-REQ-DELETE TO TRUE
093000                         PERFORM T-CALL-CRUD
093100                         IF  WS-CRUD-BAD-LOCK
093200*
093300* There is a possibility that the user took too much time
093400* and someone else obtained the lock in the meantime.
093500*
093600                             SET MSG-REQ-INT TO TRUE
093700                             PERFORM DE-SEND-MENU-WITH-MESSAGE
093800*
093900* If all is OK then we need to inform the user it was done.
094000*
094100                         ELSE
089100                             MOVE CA-ACCOUNT TO DEL-ACCOUNT-NO
094200                             SET MSG-DEL-DONE TO TRUE
094300                             PERFORM DE-SEND-MENU-WITH-MESSAGE
094400                         END-IF
094500*
094600* Only 'Y' is a valid response to confirm the Delete.
094700*
094800                     ELSE
094900                         MOVE -1 TO VFYDL
095000                         MOVE DFHBMBRY TO VFYDA
095100                         SET MSG-TO-CANCEL TO TRUE
095200                         PERFORM DB-SEND-DETAIL-WTH-MESSAGE
095300                     END-IF
095400*
095500* Pressing <Clear> causes the request to be cancelled.
095600* This requires that the previous lock be released.
095700*
095800                 WHEN DFHCLEAR
095900                     SET WS-CRUD-REQ-FREE TO TRUE
096100                     PERFORM T-CALL-CRUD
096200                     SET MSG-REQ-CANC TO TRUE
096300                     PERFORM DE-SEND-MENU-WITH-MESSAGE
096400*
096500* Other keys are invalid.
096600*
096700                 WHEN OTHER
096800                     SET MSG-ACT-KEY TO TRUE
096900                     PERFORM DB-SEND-DETAIL-WTH-MESSAGE
097000             END-EVALUATE
097100*
097200* If the previous request was to Add an account, then
097300* we need to check which key was pressed.
097400*
097500         WHEN CA-ADDING
097600             EVALUATE EIBAID
097700*
097800* We need to get and validate the input.
097900*
098000                 WHEN DFHENTER
098100                     PERFORM DA-RECEIVE-DETAIL
098200                     IF  NOT MSG-ALL-OK
098300                         MOVE -1 TO SNAMEDL
098400                         PERFORM DB-SEND-DETAIL-WTH-MESSAGE
098500                     END-IF
098600                     PERFORM DC-VALIDATE-DETAIL
098700*
098800* If the data is valid, we need to create the account.
098900*
099000                     IF  MSG-ALL-OK
099100                         PERFORM DD-MOVE-DETAIL-MAP-TO-REC
099200                         SET WS-CRUD-REQ-CREATE TO TRUE
099300                         PERFORM T-CALL-CRUD
099400*
099500* There is a possibility that the user took too much time
099600* and someone else obtained the lock in the meantime.
099700*
099800                         IF  WS-CRUD-BAD-LOCK
099900                             SET MSG-REQ-INT TO TRUE
100000                             PERFORM DE-SEND-MENU-WITH-MESSAGE
100100*
100200* If all is OK then we need to inform the user it was done.
100300*
100400                         ELSE
089100                             MOVE CA-ACCOUNT TO ADD-ACCOUNT-NO
100500                             SET MSG-ADD-DONE TO TRUE
100600                             PERFORM DE-SEND-MENU-WITH-MESSAGE
100700                         END-IF
100800*
100900* If the data is not valid, we need to tell the user.
101000*
101100                     ELSE
101200                         PERFORM DB-SEND-DETAIL-WTH-MESSAGE
101300                     END-IF
101400*
101500* Pressing <Clear> causes the request to be cancelled.
101600* This requires that the previous lock be released.
101700*
101800                 WHEN DFHCLEAR
101900                     SET WS-CRUD-REQ-FREE TO TRUE
102000                     MOVE CA-ACCOUNT TO ACCTDO IN NACTREC-DATA
102100                     PERFORM T-CALL-CRUD
102200                     SET MSG-REQ-CANC TO TRUE
102300                     PERFORM DE-SEND-MENU-WITH-MESSAGE
102400*
102500* Other keys are invalid.
102600*
102700                 WHEN OTHER
102800                     SET MSG-ACT-KEY TO TRUE
102900                     PERFORM DB-SEND-DETAIL-WTH-MESSAGE
103000             END-EVALUATE
103100     END-EVALUATE.
103101*
103110 END-D-GET-DETAIL-DATA.
103200     EXIT.
103210     EJECT.
103220*
103600 DA-RECEIVE-DETAIL SECTION.
103610*
103620* This routine get the detail input from the user.
103630*
103640 DA-010.
103700     MOVE LOW-VALUES TO ACCTDTLI
103710*
103800     EXEC CICS RECEIVE
103900               MAP('ACCTDTL')
104000               MAPSET(WS-LITS-MAPSET)
104110               RESP(RESPONSE)
104120               RESP2(REASON-CODE)
104200     END-EXEC.
104210*
104300     IF  (RESPONSE NOT = DFHRESP(NORMAL) )
104400     AND (RESPONSE NOT = DFHRESP(MAPFAIL))
104500          PERFORM Y-UNEXPECTED-ERROR
104600     END-IF.
104601*
104610 DA-020.
104700*
104800* The new data (if any) must be merged with any previously
104900* entered data before validating it. We examine each possible
105000* input field from the detail map to see if it has been entered
105100* or erased. The Length suffix field will be non-zero if the
105200* field has been entered. The Flag suffixed field will contain
105300* a special value if the field has been entirely erased. In both
105400* cases we can move the Input suffixed field to the area where
105500* we save data between pseudo-conversational tasks since it will
105600* contain the input or LOW-VALUES (nulls).
105700*
105800     IF  SNAMEDL NOT = 0
105900         MOVE SNAMEDI TO CA-SURNAME
106000     ELSE
106100         IF  SNAMEDF = DFHBMEOF
106200             MOVE SPACES TO CA-SURNAME
106300         END-IF
106400     END-IF.
106410*
106500     IF  FNAMEDL NOT = 0
106600         MOVE FNAMEDI TO CA-FIRSTNAME
106700     ELSE
106800         IF  FNAMEDF = DFHBMEOF
106900             MOVE SPACES TO CA-FIRSTNAME
107000         END-IF
107100     END-IF.
107110*
107200     IF  MIDL NOT = 0
107300         MOVE MIDI TO CA-MIDDLE-INITIAL
107400     ELSE
107500         IF  MIDF = DFHBMEOF
107600             MOVE SPACES TO CA-MIDDLE-INITIAL
107700         END-IF
107800     END-IF.
107810*
107900     IF  TTLDL NOT = 0
108000         MOVE TTLDI TO CA-TITLE
108100     ELSE
108200         IF  TTLDF = DFHBMEOF
108300             MOVE SPACES TO CA-TITLE
108400         END-IF
108500     END-IF.
108510*
108600     IF  TELDL NOT = 0
108700         MOVE TELDI TO CA-TELEPHONE-NO
108800     ELSE
108900         IF  TELDF = DFHBMEOF
109000             MOVE SPACES TO CA-TELEPHONE-NO
109100         END-IF
109200     END-IF.
109210*
109300     IF  ADDR1DL NOT = 0
109400         MOVE ADDR1DI TO CA-ADDRESS-LINE1
109500     ELSE
109600         IF  ADDR1DF = DFHBMEOF
109700             MOVE SPACES TO CA-ADDRESS-LINE1
109800         END-IF
109900     END-IF.
109910*
110000     IF  ADDR2DL NOT = 0
110100         MOVE ADDR2DI TO CA-ADDRESS-LINE2
110200     ELSE
110300         IF  ADDR2DF = DFHBMEOF
110400             MOVE SPACES TO CA-ADDRESS-LINE2
110500         END-IF
110600     END-IF.
110610*
110700     IF  ADDR3DL NOT = 0
110800         MOVE ADDR3DI TO CA-ADDRESS-LINE3
110900     ELSE
111000         IF  ADDR3DF = DFHBMEOF
111100             MOVE SPACES TO CA-ADDRESS-LINE3
111200         END-IF
111300     END-IF.
111310*
111400     IF  AUTH1DL NOT = 0
111500         MOVE AUTH1DI TO CA-AUTH1D
111600     ELSE
111700         IF  AUTH1DF = DFHBMEOF
111800             MOVE SPACES TO CA-AUTH1D
111900         END-IF
112000     END-IF.
112010*
112100     IF  AUTH2DL NOT = 0
112200         MOVE AUTH2DI TO CA-AUTH2D
112300     ELSE
112400         IF  AUTH2DF = DFHBMEOF
112500             MOVE SPACES TO CA-AUTH2D
112600         END-IF
112700     END-IF.
112710*
112800     IF  AUTH3DL NOT = 0
112900         MOVE AUTH3DI TO CA-AUTH3D
113000     ELSE
113100         IF  AUTH3DF = DFHBMEOF
113200             MOVE SPACES TO CA-AUTH3D
113300         END-IF
113400     END-IF.
113410*
113500     IF  AUTH4DL NOT = 0
113600         MOVE AUTH4DI TO CA-AUTH4D
113700     ELSE
113800         IF  AUTH4DF = DFHBMEOF
113900             MOVE SPACES TO CA-AUTH4D
114000         END-IF
114100     END-IF.
114110*
114200     IF  CARDSDL NOT = 0
114300         MOVE CARDSDI TO CA-NUMBER-CARDS-ISSUED
114400     ELSE
114500         IF  CARDSDF = DFHBMEOF
114600             MOVE SPACES TO CA-NUMBER-CARDS-ISSUED
114700         END-IF
114800     END-IF.
114810*
114900     IF  IMODL NOT = 0
115000         MOVE IMODI TO CA-MONTH-ISSUED
115100     ELSE
115200         IF  IMODF = DFHBMEOF
115300             MOVE SPACES TO CA-MONTH-ISSUED
115400         END-IF
115500     END-IF.
115510*
115600     IF  IDAYDL NOT = 0
115700         MOVE IDAYDI TO CA-DAY-ISSUED
115800     ELSE
115900         IF  IDAYDF = DFHBMEOF
116000             MOVE SPACES TO CA-DAY-ISSUED
116100         END-IF
116200     END-IF.
116210*
116300     IF  IYRDL NOT = 0
116400         MOVE IYRDI TO CA-YEAR-ISSUED
116500     ELSE
116600         IF  IYRDF = DFHBMEOF
116700             MOVE SPACES TO CA-YEAR-ISSUED
116800         END-IF
116900     END-IF.
116910*
117000     IF  RSNDL NOT = 0
117100         MOVE RSNDI TO CA-REASON-CODE
117200     ELSE
117300         IF  RSNDF = DFHBMEOF
117400             MOVE SPACES TO CA-REASON-CODE
117500         END-IF
117600     END-IF.
117610*
117700     IF  CCODEDL NOT = 0
117800         MOVE CCODEDI TO CA-CARD-CODE
117900     ELSE
118000         IF  CCODEDF = DFHBMEOF
118100             MOVE SPACES TO CA-CARD-CODE
118200         END-IF
118300     END-IF.
118310*
118400     IF  APPRDL NOT = 0
118500         MOVE APPRDI TO CA-APPROVED
118600     ELSE
118700         IF  APPRDF = DFHBMEOF
118800             MOVE SPACES TO CA-APPROVED
118900         END-IF
119000     END-IF.
119010*
119100     IF  SCODE1DL NOT = 0
119200         MOVE SCODE1DI TO CA-SPECIAL-CODE1
119300     ELSE
119400         IF  SCODE1DF = DFHBMEOF
119500             MOVE SPACES TO CA-SPECIAL-CODE1
119600         END-IF
119700     END-IF.
119710*
119800     IF  SCODE2DL NOT = 0
119900         MOVE SCODE2DI TO CA-SPECIAL-CODE2
120000     ELSE
120100         IF  SCODE2DF = DFHBMEOF
120200             MOVE SPACES TO CA-SPECIAL-CODE2
120300         END-IF
120400     END-IF.
120410*
120500     IF  SCODE3DL NOT = 0
120600         MOVE SCODE3DI TO CA-SPECIAL-CODE3
120700     ELSE
120800         IF  SCODE3DF = DFHBMEOF
120900             MOVE SPACES TO CA-SPECIAL-CODE3
121000         END-IF
121100     END-IF.
121110*
121200     IF  EIBRESP = DFHRESP(NORMAL)
121300         SET MSG-ALL-OK TO TRUE
121400     ELSE
121500         SET MSG-NEED-DATA TO TRUE
121600     END-IF.
121601*
121610 DA-030.
121700*
121800* We want to ensure that only spaces rather than LOW-VALUES
121900* (nulls) are stored on the file.
122000*
122100     INSPECT CA-DETAIL-DATA REPLACING ALL LOW-VALUES BY SPACES.
122200     INSPECT CA-SURNAME REPLACING ALL LOW-VALUES BY SPACES.
122300     INSPECT CA-FIRSTNAME REPLACING ALL LOW-VALUES BY SPACES.
122401*
122410 END-DA-RECEIVE-DETAIL.
122500     EXIT.
122510     EJECT.
122511*
122515 DB-SEND-DETAIL-WTH-MESSAGE SECTION.
122516*
122517* This routine simply completes the detail map with a
122518* message and sends it to the user's terminal.
122519*
122520 DB-010.
122521     MOVE MSG-TEXT (MSG-NO) TO MSGDO.
122522     EXEC CICS SEND
122523               MAP('ACCTDTL')
122524               MAPSET(WS-LITS-MAPSET)
122525               DATAONLY
122526               FREEKB
122527               CURSOR
122528               NOHANDLE
122529     END-EXEC.
122530*
122531 DB-020.
122532     PERFORM X-RETURN-COM-AREA.
122533*
122534 END-DB-SEND-DETAIL-WTH-MESSAGE.
122535     EXIT.
122536     EJECT.
122537*
123600 DC-VALIDATE-DETAIL SECTION.
123610*
123620* The validation rules for input are applied here. The technique
123630* sets attributes in the output map area. If the field is in
123640* error, it is highlighted and a request to put the cursor at
123650* the beginning of that field is made. (Setting the normally
123660* input Length field to a special value of -1 tells BMS of our
123670* desire to place the cursor on the screen at the beginning of
123680* that field. BMS ignores all but the first such request.) If
123690* the field is valid, its attribute is set to normal intensity
123691* in case it had been in error (highlighted) previously.
123692*
123700*
123800* Prepare the output map area so only the required
123900* data is transmitted.
124000*
124010 DC-010.
124100     MOVE LOW-VALUES TO ACCTDTLO.
124200*
124210 DC-020.
124220*
124300* The surname is mandatory and must be alphabetic.
124400*
124500     IF  CA-SURNAME = SPACES
124600     OR (CA-SURNAME NOT ALPHABETIC)
124700         MOVE -1         TO SNAMEDL
124800         MOVE DFHBMBRY   TO SNAMEDA
124900         SET MSG-CORRECT TO TRUE
125000     ELSE
125100         MOVE DFHBMUNP TO SNAMEDA
125200     END-IF.
125210*
125300     IF  CA-SURNAME = SPACES
125400         MOVE ERROR-STARS TO SNAMEDO IN ACCTDTLO
125500     END-IF.
125501*
125510 DC-030.
125600*
125700* The forename is mandatory and must be alphabetic.
125800*
125900     IF  CA-FIRSTNAME = SPACES
126000     OR (CA-FIRSTNAME NOT ALPHABETIC)
126100         MOVE -1         TO FNAMEDL
126200         MOVE DFHBMBRY   TO FNAMEDA
126300         SET MSG-CORRECT TO TRUE
126400     ELSE
126500         MOVE DFHBMUNP TO FNAMEDA
126600     END-IF.
126610*
126700     IF  CA-FIRSTNAME = SPACES
126800         MOVE ERROR-STARS TO FNAMEDO IN ACCTDTLO
126900     END-IF.
126910*
126920 DC-040.
127000*
127100* The middle initial is optional but must be an
127200* alphabetic character if it is entered.
127300*
127400     IF  CA-MIDDLE-INITIAL NOT ALPHABETIC
127500         MOVE -1         TO MIDL
127600         MOVE DFHBMBRY   TO MIDA
127700         SET MSG-CORRECT TO TRUE
127800     ELSE
127900         MOVE DFHBMUNP TO MIDA
128000     END-IF.
128010*
128020 DC-050.
128100*
128200* The title is optional but must contain
128300* alphabetic characters if it is entered.
128400*
128500     IF  CA-TITLE NOT ALPHABETIC
128600         MOVE -1         TO TTLDL
128700         MOVE DFHBMBRY   TO TTLDA
128800         SET MSG-CORRECT TO TRUE
128900     ELSE
129000         MOVE DFHBMUNP TO TTLDA
129100     END-IF.
129110*
129120 DC-060.
129200*
129300* The telephone number is mandatory and must be numeric.
129400*
129500     IF  (CA-TELEPHONE-NO = SPACES OR ZEROS)
129600     OR  (CA-TELEPHONE-NO NOT NUMERIC)
129700         MOVE -1         TO TELDL
129800         MOVE DFHBMBRY   TO TELDA
129900         SET MSG-CORRECT TO TRUE
130000     ELSE
130100         MOVE DFHBMUNP TO TELDA
130200     END-IF.
130210*
130300     IF  CA-TELEPHONE-NO = SPACES OR ZEROS
130400         MOVE ERROR-STARS TO TELDO IN ACCTDTLO
130500     END-IF.
130510*
130520 DC-070.
130600*
130700* The first 2 address lines are mandatory and must not be spaces.
130800*
130900     IF  CA-ADDRESS-LINE1 = SPACES
131000         MOVE -1          TO ADDR1DL
131100         MOVE DFHBMBRY    TO ADDR1DA
131200         MOVE ERROR-STARS TO ADDR1DO IN ACCTDTLO
131300         SET MSG-CORRECT  TO TRUE
131400     ELSE
131500         MOVE DFHBMUNP TO ADDR1DA
131600     END-IF.
131610*
131700     IF  CA-ADDRESS-LINE2 = SPACES
131800         MOVE -1          TO ADDR2DL
131900         MOVE DFHBMBRY    TO ADDR2DA
132000         MOVE ERROR-STARS TO ADDR2DO IN ACCTDTLO
132100         SET MSG-CORRECT  TO TRUE
132200     ELSE
132300         MOVE DFHBMUNP TO ADDR2DA
132400     END-IF.
132410*
132420 DC-080.
132500*
132600* The number of cards issued is mandatory
132700* and must be in the range of '1' to '9'.
132800*
132900     IF  CA-NUMBER-CARDS-ISSUED = SPACES
133000     OR (CA-NUMBER-CARDS-ISSUED <= '0'
133100     OR  CA-NUMBER-CARDS-ISSUED > '9')
133200         MOVE -1         TO CARDSDL
133300         MOVE DFHBMBRY   TO CARDSDA
133400         SET MSG-CORRECT TO TRUE
133500     ELSE
133600         MOVE DFHBMUNP TO CARDSDA
133700     END-IF.
133710*
133800     IF  CA-NUMBER-CARDS-ISSUED = SPACES
133900         MOVE ERROR-STARS TO CARDSDO IN ACCTDTLO
134000     END-IF.
134010*
134020 DC-090.
134100*
134200* The date fields are mandatory and must
134300* be a valid date.
134400*
134410* The month field must contain a value between 1 and 12.
134430*
134500     IF (CA-MONTH-ISSUED NOT NUMERIC)
134600     OR (CA-MONTH-ISSUED <= '00'
134700     OR  CA-MONTH-ISSUED > '12')
134800         MOVE -1         TO IMODL
134900         MOVE DFHBMBRY   TO IMODA
135000         SET MSG-CORRECT TO TRUE
135010         MOVE ZERO       TO NUMERIC-CA-MONTH
135100     ELSE
135200         MOVE DFHBMUNP        TO IMODA
135210         MOVE CA-MONTH-ISSUED TO NUMERIC-CA-MONTH
135300     END-IF.
135310*
135400     IF  CA-MONTH-ISSUED = SPACES
135500         MOVE ERROR-STARS TO IMODO IN ACCTDTLO
135600     END-IF.
135610*
135630* The day field must contain a numeric value not greater
135640* than the maximum-days-in-month value for the relevant month.
135650*
135700     IF (CA-DAY-ISSUED NOT NUMERIC)
135800     OR (CA-DAY-ISSUED <= '00'
135810     OR  CA-DAY-ISSUED > '31')
135900         MOVE -1         TO IDAYDL
136000         MOVE DFHBMBRY   TO IDAYDA
136100         SET MSG-CORRECT TO TRUE
136200         MOVE 0          TO NUMERIC-CA-DAY
136300     ELSE
136400         IF NUMERIC-CA-MONTH > 0
136410            MOVE CA-DAY-ISSUED TO NUMERIC-CA-DAY
136420            IF NUMERIC-CA-DAY >
136424                       MAX-DAYS-IN-MONTH (NUMERIC-CA-MONTH)
136425               MOVE -1         TO IDAYDL
136426               MOVE DFHBMBRY   TO IDAYDA
136427               SET MSG-CORRECT TO TRUE
136428               MOVE 0          TO NUMERIC-CA-DAY
136429            ELSE
136430               MOVE DFHBMUNP TO IDAYDA
136440            END-IF
136450         ELSE
136451            MOVE DFHBMUNP TO IDAYDA
136460         END-IF
136500     END-IF.
136510*
136600     IF  CA-DAY-ISSUED = SPACES
136700         MOVE ERROR-STARS TO IDAYDO IN ACCTDTLO
136800     END-IF.
136810*
136830* The year field must contain a numeric value.  The only
136840* complication being that a check for leap year needs to be
136850* performed. For simplicity we are only using a 2 digit year
136852* field, and are assuming the year value 00 refers to 2000
136854* ie the program is valid for the date window 1901 to 2099.
136856* Therefore when the year is divisible by 4 without remainder
136860* or when year value is 00, then a February date of 29 days is
136870* When a day of 29, month of 2 and a non leap year value is
136880* entered, the whole date field is highlighted in error.
136890*
136900     IF  CA-YEAR-ISSUED NOT NUMERIC
137000         MOVE -1         TO IYRDL
137100         MOVE DFHBMBRY   TO IYRDA
137200         SET MSG-CORRECT TO TRUE
137300     ELSE
137310         MOVE CA-YEAR-ISSUED TO NUMERIC-CA-YEAR
137400         IF NUMERIC-CA-MONTH = 2
137401         AND NUMERIC-CA-DAY = 29
137402         AND NUMERIC-CA-YEAR > 0
137404             DIVIDE 4 INTO NUMERIC-CA-YEAR
137405                      GIVING DATE-CHECK-ANSWER
137406                      REMAINDER DATE-CHECK-REMAINDER
137407             IF DATE-CHECK-REMAINDER = 0
137408                MOVE DFHBMUNP       TO IYRDA
137409             ELSE
137410                MOVE DFHBMBRY   TO IDAYDA
137411                                   IMODA
137412                                   IYRDA
137413                MOVE -1         TO IDAYDL
137414                                   IMODL
137415                                   IYRDL
137420                SET MSG-CORRECT TO TRUE
137421             END-IF
137422         ELSE
137430              MOVE DFHBMUNP       TO IYRDA
137500     END-IF.
137510*
137600     IF  CA-YEAR-ISSUED = SPACES
137700         MOVE ERROR-STARS TO IYRDO IN ACCTDTLO
137800     END-IF.
137810*
137820 DC-100.
137900*
138000* The reason code is mandatory and must be one
138100* of the following acceptable values, 'N', 'L', 'S', 'R'.
138200*
138300     IF  CA-REASON-CODE = SPACES
138400     OR  NOT (CA-REASON-CODE = 'N'
138500          OR  CA-REASON-CODE = 'L'
138600          OR  CA-REASON-CODE = 'S'
138700          OR  CA-REASON-CODE = 'R')
138800         MOVE -1         TO RSNDL
138900         MOVE DFHBMBRY   TO RSNDA
139000         SET MSG-CORRECT TO TRUE
139100     ELSE
139200         MOVE DFHBMUNP TO RSNDA
139300     END-IF.
139310*
139400     IF  CA-REASON-CODE = SPACES
139500         MOVE ERROR-STARS TO RSNDO IN ACCTDTLO
139600     END-IF.
139610*
139620 DC-110.
139700*
139800* The card code is mandatory but can be any value.
139900*
140000     IF  CA-CARD-CODE = SPACES
140100         MOVE -1          TO CCODEDL
140200         MOVE DFHBMBRY    TO CCODEDA
140300         MOVE ERROR-STARS TO CCODEDO IN ACCTDTLO
140400         SET MSG-CORRECT  TO TRUE
140500     ELSE
140600         MOVE DFHBMUNP TO CCODEDA
140700     END-IF.
140710*
140720 DC-120.
140800*
140900* The approved field is mandatory but can be any value.
141000*
141100     IF  CA-APPROVED = SPACES
141200         MOVE -1          TO APPRDL
141300         MOVE DFHBMBRY    TO APPRDA
141400         MOVE ERROR-STARS TO APPRDO IN ACCTDTLO
141500         SET MSG-CORRECT  TO TRUE
141600     ELSE
141700         MOVE DFHBMUNP TO APPRDA
141800     END-IF.
141910*
141920 END-DC-VALIDATE-DETAIL.
142000     EXIT.
142010     EJECT.
142020*
142300 DD-MOVE-DETAIL-MAP-TO-REC SECTION.
142310*
142320* This routine populates the record area from the data input.
142330*
142340 DD-010.
142400     MOVE CA-ACCOUNT        TO ACCTDO   IN NACTREC-DATA.
142500     MOVE CA-SURNAME        TO SNAMEDO  IN NACTREC-DATA.
142600     MOVE CA-FIRSTNAME      TO FNAMEDO  IN NACTREC-DATA.
142700     MOVE CA-MIDDLE-INITIAL TO MIDO     IN NACTREC-DATA.
142800     MOVE CA-TITLE          TO TTLDO    IN NACTREC-DATA.
142900     MOVE CA-TELEPHONE-NO   TO TELDO    IN NACTREC-DATA.
142910*
143000     MOVE CA-ADDRESS-LINE1 TO ADDR1DO IN NACTREC-DATA.
143100     MOVE CA-ADDRESS-LINE2 TO ADDR2DO IN NACTREC-DATA.
143200     MOVE CA-ADDRESS-LINE3 TO ADDR3DO IN NACTREC-DATA.
143210*
143300     MOVE CA-AUTH1D TO AUTH1DO IN NACTREC-DATA.
143400     MOVE CA-AUTH2D TO AUTH2DO IN NACTREC-DATA.
143500     MOVE CA-AUTH3D TO AUTH3DO IN NACTREC-DATA.
143600     MOVE CA-AUTH4D TO AUTH4DO IN NACTREC-DATA.
143610*
143700     MOVE CA-NUMBER-CARDS-ISSUED TO CARDSDO IN NACTREC-DATA.
143710*
143800     MOVE CA-MONTH-ISSUED TO IMODO  IN NACTREC-DATA.
143900     MOVE CA-DAY-ISSUED   TO IDAYDO IN NACTREC-DATA.
144000     MOVE CA-YEAR-ISSUED  TO IYRDO  IN NACTREC-DATA.
144010*
144100     MOVE CA-REASON-CODE TO RSNDO   IN NACTREC-DATA.
144200     MOVE CA-CARD-CODE   TO CCODEDO IN NACTREC-DATA.
144300     MOVE CA-APPROVED    TO APPRDO  IN NACTREC-DATA.
144310*
144400     MOVE CA-SPECIAL-CODE1 TO SCODE1DO IN NACTREC-DATA.
144500     MOVE CA-SPECIAL-CODE2 TO SCODE2DO IN NACTREC-DATA.
144600     MOVE CA-SPECIAL-CODE3 TO SCODE3DO IN NACTREC-DATA.
144710*
144720 END-DD-MOVE-DETAIL-MAP-TO-REC.
144800     EXIT.
144810     EJECT.
144811*
144816 DE-SEND-MENU-WITH-MESSAGE SECTION.
144817*
144818* This routine simply sends a menu screen with a
144819* message to the user and initializes the saved
144820* data for the 'first time' scenario.
144821*
144822 DE-010.
144823     MOVE LOW-VALUES TO ACCTMNUO.
144828*
144829     MOVE MSG-TEXT (MSG-NO) TO MSGMO.
144830*
144831     EXEC CICS SEND
144832               MAP('ACCTMNU')
144833               MAPSET(WS-LITS-MAPSET)
144834               ERASE
144835               FRSET
144836               FREEKB
144838               RESP(RESPONSE)
144839               RESP2(REASON-CODE)
144840     END-EXEC.
144841*
144842     IF  RESPONSE NOT = DFHRESP(NORMAL)
144843         PERFORM Y-UNEXPECTED-ERROR
144844     END-IF.
144845*
144846     MOVE LOW-VALUES  TO DFHCOMMAREA.
144847     SET CA-SENT-MENU TO TRUE.
144848*
144849     PERFORM X-RETURN-COM-AREA.
144850*
144851 END-DE-SEND-MENU-WITH-MESSAGE.
144852     EXIT.
144853     EJECT.
144854*
144855 T-CALL-CRUD SECTION.
144856*
144857* This routine completes the set up of the interface area to
144858* the CRUD program and calls it. It includes error handling
144859* apppropriate to the functions and acceptable exceptional
144860* conditions.
144861*
144862* This section is performed from the following sections -
144863*      BBA-PROCESS-VALID-MENU
144864*      D-GET-DETAIL-DATA
144866*
144867 T-010.
144868     SET WS-CRUD-CORRECT-VERSION TO TRUE.
144869*
144870     EXEC CICS LINK
144871               PROGRAM(CRUD-PROGRAM)
144872               COMMAREA(WS-CRUD-COMMAREA)
144874               RESP(RESPONSE)
144875               RESP2(REASON-CODE)
144876     END-EXEC.
144877*
144878     IF  RESPONSE NOT = DFHRESP(NORMAL)
144879         PERFORM Y-UNEXPECTED-ERROR
144880     END-IF.
144881*
144882     IF  (WS-CRUD-RESP-X NOT NUMERIC)
144883     AND (NOT WS-CRUD-BAD-LOCK)
144884         PERFORM TA-BAD-CRUD-RESPONSE
144885     END-IF.
144886*
144887     IF  NOT WS-CRUD-BAD-LOCK
144888         EVALUATE WS-CRUD-RESP
144889             WHEN DFHRESP(NORMAL)
144890                 CONTINUE
144891             WHEN DFHRESP(NOTFND)
144892                 IF  NOT CA-SENT-MENU
144893                     PERFORM TA-BAD-CRUD-RESPONSE
144894                 END-IF
144895             WHEN OTHER
144896                 PERFORM TA-BAD-CRUD-RESPONSE
144897         END-EVALUATE
144898     END-IF.
144899*
144900 END-T-CALL-CRUD.
144901     EXIT.
144910     EJECT.
144911*
144920 TA-BAD-CRUD-RESPONSE SECTION.
144921*
144922* This routine handles unexpected responses from the CRUD logic.
144923*
144928 TA-010.
144929     MOVE WS-CRUD-RESP-X          TO WS-ERRH-ERROR-X.
144930     MOVE WS-CRUD-REAS-X          TO WS-ERRH-REASON-X.
144931     MOVE WS-CRUD-CICS-FUNCTION-X TO WS-ERRH-CICS-FUNCTION-X.
144932*
144933     PERFORM Z-XCTL-TO-ERROR-HANDLER.
144934*
144935 END-TA-BAD-CRUD-RESPONSE.
144936     EXIT.
144937     EJECT.
144940*
145100 U-SEND-EMPTY-MENU SECTION.
145200*
145300* This routine simply sends an empty menu screen to the user
145400* and initializes the saved data for the 'first time' scenario.
145500*
145520* This section is performed from the following sections -
145521*      NACT01-MAIN
145530*      D-GET-DETAIL-DATA
145540*
145600 U-010.
145700     EXEC CICS SEND
145800               MAP('ACCTMNU')
145900               MAPSET(WS-LITS-MAPSET)
146000               MAPONLY
146100               ERASE
146200               FREEKB
146310               RESP(RESPONSE)
146320               RESP2(REASON-CODE)
146400     END-EXEC.
146500*
146600     IF  RESPONSE NOT = DFHRESP(NORMAL)
146700         PERFORM Y-UNEXPECTED-ERROR
146800     END-IF.
146900*
147000 U-020.
147100     MOVE LOW-VALUES TO DFHCOMMAREA.
147200     SET CA-SENT-MENU TO TRUE.
147300*
147400     PERFORM X-RETURN-COM-AREA.
147500*
147600 END-U-SEND-EMPTY-MENU.
147700     EXIT.
147800     EJECT.
148700*
149340*
149700 V-SET-UP-FOR-BROWSE SECTION.
149710*
149720* This routine completes the interface to the Browse program
149730* and sets up program name to call.
149740*
149741* This section is performed from the following sections -
149742*      BBAA-SEARCH-NAMES
149743*      C-GO-FORWARD
149744*
149750 V-010.
150000     SET WS-BRWS-CORRECT-VERSION TO TRUE.
150100     MOVE 6                      TO WS-BRWS-LIMIT-TO-GET.
150210*
150220 END-V-SET-UP-FOR-BROWSE.
150230     EXIT.
150240     EJECT.
150250*
150800 W-LINK-FOR-BROWSE SECTION.
150810*
150820* This routine calls the Browse program. It includes error
150830* handling and the logic to complete the data areas for
150840* successful matches.
150850*
150851* This section is performed from the following sections -
150852*      BBAA-SEARCH-NAMES
150853*      C-GO-FORWARD
150854*
150860 W-010.
150900     EXEC CICS LINK
151000               PROGRAM(BROWSE-PROGRAM)
151100               COMMAREA(WS-BRWS-COMMAREA)
151210               RESP(RESPONSE)
151220               RESP2(REASON-CODE)
151300     END-EXEC.
151310*
151400     IF  RESPONSE NOT = DFHRESP(NORMAL)
151500     OR  (NOT WS-BRWS-NO-ERROR)
151600         PERFORM Y-UNEXPECTED-ERROR
151700     END-IF.
151701*
151710 W-020.
151800*
151900* If there are no matches then the user must be informed
152000* and the summary title and lines initialized to remove all
152000* previously displayed summary information and the cursor
152100* positioned at the surname.
152200*
152300     IF  WS-BRWS-NONE-FOUND
152400         SET MSG-NOMATCH TO TRUE
152500         MOVE -1         TO SNAMEML
152600         MOVE DFHBMBRY   TO SNAMEMA
152700                            FNAMEMA
152800         MOVE SPACES     TO SUMTTLMO
152900         PERFORM TEST BEFORE
152910           VARYING IX1 FROM 1 BY 1 UNTIL IX1 > 6
153000             MOVE SPACES TO SUMLNMO (IX1)
153100         END-PERFORM
153200*
153300* If there are  matches then the summary lines must be built
153400* and the cursor positioned at the request field.
153500*
153600     ELSE
153700         MOVE DFHBMUNP       TO SNAMEMA
153800                                FNAMEMA
153900         MOVE SUM-TITLE-LINE TO SUMTTLMO
154000         PERFORM TEST BEFORE
154010           VARYING IX1 FROM 1 BY 1 UNTIL IX1 > WS-BRWS-FOUND
154100             MOVE ACCTDO  IN WS-BRWS-ENTRY (IX1)
154110                  TO ACCTDO  IN SUM-LINE
154200             MOVE SNAMEDO IN WS-BRWS-ENTRY (IX1)
154210                  TO SNAMEDO IN SUM-LINE
154300             MOVE FNAMEDO IN WS-BRWS-ENTRY (IX1)
154310                  TO FNAMEDO IN SUM-LINE
154400             MOVE MIDO    IN WS-BRWS-ENTRY (IX1)
154410                  TO MIDO    IN SUM-LINE
154500             MOVE TTLDO   IN WS-BRWS-ENTRY (IX1)
154510                  TO TTLDO   IN SUM-LINE
154600             MOVE ADDR1DO IN WS-BRWS-ENTRY (IX1)
154610                  TO ADDR1DO IN SUM-LINE
154700             MOVE STATDO  IN WS-BRWS-ENTRY (IX1)
154710                  TO STATDO  IN SUM-LINE
154800             MOVE LIMITDO IN WS-BRWS-ENTRY (IX1)
154810                  TO LIMITDO IN SUM-LINE
154900             MOVE SUM-LINE TO SUMLNMO (IX1)
155100         END-PERFORM
155200*
155300* If all possible matches have been displayed, then any
152000* excess summary lines must be initialized to remove
152000* previously displayed summary information.
155500*
155600         IF  WS-BRWS-NO-MORE
155700             IF  IX1 <= 6
155800                 PERFORM TEST BEFORE
155810                   VARYING IX2 FROM IX1 BY 1 UNTIL IX2 > 6
155900                     MOVE SPACES TO SUMLNMO (IX2)
156000                 END-PERFORM
156100             END-IF
156200*
156300* If there are more than can be (or have been till now) displayed,
156400* then the continuation data must be saved for the next pseudo-
156500* conversational task.
156600*
156700         ELSE
156800             SET MSG-MORE      TO TRUE
156900             SET CA-SENT-NAMES TO TRUE
157000             MOVE CA-SURNAME   TO CA-CONT-SURNAME
157100             MOVE CA-FIRSTNAME TO CA-CONT-FIRSTNAME
157200             MOVE WS-BRWS-MORE TO CA-CONT-MORE
157300         END-IF
157400         MOVE LOW-VALUES TO CA-SURNAME
157500                            CA-FIRSTNAME
157600         MOVE -1         TO REQML
157700     END-IF.
157710*
157720 W-030.
157800*
157900* Ensure any message is included in the output.
158000*
158100     IF  NOT MSG-ALL-OK
158200         MOVE MSG-TEXT (MSG-NO) TO MSGMO
158300     END-IF.
158310*
158320 W-040.
158400*
158500* If there were no matches then we leave the input data as it is.
158600*
158700     IF  MSG-NOMATCH
158800         EXEC CICS SEND
158900                   MAP('ACCTMNU')
159000                   MAPSET(WS-LITS-MAPSET)
159100                   DATAONLY
159200                   CURSOR
159300                   FRSET
159400                   FREEKB
159500                   NOHANDLE
159600         END-EXEC
159700*
159800* If there were matches then we erase the input data.
159900*
160000     ELSE
160100         EXEC CICS SEND
160200                   MAP('ACCTMNU')
160300                   MAPSET(WS-LITS-MAPSET)
160400                   DATAONLY
160500                   ERASEAUP
160600                   CURSOR
160700                   FRSET
160800                   FREEKB
160910                   RESP(RESPONSE)
160920                   RESP2(REASON-CODE)
161000         END-EXEC
161100     END-IF.
161110*
161200     IF  RESPONSE NOT = DFHRESP(NORMAL)
161300         PERFORM Y-UNEXPECTED-ERROR
161400     END-IF.
161410*
161420 W-050.
161500     PERFORM X-RETURN-COM-AREA.
161600*
161700 END-W-LINK-FOR-BROWSE.
161800     EXIT.
161900     EJECT.
162000*
179140 X-RETURN-COM-AREA SECTION.
179141*
179142* This routine saves data and ends the task
179143*
179144* This section is performed from the following sections -
179145*      B-GET-MENU-DATA
179146*      DB-SEND-DETAIL-WTH-MESSAGE
179147*      DE-SEND-MENU-WITH-MESSAGE
179148*      U-SEND-EMPTY-MENU
179149*      W-LINK-FOR-BROWSE
179150*
179151 X-010.
179152     EXEC CICS RETURN
179160               TRANSID(EIBTRNID)
179170               COMMAREA(DFHCOMMAREA)
179180     END-EXEC.
179191*
179192 END-X-RETURN-COM-AREA.
179193     EXIT.
179194     EJECT.
179195*
179500 Y-UNEXPECTED-ERROR SECTION.
179510*
179520* This routine handles unexpected conditions from CICS.
179530*
179531* This section is performed from the following sections -
179532*      BA-SEND-CONTROL
179533*      BB-MENU-DATA-ENTERED
179534*      BBA-PROCESS-VALID-MENU
179535*      BC-MENU-ERR-OUT-WTH-CURSOR
179536*      BD-MENU-ERROR-OUT
179537*      BE-FALL-THROUGH
179538*      DA-RECEIVE-DETAIL
179539*      DE-SEND-MENU-WITH-MESSAGE
179540*      T-CALL-CRUD
179541*      U-SEND-EMPTY-MENU
179542*      W-LINK-FOR-BROWSE
179543*
179550 Y-010.
179600     MOVE RESPONSE    TO WS-ERRH-ERROR.
179700     MOVE REASON-CODE TO WS-ERRH-REASON.
179800     MOVE EIBFN       TO WORK-FN-X.
179900     MOVE WORK-FN     TO WS-ERRH-CICS-FUNCTION.
179910*
179920 Y-020.
180000     PERFORM Z-XCTL-TO-ERROR-HANDLER.
180110*
180120 END-Y-UNEXPECTED-ERROR.
180200     EXIT.
180210     EJECT.
180220*
180700 Z-XCTL-TO-ERROR-HANDLER SECTION.
180710*
180720* This routine invokes the error handler for unexpected
180730* responses form the CRUD and Browse programs as well
180740* as unexpected conditions from CICS.
180750*
180751* This section is performed from the following sections -
180752*      TA-BAD-CRUD-RESPONSE
180753*      Y-UNEXPECTED-ERROR
180754*
180760 Z-010.
180800     SET WS-ERRH-CORRECT-VERSION TO TRUE.
180801     MOVE WS-PROGRAM-NAME        TO WS-ERRH-PROGRAM.
180810*
180900     EXEC CICS XCTL
181000               PROGRAM(ABEND-PROGRAM)
181100               COMMAREA(WS-ERRH-ERROR-COMMAREA)
181200               NOHANDLE
181300     END-EXEC.
181301*
181310 Z-020.
181400*
181500* The following will only be executed if the XCTL fails.
181600* The primary reason that might happen is if the error
181700* handling program has become unavailable for some reason.
181800*
181900     EXEC CICS ABEND
182000               ABCODE(WS-LITS-ABEND-ERROR-ABEND)
182100     END-EXEC.
182300*
182400 END-Z-XCTL-TO-ERROR-HANDLER.
182500     EXIT.
182600*
