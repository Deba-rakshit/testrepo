000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    CNTRLBRK.
000300 AUTHOR.        IBM
000400 DATE-WRITTEN.  01-17-02.
000500
000600******************************************************************
000700*  PROGRAM USED TO CREATE CONTROL BREAK REPORT: CNTRLBRK SELECTS *
000800*  NON-PAID INVOICES FROM THE INVOICE FILE, PERFORMS INPUT DATA  *
000900*  VALIDATION, SORTS THE RECORDS IN CUSTOMER NAME AND INVOICE    *
001000*  NUMBER ORDER, THEN CREATES A NEW SORTED OUTPUT FILE OF NON-   *
001100*  PAID INVOICES AND A PRINTED OUTPUT REPORT.  ALL ERROR CON-    *
001200*  CONDITIONS ARE DISPLAYED TO SYSOUT.                           *
001300******************************************************************
001400
001500 ENVIRONMENT DIVISION.
001600
001700 CONFIGURATION SECTION.
001800 SOURCE-COMPUTER.   IBM-390.
001900 OBJECT-COMPUTER.   IBM-390.
002000
002100 INPUT-OUTPUT SECTION.
002200 FILE-CONTROL.
002300
002400     SELECT MI-INPUT-FILE ASSIGN TO UT-S-ACT2DATA.
002500     SELECT SW-MI-SORT-FILE ASSIGN TO UT-S-SORTFILE.
002600     SELECT MI-OUTPUT-SRT-FILE ASSIGN TO UT-S-SORT2FIL.
002700     SELECT MATB-OUTPUT-RPT ASSIGN TO UT-S-REPORT.
002800
002900
003000 DATA DIVISION.
003100
003200 FILE SECTION.
003300
003400 FD  MI-INPUT-FILE
003500     RECORDING MODE IS F
003600     LABEL RECORDS ARE STANDARD
003700     RECORD CONTAINS 80 CHARACTERS
003800     BLOCK CONTAINS 0 RECORDS
003900     DATA RECORD IS MI-INPUT-RECORD.
004000
004100 01  MI-INPUT-RECORD                  PIC X(80).
004200
004300 FD  MI-OUTPUT-SRT-FILE
004400     RECORDING MODE IS F
004500     LABEL RECORDS ARE STANDARD
004600     RECORD CONTAINS 80 CHARACTERS
004700     BLOCK CONTAINS 0 RECORDS
004800     DATA RECORD IS MI-OUTPUT-SRT-RECORD.
004900
005000 01  MI-OUTPUT-SRT-RECORD             PIC X(80).
005100
005200 FD  MATB-OUTPUT-RPT
005300     RECORDING MODE IS F
005400     LABEL RECORDS ARE STANDARD
005500     RECORD CONTAINS 133 CHARACTERS
005600     BLOCK CONTAINS 0 RECORDS
005700     DATA RECORD IS MATB-OUTPUT-REP-LINE.
005800
005900 01  MATB-OUTPUT-REP-LINE             PIC X(133).
006000
006100
006200 SD  SW-MI-SORT-FILE
006300     RECORD CONTAINS 80 CHARACTERS
006400     DATA RECORD IS SW-MI-SORT-WORK.
006500
006600 01  SW-MI-SORT-WORK.
006700     05  CUST-NO-SRT-WK               PIC 99.
006800     05  CUST-NAME-SRT-WK             PIC X(15).
006900     05  INV-NO-SRT-WK                PIC X(5).
007000     05  AGED-CODE-SRT-WK             PIC 9.
007100     05  INV-AMT-SRT-WK               PIC S9(7)V99.
007200     05  PAID-DATE-SRT-WK             PIC X(6).
007300     05  FILLER                       PIC X(42).
007400
007500 WORKING-STORAGE SECTION.
007600
       01  PROGRAM-INDICATOR-SWITCHES.
           05  WS-EOF-INPUT-SW              PIC X(3)       VALUE 'NO '.
               88  EOF-INPUT                               VALUE 'YES'.
           05  WS-EOF-SRT-OUTPUT-SW         PIC X(3)       VALUE 'NO '.
               88  EOF-SRT-OUTPUT                          VALUE 'YES'.
           05  WS-INPUT-OK-SW               PIC X(3)       VALUE 'NO '.
               88  INPUT-OK                                VALUE 'YES'.
           05  WS-NAME-FOUND-SW             PIC X(3)       VALUE SPACES.
               88  NAME-FOUND                              VALUE 'YES'.
               88  NAME-NOT-FOUND                          VALUE 'NO '.
           05  WS-PAID-SW                   PIC X(3)       VALUE SPACES.
               88  UNPAID                                  VALUE 'NO '.
               88  PAID                                    VALUE 'YES'.
009100
009200 01  WS-REPORT-CONTROLS.
009300     05  WS-PAGE-COUNT                PIC S9(3)     VALUE ZERO.
009400     05  WS-LINES-PER-PAGE            PIC S9(2)     VALUE +50.
009500     05  WS-LINES-USED                PIC S9(2)     VALUE +51.
009600     05  WS-LINE-SPACING              PIC S9(1)     VALUE ZERO.
009700
009800 01  WS-BREAK-CONTROLS.
009900     05  WS-PREVIOUS-CUST-NO          PIC 9(2).
010000
010100     copy cobtable.
012000
012100 01  WS-ACCUMULATORS.
012200*  FOR PROGRAM RECORD TRACKING
012300     05  WS-READ-CTR       PIC 9(4)       VALUE ZERO.
012400     05  WS-REL-CTR        PIC 9(4)       VALUE ZERO.
012500     05  WS-RETR-CTR       PIC 9(4)       VALUE ZERO.
012600     05  WS-WRTN-CTR       PIC 9(4)       VALUE ZERO.
012700     05  WS-PAID-CTR       PIC 9(4)       VALUE ZERO.
012800     05  WS-UNPD-CTR       PIC 9(4)       VALUE ZERO.
012900
013000
013100*  FOR CUSTOMER TOTALS
013200     05  WS-CURRENT-TL     PIC 9(7)V99    VALUE ZERO.
013300     05  WS-OVER-30-TL     PIC 9(7)V99    VALUE ZERO.
013400     05  WS-OVER-60-TL     PIC 9(7)V99    VALUE ZERO.
013500     05  WS-OVER-90-TL     PIC 9(7)V99    VALUE ZERO.
013600     05  WS-CUST-TOTAL-TL  PIC 9(7)V99    VALUE ZERO.
013700
013800*  FOR REPORT GRAND TOTALS
013900     05  WS-CURRENT-GTL    PIC 9(7)V99    VALUE ZERO.
014000     05  WS-OVER-30-GTL    PIC 9(7)V99    VALUE ZERO.
014100     05  WS-OVER-60-GTL    PIC 9(7)V99    VALUE ZERO.
014200     05  WS-OVER-90-GTL    PIC 9(7)V99    VALUE ZERO.
014300
014400
014500* PROGRAM FILES DEFINED:
014600
014700
014800 01  MI-INPUT-REC.
014900     05  CUST-NO-IN                   PIC 99.
015000     05  INV-NO-IN                    PIC X(5).
015100     05  INV-NO-IN-NUM   REDEFINES   INV-NO-IN         PIC 9(5).
015200     05  AGED-CODE-IN                 PIC 9.
015300     05  INV-AMT-IN                   PIC S9(7)V99.
015400     05  PAID-DATE-IN                 PIC X(6).
015500     05  PAID-DATE-IN-NUM   REDEFINES   PAID-DATE-IN   PIC 9(6).
015600     05  FILLER                       PIC X(57).
015700
015800 01  MI-OUTPUT-SRT-REC.
015900     05  CUST-NO-SRT                  PIC 99.
016000     05  CUST-NAME-SRT                PIC X(15).
016100     05  INV-NO-SRT                   PIC X(5).
016200     05  AGED-CODE-SRT                PIC 9.
016300     05  INV-AMT-SRT                  PIC S9(7)V99.
016400     05  PAID-DATE-SRT                PIC X(6).
016500     05  FILLER                       PIC X(42).
016600
016700
016800* PROGRAM REPORT LINES.
016900
017000 01  HL-HEADER-1.
017100     05  FILLER            PIC X(1)   VALUE SPACES.
017200     05  FILLER            PIC X(15)  VALUE 'REPORT NO 12345'.
017300     05  FILLER            PIC X(34)  VALUE SPACES.
017400     05  FILLER            PIC X(25)
017500                  VALUE 'AGED TRIAL BALANCE AS OF '.
017600     05  RPT-DATE          PIC X(8).
017700     05  FILLER            PIC X(29)  VALUE SPACES.
017800     05  FILLER            PIC X(5)   VALUE 'PAGE '.
017900     05  RPT-PAGE-NO       PIC ZZZ.
018000     05  FILLER            PIC X(12)  VALUE SPACES.
018100
018200 01  HL-HEADER-2.
018300     05  FILLER            PIC X(3)   VALUE SPACES.
018400     05  FILLER            PIC X(10)  VALUE 'CUST NO   '.
018500     05  FILLER            PIC X(9)   VALUE 'CUST NAME'.
018600     05  FILLER            PIC X(8)   VALUE SPACES.
018700     05  FILLER            PIC X(7)   VALUE 'INVOICE'.
018800     05  FILLER            PIC X(8)   VALUE SPACES.
018900     05  FILLER            PIC X(7)   VALUE 'CURRENT'.
019000     05  FILLER            PIC X(7)   VALUE SPACES.
019100     05  FILLER            PIC X(7)   VALUE 'OVER 30'.
019200     05  FILLER            PIC X(7)   VALUE SPACES.
019300     05  FILLER            PIC X(7)   VALUE 'OVER 60'.
019400     05  FILLER            PIC X(7)   VALUE SPACES.
019500     05  FILLER            PIC X(7)   VALUE 'OVER 90'.
019600     05  FILLER            PIC X(9)   VALUE SPACES.
019700     05  FILLER            PIC X(5)   VALUE 'TOTAL'.
019800     05  FILLER            PIC X(25)  VALUE SPACES.
019900
020000
020100 01  DL-DETAIL.
020200     05  FILLER            PIC X(5)   VALUE SPACES.
020300     05  CUST-NO-DL        PIC X(2).
020400     05  FILLER            PIC X(6)   VALUE SPACES.
020500     05  CUST-NAME-DL      PIC X(15).
020600     05  FILLER            PIC X(3)   VALUE SPACES.
020700     05  CUST-INV-DL       PIC X(5).
020800     05  FILLER            PIC X(4)   VALUE SPACES.
020900     05  CURRENT-DL        PIC Z,ZZZ,ZZ9.99-.
021000     05  FILLER            PIC X(1)   VALUE SPACES.
021100     05  OVER-30-DL        PIC Z,ZZZ,ZZ9.99-.
021200     05  FILLER            PIC X(1)   VALUE SPACES.
021300     05  OVER-60-DL        PIC Z,ZZZ,ZZ9.99-.
021400     05  FILLER            PIC X(1)   VALUE SPACES.
021500     05  OVER-90-DL        PIC Z,ZZZ,ZZ9.99-.
021600     05  FILLER            PIC X(38)  VALUE SPACES.
021700
021800
021900 01  TL-CUST-TOTAL.
022000     05  FILLER            PIC X(5)   VALUE SPACES.
022100     05  FILLER            PIC X(19)
022200                  VALUE 'TOTALS FOR CUST NO '.
022300     05  CUST-NO-TL        PIC X(2).
022400     05  FILLER            PIC X(14)  VALUE SPACES.
022500     05  CURRENT-TL        PIC Z,ZZZ,ZZ9.99-.
022600     05  FILLER            PIC X(1)   VALUE SPACES.
022700     05  OVER-30-TL        PIC Z,ZZZ,ZZ9.99-.
022800     05  FILLER            PIC X(1)   VALUE SPACES.
022900     05  OVER-60-TL        PIC Z,ZZZ,ZZ9.99-.
023000     05  FILLER            PIC X(1)   VALUE SPACES.
023100     05  OVER-90-TL        PIC Z,ZZZ,ZZ9.99-.
023200     05  FILLER            PIC X(1)   VALUE SPACES.
023300     05  CUST-TOTAL-TL     PIC Z,ZZZ,ZZ9.99-.
023400     05  FILLER            PIC X(24)  VALUE SPACES.
023500
023600
023700 01  GTL-REPORT-TOTALS.
023800     05  FILLER            PIC X(5)   VALUE SPACES.
023900     05  FILLER            PIC X(21)
024000                  VALUE 'GRAND TOTALS         '.
024100     05  FILLER            PIC X(14)  VALUE SPACES.
024200     05  CURRENT-GTL       PIC Z,ZZZ,ZZ9.99-.
024300     05  FILLER            PIC X(1)   VALUE SPACES.
024400     05  OVER-30-GTL       PIC Z,ZZZ,ZZ9.99-.
024500     05  FILLER            PIC X(1)   VALUE SPACES.
024600     05  OVER-60-GTL       PIC Z,ZZZ,ZZ9.99-.
024700     05  FILLER            PIC X(1)   VALUE SPACES.
024800     05  OVER-90-GTL       PIC Z,ZZZ,ZZ9.99-.
024900     05  FILLER            PIC X(38)   VALUE SPACES.
025000
025100
025200 01  ERROR-MESSAGE-EL     PIC X(133).
025300
025400 01  ERROR-RECORD-EL.
025500     05  CUST-NO-EL       PIC 99.
025600     05  FILLER           PIC X(4)   VALUE SPACES.
025700     05  INV-NO-EL        PIC 9(5).
025800     05  FILLER           PIC X(4)   VALUE SPACES.
025900     05  AGED-CODE-EL     PIC 9.
026000     05  FILLER           PIC X(4)   VALUE SPACES.
026100     05  INV-AMT-EL       PIC Z,ZZZ,ZZ9.99-.
026200     05  FILLER           PIC X(4)   VALUE SPACES.
026300     05  PAID-DATE-EL     PIC 9(6).
026400     05  FILLER           PIC X(57).
026500
026600 01  DISPLAY-LINE.
026700     05  DISP-MESSAGE     PIC X(45).
026800     05  DISP-VALUE       PIC ZZZ9.
026900
027000
027100 PROCEDURE DIVISION.
027200
027300 000-MAINLINE SECTION.
027400
027500     OPEN INPUT  MI-INPUT-FILE
027600          OUTPUT MI-OUTPUT-SRT-FILE
027700          OUTPUT MATB-OUTPUT-RPT.
027800     SORT SW-MI-SORT-FILE
027900          ON ASCENDING KEY CUST-NAME-SRT-WK
028000                           INV-NO-SRT-WK
028100          INPUT  PROCEDURE 200-SRT-INPUT-PROCD THRU 200-EXIT
028200          OUTPUT PROCEDURE 300-SRT-OUTPUT-PROCD THRU 300-EXIT.
028300     PERFORM 550-DISPLAY-PROG-DIAG THRU 550-DISPLAY-PROG-DIAG-EXIT
028400     CLOSE MI-INPUT-FILE
028500           MI-OUTPUT-SRT-FILE
028600           MATB-OUTPUT-RPT.
028700     MOVE ZERO TO RETURN-CODE.
028800     GOBACK.
028900
029000
029100 200-SRT-INPUT-PROCD SECTION.
029200
029300     MOVE 'NO ' TO WS-EOF-INPUT-SW.
029400     PERFORM 800-READ-INPUT-FILE THRU 800-READ-INPUT-FILE-EXIT.
029500     IF EOF-INPUT
029600        DISPLAY 'ERROR!!  INPUT FILE EMPTY!'
029700        GO TO 200-EXIT.
029800     PERFORM 210-PRSS-INPUT-RECORDS THRU
029900                         210-PRSS-INPUT-RECORDS-EXIT
030000         UNTIL EOF-INPUT.
030100
030200 200-EXIT.
030300     EXIT.
030400
030500
030600 210-PRSS-INPUT-RECORDS.
030700
030800     PERFORM 214-CK-INPUT-DATA THRU 214-CK-INPUT-DATA-EXIT.
030900     IF INPUT-OK AND UNPAID
031000        PERFORM 218-SEARCH-FOR-CUST-NAME THRU
031100                     218-SEARCH-FOR-CUST-NAME-EXIT
031200        IF NAME-FOUND
031300           PERFORM 850-FORMAT-RELEASE THRU 850-FORMAT-RELEASE-EXIT
031400        ELSE
031500           NEXT SENTENCE
031600     ELSE
031700        NEXT SENTENCE.
031800     PERFORM 800-READ-INPUT-FILE THRU 800-READ-INPUT-FILE-EXIT.
031900
032000
032100 210-PRSS-INPUT-RECORDS-EXIT.
032200     EXIT.
032300
032400
032500 214-CK-INPUT-DATA.
032600
032700     MOVE 'YES' TO WS-INPUT-OK-SW.
032800     IF PAID-DATE-IN-NUM IS NUMERIC
032900
033000        IF PAID-DATE-IN-NUM IS ZERO
033100           ADD 1 TO WS-UNPD-CTR
033200           MOVE 'NO ' TO WS-PAID-SW
033300
033400           IF AGED-CODE-IN IS > ZERO AND < 5
033500
033600              IF CUST-NO-IN IS NUMERIC
033700
033800                 IF INV-NO-IN-NUM IS NUMERIC
033900
034000                    IF INV-AMT-IN IS NUMERIC
034100                       MOVE 'YES' TO WS-INPUT-OK-SW
034200                    ELSE
034300                        MOVE
034400                        '** ERROR **  INVOICE AMOUNT NOT NUMERIC '
034500                               TO ERROR-MESSAGE-EL
034600                        PERFORM 700-ERROR-DISPLAY THRU
034700                                           700-ERROR-DISPLAY-EXIT
034800
034900                 ELSE
035000                     MOVE
035100                     '** ERROR **  INVOICE NUMBER NOT NUMERIC '
035200                            TO ERROR-MESSAGE-EL
035300                     PERFORM 700-ERROR-DISPLAY THRU
035400                                        700-ERROR-DISPLAY-EXIT
035500
035600              ELSE
035700                  MOVE
035800                  '** ERROR **  CUSTOMER NUMBER NOT NUMERIC '
035900                         TO ERROR-MESSAGE-EL
036000                  PERFORM 700-ERROR-DISPLAY THRU
036100                                      700-ERROR-DISPLAY-EXIT
036200
036300           ELSE
036400               MOVE
036500               '** ERROR **  INVALID AGED CODE '
036600                      TO ERROR-MESSAGE-EL
036700               PERFORM 700-ERROR-DISPLAY THRU
036800                                   700-ERROR-DISPLAY-EXIT
036900
037000        ELSE
037100            ADD 1 TO WS-PAID-CTR
037200            MOVE 'YES' TO WS-PAID-SW
037300
037400     ELSE
037500         MOVE
037600         '** ERROR **  PAID DATE NOT NUMERIC'
037700                TO ERROR-MESSAGE-EL
037800         PERFORM 700-ERROR-DISPLAY THRU
037900                             700-ERROR-DISPLAY-EXIT.
038000
038100
038200
038300 214-CK-INPUT-DATA-EXIT.
038400     EXIT.
038500
038600
038700
038800 218-SEARCH-FOR-CUST-NAME.
038900
039000     MOVE SPACES TO WS-NAME-FOUND-SW.
039100     SET CUST-INDEX TO 1.
039200     SEARCH CUSTOMER-REC
039300         AT END
039400             MOVE 'NO ' TO WS-NAME-FOUND-SW
039500             MOVE
039600             '** ERROR **  CUSTOMER NAME NOT FOUND'
039700                TO ERROR-MESSAGE-EL
039800             PERFORM 700-ERROR-DISPLAY THRU
039900                                 700-ERROR-DISPLAY-EXIT
040000         WHEN
040100             CUSTOMER-CODE (CUST-INDEX) IS EQUAL TO CUST-NO-IN
040200                MOVE 'YES' TO WS-NAME-FOUND-SW
040300                MOVE CUSTOMER-NAME (CUST-INDEX) TO
040400                           CUST-NAME-SRT-WK.
040500
040600
040700 218-SEARCH-FOR-CUST-NAME-EXIT.
040800     EXIT.
040900
041000
041100
041200 300-SRT-OUTPUT-PROCD.
041300
041400     PERFORM 320-INITIALIZE-OUTPUT THRU
041500                  320-INITIALIZE-OUTPUT-EXIT.
041600     PERFORM 900-RETURN-SRTD-REC THRU 900-RETURN-SRTD-REC-EXIT.
041700     IF EOF-SRT-OUTPUT
041800        DISPLAY 'SORTED RECORDS NOT AVAILIBLE'
041900        GO TO 300-EXIT.
042000     MOVE CUST-NO-SRT-WK TO WS-PREVIOUS-CUST-NO.
042100     MOVE  CUST-NAME-SRT-WK TO CUST-NAME-DL.
042200     PERFORM 340-PRSS-SORTED-OUTPUT THRU
042300                                    340-PRSS-SORTED-OUTPUT-EXIT
042400        UNTIL  EOF-SRT-OUTPUT.
042500     PERFORM 400-PRSS-CUST-BREAK THRU 400-PRSS-CUST-BREAK-EXIT.
042600     PERFORM 500-PRSS-GRAND-TOTALS THRU
042700                                    500-PRSS-GRAND-TOTALS-EXIT.
042800
042900
043000 300-EXIT.
043100     EXIT.
043200
043300
043400
043500 320-INITIALIZE-OUTPUT.
043600
043700     MOVE ZEROS TO WS-PAGE-COUNT,
043800                   WS-LINE-SPACING,
043900                   WS-CURRENT-TL,
044000                   WS-OVER-30-TL,
044100                   WS-OVER-60-TL,
044200                   WS-OVER-90-TL,
044300                   WS-CUST-TOTAL-TL,
044400                   WS-CURRENT-GTL,
044500                   WS-OVER-30-GTL,
044600                   WS-OVER-60-GTL,
044700                   WS-OVER-90-GTL.
044800      MOVE 'NO ' TO WS-EOF-SRT-OUTPUT-SW.
044900      MOVE SPACES TO DL-DETAIL.
045000      MOVE '01-01-92' TO RPT-DATE.
045100
045200
045300 320-INITIALIZE-OUTPUT-EXIT.
045400     EXIT.
045500
045600
045700 340-PRSS-SORTED-OUTPUT.
045800
045900     IF CUST-NO-SRT-WK IS NOT EQUAL TO WS-PREVIOUS-CUST-NO
046000        PERFORM 400-PRSS-CUST-BREAK THRU
046100                                    400-PRSS-CUST-BREAK-EXIT.
046200     PERFORM 343-DETAIL-LINE-PRSS THRU
046300                                    343-DETAIL-LINE-PRSS-EXIT.
046400     PERFORM 950-WRITE-OUTPUT-REC THRU
046500                                    950-WRITE-OUTPUT-REC-EXIT.
046600     PERFORM 900-RETURN-SRTD-REC THRU
046700                                    900-RETURN-SRTD-REC-EXIT.
046800
046900
047000
047100 340-PRSS-SORTED-OUTPUT-EXIT.
047200     EXIT.
047300
047400
047500 343-DETAIL-LINE-PRSS.
047600
047700     IF WS-LINES-USED IS GREATER THAN WS-LINES-PER-PAGE OR
047800        WS-LINES-USED IS EQUAL TO WS-LINES-PER-PAGE
047900           PERFORM 955-HEADINGS THRU 955-HEADINGS-EXIT.
048000     MOVE  CUST-NO-SRT-WK  TO  CUST-NO-DL.
048100     MOVE  INV-NO-SRT-WK  TO  CUST-INV-DL.
048200     IF AGED-CODE-SRT-WK IS EQUAL TO 1
048300        MOVE  INV-AMT-SRT-WK TO CURRENT-DL
048400        ADD INV-AMT-SRT-WK TO WS-CURRENT-TL
048500     ELSE
048600        IF AGED-CODE-SRT-WK IS EQUAL TO 2
048700           MOVE  INV-AMT-SRT-WK TO OVER-30-DL
048800           ADD INV-AMT-SRT-WK TO WS-OVER-30-TL
048900        ELSE
049000           IF AGED-CODE-SRT-WK IS EQUAL TO 3
049100              MOVE  INV-AMT-SRT-WK TO OVER-60-DL
049200              ADD INV-AMT-SRT-WK TO WS-OVER-60-TL
049300           ELSE
049400              MOVE  INV-AMT-SRT-WK TO OVER-90-DL
049500              ADD INV-AMT-SRT-WK TO WS-OVER-90-TL.
049600
049700     WRITE MATB-OUTPUT-REP-LINE FROM DL-DETAIL
049800         AFTER ADVANCING WS-LINE-SPACING.
049900     ADD WS-LINE-SPACING TO WS-LINES-USED.
050000     MOVE 1 TO WS-LINE-SPACING.
050100     MOVE  SPACES TO DL-DETAIL.
050200
050300
050400 343-DETAIL-LINE-PRSS-EXIT.
050500     EXIT.
050600
050700
050800 400-PRSS-CUST-BREAK.
050900
051000     COMPUTE WS-CUST-TOTAL-TL = WS-CURRENT-TL +
051100                                WS-OVER-30-TL +
051200                                WS-OVER-60-TL +
051300                                WS-OVER-90-TL .
051400     MOVE 2 TO WS-LINE-SPACING.
051500     MOVE WS-PREVIOUS-CUST-NO TO CUST-NO-TL.
051600     MOVE WS-CURRENT-TL TO CURRENT-TL.
051700     MOVE WS-OVER-30-TL TO OVER-30-TL.
051800     MOVE WS-OVER-60-TL TO OVER-60-TL.
051900     MOVE WS-OVER-90-TL TO OVER-90-TL.
052000     MOVE WS-CUST-TOTAL-TL TO CUST-TOTAL-TL.
052100     WRITE MATB-OUTPUT-REP-LINE FROM TL-CUST-TOTAL
052200         AFTER ADVANCING WS-LINE-SPACING.
052300     ADD WS-LINE-SPACING TO WS-LINES-USED.
052400     ADD WS-CURRENT-TL TO WS-CURRENT-GTL.
052500     ADD WS-OVER-30-TL TO WS-OVER-30-GTL.
052600     ADD WS-OVER-60-TL TO WS-OVER-60-GTL.
052700     ADD WS-OVER-90-TL TO WS-OVER-90-GTL.
052800     IF NOT EOF-SRT-OUTPUT
052900        MOVE ZEROS TO WS-CURRENT-TL,
053000                      WS-OVER-30-TL,
053100                      WS-OVER-60-TL,
053200                      WS-OVER-90-TL,
053300                      WS-CUST-TOTAL-TL
053400        MOVE CUST-NO-SRT-WK TO WS-PREVIOUS-CUST-NO
053500        MOVE CUST-NAME-SRT-WK TO CUST-NAME-DL
053600        IF WS-LINES-USED IS GREATER THAN WS-LINES-PER-PAGE OR
053700           WS-LINES-USED IS EQUAL TO WS-LINES-PER-PAGE
053800           PERFORM 955-HEADINGS THRU 955-HEADINGS-EXIT
053900        ELSE
054000           MOVE 2 TO WS-LINE-SPACING.
054100
054200
054300 400-PRSS-CUST-BREAK-EXIT.
054400     EXIT.
054500
054600
054700 500-PRSS-GRAND-TOTALS.
054800
054900     MOVE 2 TO WS-LINE-SPACING.
055000     MOVE  WS-CURRENT-GTL TO CURRENT-GTL.
055100     MOVE  WS-OVER-30-GTL TO OVER-30-GTL.
055200     MOVE  WS-OVER-60-GTL TO OVER-60-GTL.
055300     MOVE  WS-OVER-90-GTL TO OVER-90-GTL.
055400     WRITE MATB-OUTPUT-REP-LINE FROM GTL-REPORT-TOTALS
055500         AFTER ADVANCING WS-LINE-SPACING.
055600
055700
055800 500-PRSS-GRAND-TOTALS-EXIT.
055900     EXIT.
056000
056100
056200
056300
056400 550-DISPLAY-PROG-DIAG.
056500
056600     DISPLAY '****     B999BLK2 RUNNING    ****'.
056700     DISPLAY '                                                 '.
056800     MOVE 'MONTHLY INVOICE RECORDS READ                 '  TO
056900          DISP-MESSAGE.
057000     MOVE WS-READ-CTR TO DISP-VALUE.
057100     DISPLAY DISPLAY-LINE.
057200     DISPLAY '                                                 '.
057300     MOVE 'MONTHLY INVOICE RECORDS RELEASED TO SORT     '  TO
057400          DISP-MESSAGE.
057500     MOVE WS-REL-CTR TO DISP-VALUE.
057600     DISPLAY DISPLAY-LINE.
057700     MOVE 'MONTHLY INVOICE RECORDS RETURNED FROM SORT   '  TO
057800          DISP-MESSAGE.
057900     MOVE WS-RETR-CTR TO DISP-VALUE.
058000     DISPLAY DISPLAY-LINE.
058100     DISPLAY '                                                 '.
058200     MOVE 'MONTHLY INVOICE RECORDS WRITTEN TO MRA       '  TO
058300          DISP-MESSAGE.
058400     MOVE WS-WRTN-CTR TO DISP-VALUE.
058500     DISPLAY DISPLAY-LINE.
058600     DISPLAY '                                                 '.
058700     MOVE 'MONTHLY INVOICE PAID                         '  TO
058800          DISP-MESSAGE.
058900     MOVE WS-PAID-CTR TO DISP-VALUE.
059000     DISPLAY DISPLAY-LINE.
059100     MOVE 'MONTHLY INVOICE UNPAID                       '  TO
059200          DISP-MESSAGE.
059300     MOVE WS-UNPD-CTR TO DISP-VALUE.
059400     DISPLAY DISPLAY-LINE.
059500     DISPLAY '                                                 '.
059600     DISPLAY '****     B999BLK2 EOJ        ****'.
059700
059800
059900
060000 550-DISPLAY-PROG-DIAG-EXIT.
060100     EXIT.
060200
060300
060400 700-ERROR-DISPLAY.
060500
060600     MOVE 'NO ' TO WS-INPUT-OK-SW.
060700     DISPLAY ERROR-MESSAGE-EL.
060800     MOVE CUST-NO-IN TO CUST-NO-EL.
060900     MOVE INV-NO-IN-NUM TO INV-NO-EL.
061000     MOVE AGED-CODE-IN TO AGED-CODE-EL.
061100     MOVE INV-AMT-IN TO INV-AMT-EL.
061200     MOVE PAID-DATE-IN-NUM TO PAID-DATE-EL.
061300     DISPLAY ERROR-RECORD-EL.
061400
061500 700-ERROR-DISPLAY-EXIT.
061600     EXIT.
061700
061800
061900 800-READ-INPUT-FILE.
062000
062100     READ MI-INPUT-FILE INTO MI-INPUT-REC
062200         AT END  MOVE 'YES' TO WS-EOF-INPUT-SW,
062300                 GO TO 800-READ-INPUT-FILE-EXIT.
062400     ADD 1 TO WS-READ-CTR.
062500
062600 800-READ-INPUT-FILE-EXIT.
062700     EXIT.
062800
062900
063000 850-FORMAT-RELEASE.
063100
063200     MOVE CUST-NO-IN TO CUST-NO-SRT-WK.
063300     MOVE INV-NO-IN TO INV-NO-SRT-WK.
063400     MOVE AGED-CODE-IN TO AGED-CODE-SRT-WK.
063500     MOVE INV-AMT-IN TO INV-AMT-SRT-WK.
063600     MOVE PAID-DATE-IN TO  PAID-DATE-SRT-WK.
063700     RELEASE SW-MI-SORT-WORK.
063800     ADD 1 TO WS-REL-CTR.
063900
064000
064100 850-FORMAT-RELEASE-EXIT.
064200     EXIT.
064300
064400
064500
064600
064700 900-RETURN-SRTD-REC.
064800
064900     RETURN SW-MI-SORT-FILE
065000         AT END  MOVE 'YES' TO WS-EOF-SRT-OUTPUT-SW,
065100                 GO TO 900-RETURN-SRTD-REC-EXIT.
065200     ADD 1 TO WS-RETR-CTR.
065300
065400 900-RETURN-SRTD-REC-EXIT.
065500     EXIT.
065600
065700
065800 950-WRITE-OUTPUT-REC.
065900
066000     WRITE MI-OUTPUT-SRT-RECORD FROM SW-MI-SORT-WORK.
066100     ADD 1 TO WS-WRTN-CTR.
066200
066300 950-WRITE-OUTPUT-REC-EXIT.
066400     EXIT.
066500
066600
066700 955-HEADINGS.
066800
066900     ADD 1 TO WS-PAGE-COUNT.
067000     MOVE WS-PAGE-COUNT TO RPT-PAGE-NO.
067100     WRITE MATB-OUTPUT-REP-LINE FROM HL-HEADER-1
067200         AFTER ADVANCING PAGE.
067300     MOVE 1 TO WS-LINES-USED.
067400     MOVE 2 TO WS-LINE-SPACING.
067500     WRITE MATB-OUTPUT-REP-LINE FROM HL-HEADER-2
067600         AFTER ADVANCING WS-LINE-SPACING.
067700     ADD WS-LINE-SPACING TO WS-LINES-USED.
067800
067900
068000 955-HEADINGS-EXIT.
068100     EXIT.