      ****************************************************************
      * PROGRAM:  ISAMOS1
      *           SAMPLE PROGRAM FOR THE OS/VS COBOL COMPILER
      *
      * READS AN IMS DATA BASE AND WRITES A REPORT
      * PROCESSING IS CONTROLLED BY A TRANSACTION FILE
      *
      * THIS EXAMPLE APPLICATION IS A TEACHING AID.  INTENDED USES ARE:
      *   FOR DEBUG TOOL WORKSHOP:
      *      - FOLLOW PROGRAM LOGIC, SET BREAKPOINTS, VIEW VARIABLES
      *      - INTERCEPT THE ABEND THAT CAN OCCUR IN PROGRAM ISAM2
      *   FOR FAULT ANALYZER WORKSHOP:
      *      - VIEW INFORMATION FOR ABENDS IN PROGRAM ISAM2
      *****************************************************************
      *
      * TRANSACTION FILE RECORD DESCRIPTIONS:
      *     0    1    1    2    2    3    3    4    4    5    5    6    6
      * ....5....0....5....0....5....0....5....0....5....0....5....0....5
      * *        <== AN ASTERISK IN FIRST COLUMN IS A COMMENT
      * ABEND    <== IF CODED BEFORE PRINT, CAUSES ABEND DURING PRINT
      * PRINT    <== PRODUCES A DETAIL REPORT (THERE CAN ONLY BE ONE)
      * TOTALS   <== PRODUCES REPORT TOTALS
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ISAMOS1.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-TRANFILE-STATUS.

            SELECT REPORT-FILE      ASSIGN TO CUSTRPT
               FILE STATUS  IS  WS-REPORT-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0
           RECORDING MODE IS F.
      ***** COPY TRANRCOB.
       01  TRAN-RECORD.
           05  TRAN-CODE              PIC X(6).
           05  FILLER  REDEFINES TRAN-CODE.
               10  TRAN-COMMENT       PIC X.
               10  FILLER             PIC X(5).
           05  FILLER                 PIC X.
           05  TRAN-PARMS             PIC X(73).
           05  CRUNCH-PARMS   REDEFINES TRAN-PARMS.
               10  CRUNCH-IO-LOOPS        PIC 9(5).
               10  FILLER                 PIC X.
               10  CRUNCH-CPU-LOOPS       PIC 9(9).
               10  FILLER                 PIC X(58).

       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
           05  WS-TRANFILE-STATUS      PIC X(2)  VALUE SPACES.
           05  WS-REPORT-STATUS        PIC X(2)  VALUE SPACES.
           05  WS-TRAN-FILE-EOF        PIC X     VALUE SPACES.
           05  WS-TRAN-OK              PIC X     VALUE 'N'.
           05  WS-TRAN-MSG             PIC X(50) VALUE SPACES.

       01  TOTALS-VARS.
           05  NUM-TRANFILE-RECS     PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TRAN-ERRORS       PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TRANSACTIONS      PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-PRINT-REQUESTS    PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-PRINT-COMPLETED   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TOTALS-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TOTALS-COMPLETED  PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-ABEND-REQUESTS    PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-ABEND-COMPLETED   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-DETAIL-LINES      PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-CUST-SEGS         PIC S9(9)   COMP-3  VALUE +0.
           05  TOT-TOTUSE-SEGS       PIC S9(9)   COMP-3  VALUE +0.
           05  TOT-TOTAL-USAGE       PIC S9(9)   COMP-3  VALUE +0.

       01  TOTUSE-STATS.
           05  NUM-TOTUSE-SEGMENTS   PIC S9(9)   COMP-3  VALUE +0.
           05  TOTAL-USAGE           PIC S9(9)   COMP-3  VALUE +0.
           05  TOTAL-USAGE-ALPHA   REDEFINES TOTAL-USAGE
                                     PIC X(5).

       01  SEGA-CUSTOMER-ACCOUNT.
           03  SEGA-ACCOUNT-NUM       PIC X(10).
           03  SEGA-CUSTOMER-PHONE-NB PIC X(10).
           03  SEGA-CUSTOMER-NAME     PIC X(30).
           03  SEGA-CUSTOMER-ADDRESS  PIC X(40).

       01  SEG00010-SSA.
           02  SEG-NAME-00010       PICTURE X(08) VALUE 'CUSTADDR'.
           02  BEGIN-OP-00010       PICTURE X(01) VALUE ' '.
           02  KEY-NAME-00010       PICTURE X(08) VALUE 'PARTKEY '.
           02  REL-OPER-00010       PICTURE X(02) VALUE ' ='.
           02  KEY-VALUE-00010      PICTURE X(17).
           02  END-OP-00010         PICTURE X(01) VALUE ')'.

      *        *******************
      *            REPORT LINES
      *        *******************
       01  RPT-HEADER1.
           05  FILLER                     PIC X(20)
                     VALUE 'TOTAL USAGE REPORT '.
           05  FILLER PIC X(112) VALUE SPACES.
       01  RPT-HEADER2.
           05  FILLER PIC X(10) VALUE 'ACCOUNT   '.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE '# TOTUSE  '.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE 'TOTAL     '.
           05  FILLER PIC X(96) VALUE SPACES.
       01  RPT-HEADER3.
           05  FILLER PIC X(10) VALUE 'NUMBER    '.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE 'SEGMENTS  '.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE 'USAGE     '.
           05  FILLER PIC X(96) VALUE SPACES.
       01  RPT-HEADER4.
           05  FILLER PIC X(10) VALUE ALL '-'.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE ALL '-'.
           05  FILLER PIC X(3)  VALUE '   '.
           05  FILLER PIC X(10) VALUE ALL '-'.
           05  FILLER PIC X(96) VALUE SPACES.
       01  RPT-DETAIL.
           05  RPT-ACCT-NUM           PIC X(10).
           05  FILLER                 PIC X(3)  VALUE '   '.
           05  RPT-NUM-SEGMENTS       PIC ZZZZZZZZZ9.
           05  FILLER                 PIC X(3)  VALUE '   '.
           05  RPT-TOTAL-USAGE        PIC ZZ,ZZZ,ZZ9.
           05  FILLER PIC X(96) VALUE SPACES.
       01  RPT-TRAN-DETAIL.
           05  RPT-TRAN-MSG1      PIC X(31)
                        VALUE ' TRANSACTION:                 '.
           05  RPT-TRAN-RECORD            PIC X(80)  VALUE SPACES.
           05  FILLER                     PIC X(21)  VALUE SPACES.
       01  ERR-MSG-BAD-TRAN.
           05  FILLER PIC X(31)
                        VALUE '    TRANSACTION ERROR:        '.
           05  ERR-MSG-DATA1              PIC X(35)  VALUE SPACES.
           05  ERR-MSG-DATA2              PIC X(66)  VALUE SPACES.
       01  RPT-TOTALS-HDR1.
           05  FILLER PIC X(36)
                    VALUE ALL 'TOTALS -----------------------------'.
           05  FILLER PIC X(96) VALUE SPACES.
       01  RPT-ABEND-TRAN.
           05  FILLER PIC X(30) VALUE ' ABEND REQUESTED.            '.
           05  FILLER PIC X(102)  VALUE SPACES.
       01  RPT-SPACES.
           05  FILLER              PIC X(132)   VALUE SPACES.

       77  DL1-FUNCTION             PICTURE X(04).

       01  WS-STATUS-CODES.
              03 STATUS-START       PICTURE  X(01).
              03 STATUS-END         PICTURE  X(01).

       LINKAGE SECTION.
       01  PCB-AREA-1.
           02  DBD-NAME             PICTURE  X(08).
           02  SEGMENT-LEVEL        PICTURE  X(02).
           02  STATUS-CODES         PICTURE  X(02).
           02  PROCESS-OPTIONS      PICTURE  X(04).
           02  FILLER               PICTURE  S9(05)  COMPUTATIONAL.
           02  SEG-NAME-FEEDBACK    PICTURE  X(08).
           02  KEY-LENGTH           PICTURE  S9(05)  COMPUTATIONAL.
           02  NUMBER-SEGS          PICTURE  S9(05)  COMPUTATIONAL.
           02  KEY-FEEDBACK-AREA.
               03  KFB-CUSTOMER-NUMBER PIC    X(10).
               03  KFB-USE-YYYYMM      PIC    X(06).

       PROCEDURE DIVISION.
           ENTRY 'DLITCBL' USING PCB-AREA-1.
       000-MAIN.
           DISPLAY 'START ISAMOS1'.

           PERFORM 900-OPEN-TRAN-AND-RPT-FILES.
           PERFORM 800-INIT-REPORT .

           PERFORM 100-PROCESS-TRANSACTIONS THROUGH 100-EXIT
                   UNTIL WS-TRAN-FILE-EOF = 'Y' .

           PERFORM 905-CLOSE-TRAN-AND-RPT-FILES.
           GOBACK.

       100-PROCESS-TRANSACTIONS.
           PERFORM 700-READ-TRAN-FILE.
           IF WS-TRAN-FILE-EOF = 'Y'
               GO TO 100-EXIT.
           IF TRAN-COMMENT = '*'
               GO TO 100-EXIT.
           WRITE REPORT-RECORD FROM RPT-SPACES AFTER 1.
           MOVE TRAN-RECORD TO RPT-TRAN-RECORD.
           WRITE REPORT-RECORD FROM RPT-TRAN-DETAIL.
           MOVE 'Y' TO WS-TRAN-OK.
           IF TRAN-CODE = 'PRINT '
               PERFORM 200-PROCESS-PRINT-TRAN
           ELSE IF TRAN-CODE = 'TOTALS'
               PERFORM 300-PROCESS-TOTALS-TRAN
           ELSE IF TRAN-CODE = 'ABEND '
               PERFORM 400-PROCESS-ABEND-TRAN
           ELSE
               MOVE 'INVALID TRAN CODE:' TO ERR-MSG-DATA1
               MOVE TRAN-CODE TO ERR-MSG-DATA2
               PERFORM 820-REPORT-BAD-TRAN
               ADD +1 TO NUM-TRANSACTIONS.
       100-EXIT.
           EXIT.

       200-PROCESS-PRINT-TRAN.
           ADD +1 TO NUM-PRINT-REQUESTS.
           ADD +1 TO NUM-TRANSACTIONS.
           WRITE REPORT-RECORD FROM RPT-SPACES  AFTER 1.
           WRITE REPORT-RECORD FROM RPT-HEADER2.
           WRITE REPORT-RECORD FROM RPT-HEADER3.
           WRITE REPORT-RECORD FROM RPT-HEADER4.

           MOVE 'GN'  TO DL1-FUNCTION.
           MOVE '  '  TO WS-STATUS-CODES.
           MOVE 0     TO NUM-CUST-SEGS.
           PERFORM 210-PROCESS-ACCOUNTS UNTIL WS-STATUS-CODES = 'GE'.
           ADD +1 TO NUM-PRINT-COMPLETED.

       210-PROCESS-ACCOUNTS.
           CALL  'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,
                                 SEGA-CUSTOMER-ACCOUNT, SEG00010-SSA.
           MOVE STATUS-CODES TO WS-STATUS-CODES.
           IF NUM-ABEND-REQUESTS > 0
               MOVE '@#*%&' TO TOTAL-USAGE-ALPHA
           ELSE
               MOVE 0  TO TOTAL-USAGE.
           IF WS-STATUS-CODES = SPACES
              MOVE SEGA-CUSTOMER-ACCOUNT TO RPT-ACCT-NUM
              ADD  1  TO NUM-CUST-SEGS
              MOVE 0  TO NUM-TOTUSE-SEGMENTS
              CALL 'ISAMOS2' USING PCB-AREA-1,
                                 SEGA-ACCOUNT-NUM, TOTUSE-STATS
              MOVE NUM-TOTUSE-SEGMENTS TO RPT-NUM-SEGMENTS
              MOVE TOTAL-USAGE         TO RPT-TOTAL-USAGE
              WRITE REPORT-RECORD FROM RPT-DETAIL AFTER 1
              ADD  NUM-TOTUSE-SEGMENTS TO TOT-TOTUSE-SEGS
              ADD  TOTAL-USAGE         TO TOT-TOTAL-USAGE
              ADD +1 TO NUM-DETAIL-LINES
           ELSE
              IF (WS-STATUS-CODES = 'GB' OR
                  WS-STATUS-CODES = 'GE')
                 MOVE 'GE' TO WS-STATUS-CODES
              ELSE
                 DISPLAY 'INVALID STATUS FROM GN = ' WS-STATUS-CODES
                 MOVE 'GE' TO WS-STATUS-CODES.

       300-PROCESS-TOTALS-TRAN.
           ADD +1 TO NUM-TOTALS-REQUESTS .
           ADD +1 TO NUM-TRANSACTIONS.
           WRITE REPORT-RECORD FROM RPT-SPACES      AFTER 1.
           WRITE REPORT-RECORD FROM RPT-TOTALS-HDR1.
           IF NUM-PRINT-COMPLETED > 0
               MOVE SPACES              TO RPT-DETAIL
               MOVE TOT-TOTUSE-SEGS     TO RPT-NUM-SEGMENTS
               MOVE TOT-TOTAL-USAGE     TO RPT-TOTAL-USAGE
               WRITE REPORT-RECORD FROM RPT-DETAIL AFTER 1
               ADD +1 TO NUM-TOTALS-COMPLETED
           ELSE
               MOVE 'CANNOT COMPLETE TOTALS TRAN.' TO ERR-MSG-DATA1
               MOVE 'A PRINT TRAN MUST BE REQUESTED/PROCESSED FIRST.'
                   TO ERR-MSG-DATA2
               PERFORM 820-REPORT-BAD-TRAN.

       400-PROCESS-ABEND-TRAN.
           ADD +1 TO NUM-ABEND-REQUESTS .
           WRITE REPORT-RECORD FROM RPT-ABEND-TRAN  AFTER 2.
           WRITE REPORT-RECORD FROM RPT-SPACES.
           IF NUM-PRINT-COMPLETED > 0
               MOVE 'CANNOT PROCESS ABEND TRAN' TO ERR-MSG-DATA1
               MOVE 'AN ABEND TRAN MUST COME BEFORE A PRINT TRAN'
                   TO ERR-MSG-DATA2
               PERFORM 820-REPORT-BAD-TRAN.

       700-READ-TRAN-FILE.
           READ TRANSACTION-FILE
             AT END MOVE 'Y' TO WS-TRAN-FILE-EOF .
           IF WS-TRANFILE-STATUS = '00'
               COMPUTE NUM-TRANFILE-RECS = NUM-TRANFILE-RECS + 1
           ELSE IF WS-TRANFILE-STATUS = '10'
               MOVE 'Y' TO WS-TRAN-FILE-EOF
           ELSE
               MOVE 'ERROR ON TRAN FILE READ.  CODE:'
                              TO ERR-MSG-DATA1
               MOVE WS-TRANFILE-STATUS TO ERR-MSG-DATA2
               PERFORM 820-REPORT-BAD-TRAN
               MOVE 'Y' TO WS-TRAN-FILE-EOF.

       800-INIT-REPORT.
           WRITE REPORT-RECORD FROM RPT-HEADER1 AFTER PAGE.

       820-REPORT-BAD-TRAN.
           ADD +1 TO NUM-TRAN-ERRORS.
           MOVE 'N' TO WS-TRAN-OK.
           WRITE REPORT-RECORD FROM ERR-MSG-BAD-TRAN.
           WRITE REPORT-RECORD FROM RPT-SPACES.

       900-OPEN-TRAN-AND-RPT-FILES.
           OPEN INPUT    TRANSACTION-FILE
                OUTPUT   REPORT-FILE .
           IF WS-TRANFILE-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING TRAN FILE. RC:' WS-TRANFILE-STATUS
             DISPLAY 'ENDING PROGRAM DUE TO FILE ERROR'
             MOVE 16 TO RETURN-CODE
             MOVE 'Y' TO WS-TRAN-FILE-EOF.
           IF WS-REPORT-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING REPORT FILE. RC:' WS-REPORT-STATUS
             DISPLAY 'ENDING PROGRAM DUE TO FILE ERROR'
             MOVE 16 TO RETURN-CODE
             MOVE 'Y' TO WS-TRAN-FILE-EOF.

       905-CLOSE-TRAN-AND-RPT-FILES.
           CLOSE TRANSACTION-FILE .
           CLOSE REPORT-FILE .

      * END OF PROGRAM ISAMOS1