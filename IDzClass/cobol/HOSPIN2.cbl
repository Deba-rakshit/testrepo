       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPIN.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/99.
       DATE-COMPILED. 01/01/99.
       SECURITY. NON-CONFIDENTIAL.

     *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO UT-S-SYSIN
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS IFCODE.

           SELECT OUTFILE ASSIGN TO UT-S-RPTOUT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS OFCODE.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC  PIC X(100).

       FD  OUTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OUT-Rec.
       01  OUT-REC PIC X(100).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       01  WS-INPUT-REC                PIC X(100).
       01  WS-TEMP.
           05  PATIENT-NBR             PIC 9(5).
           05  PATIENT-NAME.
               10 LAST-NAME            PIC X(10).
               10 FIRST-NAME           PIC X(10).
           05  PATIENT-PHONE           PIC X(10).
           05  PATIENT-TYPE            PIC X(1).
               88 INPATIENT   VALUE "I".
               88 OUTPATIENT  VALUE "0".
               88 VALID-TYPE  VALUES ARE "I", "O".
           05  BED-IDENTITY            PIC 9(4).
           05  DATE-ADMIT              PIC X(10).
           05  AMT-PER-DAY             PIC 9(5)V99.
           05  DIAGNOSTIC-CODE         PIC 999.
           05  INS-TYPE                PIC X(3).
               88 VALID-INS-TYPE VALUES ARE "HMO", "PPO", "POS" "MAN".
           05  HOSPITAL-STAY-LTH       PIC 999.
           05  PATIENT-TOT-AMT         PIC 9(7)V99.
           05  PCP-ID                  PIC X(6).
           05  IN-OUT-NETWORK          PIC X(1).
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                   PIC S9(3).
           05  DEDUCTIBLE              PIC S9(4).

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(5).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-O          PIC X(20).
           05  PATIENT-PHONE-O         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(2).
           05  BED-IDENTITY-O          PIC ZZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  CURR-DATE-O             PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-O              PIC X(4).
           05  HOSPITAL-STAY-LTH-O     PIC 999.
           05  FILLER                  PIC X(7) VALUE SPACES.

       01  WS-TOTALS-REC.
           05  FILLER                  PIC X(8)
                  VALUE "Rec in: ".
           05  READ-OUT                PIC Z(4).
           05  FILLER                  PIC X(13)
                  VALUE " Rec written:".
           05  WRITTEN-OUT             PIC Z(3).
           05  FILLER                  PIC X(8)
                  VALUE " Errors:".
           05  ERRORS-OUT              PIC Z(3).
           05  FILLER                  PIC X(7)
                  VALUE " Inpat:".
           05  INPATIENTS-OUT          PIC Z(3).
           05  FILLER                  PIC X(8)
                  VALUE " Outpat:".
           05  OUTPATIENTS-OUT         PIC Z(3).
           05  FILLER                  PIC X(5)
                  VALUE " HMO:".
           05  HMO-OUT                 PIC Z(4).
           05  FILLER                  PIC X(5)
                  VALUE " S/F:".
           05  STATE-FED-OUT           PIC Z(3).
           05  FILLER                  PIC X(8)
                  VALUE " No Cov:".
           05  NO-COVERAGE-OUT         PIC Z(3).
           05  FILLER                  PIC X(7)
                   VALUE " GROSS:".
           05  TOTAL-GROSS-OUT         PIC $,$$$,$99.99.


       77  WS-DATE                     PIC 9(8).
       77  MORE-RECORDS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-RECORDS  VALUE 'N'.

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(09) COMP.
           05 RECORDS-WRITTEN          PIC S9(4) COMP.
           05 ERROR-RECS               PIC S9(4) COMP.
           05 NBR-INPATIENTS           PIC S9(4) COMP.
           05 NBR-OUTPATIENTS          PIC S9(4) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 TOTAL-ERRORS             PIC S9(7)V99 COMP-3.

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           STOP RUN.

      ******************************************************************
      *  This routine should perform file open and an initial read
      ******************************************************************
       000-HOUSEKEEPING.
           DISPLAY "HOUSEKEEPING PARAGRAPH".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE.

      *  Code your statement here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ INFILE INTO WS-INPUT-REC
               AT END
               MOVE 'N' TO MORE-RECORDS-SW
               GO TO 000-EXIT
           END-READ
           INITIALIZE  COUNTERS-AND-ACCUMULATORS,
                       WS-OUTPUT-REC,
                       WS-TOTALS-REC
           ADD +1 TO RECORDS-READ.
       000-EXIT.
           EXIT.

      ******************************************************************
      *  This routine contains the business logic for the program
      ******************************************************************
       100-MAINLINE.
           WRITE OUT-REC FROM WS-INPUT-REC.
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ INFILE INTO WS-INPUT-REC
               AT END MOVE "N" TO MORE-RECORDS-SW
               GO TO 100-EXIT
           END-READ
           ADD +1 TO RECORDS-READ.
       100-EXIT.
           EXIT.

      ******************************************************************
      *  This routine should perform file close operations
      ******************************************************************
       200-CLEANUP.
           CLOSE OUTFILE, INFILE.
           DISPLAY "NORMAL END OF JOB".
       200-EXIT.
           EXIT.