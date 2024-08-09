       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPCALC.
       AUTHOR. JON SAYLES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO UT-S-INFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS IFCODE.

           SELECT RPTFILE ASSIGN TO UT-S-RPTOUT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS RFCODE.

           SELECT OUTFILE ASSIGN TO UT-S-OUTFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS OFCODE.

      *IDz 3. *** Oooops ERROROUT or ERROUT  Which?
           SELECT ERRFILE ASSIGN TO UT-S-ERROROUT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS EFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC  PIC X(100).

       FD  OUTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OUT-Rec.
       01  OUT-REC  PIC X(100).

       FD  ERRFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS ERR-Rec.
       01  ERR-REC  PIC X(100).

       FD  RPTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS RPT-Rec.
       01  RPT-REC PIC X(100).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  RFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       77  INS-COVERAGE-PERC           PIC 9(3) VALUE 10.

       01  Test-SRCH-ALL.
           05 code-value pic x(30)
               value "010623547483746439138329484245".
           05 codes redefines code-value OCCURS 10 TIMES
                    INDEXED BY IDX.
               10 d-code1 pic 9(1).
               10 Rest-Record Pic 9(2).
       77  table-max                   pic s9(3) comp value +100.
       77  Diag-code-ws                pic s9(4) comp-3 value +0.


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
           05  FILLER                  PIC X(6)
                   VALUE " NET:".
           05  TOTAL-NET-OUT           PIC $,$$$,$99.99.

       77  WS-DATE                     PIC 9(6).
       77  MORE-RECORDS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-RECORDS  VALUE 'N'.

      *IDz 1. *** Oooops - initialize this field: value +0
       77  NBR-MANAGED-CARE       PIC S9(4) COMP-3.

      *IDz 2. *** Oooops ... ... ... ...  should be value +100.
       77  Divisor                PIC S9(3) comp-3 value +00.

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(4) COMP.
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

         01  WS-INPUT-REC.
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
               88 Managed-Care value "MAN".
           05  HOSPITAL-STAY-LTH       PIC 999.
           05  PATIENT-TOT-AMT         PIC 9(7)V99.
           05  PCP-ID                  PIC X(6).
           05  IN-OUT-NETWORK          PIC X(1).
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                   PIC S9(3).
           05  DEDUCTIBLE              PIC S9(4).


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           Call 'HOSPCALL' USING WS-INPUT-REC.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE, RPTFILE, ERRFILE.

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

       100-MAINLINE.
      *  Validate patient type and insurance coverage
           DISPLAY "MAINLINE".
           DISPLAY WS-INPUT-REC.
           If VALID-TYPE AND VALID-INS-TYPE
                WRITE OUT-REC FROM WS-INPUT-REC
              ELSE
               MOVE WS-INPUT-REC TO ERR-REC
               WRITE ERR-REC
               ADD +1 TO ERROR-RECS
               READ INFILE INTO WS-INPUT-REC
                   AT END MOVE "N" TO MORE-RECORDS-SW
                   GO TO 100-EXIT
               END-READ
               ADD +1 TO RECORDS-READ
               GO TO 100-EXIT
           END-IF

           EVALUATE INS-TYPE
               WHEN "HMO" ADD +1 TO NBR-HMO
               WHEN "GOV" ADD +1 TO NBR-STATE-FED
               WHEN "MAN" ADD +1 TO NBR-MANAGED-CARE
               WHEN OTHER ADD +1 TO NBR-NO-COVERAGE
           END-EVALUATE

           IF INPATIENT
               ADD +1 TO NBR-INPATIENTS
           ELSE
               ADD +1 TO NBR-OUTPATIENTS
           END-IF

           IF Managed-Care
               PERFORM 300-Table-rtn  thru 300-Exit
               COMPUTE PAT-TOTAL-AMT-NET =
                (PATIENT-TOT-AMT  +
                    AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / Divisor)
                    - Deductible + COPAY )
               END-COMPUTE
           Else
               COMPUTE PAT-TOTAL-AMT-NET =
                (PATIENT-TOT-AMT  +
                    AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / Divisor))
               END-COMPUTE
           END-IF

           ADD PAT-TOTAL-AMT-NET   TO TOTAL-AMT-NET.
           ADD PATIENT-TOT-AMT     TO TOTAL-AMT-GROSS.

      *  Move input data to output data
           MOVE PATIENT-NBR        TO PATIENT-NBR-O.
           MOVE PATIENT-NAME       TO PATIENT-NAME-O.
           MOVE PATIENT-PHONE      TO PATIENT-PHONE-O.
           MOVE PATIENT-TYPE       TO PATIENT-TYPE-O.
           MOVE WS-DATE            TO CURR-DATE-O.
           MOVE BED-IDENTITY       TO BED-IDENTITY-O.
           ADD  PAT-TOTAL-AMT-NET  TO PATIENT-TOT-AMT
                                   GIVING PATIENT-AMT-PER-DAY-O.
           MOVE INS-COVERAGE-PERC  TO INS-COVERAGE-PERC-O.
           MOVE INS-TYPE           TO INS-TYPE-O.
           ADD  +1                 TO HOSPITAL-STAY-LTH
                                   GIVING  HOSPITAL-STAY-LTH-O.

      *      move in-rec TO out-rec.
           WRITE RPT-REC FROM WS-OUTPUT-REC.
           ADD +1 TO RECORDS-WRITTEN.
      *
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ INFILE INTO WS-INPUT-REC
               AT END MOVE "N" TO MORE-RECORDS-SW
               GO TO 100-EXIT
           END-READ
           ADD +1 TO RECORDS-READ.
       100-EXIT.
           EXIT.

       200-CLEANUP.
      *  Move the final computational fields
           DISPLAY "CLEAN-UP".
           ADD +1 TO RECORDS-WRITTEN.
           MOVE RECORDS-READ            TO READ-OUT .
           MOVE RECORDS-WRITTEN         TO WRITTEN-OUT.
           MOVE ERROR-RECS              TO ERRORS-OUT.
           MOVE NBR-INPATIENTS          TO INPATIENTS-OUT.
           MOVE NBR-OUTPATIENTS         TO OUTPATIENTS-OUT.
           MOVE NBR-HMO                 TO HMO-OUT.
           MOVE NBR-STATE-FED           TO STATE-FED-OUT.
           MOVE NBR-NO-COVERAGE         TO NO-COVERAGE-OUT.
           MOVE TOTAL-AMT-GROSS         TO TOTAL-GROSS-OUT.
           MOVE TOTAL-AMT-NET           TO TOTAL-NET-OUT.

      *  Code the statement write the final output record
           WRITE RPT-REC FROM WS-TOTALS-REC.
      *  Code the statement to close all files
           CLOSE OUTFILE, RPTFILE, ERRFILE, INFILE.
      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "NORMAL END OF JOB".
       200-EXIT.
           EXIT.

       300-Table-rtn.
      *IDz 4. *** Oops - Initialize IDX to 1.
           SET IDX to table-max.
           add d-code1 (IDX) to Diag-code-ws.
       300-Exit.
           EXIT.