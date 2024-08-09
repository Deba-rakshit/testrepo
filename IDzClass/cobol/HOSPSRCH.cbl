       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPSRCH.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO UT-S-INFILE.
           SELECT DIAGFILE ASSIGN TO UT-S-DIAGFILE.
           SELECT OUTFILE ASSIGN TO UT-S-OUTFILE.
           SELECT ERRFILE ASSIGN TO UT-S-ERRFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC  PIC X(100).

       FD  DIAGFILE
           RECORD CONTAINS 4 CHARACTERS
           DATA RECORD IS OUT-REC.
       01  DIAG-REC  PIC X(3).

       FD  OUTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OUT-REC.
       01  OUT-REC  PIC X(100).

       FD  ERRFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS ERR-REC.
       01  ERR-REC  PIC X(100).


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
           05  DFCODE                  PIC X(2).
               88 DIAG-READ    VALUE SPACES.
               88 NO-MORE-DIAG  VALUE "10".

       77  INS-COVERAGE-PERC           PIC 9(3) VALUE 10.

       01  Diagnostic-code-table.
           05 codes OCCURS 10 TIMES
                   INDEXED BY D-IDX.
               10 d-code pic 9(3).

       01  Test-SRCH-ALL.
           05 code-value-2 pic x(30)
               value "010623547483746439138329484245".
           05 codes2 redefines code-value-2 OCCURS 10 TIMES
                  ascending key is d-code1 INDEXED BY IDX2.
               10 d-code1 pic 9(1).
               10 Rest-Record Pic 9(2).

       01  Diag-Values-Table.
           05 Table-Values.
               10 filler pic x(55) value
             "245HMO10100623PPO20200547HMO10150483MAN30400746POS15200".
               10 filler pic x(55) value
             "439HMO20100138POS30100329PPO10200010MAN40200010IPA40200".
           05 diags redefines Table-Values OCCURS 10 TIMES
                     indexed by dia.
               10  DG-Code                 Pic x(3).
               10  Ins-Type                PIC X(3).
               10  Cpay                    Pic s9(2).
               10  Ded                     Pic s9(3).

       77  WS-DATE                     PIC 9(6).
       77  MORE-RECORDS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-RECORDS  VALUE 'N'.

       77  MORE-DIAGS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-DIAGS  VALUE 'N'.

      *01  DB2-Fields.
      *    05  DG-Code                 Pic x(3).
      *    05  Ins-Type                PIC X(3).
      *    05  Copay                   Pic s9(4) comp.
      *    05  Deductible              Pic s9(4) comp.

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

      *    exec sql include sqlca end-exec.

       LINKAGE SECTION.
           COPY PATIENT.

       PROCEDURE DIVISION USING WS-INPUT-REC.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 050-LOAD-DIAG-TABLE THRU 050-EXIT
               VARYING D-IDX from 1 BY 1 Until NO-MORE-DIAGS.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT INFILE, DIAGFILE.
           OPEN OUTPUT OUTFILE, ERRFILE.

      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ INFILE INTO WS-INPUT-REC
               AT END
               MOVE 'N' TO MORE-RECORDS-SW
               GO TO 000-EXIT
           END-READ

           READ DIAGFILE
               AT END
               MOVE 'N' TO MORE-DIAGS-SW
               GO TO 050-EXIT
           END-READ

           INITIALIZE  COUNTERS-AND-ACCUMULATORS.
           ADD +1 TO RECORDS-READ.
      *    exec sql connect to SAMPLE end-exec.
       000-EXIT.
           EXIT.

       050-LOAD-DIAG-TABLE.
           DISPLAY "LOAD DIAGNOSTIC CODES".
      *  Code your statement here to OPEN files
           Move DIAG-REC to codes IN Diagnostic-code-table(D-IDX).
      *  Remember to move "NO" to MORE-SW if the input file is AT END
           READ DIAGFILE
               AT END
               MOVE 'N' TO MORE-DIAGS-SW
               GO TO 050-EXIT
           END-READ.
       050-EXIT.
           EXIT.

       100-MAINLINE.
      *  Validate patient type and insurance coverage
           set D-IDX to 1.
           SEARCH CODES
           AT END
               WRITE ERR-REC FROM WS-INPUT-REC
               ADD +1 TO ERROR-RECS
               READ INFILE INTO WS-INPUT-REC
                   AT END MOVE "N" TO MORE-RECORDS-SW
                   GO TO 100-EXIT
               END-READ
               ADD +1 TO RECORDS-READ
               GO TO 100-EXIT
               WHEN Diagnostic-code = CODES(D-IDX)
                 Perform 300-Search-DB2-Table THRU 300-EXIT
      *          if SQLCODE of SQLCA = +0
      *            move COPAY of DB2-Fields to
      *                 COPAY of WS-INPUT-REC
      *            move DEDUCTIBLE of DB2-Fields to
      *                 DEDUCTIBLE of WS-INPUT-REC
      *          end-if
                 COMPUTE PATIENT-TOT-AMT  =
                   PATIENT-TOT-AMT * 1.05
               WRITE OUT-REC FROM WS-INPUT-REC
           END-Search

           SEARCH ALL CODES2
           AT END
               WRITE ERR-REC FROM WS-INPUT-REC
               ADD +1 TO ERROR-RECS
               READ INFILE INTO WS-INPUT-REC
                   AT END MOVE "N" TO MORE-RECORDS-SW
                   GO TO 100-EXIT
               END-READ
               ADD +1 TO RECORDS-READ
               GO TO 100-EXIT
               WHEN d-code1 (IDX2) = diagnostic-code
                 COMPUTE PATIENT-TOT-AMT  =
                   PATIENT-TOT-AMT * 1.05
               WRITE OUT-REC FROM WS-INPUT-REC
           END-Search

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


      *  Code the statement write the final output record
      *  Code the statement to close all files
           CLOSE OUTFILE, ERRFILE, INFILE.
      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "NORMAL END OF JOB".
      *    exec sql disconnect ALL end-exec.
       200-EXIT.
           EXIT.

       300-Search-DB2-Table.
      *    move DIAGNOSTIC-CODE to DG-Code.
      *    move INS-type of WS-INPUT-REC
      *        to INS-TYPE of DB2-Fields.
      *    exec SQL
      *    SELECT   D.Copay, D.Deductible
      *        INTO  :DB2-Fields.Copay, :DB2-fields.Deductible
      *        FROM   DB2Admin.Diag_Codes D
      *        WHERE   D.Diag_Code = :DB2-Fields.DG-Code and
      *                D.Ins_Type = :DB2-Fields.Ins-Type
      *    end-exec.
           Set dia to 1.
           SEARCH diags
               WHEN Diagnostic-code = DG-Code (dia)
                   move CPAY (dia) to
                        COPAY of WS-INPUT-REC
                   move DED (dia) to
                        DEDUCTIBLE of WS-INPUT-REC.
       300-EXIT.
           EXIT.
