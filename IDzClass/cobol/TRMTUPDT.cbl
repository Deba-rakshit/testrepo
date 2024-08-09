       IDENTIFICATION DIVISION.
      * My comment
       PROGRAM-ID.  TRMTUPDT.
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
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT TRMTSRCH-FILE
           ASSIGN TO UT-S-TRMTSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       to PATMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATIENT-KEY
                  FILE STATUS  is PATMSTR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(130).

      ****** THIS FILE IS PASSED IN FROM THE SORTED TREATMENT DATA
      ****** IT CONSISTS OF ALL SCRUBBED DAILY TREATMENT RECORDS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
      ****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS
      ****** THE PROGRAM'S SEARCH ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  TRMTSRCH-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 1101 CHARACTERS
           DATA RECORD IS INPATIENT-TREATMENT-REC-SRCH.
       01  INPATIENT-TREATMENT-REC-SRCH PIC X(1101).


       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

      ** QSAM FILE
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
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND         VALUE "00".
               88 PATMSTR-NOT-FOUND    VALUE "23".
           05  TRTMNT-CODE    PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       COPY TREATMNT.

       01  WS-TRAILER-REC.
           05  FILLER                   PIC X(1).
           05  IN-RECORD-COUNT          PIC 9(9).
           05  FILLER                   PIC X(1).
           05  IN-MEDICATION-CHARGES    PIC S9(9)V99.
           05  IN-PHARMACY-CHARGES      PIC S9(7)V99.
           05  IN-ANCILLARY-CHARGES     PIC S9(5)V99.
           05  IN-LABTEST-CHARGES       PIC S9(9)V99.
           05  IN-LAB-ANCILLARY-CHARGES PIC S9(7)V99.
           05  IN-VENIPUNCTURE-CHARGES  PIC S9(7)V99.

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(6).
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


      ** VSAM FILE
       COPY PATMSTR.

       77  INS-COVERAGE-PERC            PIC 9(3) VALUE 10.

       77  WS-DATE                      PIC 9(6).
       01  MORE-TRANSRCH-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-TRANSRCH-RECS  VALUE "N".
           88 MORE-TRANSRCH-RECS     VALUE " ".

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(7) COMP.
           05 RECORDS-WRITTEN          PIC S9(7) COMP.
           05 ERROR-RECS               PIC S9(7) COMP.
           05 NBR-INPATIENTS           PIC S9(7) COMP.
           05 NBR-OUTPATIENTS          PIC S9(7) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(02) VALUE ZERO.
           05 PAT-SUB                  PIC 9(02) VALUE ZERO.
           05 CALC-CALL-RET-CODE       PIC S9(4) COMP.
           05 WS-LABTEST-CHARGES       PIC 9(9)V99 COMP-3.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.
           05 WS-VENIPUNCTURE-CHARGES  PIC S9(5)V99 COMP-3.
           05 HOLD-PHYSICIAN-ID        PIC X(8).
           05 HOLD-PATIENT-ID          PIC X(8).
           05 HOLD-LAB-TEST-ID         PIC X(8).
           05 HOLD-SHORT-DESC-ID       PIC X(30).
           05 HOLD-FIELD               PIC X(255).
           05 HOLD-TALLY               PIC 9(4) COMP.
           05 HOLD-TALLY-END           PIC 9(4) COMP.
           05 CURRENT-NEW-DIAG         PIC 9(4) COMP.
           05 MORE-TABLE-ROWS          PIC X(1).
              88 NO-MORE-LABS   VALUE "N".
           05 CURRENT-ROW-SW          PIC X(1).
              88 CURRENT-ROW    VALUE "Y".
           05  HOLD-DIAGNOSTIC-CODE-PRIMARY      PIC X(05).
           05  HOLD-DIAGNOSTIC-CODE-SECONDARY    PIC X(05).
           05  HOLD-DIAGNOSTIC-CODE-TERTIARY     PIC X(05).

       COPY ABENDREC.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-TRANSRCH-RECS OR TRAILER-REC.
           PERFORM 400-APPLY-UPDATES THRU 400-EXIT.
           PERFORM 900-CLEANUP THRU 900-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT TRMTSRCH-FILE.
           OPEN I-O PATMSTR.
           OPEN OUTPUT SYSOUT.

      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTSRCH-FILE INTO INPATIENT-TREATMENT-REC
               AT END
               MOVE 'N' TO MORE-TRANSRCH-SW
               GO TO 000-EXIT
           END-READ

           INITIALIZE  COUNTERS-AND-ACCUMULATORS, WS-TRAILER-REC.
           ADD +1 TO RECORDS-READ.
           MOVE 1 TO ROW-SUB.
           MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
                   HOLD-PATIENT-ID.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *** PROCESS ONE UNIQUE PATIENT (MULTIPLE TREATMENT RECORDS)

           MOVE "Y"  TO MORE-TABLE-ROWS.
           MOVE ZERO TO WS-LABTEST-CHARGES, WS-VENIPUNCTURE-CHARGES.
           MOVE ZERO TO WS-PHARMACY-CHARGES, WS-ANCILLARY-CHARGES.

           PERFORM 200-CALCULATE-TRMT-CHARGES-RTN THRU 200-EXIT.

      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
       100-EXIT.
           EXIT.

       200-CALCULATE-TRMT-CHARGES-RTN.
      ****** Total up one Treatment rec
           MOVE "200-CALCULATE-TRMT-CHARGES-RTN" TO PARA-NAME.

           ADD MEDICATION-COST   TO WS-MEDICATION-CHARGES.
           ADD PHARMACY-COST     TO WS-PHARMACY-CHARGES.
           ADD ANCILLARY-CHARGE  TO WS-ANCILLARY-CHARGES.


           MOVE "Y" TO MORE-TABLE-ROWS.
           PERFORM 250-PROCESS-LAB-CHARGES-TABLE THRU 250-EXIT
                  VARYING ROW-SUB FROM 1 BY 1 UNTIL
                  NO-MORE-LABS or ROW-SUB = 12.

           READ TRMTSRCH-FILE INTO INPATIENT-TREATMENT-REC
               AT END
               MOVE 'N' TO MORE-TRANSRCH-SW
               GO TO 000-EXIT
           END-READ

           IF MORE-TRANSRCH-RECS
              ADD +1 TO RECORDS-READ
              IF HOLD-PATIENT-ID = PATIENT-ID IN INPATIENT-TREATMENT-REC
                  NEXT SENTENCE
              ELSE
                 PERFORM 400-APPLY-UPDATES THRU 400-EXIT
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
                   HOLD-PATIENT-ID.

       200-EXIT.
           EXIT.

       250-PROCESS-LAB-CHARGES-TABLE.
           MOVE "250-PROCESS-LAB-CHARGES-TABLE" TO PARA-NAME.
           IF LAB-TEST-ID IN LAB-CHARGES(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 250-EXIT.

      *** ADD TO HOLD-WS-VALUES
           ADD TEST-COST(ROW-SUB) TO WS-LABTEST-CHARGES,
                                     IN-LABTEST-CHARGES.
           ADD VENIPUNCTURE-COST IN LAB-CHARGES(ROW-SUB)
                        TO WS-VENIPUNCTURE-CHARGES
                           IN-VENIPUNCTURE-CHARGES.

           MOVE DIAG-CDE(ROW-SUB)  TO HOLD-DIAGNOSTIC-CODE-PRIMARY.

           PERFORM 300-RECONCILE-DIAG-CODES-HOLD THRU 300-EXIT.

       250-EXIT.
           EXIT.

       300-RECONCILE-DIAG-CODES-HOLD.
           MOVE "300-RECONCILE-DIAG-CODES-HOLD" TO PARA-NAME.

           IF DIAG-CDE(ROW-SUB) EQUAL TO HOLD-DIAGNOSTIC-CODE-PRIMARY
            OR HOLD-DIAGNOSTIC-CODE-SECONDARY
            OR HOLD-DIAGNOSTIC-CODE-TERTIARY
            GO TO 300-EXIT.

           IF DIAG-CDE(ROW-SUB) NOT =  HOLD-DIAGNOSTIC-CODE-PRIMARY
              AND HOLD-DIAGNOSTIC-CODE-SECONDARY = SPACES
              MOVE DIAG-CDE(ROW-SUB) TO HOLD-DIAGNOSTIC-CODE-SECONDARY
              GO TO 300-EXIT.

           IF DIAG-CDE(ROW-SUB) NOT = DIAGNOSTIC-CODE-SECONDARY
              AND HOLD-DIAGNOSTIC-CODE-TERTIARY  = SPACES
              MOVE DIAG-CDE(ROW-SUB) TO HOLD-DIAGNOSTIC-CODE-TERTIARY
              GO TO 300-EXIT.

       300-EXIT.
           EXIT.

       400-APPLY-UPDATES.
           MOVE "400-APPLY-UPDATES" TO PARA-NAME.

           MOVE ZERO TO CURRENT-NEW-DIAG.
           MOVE HOLD-PATIENT-ID TO PATIENT-KEY.

           READ PATMSTR INTO PATIENT-MASTER-REC.
           IF PATMSTR-STATUS = "23"
              MOVE INPATIENT-TREATMENT-REC-SRCH TO SYSOUT-REC
              WRITE SYSOUT-REC
              GO TO 400-EXIT.

           ADD WS-ANCILLARY-CHARGES, WS-MEDICATION-CHARGES,
               WS-PHARMACY-CHARGES TO PATIENT-TOT-AMT.

           PERFORM 425-POSITION-PAT-TABLE-IDX THRU 425-EXIT.

           ADD WS-LABTEST-CHARGES, WS-VENIPUNCTURE-CHARGES
                            GIVING TEST-CHARGES (PAT-SUB).

           PERFORM 500-RECONCILE-DIAGNOSTIC-CODES THRU 500-EXIT.

           MOVE HOLD-LAB-TEST-ID          TO LAB-TEST-S-ID(PAT-SUB).
           MOVE HOLD-SHORT-DESC-ID        TO TEST-SHORT-S-DESC(PAT-SUB).
           MOVE WS-DATE                   TO LAB-TEST-DATE(PAT-SUB).

           REWRITE PATMSTR-REC FROM PATIENT-MASTER-REC
             INVALID KEY
                 MOVE "** PROBLEM REWRITING PATMSTR" TO ABEND-REASON
                 GO TO 1000-ABEND-RTN
           END-REWRITE.

       400-EXIT.
           EXIT.

       425-POSITION-PAT-TABLE-IDX.
           MOVE "250-PROCESS-LAB-CHARGES-TABLE" TO PARA-NAME.
           IF LAB-TEST-S-ID(PAT-SUB) = SPACES
              MOVE "Y" TO CURRENT-ROW-SW
              GO TO 425-EXIT.

       425-EXIT.
           EXIT.

       500-RECONCILE-DIAGNOSTIC-CODES.
           MOVE "300-RECONCILE-DIAGNOSTIC-CODES" TO PARA-NAME.

           IF DIAG-CDE(ROW-SUB) EQUAL TO DIAGNOSTIC-CODE-PRIMARY
            OR DIAGNOSTIC-CODE-SECONDARY OR DIAGNOSTIC-CODE-TERTIARY
            GO TO 500-EXIT.

           IF DIAG-CDE(ROW-SUB) NOT EQUAL TO DIAGNOSTIC-CODE-PRIMARY
              AND DIAGNOSTIC-CODE-SECONDARY = SPACES
              MOVE DIAG-CDE(ROW-SUB) TO DIAGNOSTIC-CODE-SECONDARY
              GO TO 500-EXIT.

           IF DIAG-CDE(ROW-SUB) NOT EQUAL TO DIAGNOSTIC-CODE-SECONDARY
              AND DIAGNOSTIC-CODE-TERTIARY  = SPACES
              MOVE DIAG-CDE(ROW-SUB) TO DIAGNOSTIC-CODE-TERTIARY
              GO TO 500-EXIT.

           IF DIAGNOSTIC-CODE-TERTIARY = SPACES
              MOVE DIAG-CDE(ROW-SUB) TO DIAGNOSTIC-CODE-TERTIARY
              GO TO 500-EXIT
           ELSE
              MOVE "NEW PATIENT DIAGNOSTIC CODES:" TO
                PATIENT-ADMIT-COMMENTS.
              EVALUATE CURRENT-NEW-DIAG
              WHEN 0
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(30:5)
              WHEN 1
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(37:5)
              WHEN 2
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(44:5)
               WHEN 3
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(51:5)
              WHEN 4
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(58:5)
              WHEN 5
                MOVE DIAG-CDE(ROW-SUB)
                        TO PATIENT-ADMIT-COMMENTS(65:5)
              END-EVALUATE.

       500-EXIT.
           EXIT.


       700-CLOSE-FILES.
           MOVE "700-CLOSE-FILES" TO PARA-NAME.
      *  Code the statement to close all files
           CLOSE TRMTSRCH-FILE,
                  SYSOUT, PATMSTR.
       700-EXIT.
           EXIT.

       900-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-TREATMENT-REC-SRCH TO WS-TRAILER-REC.
      *     ADD +1 TO RECORDS-READ.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                                     TO ABEND-REASON
               WRITE SYSOUT-REC FROM ABEND-REC.
      *         GO TO 1000-ABEND-RTN.

           MOVE "T" TO RECORD-TYPE.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.

           WRITE SYSOUT-REC FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.

           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB TRMTUPDT ********".
       900-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB-TRTMTSRCH ***" UPON CONSOLE.
           divide  zero-val into one-val.