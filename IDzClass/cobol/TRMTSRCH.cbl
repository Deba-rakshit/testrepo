       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRMTSRCH.
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

           SELECT TRMTSORT-FILE
           ASSIGN TO UT-S-TRMTSORT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTSRCH-FILE
           ASSIGN TO UT-S-TRMTSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT LABTEST-FILE
           ASSIGN TO UT-S-LABTEST
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
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(120).

      ****** THIS FILE IS PASSED IN FROM THE SORTED TREATMENT DATA
      ****** IT CONSISTS OF ALL SCRUBBED DAILY TREATMENT RECORDS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  TRMTSORT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1101 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-SORT.
       01  INPATIENT-TREATMENT-REC-SORT PIC X(1101).

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

       FD  LABTEST-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FD-LABTEST-REC.
       01  FD-LABTEST-REC                     PIC X(100).

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

      ****** STOP
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

       01  WS-LABTEST-REC.
           05  LAB-TABLE-REC OCCURS 100 TIMES INDEXED BY ROW-IDX.
             10  LAB-TEST-ID         PIC X(08).
             10  TEST-CATEGORY       PIC X(04).
                   88 PULMINARY           VALUE "PULM".
                   88 BLOOD               VALUE "BLOD".
                   88 SPINAL              VALUE "SPNL".
                   88 H1N1                VALUE "H1N1".
                   88 GASTRO              VALUE "GAST".
                   88 LUNG                VALUE "LUNG".
                   88 NUCLEAR-MEDICINE    VALUE "NUCL".
                   88 RENAL               VALUE "RNAL".
                   88 MISCELLANEOUS      VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "PULM", "BLOD", "NUCL",
                      "GAST", "SPNL", "LUNG", "RNAL", "H1N1", "MISC".
      ****** FOR PERFORMANCE, MOVE H1N1 TO THE TOP OF THE LIST
             10  TEST-SHORT-DESC         PIC X(25).
             10  TEST-COST               PIC 9(5)V99.
             10  VENIPUNCTURE-COST       PIC 9(3)V99.
             10  PRESCRIBING-PHYS        PIC X(08).
             10  DIAG-CDE                PIC X(05).
             10  TEST-LONG-DESCRIPTION   PIC X(39).

       COPY PATMSTR.
      ** VSAM FILE

       01  CALC-COSTS-REC.
           05  CALC-TYPE-SW               PIC X.
               88 LAB-TEST VALUE "L".
               88 EQUIPMENT VALUE "E".
           05  PATIENT-ID                 PIC X(8).
           05  LAB-TEST-ID                PIC X(8).
           05  PATIENT-DEDUCTIBLE-REM     PIC 9(4) COMP.
           05  PATIENT-COPAY              PIC 9(3) COMP-3.
           05  REIMBURSE-PCT              PIC 9(3) COMP-3.
           05  PROCEDURE-BASE-COST        PIC 9(7)V99 COMP-3.
           05  ANCILLARY-COSTS            PIC 9(5)V99 COMP-3.
           05  VENIPUNCTURE-COSTS         PIC 9(5)V99 COMP-3.
           05  NET-PATIENT-COSTS          PIC 9(7)V99 COMP-3.
           05  EQUIPMENT-COSTS            PIC 9(7)V99 COMP-3.

       77  INS-COVERAGE-PERC           PIC 9(3) VALUE 10.

       77  WS-DATE                     PIC 9(6).
       77  MORE-TRANSORT-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-TRANSORT-RECS  VALUE 'N'.

       77  MORE-LABTEST-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-LABTESTS  VALUE 'N'.

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(9) COMP.
           05 RECORDS-WRITTEN          PIC S9(9) COMP.
           05 ERROR-RECS               PIC S9(9) COMP.
           05 NBR-INPATIENTS           PIC S9(7) COMP.
           05 NBR-OUTPATIENTS          PIC S9(7) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(02).
           05 HOLD-SUB                 PIC 9(02) COMP.
           05 CALC-CALL-RET-CODE       PIC S9(4) COMP.
           05 WS-LABTEST-CHARGES       PIC 9(9)V99 COMP-3.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.
           05 WS-VENIPUNCTURE-CHARGES  PIC S9(5)V99 COMP-3.
           05 MORE-TABLE-ROWS          PIC X(1).
              88 NO-MORE-LABS   VALUE "N".

       COPY ABENDREC.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 050-LOAD-LABTEST-TABLE THRU 050-EXIT
               VARYING ROW-IDX from 1 BY 1 Until NO-MORE-LABTESTS.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-TRANSORT-RECS
                      or TRAILER-REC.
           PERFORM 900-CLEANUP THRU 900-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *     ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT LABTEST-FILE, TRMTSORT-FILE, PATMSTR.
           OPEN OUTPUT TRMTSRCH-FILE, SYSOUT.

      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTSORT-FILE INTO INPATIENT-TREATMENT-REC
               AT END
               MOVE 'N' TO MORE-TRANSORT-SW
               GO TO 000-EXIT
           END-READ

           READ LABTEST-FILE
               AT END
               MOVE 'N' TO MORE-LABTEST-SW
               GO TO 050-EXIT
           END-READ

           INITIALIZE  COUNTERS-AND-ACCUMULATORS, CALC-COSTS-REC.
           ADD +1 TO RECORDS-READ.
           SET ROW-IDX TO 1.
           MOVE 1 TO ROW-SUB.
       000-EXIT.
           EXIT.

       050-LOAD-LABTEST-TABLE.
           MOVE "050-LOAD-LABTEST-TABLE" TO PARA-NAME.
           MOVE FD-LABTEST-REC TO LAB-TABLE-REC(ROW-IDX)

           READ LABTEST-FILE
               AT END
               MOVE 'N' TO MORE-LABTEST-SW
               GO TO 050-EXIT
           END-READ.

       050-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *  Validate patient type and insurance coverage

           MOVE "Y"  TO MORE-TABLE-ROWS.
           PERFORM 200-SEARCH-RTN THRU 200-EXIT
                  VARYING ROW-SUB FROM 1 BY 1 UNTIL
                  NO-MORE-LABS OR ROW-SUB > 12.

           WRITE INPATIENT-TREATMENT-REC-SRCH
               FROM INPATIENT-TREATMENT-REC.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTSORT-FILE INTO INPATIENT-TREATMENT-REC
               AT END MOVE "N" TO MORE-TRANSORT-SW
               GO TO 100-EXIT
           END-READ
           ADD +1 TO RECORDS-READ, RECORDS-WRITTEN.
       100-EXIT.
           EXIT.

       200-SEARCH-RTN.
           MOVE "200-SEARCH-RTN" TO PARA-NAME.
           SET ROW-IDX to 1.
      *     MOVE 1 TO ROW-SUB.
           IF LAB-TEST-ID IN LAB-CHARGES(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 200-EXIT.

      *  Validate patient type and insurance coverage
           SEARCH LAB-TABLE-REC

           AT END
               MOVE ZEROS TO TEST-COST IN LAB-CHARGES (ROW-SUB),
                             VENIPUNCTURE-COST IN LAB-CHARGES (ROW-SUB)
               MOVE "NO LABTEST-DATA FOUND" TO
                           TEST-SHORT-DESC IN LAB-CHARGES (ROW-SUB)
               MOVE SPACES TO DIAG-CDE IN LAB-CHARGES (ROW-SUB),
                                TEST-CATEGORY IN LAB-CHARGES (ROW-SUB)

           WHEN LAB-TEST-ID IN LAB-CHARGES (ROW-SUB)
                        = LAB-TEST-ID IN LAB-TABLE-REC (ROW-IDX)
               SET HOLD-SUB TO ROW-IDX
               MOVE TEST-SHORT-DESC IN LAB-TABLE-REC(ROW-IDX) TO
                  TEST-SHORT-DESC IN INPATIENT-TREATMENT-REC (HOLD-SUB)
               MOVE TEST-CATEGORY IN LAB-TABLE-REC(ROW-IDX) TO
                  TEST-CATEGORY IN INPATIENT-TREATMENT-REC (HOLD-SUB)
               MOVE TEST-COST IN LAB-TABLE-REC(ROW-IDX) TO
                  PROCEDURE-BASE-COST IN CALC-COSTS-REC
               MOVE VENIPUNCTURE-COST IN LAB-TABLE-REC(ROW-IDX) TO
                  VENIPUNCTURE-COSTS IN CALC-COSTS-REC

               Perform 300-CALC-LAB-COSTS THRU 300-EXIT

           END-Search.

       200-EXIT.
           EXIT.

       300-CALC-LAB-COSTS.
           MOVE "300-CALC-LAB-COSTS" TO PARA-NAME.
           MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
               PATIENT-ID IN PATIENT-MASTER-REC,
               PATIENT-KEY,
               PATIENT-ID IN CALC-COSTS-REC.

           READ PATMSTR INTO PATIENT-MASTER-REC.

           MOVE COPAY  TO PATIENT-COPAY.
           MOVE REMAINING-DEDUCTIBLE TO PATIENT-DEDUCTIBLE-REM.
           IF IN-NETWORK
              MOVE 80 TO REIMBURSE-PCT
           ELSE
              MOVE 70 TO REIMBURSE-PCT.

           MOVE "L" TO  CALC-TYPE-SW.
           MOVE ZERO TO CALC-CALL-RET-CODE.
           CALL 'CLCLBCST' USING CALC-COSTS-REC, CALC-CALL-RET-CODE.

           IF CALC-CALL-RET-CODE NOT EQUAL TO ZERO
               MOVE "** NON-ZERO RETURN-CODE FROM CLCBCST"
                                        TO ABEND-REASON
               GO TO 1000-ABEND-RTN.
      *** NICE BUG = THIS SHOULD BE IN 100-MAINLINE!
           ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.
           ADD VENIPUNCTURE-COSTS IN CALC-COSTS-REC TO
                       WS-VENIPUNCTURE-CHARGES.
           ADD NET-PATIENT-COSTS TO WS-LABTEST-CHARGES,
                          TEST-COST IN LAB-CHARGES(ROW-SUB).

       300-EXIT.
           EXIT.

       700-CLOSE-FILES.
           MOVE "700-CLOSE-FILES" TO PARA-NAME.
      *  Code the statement to close all files
           CLOSE LABTEST-FILE, TRMTSRCH-FILE,
                     SYSOUT, TRMTSORT-FILE, PATMSTR.
       700-EXIT.
           EXIT.

       900-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-TREATMENT-REC-SORT TO WS-TRAILER-REC.
           ADD +1 TO RECORDS-WRITTEN.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                                     TO ABEND-REASON
               MOVE RECORDS-READ     TO ACTUAL-VAL
               MOVE IN-RECORD-COUNT  TO EXPECTED-VAL
               WRITE SYSOUT-REC FROM ABEND-REC.
      *         GO TO 1000-ABEND-RTN.


           MOVE "T" TO RECORD-TYPE.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
           MOVE WS-LABTEST-CHARGES TO IN-LABTEST-CHARGES.
           MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.
           MOVE WS-VENIPUNCTURE-CHARGES TO IN-VENIPUNCTURE-CHARGES.

           WRITE INPATIENT-TREATMENT-REC-SRCH FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.

           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB TRMTSRCH ********".
       900-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB-TRTMTSRCH ***" UPON CONSOLE.
                  DIVIDE ZERO-VAL INTO ONE-VAL.