       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PATSRCH.
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

           SELECT PATSORT-FILE
           ASSIGN TO UT-S-PATSORT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATSRCH-FILE
           ASSIGN TO UT-S-PATSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT EQUIPMENT-FILE
           ASSIGN TO UT-S-EQUIP
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
       FD  PATSORT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-SORT.
       01  INPATIENT-DAILY-REC-SORT PIC X(993).

      ****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS
      ****** THE PROGRAM'S SEARCH ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  PATSRCH-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-SRCH.
       01  INPATIENT-DAILY-REC-SRCH PIC X(993).

       FD  EQUIPMENT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FD-EQUIPMENT-REC.
       01  FD-EQUIPMENT-REC                     PIC X(100).

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
               88 RECORD-FOUND    VALUE "00".
           05  TRTMNT-CODE    PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

      ** QSAM FILE
       COPY PATDALY.

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1).
           05  IN-RECORD-COUNT         PIC 9(9).
           05  FILLER                  PIC X(1).
           05  IN-TOTAL-ROOM-CHARGE    PIC S9(9)V99.
           05  IN-BASE-ROOM-CHARGE     PIC S9(9)V99.
           05  IN-EQUIPMENT-CHARGES    PIC S9(9)V99.

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

       01  WS-EQUIPMENT-REC.
           05  EQUIP-TABLE-REC OCCURS 100 TIMES INDEXED BY ROW-IDX.
             10  EQUIPMENT-ID         PIC X(08).
             10  EQUIP-CATEGORY       PIC X(04).
                   88 HEATING-PAD   VALUE "HEAT".
                   88 AUTOCLAVE     VALUE "AUTO".
                   88 SCOPE         VALUE "SCOP".
                   88 DRIP          VALUE "DRIP".
                   88 MONITOR       VALUE "MON ".
                   88 SHUNT         VALUE "SHNT".
                   88 MISCELLANEOUS VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "HEAT", "AUTO",
                      "SCOP", "DRIP", "MON ", "SHNT", "MISC".
      ****** FOR PERFORMANCE, MOVE H1N1 TO THE TOP OF THE LIST
             10  EQUIP-SHORT-DESC         PIC X(25).
             10  EQUIP-COST               PIC 9(5)V99.
             10  PRESCRIBING-PHYS         PIC X(08).
             10  DIAG-CDE                 PIC X(05).
             10  EQUIP-LONG-DESCRIPTION   PIC X(39).

      ** VSAM FILE
       COPY PATMSTR.

       01  CALC-COSTS-REC.
           05  CALC-TYPE-SW               PIC X.
               88 LAB-TEST VALUE "L".
               88 EQUIPMENT VALUE "E".
           05  PATIENT-ID                 PIC X(8).
           05  EQUIPMENT-ID               PIC X(8).
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
       77  MORE-PATSORT-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-PATSORT-RECS  VALUE 'N'.

       77  MORE-EQUIPMENT-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-EQUIPMENTS  VALUE 'N'.

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(9) COMP.
           05 RECORDS-WRITTEN          PIC S9(9) COMP.
           05 ERROR-RECS               PIC S9(7) COMP.
           05 NBR-INPATIENTS           PIC S9(7) COMP.
           05 NBR-OUTPATIENTS          PIC S9(7) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(02).
           05 HOLD-SUB                 PIC 9(02).
           05 CALC-CALL-RET-CODE       PIC S9(4) COMP.
           05 WS-EQUIPMENT-CHARGES     PIC 9(9)V99 COMP-3.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.
           05 WS-VENIPUNCTURE-CHARGES  PIC S9(5)V99 COMP-3.
           05 MORE-TABLE-ROWS          PIC X(1).
              88 NO-MORE-LABS   VALUE "N".

       COPY ABENDREC.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 050-LOAD-EQUIPMENT-TABLE THRU 050-EXIT
               VARYING ROW-IDX from 1 BY 1 Until NO-MORE-EQUIPMENTS.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-PATSORT-RECS
                      or TRAILER-REC.
           PERFORM 900-CLEANUP THRU 900-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "******** BEGIN JOB DALYEDIT ********".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN INPUT EQUIPMENT-FILE, PATSORT-FILE, PATMSTR.
           OPEN OUTPUT PATSRCH-FILE, SYSOUT.

      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSORT-FILE INTO INPATIENT-DAILY-REC
               AT END
               MOVE 'N' TO MORE-PATSORT-SW
               GO TO 000-EXIT
           END-READ

           READ EQUIPMENT-FILE
               AT END
               MOVE 'N' TO MORE-EQUIPMENT-SW
               GO TO 050-EXIT
           END-READ

           INITIALIZE  COUNTERS-AND-ACCUMULATORS, CALC-COSTS-REC.
           ADD +1 TO RECORDS-READ.

           MOVE 1 TO ROW-SUB.
           SET ROW-IDX TO 1.
       000-EXIT.
           EXIT.

       050-LOAD-EQUIPMENT-TABLE.
           MOVE "050-LOAD-EQUIPMENT-TABLE" TO PARA-NAME.
           MOVE FD-EQUIPMENT-REC TO EQUIP-TABLE-REC(ROW-IDX)

           READ EQUIPMENT-FILE
               AT END
               MOVE 'N' TO MORE-EQUIPMENT-SW
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

      *  Code your statements here to read the input file
           WRITE INPATIENT-DAILY-REC-SRCH
               FROM INPATIENT-DAILY-REC.
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSORT-FILE INTO INPATIENT-DAILY-REC
               AT END MOVE "N" TO MORE-PATSORT-SW
               GO TO 100-EXIT
           END-READ

           ADD +1 TO RECORDS-WRITTEN.
           ADD +1 TO RECORDS-READ.
       100-EXIT.
           EXIT.

       200-SEARCH-RTN.
           MOVE "200-SEARCH-RTN" TO PARA-NAME.
           SET ROW-IDX to 1.
           IF EQUIPMENT-ID IN ADDITIONAL-EQUIP-CHARGES(ROW-SUB) = SPACE
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 200-EXIT.

      *  Validate patient type and insurance coverage
           SEARCH EQUIP-TABLE-REC

           AT END
               MOVE ZEROS TO EQUIPMENT-COST
                         IN ADDITIONAL-EQUIP-CHARGES (ROW-SUB)
               MOVE "NO EQUIPMENT-DATA FOUND" TO
                  EQUIPMENT-SHORT-DESC
                         IN ADDITIONAL-EQUIP-CHARGES (ROW-SUB)
               MOVE HIGH-VALUES TO EQUIPMENT-REASON-CDE
                IN ADDITIONAL-EQUIP-CHARGES(ROW-SUB),
                EQUIPMENT-CATEGORY
                         IN ADDITIONAL-EQUIP-CHARGES (ROW-SUB)

           WHEN EQUIPMENT-ID IN ADDITIONAL-EQUIP-CHARGES (ROW-SUB)
                        = EQUIPMENT-ID IN EQUIP-TABLE-REC (ROW-IDX)

               SET  HOLD-SUB TO ROW-IDX
               MOVE EQUIP-SHORT-DESC IN EQUIP-TABLE-REC(ROW-IDX) TO
                  EQUIPMENT-SHORT-DESC IN INPATIENT-DAILY-REC (HOLD-SUB)
               MOVE EQUIP-CATEGORY     IN EQUIP-TABLE-REC(ROW-IDX) TO
                  EQUIPMENT-CATEGORY IN INPATIENT-DAILY-REC (HOLD-SUB)
               MOVE EQUIP-COST IN EQUIP-TABLE-REC(ROW-IDX) TO
                  EQUIPMENT-COSTS IN CALC-COSTS-REC

               PERFORM 300-CALC-EQUIP-COSTS THRU 300-EXIT

           END-SEARCH.

       200-EXIT.
           EXIT.

       300-CALC-EQUIP-COSTS.
           MOVE "300-CALC-EQUIP-COSTS" TO PARA-NAME.
           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
             PATIENT-KEY,
               PATIENT-ID IN CALC-COSTS-REC.

           READ PATMSTR INTO PATIENT-MASTER-REC.

           MOVE COPAY  TO PATIENT-COPAY.
           MOVE REMAINING-DEDUCTIBLE TO PATIENT-DEDUCTIBLE-REM.
           IF IN-NETWORK
              MOVE 80 TO REIMBURSE-PCT
           ELSE
              MOVE 70 TO REIMBURSE-PCT.

           MOVE "E" TO  CALC-TYPE-SW.
           MOVE ZERO TO CALC-CALL-RET-CODE.
           CALL 'CLCLBCST' USING CALC-COSTS-REC, CALC-CALL-RET-CODE.

           IF CALC-CALL-RET-CODE NOT EQUAL TO ZERO
               MOVE "** NON-ZERO RETURN-CODE FROM CLCBCST"
                                        TO ABEND-REASON
               GO TO 1000-ABEND-RTN.
      *** NICE BUG = THIS SHOULD BE IN 100-MAINLINE!
      *     ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.
           ADD NET-PATIENT-COSTS TO WS-EQUIPMENT-CHARGES
                  EQUIPMENT-COST IN ADDITIONAL-EQUIP-CHARGES(ROW-SUB).
       300-EXIT.
           EXIT.

       700-CLOSE-FILES.
           MOVE "700-CLOSE-FILES" TO PARA-NAME.
      *  Code the statement to close all files
           CLOSE EQUIPMENT-FILE, PATSRCH-FILE,
                 SYSOUT, PATSORT-FILE, PATMSTR.
       700-EXIT.
           EXIT.

       900-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           ADD +1 TO RECORDS-WRITTEN.
           MOVE INPATIENT-DAILY-REC-SORT TO WS-TRAILER-REC.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                    TO ABEND-REASON
               MOVE RECORDS-READ     TO ACTUAL-VAL
               MOVE IN-RECORD-COUNT  TO EXPECTED-VAL
               WRITE SYSOUT-REC FROM ABEND-REC.
      *         GO TO 1000-ABEND-RTN.


           MOVE "T" TO PATIENT-RECORD-TYPE.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
           MOVE WS-EQUIPMENT-CHARGES TO IN-EQUIPMENT-CHARGES.

           WRITE INPATIENT-DAILY-REC-SRCH FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.

           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB PATSRCH ********".
       900-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB-TRTMTSRCH ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.