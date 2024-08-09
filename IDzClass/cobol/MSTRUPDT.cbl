       IDENTIFICATION DIVISION.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       PROGRAM-ID.  MSTRUPDT.
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

           SELECT PATINS
                  ASSIGN       to PATINS
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATIENT-INS-KEY
                  FILE STATUS  is PATINS-STATUS.

           SELECT PATERR
           ASSIGN TO UT-S-PATERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       to PATMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is SEQUENTIAL
                  RECORD KEY   is PATIENT-KEY
                  FILE STATUS  is PATMSTR-STATUS.

           SELECT PRSNMSTR
                  ASSIGN       to PRSNMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PRSN-KEY
                  FILE STATUS  is PRSN-STATUS.

           SELECT PATRPT
           ASSIGN TO UT-S-PATRPT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

       DATA DIVISION.
       FILE SECTION.

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

        FD PATRPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC.
       01  RPT-REC  PIC X(133).

       FD  PATERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-ERR.
       01  INPATIENT-DAILY-REC-ERR.
           05  ERR-MSG-PAT                  PIC X(40).
           05  REST-OF-PAT-REC              PIC X(993).

       FD  PATINS
           DATA RECORD IS PATINS-REC.
       01  PATINS-REC.
           05 PATIENT-INS-KEY      PIC X(06).
           05 FILLER               PIC X(696).

       FD  PRSNMSTR
           DATA RECORD IS PRSNMSTR-REC.
       01  PRSNMSTR-REC.
           05 PRSN-KEY      PIC X(06).
           05 FILLER           PIC X(794).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 PATMSTR-FOUND    VALUE "00".
               88 END-OF-PATMSTR   VALUE "10".
           05  PRSN-STATUS          PIC X(2).
               88 PRSN-FOUND    VALUE "00".
           05  PATINS-STATUS          PIC X(2).
               88 PATINS-FOUND    VALUE "00".
               88 PATINS-OPEN     VALUE "00".
           05  OFCODE                  PIC X(2).
              88 CODE-WRITE    VALUE SPACES.

       01  MISC-FIELDS.
           05 TEMP-COST                   PIC S9(9)V99 COMP-3.
           05 PARA-NAME                   PIC X(40).
           05 TEMP-AMOUNT-TOTAL           PIC S9(9)V99 COMP-3.
           05 PLAN-FOUND-SW               PIC X(1) VALUE "N".
              88 PLAN-FOUND VALUE "Y".
           05 PROVIDER-FOUND-SW           PIC X(1) VALUE "N".
              88 PROVIDER-FOUND VALUE "Y".
           05 ERROR-FOUND-SW              PIC X(1) VALUE " ".
              88 ERROR-FOUND   VALUE "Y".
           05 CALC-CALL-RET-CODE          PIC S9(4) VALUE 0.
           05 ABEND-REASON                PIC X(50).
           05 WS-ANCILLARY-CHARGES        PIC S9(9)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES        PIC S9(9)V99 COMP-3.
           05 WS-LAB-CHARGES              PIC S9(9)V99 COMP-3.
           05 WS-EQUIP-CHARGES            PIC S9(9)V99 COMP-3.
           05 ROW-SUB                     PIC 9(3) VALUE 0.
           05 PRIMARY-PHYS-NETWORK        PIC X(1) VALUE "N".
              88 PRIMARY-PHYS-IN-NETWORK VALUE "Y".
           05 LAB-PHYS-SW                 PIC X(1) VALUE "N".
              88 LAB-IN-NETWORK VALUE "Y".
           05 PHYS-FOUND-SW               PIC X(1) VALUE "N".
              88 PHYSICIAN-FOUND VALUE "Y".
           05 EQUIP-PHYS-SW               PIC X(1) VALUE "N".
              88 EQUIP-IN-NETWORK VALUE "Y".
           05 RPT-KTR                     PIC 9(4) VALUE 0.

       01  CALC-COSTS-REC.
           05  CALC-TYPE-SW               PIC X.
               88 LAB-TEST VALUE "L".
               88 EQUIPMENT VALUE "E".
           05  PATIENT-ID                 PIC X(8)  VALUE SPACES.
           05  LAB-TEST-ID                PIC X(8)  VALUE SPACES.
           05  PATIENT-DEDUCTIBLE-REM     PIC 9(4) COMP  VALUE 0.
           05  PATIENT-COPAY              PIC 9(3) COMP-3   VALUE 0.
           05  REIMBURSE-PCT              PIC 9(3) COMP-3   VALUE 0.
           05  PROCEDURE-BASE-COST    PIC 9(7)V99 COMP-3   VALUE 0.
           05  ANCILLARY-COSTS        PIC 9(5)V99 COMP-3   VALUE 0.
           05  VENIPUNCTURE-COSTS     PIC 9(5)V99 COMP-3  VALUE 0.
           05  NET-PATIENT-COSTS      PIC 9(7)V99 COMP-3 VALUE 0.
           05  VENIPUNCTURE-COSTS     PIC 9(7)V99 COMP-3   VALUE 0.
           05  PHYS-ID-TEMP           PIC X(8) VALUE SPACES.
           05  STATE-FACTOR           PIC 999 VALUE 0.

        01  WS-BLANK-LINE.
           05  FILLER                  PIC X(134) VALUE " ".
           05  FILLER                  PIC X(134) VALUE " ".

        01  WS-PATIENT-RPT-REC.
           05  FILLER                  PIC X(12) VALUE "PATIENT ID:".
           05  PATIENT-ID-O            PIC 9(6).
           05  FILLER          PIC X(20) VALUE "      PATIENT NAME:".
           05  PATIENT-NAME-O.
              15  LAST-NAME-O   PIC X(11).
              15  FILLER        PIC X(1) VALUE SPACES.
              15  MIDINIT-O     PIC X(1).
              15  FILLER        PIC X(2) VALUE ". ".
              15  FIRST-NAME-O  PIC X(11).
           05  FILLER       PIC X(18) VALUE "    TOTAL CHARGES:".
           05  TOTAL-CHARGES   PIC $$,$$$,$$9.99.

       COPY HLTHPLAN.
       COPY PROVIDER.
       COPY PATMSTR.
       COPY PATPERSN.
       COPY PATINS.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  RETURN-CD                      PIC 9(4) COMP.
           88 VALID-CALC     VALUE 0.

       PROCEDURE DIVISION.
      *
      * READ EACH PATIENT ON THE PATMSTR FILE
      *    call CALCLOST for UPDATING VALUES
      *    WRITE GOOD AND BAD UPDATE RECORDS
      *
           display "Hi Jon!".
           PERFORM 000-SETUP-RTN THRU 000-EXIT.

           PERFORM 100-UPDATE-MASTER-FILE THRU 100-EXIT
                UNTIL END-OF-PATMSTR.

           PERFORM 900-CLOSE-FILES THRU 900-EXIT.

           GOBACK.

       000-SETUP-RTN.
      **** OPEN FILES - START BROWSE
           MOVE "000-SETUP-RTN" TO PARA-NAME.
           INITIALIZE MISC-FIELDS.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.

           MOVE "000000" TO PATIENT-KEY.
           START PATMSTR KEY > PATIENT-KEY.

           READ PATMSTR INTO PATIENT-MASTER-REC.

           IF PATMSTR-FOUND NEXT SENTENCE
           ELSE
               MOVE "ERROR WITH PATMSTR FILE" TO ERR-MSG-PAT
               GO TO 1000-ERROR-RTN.

       000-EXIT.
           EXIT.


       100-UPDATE-MASTER-FILE.
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE "100-UPDATE-MASTER-FILE" TO PARA-NAME.
           MOVE ZERO TO RETURN-CD.

           IF DATE-DISCHARGE = " "
              PERFORM 1000-GET-PLAN-DATA THRU 1000-EXIT
              IF NOT PLAN-FOUND
                MOVE -1 TO RETURN-CD
                GO TO 1000-ERROR-RTN
              END-IF

              PERFORM 2000-GET-PRIMARY-PROVIDER THRU 2000-EXIT
              IF PROVIDER-FOUND
                IF NETWORK-FLAG = "Y"
                      MOVE "Y" TO PRIMARY-PHYS-NETWORK
                ELSE
                      MOVE "N" TO PRIMARY-PHYS-NETWORK
                END-IF
              ELSE
                    MOVE -1 TO RETURN-CD
                    GO TO 1000-ERROR-RTN
              END-IF
              PERFORM 3000-CALCULATE-TREATMENT-COSTS THRU 3000-EXIT
              PERFORM 4000-CALCULATE-EQUIPMENT-COSTS THRU 4000-EXIT
              PERFORM 5000-COMPUTE-TOTAL-AMOUNT THRU 5000-EXIT
           END-IF

           IF VALID-CALC
                 PERFORM 200-WRITE-RPT THRU 200-EXIT
                 PERFORM 300-REWRITE-PATMSTR
                                      THRU 300-EXIT
           ELSE
                 PERFORM 400-BAD-CALC-REC THRU 400-EXIT
           END-IF.

           READ PATMSTR INTO PATIENT-MASTER-REC.

       100-EXIT.
           EXIT.

       200-WRITE-RPT.
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE " 200-WRITE-RPT" TO PARA-NAME.
           MOVE PATIENT-ID IN PATIENT-MASTER-REC
               TO PRSN-KEY.
           READ PRSNMSTR INTO PATIENT-PERSONAL-MASTER-REC.
           IF NOT PRSN-FOUND
              MOVE "PATIENT NOT IN PRSNMSTR FILE" TO ERR-MSG-PAT
              MOVE PATIENT-MASTER-REC TO REST-OF-PAT-REC
              GO TO 1000-ERROR-RTN.

           MOVE PATIENT-ID IN PATIENT-MASTER-REC
                                  TO PATIENT-ID-O.
           MOVE LAST-NAME TO LAST-NAME-O.
           MOVE FIRST-NAME TO FIRST-NAME-O.
           MOVE MIDINIT IN PATIENT-NAME  TO MIDINIT-O.
           MOVE PATIENT-TOT-AMT   TO TOTAL-CHARGES.

           ADD +1 TO RPT-KTR.
           IF RPT-KTR > 60
              WRITE RPT-REC FROM WS-BLANK-LINE
              WRITE RPT-REC FROM WS-BLANK-LINE
              MOvE 2 TO RPT-KTR.

           WRITE RPT-REC FROM WS-PATIENT-RPT-REC.


       200-EXIT.
           EXIT.

       300-REWRITE-PATMSTR.
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE "300-REWRITE-PATMSTR" TO PARA-NAME.

           REWRITE PATMSTR-REC  FROM PATIENT-MASTER-REC
           INVALID KEY
                   MOVE "PATIENT NOT IN PRSNMSTR FILE" TO ERR-MSG-PAT
                   GO TO 1000-ERROR-RTN
           END-REWRITE.

       300-EXIT.
           EXIT.

       400-BAD-CALC-REC.
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE "400-BAD-CALC-REC" TO PARA-NAME.

           MOVE PATIENT-ID IN PATIENT-MASTER-REC
                                  TO PATIENT-ID-O.
           MOVE "***** BAD CALC *****" TO PATIENT-NAME-O.

           ADD +1 TO RPT-KTR.
           IF RPT-KTR > 60
              WRITE RPT-REC FROM WS-BLANK-LINE
              WRITE RPT-REC FROM WS-BLANK-LINE
              MOVE 2 TO RPT-KTR.

           WRITE RPT-REC FROM WS-PATIENT-RPT-REC.

       400-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN I-O PATMSTR.
           OPEN INPUT PATINS, PRSNMSTR.
           OPEN OUTPUT PATRPT, PATERR.
           DISPLAY "OPEN FILES".
           DISPLAY PATINS-STATUS.
           DISPLAY PRSN-STATUS.
           DISPLAY PATMSTR-STATUS.
      *     GOBACK.
       800-EXIT.
           EXIT.

       900-CLOSE-FILES.
           MOVE "900-CLOSE-FILES" TO PARA-NAME.
           CLOSE PATINS, PRSNMSTR, PATERR, PATRPT.
           DISPLAY "FILES CLOSED".
      *     GOBACK.
       900-EXIT.
           EXIT.

       1000-ERROR-RTN.
           GOBACK.

       1000-GET-PLAN-DATA.
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "1000-GET-PLAN-DATA" TO PARA-NAME.
           MOVE "N" TO PLAN-FOUND-SW.
      ******** EXEC SQL to get info from DB2
           MOVE PATIENT-ID IN PATIENT-MASTER-REC TO
                PATIENT-INS-KEY, PRSN-KEY.

           READ PATINS INTO PATIENT-INSURANCE.

           IF NOT PATINS-FOUND
              MOVE "** PATIENT NOT ON PATINS" TO ERR-MSG-PAT
              MOVE PATIENT-MASTER-REC  TO
                   REST-OF-PAT-REC
              GO TO 1000-ERROR-RTN.

           MOVE INS-IDENT-NBR IN INS-COMPANY-PRIMARY
                        TO  PLAN-ID IN DCLHEALTH-PLAN.

           READ PRSNMSTR  INTO PATIENT-INSURANCE.

           IF NOT PRSN-FOUND IN PRSN-STATUS
              MOVE "** PATIENT NOT ON PRSNMSTR" TO ERR-MSG-PAT
              MOVE PATIENT-MASTER-REC  TO
                   REST-OF-PAT-REC
              GO TO 1000-ERROR-RTN.


      ****** CHECK FOR VALID DIAGNOSTIC CODE
           EXEC SQL
           SELECT
            PLAN_ID,
             GROUP_ID,
             PROVIDER,
             DEDUCTIBLE,
             COPAYMENT,
             CO_INSURANCE,
             COVERAGE_LIMITS,
             OOP_MAX       ,
             IN_NETWORK_REQ  ,
             PRIOR_AUTHORIZATION    ,
             EXCLUSIONS     ,
             PLAN_COMMENTS
           INTO
           :PLAN-ID               ,
           :GROUP-ID              ,
           :PROVIDER              ,
           :DEDUCTIBLE            ,
           :COPAYMENT             ,
           :CO-INSURANCE          ,
           :COVERAGE-LIMITS       ,
           :OOP-MAX               ,
           :IN-NETWORK-REQ        ,
           :PRIOR-AUTHORIZATION   ,
           :EXCLUSIONS ,
           :PLAN-COMMENTS
              FROM DDS0001.HEALTH_PLAN
              WHERE PLAN_ID = :PLAN-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               MOVE "Y" TO PLAN-FOUND-SW
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 1000-EXIT.
       1000-EXIT.
           EXIT.

       2000-GET-PRIMARY-PROVIDER.
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "2000-GET-PRIMARY-PROVIDER" TO PARA-NAME
           MOVE "N" TO PROVIDER-FOUND-SW .
      ******** EXEC SQL to get info from DB2
           MOVE PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK
           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               MOVE 'Y' TO PROVIDER-FOUND-SW
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "** PRIMARY PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 2000-EXIT.
       2000-EXIT.
           EXIT.

       2200-GET-LAB-PROVIDER.
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "2200-GET-LAB-PROVIDER" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PHYS-ID-TEMP  TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK
           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               MOVE "Y" TO PHYS-FOUND-SW
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** LAB PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 2200-EXIT.
       2200-EXIT.
           EXIT.

       2400-GET-EQUIP-PROVIDER.
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "2400-GET-EQUIP-PROVIDER" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK
           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               MOVE "Y" TO PHYS-FOUND-SW
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** EQUIP PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 2400-EXIT .
       2400-EXIT.
           EXIT.

       3000-CALCULATE-TREATMENT-COSTS.
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE "3000-CALCULATE-TREATMENT-COSTS" TO PARA-NAME.
           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
               ROW-SUB > 20 OR LAB-TEST-S-ID(ROW-SUB) = " "
               MOVE "N" TO PHYS-FOUND-SW
               MOVE PRESCRIBING-S-PHYS-ID(ROW-SUB) TO PHYS-ID-TEMP
               PERFORM 2200-GET-LAB-PROVIDER THRU 2200-EXIT
               IF PHYSICIAN-FOUND
                   IF NETWORK-FLAG = "Y"
                      MOVE 80 TO REIMBURSE-PCT
                      COMPUTE WS-LAB-CHARGES =
                       WS-LAB-CHARGES +
                          ( TEST-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   ELSE
                      COMPUTE REIMBURSE-PCT = 80 - COST-OVERRIDE-PCT
                      COMPUTE WS-LAB-CHARGES =
                      WS-LAB-CHARGES +
                          ( TEST-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   END-IF
                END-IF
           END-PERFORM.
       3000-EXIT.
           EXIT.

       4000-CALCULATE-EQUIPMENT-COSTS.
      *** ROLL UP ALL EQUIPMENT COSTS
           MOVE "4000-CALCULATE-EQUIPMENT-COSTS" TO PARA-NAME.

           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
               ROW-SUB > 20 OR EQUIPMENT-S-ID(ROW-SUB) = " "
               MOVE "N" TO PHYS-FOUND-SW
               MOVE EQUIPMENT-PRES-PHYS-ID(ROW-SUB) TO PHYS-ID-TEMP
               PERFORM 2400-GET-EQUIP-PROVIDER THRU 2400-EXIT
               IF PHYSICIAN-FOUND
                   IF NETWORK-FLAG = "Y"
                      MOVE 80 TO REIMBURSE-PCT
                      COMPUTE WS-EQUIP-CHARGES  =
                       WS-EQUIP-CHARGES  +
                        ( EQUIPMENT-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   ELSE
                      COMPUTE REIMBURSE-PCT = 80 - COST-OVERRIDE-PCT
                      COMPUTE WS-EQUIP-CHARGES =
                      WS-EQUIP-CHARGES  +
                        ( EQUIPMENT-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   END-IF
                END-IF
           END-PERFORM.
       4000-EXIT.
           EXIT.

       5000-COMPUTE-TOTAL-AMOUNT.
      *** FINAL TOTALS PROCESSING
           MOVE ZERO TO PATIENT-TOT-AMT, STATE-FACTOR.

           IF PRIMARY-PHYS-IN-NETWORK
              PERFORM 6000-COMPUTE-IN-NETWORK THRU 6000-EXIT
           ELSE
              PERFORM 7000-COMPUTE-OUT-OF-NETWORK THRU 7000-EXIT.

       5000-EXIT.
           EXIT.

       6000-COMPUTE-IN-NETWORK.
      *** STANDARD RATES - REIMBURSE% BY STATE VALUE

           MOVE 80 TO REIMBURSE-PCT IN CALC-COSTS-REC.

           EVALUATE EMP-STATE
               WHEN "NC" MOVE 100 TO STATE-FACTOR
               WHEN "NJ" MOVE 100 TO STATE-FACTOR
               WHEN "NY" MOVE 100 TO STATE-FACTOR
               WHEN "ND" MOVE  60 TO STATE-FACTOR
               WHEN "AZ" MOVE 100 TO STATE-FACTOR
               WHEN "AR" MOVE  75 TO STATE-FACTOR
               WHEN "ID" MOVE 100 TO STATE-FACTOR
               WHEN "DE" MOVE  80 TO STATE-FACTOR
               WHEN "WA" MOVE 100 TO STATE-FACTOR
               WHEN "TX" MOVE 100 TO STATE-FACTOR
               WHEN "PA" MOVE  90 TO STATE-FACTOR
               WHEN "HI" MOVE 100 TO STATE-FACTOR
               WHEN "CA" MOVE  99 TO STATE-FACTOR
               WHEN "OR" MOVE  80 TO STATE-FACTOR
      *     END-EVALUATE

           COMPUTE PATIENT-TOT-AMT =
              ( WS-LAB-CHARGES + WS-EQUIP-CHARGES )
               * ( ( REIMBURSE-PCT / 100 ) + (  STATE-FACTOR / 100 ) )


           MOVE STATE-FACTOR  TO COPAY IN PATIENT-MASTER-REC.

       6000-EXIT.
           EXIT.

       7000-COMPUTE-OUT-OF-NETWORK.
      *** OUT OF NETWORK RATES FOR PATIENTS
           MOVE 72 TO REIMBURSE-PCT IN CALC-COSTS-REC.
           MOVE ZERO TO STATE-FACTOR.

           EVALUATE EMP-STATE
               WHEN "NC" MOVE  82 TO STATE-FACTOR
               WHEN "NJ" MOVE  54 TO STATE-FACTOR
               WHEN "NY" MOVE  19 TO STATE-FACTOR
               WHEN "ND" MOVE  79 TO STATE-FACTOR
               WHEN "AZ" MOVE  40 TO STATE-FACTOR
               WHEN "AR" MOVE  68 TO STATE-FACTOR
               WHEN "ID" MOVE  17 TO STATE-FACTOR
               WHEN "DE" MOVE  90 TO STATE-FACTOR
               WHEN "WA" MOVE  85 TO STATE-FACTOR
               WHEN "TX" MOVE  58 TO STATE-FACTOR
               WHEN "PA" MOVE  58 TO STATE-FACTOR
               WHEN "HI" MOVE  92 TO STATE-FACTOR
               WHEN "OR" MOVE  60 TO STATE-FACTOR
           END-EVALUATE

           COMPUTE PATIENT-TOT-AMT =
              ( WS-LAB-CHARGES + WS-EQUIP-CHARGES )
               * ( ( REIMBURSE-PCT / 100 ) + (  STATE-FACTOR / 100 ) )

           MOVE STATE-FACTOR  TO COPAY IN PATIENT-MASTER-REC.

       7000-EXIT.
           EXIT.

       1000-DB2-ERROR-RTN.
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************

            DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.
            DISPLAY '999-ERROR-TRAP-RTN '.
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
            DISPLAY 'SQLCODE ==> ' SQLCODE.
            DISPLAY SQLCA.
            DISPLAY SQLERRM.
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
            EXEC SQL ROLLBACK WORK END-EXEC.
            GO TO 1000-ERROR-RTN. 