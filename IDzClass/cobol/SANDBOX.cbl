       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SANDBOX.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
       SECURITY. NON-CONFIDENTIAL.

      ******************************************************************
      *REMARKS.
      *
      *          THE PROGRAM'S PROCEDURE INCLUDES
      *                A DAILY CONTROL BREAK REPORT AND FILE UPDATES
      *                FOR THE AVAILABLE HOSPITAL BEDS/ROOMS/WARDS
      *
      *          IT BASES IT'S PROCESSING ON THE DAILY PATIENT FILE
      *          WHICH IS SORTED ON WARD/ROOM/BED - FOR CORRECT
      *          CONTROL-BREAK REPORTING
      *
      *
      ******************************************************************

             INPUT FILE           - DDS0001.PATSRCH

             VSAM MASTER FILES    - DDS0001.PATMASTR & DDS0001.PATPERSN

             INPUT ERROR FILE     - DDS0001.PATERR

             OUTPUT FILE PRODUCED -  DDS001.WARDPRT

             DUMP FILE            -   SYSOUT

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       SPECIAL-NAMES.
           C01 IS NEXT-PAGE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT TRMTDATA
           ASSIGN TO UT-S-TRMTDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.


           SELECT PATSRCH
           ASSIGN TO UT-S-PATSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT WARDFILE
           ASSIGN TO UT-S-WARDRPT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATERR
           ASSIGN TO UT-S-PATERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.


           SELECT TRMTERR
           ASSIGN TO UT-S-TRMTERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       to PATMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATMSTR-KEY
                  FILE STATUS  is PATMSTR-STATUS.

           SELECT PATPERSN
                  ASSIGN       to PATPERSN
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATPERSN-KEY
                  FILE STATUS  is PATPERSN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(130).


       FD  TRMTERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1141 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.
       01  INPATIENT-TREATMENT-REC-ERR.
           05  ERR-MSG                     PIC X(40).
           05  REST-OF-REC                 PIC X(1101).

     ****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM
      ****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  TRMTDATA
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1101 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-DATA.
       01  INPATIENT-TREATMENT-REC-DATA PIC X(1101).

       FD  WARDFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  RPT-REC  PIC X(132).

      ****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM
      ****** IT CONSISTS OF ALL PATIENT RECORDS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  PATSRCH
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-SRCH.
       01  INPATIENT-DAILY-REC-SRCH PIC X(993).

       FD  PATERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-ERR.
       01  INPATIENT-DAILY-REC-ERR.
           05  ERR-MSG                      PIC X(40).
           05  REST-OF-PAT-REC              PIC X(993).

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATMSTR-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

       FD  PATPERSN
           RECORD CONTAINS 800 CHARACTERS
           DATA RECORD IS PATPERSN-REC.
       01  PATPERSN-REC.
           05 PATPERSN-KEY      PIC X(06).
           05 FILLER           PIC X(794).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS            PIC X(2).
               88 PATMSTR-FOUND    VALUE "00".
           05  PATPERSN-STATUS            PIC X(2).
               88 PATPERSN-FOUND    VALUE "00".
           05  OFCODE                    PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05 MORE-DATA-SW               PIC X(1) VALUE SPACE.
           05 INPUT-FILE-RECORDS-READ    PIC 9(7) COMP VALUE 0.

       01  FLAGS-AND-SWITCHES.
           05 MORE-DATA-SWITCH             PIC X(01) VALUE "Y".
               88 NO-MORE-DATA VALUE "N".
           05 ERR-FOUND-SWITCH           PIC X(01) VALUE "N".
           05 ERROR-FOUND-SW           PIC X(01) VALUE "N".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".
           05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".
               88 NO-MORE-TABLE-ROWS VALUE "N".

       COPY BNCHTRMT.
      ** QSAM FILE
       01  INPATIENT-DAILY-REC.
           05  PATIENT-RECORD-TYPE     PIC X(01).
               88  TRAILER-REC-D     VALUE "T".
           05  PATIENT-ID              PIC 9(6).
           05  CURR-DTE                PIC X(08).
           05  BED-IDENTITY            PIC 9(4).
           05  ROOM-IDENTITY           PIC 9(4).
           05  TOTAL-ROOM-CHARGE       PIC 9(7)V99.
           05  BASE-ROOM-CHARGE        PIC 9(7)V99.
           05  ROOM-DATE-FROM          PIC X(08).
           05  ROOM-DATE-TO            PIC X(08).
           05  PRIMARY-DIAGNOSTIC-CODE PIC X(5).
           05  WARD-NBR                PIC X(4).
               88  INTENSIVE-CARE  VALUE "0010".
               88  OBSTETRICS      VALUE "2010".
               88  PEDIATRICS      VALUE "1010".
               88  ONCOLOGY        VALUE "0011".
               88  CARDIO-THORACIC VALUE "0110".
               88  GENERAL         VALUE "0000".
               88  VALID-WARD VALUES ARE
               "0010", "2010", "1010", "0011", "0110", "0000".
           05  ADDITIONAL-EQUIP-CHARGES OCCURS 12 TIMES.
               10  EQUIPMENT-ID            PIC X(08).
               10  EQUIPMENT-CATEGORY      PIC X(04).
                   88 HEATING-PAD   VALUE "HEAT".
                   88 AUTOCLAVE     VALUE "AUTO".
                   88 SCOPE         VALUE "SCOP".
                   88 DRIP          VALUE "DRIP".
                   88 MONITOR       VALUE "MON ".
                   88 SHUNT         VALUE "SHNT".
                   88 MISCELLANEOUS VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "HEAT", "AUTO",
                      "SCOP", "DRIP", "MON ", "SHNT", "MISC".
               10  EQUIPMENT-SHORT-DESC    PIC X(30).
               10  EQUIPMENT-COST          PIC 9(5)V99.
               10  EQUIPMENT-PRES-PHYS     PIC X(08).
               10  EQUIPMENT-REASON-CDE    PIC X(04).
           05  DAILY-CHARGES-COMMENTS      PIC X(255).

       01  WS-HDR-REC.
           05  FILLER                  PIC X(1) VALUE " ".
           05  HDR-DATE.
               10  HDR-YY              PIC 9(4).
               10  DASH-1              PIC X(1) VALUE "-".
               10  HDR-MM              PIC 9(2).
               10  DASH-2              PIC X(1) VALUE "-".
               10  HDR-DD              PIC 9(2).
           05  FILLER                  PIC X(20) VALUE SPACE.
           05  FILLER                  PIC X(65) VALUE
           "HOSPITAL WARDS / ROOMS / BEDS AVAILABILITY REPORT".
           05  FILLER         PIC X(14)
                         VALUE "Page Number:" Justified Right.
           05  PAGE-NBR-O              PIC ZZ9.

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1).
           05  IN-RECORD-COUNT         PIC 9(9).
           05  FILLER                  PIC X(1).
           05  IN-TOTAL-ROOM-CHARGE    PIC S9(9)V99.
           05  IN-BASE-ROOM-CHARGE     PIC S9(9)V99.
           05  IN-EQUIPMENT-CHARGES    PIC S9(9)V99.

       01  WS-BLANK-LINE.
           05  FILLER     PIC X(130) VALUE SPACES.

       01  WS-WARD-RPT-REC.
           05  FILLER     PIC X(1) VALUE SPACES.
           05  FILLER     PIC X(09) VALUE "WARD-ID:".
           05  WARD-O               PIC X(8).
           05  FILLER     PIC X(19) VALUE "PRIMARY PHYSICIAN:".
           05  PHYS-O               PIC X(12).
           05  FILLER     PIC X(18) VALUE "NURSE SUPERVISOR:".
           05  NURSE-O              PIC X(12).
           05  FILLER     PIC X(16) VALUE "NUMBER OF BEDS:".
           05  BEDS-O           PIC ZZZ9.
           05  FILLER     PIC X(22) VALUE "    BASE ROOM CHARGE:".
           05  ROOM-CHARGE-O       PIC $,$$9.99.
           05  FILLER     PIC X(50) VALUE SPACES.

       01  WS-ROOM-RPT-REC.
           05  FILLER     PIC X(2) VALUE SPACES.
           05  FILLER     PIC X(10) VALUE "ROOM-NBR:".
           05  ROOM-O               PIC X(6).
           05  FILLER     PIC X(12) VALUE " ROOM TYPE:".
           05  ROOM-TYPE            PIC X(14).
           05  FILLER     PIC X(17) VALUE " NUMBER OF BEDS:".
           05  BEDS-O           PIC Z99.
           05  FILLER     PIC X(23) VALUE "  SPECIAL EQUIPMENT:".
           05  SPECIAL-EQUIP-O  PIC X(60).

       01  WS-BED-PATIENT-DETAIL.
           05  FILLER     PIC X(4) VALUE SPACES.
           05  FILLER     PIC X(14) VALUE "PATIENT NAME:".
           05  PAT-NAME.
              15  LAST-NAME-O   PIC X(10).
              15  FILLER        PIC X(1) VALUE SPACES.
              15  MIDINIT-O     PIC X(1).
              15  FILLER        PIC X(1) VALUE SPACES.
              15  FIRST-NAME-O  PIC X(10).
           05  FILLER     PIC X(11) VALUE "  BED-NBR:".
           05  BED-O      PIC X(4).
           05  FILLER     PIC X(14) VALUE "  ADMIT DATE:".
           05  ADMIT-DATE-O   PIC X(10).
           05  FILLER     PIC X(13) VALUE "  DIAGNOSIS:".
           05  DIAGNOSIS-O  PIC X(7).
           05  FILLER     PIC X(11) VALUE "COMMENTS:".
           05  DAILY-COMMENTS-O       PIC X(50).

       01  COUNTERS-AND-ACCUMULATORS.
           05 WS-INFILE-RECORD-READ-CTR  PIC S9(7) COMP.
           05 RECORDS-IN-ERROR         PIC 9(7) COMP.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.


       COPY BNCHMSTR.
      ** VSAM FILE

       01  WS-SYSOUT-REC.
           05  MSG                     PIC X(80).

       01  WS-CURRENT-DATE-FIELDS.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  WS-CURRENT-DAY     PIC  9(2).
             05  WS-CURRENT-TIME.
                 10  WS-CURRENT-HOUR    PIC  9(2).
                 10  WS-CURRENT-MINUTE  PIC  9(2).
                 10  WS-CURRENT-SECOND  PIC  9(2).
                 10  WS-CURRENT-MS      PIC  9(2).
             05  WS-DIFF-FROM-GMT       PIC S9(4).


       01  COUNTERS-IDXS-AND-ACCUMULATORS.
           05 RECORDS-WRITTEN          PIC 9(7) COMP.
           05 PAT-RECORDS-IN-ERROR     PIC 9(7) COMP.
           05 PAT-RECORDS-READ         PIC 9(9) COMP.
           05 WS-BASE-ROOM-CHARGE      PIC S9(9)V99 COMP-3.
           05 WS-TOTAL-ROOM-CHARGE     PIC S9(9)V99 COMP-3.
           05 WS-EQUIPMENT-COST        PIC S9(7)V99 COMP-3.
           05 HOLD-WARD-ID             PIC 9(4) VALUE 0.
           05 HOLD-ROOM-NBR            PIC 9(4) VALUE 0.
           05 ROW-SUB                  PIC 9(2) VALUE 0.
           05 WS-LINES                 PIC 9(03) VALUE 0.
           05 WS-PAGES                 PIC 9(03) VALUE 1.
           05 TRLR-REC-SW              PIC 9(01) VALUE 0.
              88 TRLR-REC-FOUND        VALUE 1.

       01  MISC-WS-FLDS.
           05 STR-LTH                  PIC 9(04) VALUE 0.
           05 RETURN-CD                PIC S9(04) VALUE 0.
           05 INPUT-FILE-ERROR-MESSAGE PIC X(50) VALUE SPACES.
           05 TABLE-SIZE               PIC 9(02) VALUE 12.
           05 MORE-TABLE-ROWS          PIC X(01).
              88 MORE-TABLE-ROWS     VALUE "Y".
              88 NO-MORE-TABLE-ROWS  VALUE "N".

       01  FLAGS-AND-SWITCHES.
           05 MORE-WARD-DATA-SW          PIC X(01) VALUE "Y".
               88 NO-MORE-PATIENTS VALUE "N".
               88 MORE-PATIENTS VALUE "Y".
           05 ERROR-FOUND-SWT           PIC X(01) VALUE "Y".
           05 ERROR-FOUND-SWTCH         PIC X(01) VALUE "Y".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".
           05 FIRST-TIME-IN-SW           PIC X(01) VALUE "Y".
               88 FIRST-TREATMENT-READ VALUE "Y".
               88 NOT-FIRST-TIME  VALUE "N".
           05 WARD-SW           PIC X(01) VALUE "N".
               88 NEW-WARD VALUE "Y".
           05 ROOM-SW           PIC X(01) VALUE "N".
               88 NEW-ROOM VALUE "Y".

       COPY BNCHPRSN.
       COPY ABENDREC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
      ** QSAM FILE

      * COPY DIAGCODE.
      ******************************************************************
      ***** DB2 TABLE DCLGENS
       01  DCLWARD-CODES.
           10 WARD-ID                        PIC X(04).
           10 PRIMARY-PHYSICIAN-ID           PIC X(08).
           10 SUPERVISE-NURSE-ID             PIC X(08).
           10 LOCATION                       PIC X(08).
           10 NUMBER-OF-BEDS                 PIC S9(4) COMP.
           10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.

       01  DCLHOSP-BED.
           10 BED-ID                         PIC X(04).
           10 ROOM-ID                        PIC X(08).
           10 WARD-ID                        PIC X(08).
           10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.

       01  DCLROOM-DATA.
           10 WARD-ID                        PIC X(04).
           10 ROOM-ID                        PIC X(08).
           10 PRIVATE                        PIC S9(4) COMP.
           10 SEMI-PRIVATE                   PIC S9(4) COMP.
           10 NUMBER-OF-BEDS                 PIC S9(4) COMP.
           10 SPECIAL-EQUIPMENT              PIC X(254).

       01  DCLHOSP-BED.
           10 BED-ID                         PIC X(04).
           10 ROOM-IDB                       PIC X(08).
           10 WARD-ID                        PIC X(08).
           10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-PATIENTS OR
      ******* Balancing logic put in by TGD 02/12/92
                   TRAILER-REC.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-START-PROGRAM.
           INITIALIZE COUNTERS-AND-ACCUMULATORS.
           IF RECORD-ERROR-FOUND IN ERROR-FOUND-SWTCH
               ADD +1 TO RECORDS-IN-ERROR
               PERFORM 710-WRITE-TRMTERR
           ELSE
               PERFORM 700-WRITE-TRMTEDIT
               PERFORM 900-READ-TRMTDATA
               MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES
               MOVE "N" TO MORE-TABLE-ROWS IN FLAGS-AND-SWITCHES.
           IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY
              OR SURGERY OR LAB-TESTS
              IF MEDICATION-COST = ZERO OR
                 ANCILLARY-CHARGE = ZERO
                   MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO
                   INPUT-FILE-ERROR-MESSAGE
                   MOVE "Y" TO ERROR-FOUND-SW
                   GO TO 000-START-EXIT
               END-IF
           END-IF.
           MOVE DATE-ADMIT IN PATIENT-MASTER-REC TO
                DATE-DISCHARGE IN PATIENT-MASTER-REC.
           MOVE DIAGNOSTIC-CODE-PRIMARY TO DIAGNOSTIC-CODE-TERTIARY.
           COMPUTE PATIENT-TOT-AMT =
                   PATIENT-TOT-AMT * COPAY - REMAINING DEDUCTIBLE.
           PERFORM 400-NUMERIC-RANGE-EDITS.
           PERFORM 350-PROCESS-ROOM-DATA.
           PERFORM 250-PROCESS-WARD-DATA.
           ADD +1 TO WS-INFILE-READ-CTR.

           move PRIMARY-CARE-PHYSICIAN-ID TO ATTENDING-PHYSICIAN.
           MOVE WS-CURRENT-DATE TO
                                    DATE-ADMIT
                                    DATE-DISCHARGE

           MOVE DIAGNOSTIC-CODE-PRIMARY TO
                DIAGNOSTIC-CODE-SECONDARY,
                DIAGNOSTIC-CODE-TERTIARY.

           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL ROW-SUB > 20
                INITIALIZE DAILY-LAB-CHARGES-SUMMARY(ROW-SUB)
                INITIALIZE  EQUIPMENT-CHARGES-SUMMARY(ROW-SUB).

           MOVE PATIENT-NAME TO FAMILY-CONTACT-PRIMARY
                                 RESPONSIBLE-PARTY.

       000-START-EXIT.
           EXIT.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "HOUSEKEEPING".
      *  DATE VALUES
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR  TO HDR-YY.
           MOVE WS-CURRENT-MONTH  TO HDR-MM.
           MOVE WS-CURRENT-DAY  TO HDR-DD.

           INITIALIZE COUNTERS-IDXS-AND-ACCUMULATORS, WS-TRAILER-REC,
            INPATIENT-TREATMENT-REC-DATA.
           MOVE +1 TO WS-LINES, WS-PAGES.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
           PERFORM 900-READ-WARD-DATA THRU 900-EXIT.

           IF NO-MORE-PATIENTS
               MOVE "EMPTY PATIENT INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

      **** PUT IN TO HANDLE NEW SORT REQUIREMENTS
           IF TRAILER-REC
               MOVE 1 TO TRLR-REC-SW
               PERFORM 900-READ-WARD-DATA THRU 900-EXIT.                011295

       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
           IF WARD-NBR IN INPATIENT-DAILY-REC NOT = HOLD-WARD-ID
               PERFORM 200-NEW-WARD THRU 200-EXIT
               PERFORM 300-NEW-ROOM THRU 300-EXIT
               PERFORM 400-NEW-PATIENT THRU 400-EXIT
               MOVE WARD-NBR IN INPATIENT-DAILY-REC TO HOLD-WARD-ID
               MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC
                                TO HOLD-ROOM-NBR
           ELSE
           IF ROOM-IDENTITY IN INPATIENT-DAILY-REC
                            NOT = HOLD-ROOM-NBR
               PERFORM 300-NEW-ROOM THRU 300-EXIT
               PERFORM 400-NEW-PATIENT THRU 400-EXIT
               MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC
                            TO HOLD-ROOM-NBR
           ELSE
               PERFORM 400-NEW-PATIENT THRU 400-EXIT.

           PERFORM 900-READ-WARD-DATA THRU 900-EXIT.

       100-EXIT.
           EXIT.

       200-NEW-WARD.
           MOVE "200-NEW-WARD" TO PARA-NAME.
           MOVE "N" TO ERR-FOUND-SWITCH IN FLAGS-AND-SWITCHES.

           MOVE WARD-NBR IN INPATIENT-DAILY-REC TO
              WARD-ID IN DCLWARD-CODES,
              WARD-ID IN DCLROOM-DATA.
      ***     WARD-ID IN DCLHOSP-BED. ??

           PERFORM 250-PROCESS-WARD-TABLE-DATA THRU 250-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 700-WRITE-PAGE-HDR    THRU 700-EXIT.
           PERFORM 720-WRITE-WARD-RPT    THRU 720-EXIT.

      ***PROCESS PATIENT TREATMENTS
           PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.
       200-EXIT.
           EXIT.

       250-PROCESS-WARD-TABLE-DATA.
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
             SELECT PRIMARY_PHYSICIAN_ID,
                    SUPERVISE_NURSE_ID,
                    LOCATION,
                    NUMBER_OF_BEDS,
                    BASE_ROOM_CHARGE
             INTO
                    :PRIMARY-PHYSICIAN-ID,
                    :SUPERVISE-NURSE-ID,
                    :LOCATION,
                    :DCLWARD-CODES.NUMBER-OF-BEDS,
                    :DCLWARD-CODES.BASE-ROOM-CHARGE
             FROM DDS0001.WARD_DATA
             WHERE WARD_ID = :DCLWARD-CODES.WARD-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** PATIENT WARD DATA IN ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERR-FOUND-SWITCH
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 250-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERR-FOUND-SWITCH
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

           MOVE WARD-ID IN DCLWARD-CODES TO WARD-O.
           MOVE PRIMARY-PHYSICIAN-ID IN DCLWARD-CODES TO PHYS-O.
           MOVE SUPERVISE-NURSE-ID TO NURSE-O.
           MOVE NUMBER-OF-BEDS IN DCLWARD-CODES
                                    TO BEDS-O IN WS-WARD-RPT-REC.
           MOVE BASE-ROOM-CHARGE IN DCLWARD-CODES TO ROOM-CHARGE-O.
           PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.
       250-EXIT.
           EXIT.

       300-NEW-ROOM.
           MOVE "300-NEW-ROOM" TO PARA-NAME.
           MOVE "N" TO ERR-FOUND-SWITCH IN FLAGS-AND-SWITCHES.

           MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC TO
              ROOM-IDB IN DCLHOSP-BED,
              ROOM-ID  IN DCLROOM-DATA.
      ***     WARD-ID IN DCLHOSP-BED. ??

           PERFORM 350-PROCESS-ROOM-TABLE-DATA THRU 350-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 740-WRITE-ROOM-RPT   THRU 740-EXIT.
           PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.
       300-EXIT.
           EXIT.

       350-PROCESS-ROOM-TABLE-DATA.
           PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
             SELECT PRIVATE,
                    SEMI_PRIVATE,
                    NUMBER_OF_BEDS,
                    SPECIAL_EQUIPMENT
             INTO
                    :DCLROOM-DATA.PRIVATE,
                    :DCLROOM-DATA.SEMI-PRIVATE,
                    :DCLROOM-DATA.NUMBER-OF-BEDS,
                    :DCLROOM-DATA.SPECIAL-EQUIPMENT
             FROM DDS0001.ROOM_DATA
             WHERE WARD_ID = :DCLROOM-DATA.WARD-ID
             AND   ROOM_ID = :DCLROOM-DATA.ROOM-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** PATIENT ROOM DATA IN ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 350-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

           MOVE ROOM-ID IN DCLROOM-DATA TO ROOM-O.
           IF PRIVATE IN DCLROOM-DATA = 1
              MOVE "PRIVATE" TO ROOM-TYPE
           ELSE
           IF SEMI-PRIVATE = 1
              MOVE "SEMI-PRIVATE" TO ROOM-TYPE
           ELSE
              MOVE "SPECIAL-NEEDS" TO ROOM-TYPE.

           MOVE WARD-ID IN DCLWARD-CODES TO PHYS-O.
           MOVE SUPERVISE-NURSE-ID TO NURSE-O.
           MOVE NUMBER-OF-BEDS IN DCLWARD-CODES
                                  TO BEDS-O IN WS-ROOM-RPT-REC.
           MOVE SPECIAL-EQUIPMENT IN DCLROOM-DATA TO SPECIAL-EQUIP-O.
           PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.
       350-EXIT.
           EXIT.

       400-NUMERIC-RANGE-EDITS.
           MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.
      ******** Call to VSAM file to read record
           IF  (MEDICATION-COST > 99000
           OR  MEDICATION-COST < 1.01)
               MOVE "*** INVALID MEDICATION COST" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-N-EXIT.


           IF  (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990
           OR  PHARMACY-COST IN INPATIENT-TREATMENT-REC < .99)
               MOVE "*** INVALID PHARMACY COSTS" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-N-EXIT.


           IF  (ANCILLARY-CHARGE > 900
           OR  ANCILLARY-CHARGE < 1.01)
               MOVE "*** INVALID ANCILLARY CHARGES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-N-EXIT.



       400-N-EXIT.
           EXIT.

       400-NEW-PATIENT.
           MOVE "400-NEW-PATIENT" TO PARA-NAME.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.

           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
           PATMSTR-KEY, PATPERSN-KEY.

           PERFORM 450-GET-PATIENT-DATA THRU 450-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 760-WRITE-PATIENT-RPT THRU 760-EXIT.

       400-EXIT.
           EXIT.

       450-GET-PATIENT-DATA.
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.

           MOVE DAILY-CHARGES-COMMENTS TO DAILY-COMMENTS-O.

           READ PATMSTR.
           IF PATMSTR-FOUND
              MOVE PATMSTR-REC TO PATIENT-MASTER-REC
              MOVE DATE-ADMIT TO ADMIT-DATE-O
              MOVE DIAGNOSTIC-CODE-PRIMARY TO DIAGNOSIS-O
              MOVE BED-IDENTITY-PRIMARY TO BED-O
           ELSE
              MOVE "PATIENT NOT FOUND IN PATMASTR" TO ABEND-REASON
              MOVE "500-GET-PATIENT-DATA" TO PARA-NAME
              MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL
              GO TO 1000-ABEND-RTN.

           READ PATPERSN.
           IF PATPERSN-FOUND
              MOVE PATPERSN-REC TO PATIENT-PERSONAL-MASTER-REC
              MOVE LAST-NAME TO LAST-NAME-O
              MOVE MIDINIT TO MIDINIT-O
              MOVE DIAGNOSTIC-CODE-SECONDARY TO DIAGNOSIS-O
              MOVE FIRST-NAME TO FIRST-NAME-O
           ELSE
              MOVE "PATIENT NOT FOUND IN PATPERSN" TO ABEND-REASON
              MOVE "500-GET-PATIENT-DATA" TO PARA-NAME
              MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL
              MOVE PRIMARY-CARE-PHYSICIAN-ID  TO DIAGNOSIS-O
              GO TO 1000-ABEND-RTN.

       450-EXIT.
           EXIT.

       700-WRITE-TRMTEDIT.
           PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.
           MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.

           WRITE INPATIENT-TREATMENT-REC-DATA
               FROM INPATIENT-TREATMENT-REC.
           ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.
           ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.
           ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC
                                TO WS-PHARMACY-CHARGES.
           ADD +1 TO RECORDS-WRITTEN.
       700-T-EXIT.
           EXIT.

       700-WRITE-PAGE-HDR.
           MOVE "700-WRITE-PAGE-HDR" TO PARA-NAME.
           MOVE WS-PAGES TO PAGE-NBR-O.
           WRITE RPT-REC FROM WS-HDR-REC
               AFTER ADVANCING NEXT-PAGE.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +1 TO WS-PAGES.
           MOVE +2 TO WS-LINES.
       700-EXIT.
           EXIT.


       710-WRITE-TRMTERR.
           MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.
           WRITE INPATIENT-TREATMENT-REC-ERR.
           ADD +1 TO RECORDS-IN-ERROR.
       710-EXIT.
           EXIT.

       720-WRITE-WARD-RPT.
           MOVE "720-WRITE-WARD-RPT" TO PARA-NAME.
           WRITE RPT-REC FROM WS-WARD-RPT-REC
               AFTER ADVANCING 2.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +3 TO WS-LINES.
       720-EXIT.
           EXIT.

       740-WRITE-ROOM-RPT.
           MOVE "740-WRITE-ROOM-RPT" TO PARA-NAME.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           WRITE RPT-REC FROM WS-ROOM-RPT-REC
               AFTER ADVANCING 1.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +2 TO WS-LINES.
       740-EXIT.
           EXIT.

       760-WRITE-PATIENT-RPT.
           MOVE "760-WRITE-PATIENT-RPT" TO PARA-NAME.
           WRITE RPT-REC FROM WS-BED-PATIENT-DETAIL
               AFTER ADVANCING 1.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           ADD +1 TO WS-LINES.
       760-EXIT.
           EXIT.

       790-CHECK-PAGINATION.
           MOVE "790-CHECK-PAGINATION" TO PARA-NAME.
           IF WS-LINES > 50
              WRITE RPT-REC FROM WS-BLANK-LINE
              WRITE RPT-REC FROM WS-BLANK-LINE
              PERFORM 700-WRITE-PAGE-HDR THRU 700-EXIT.
       790-EXIT.
           EXIT.

       795-WRITE-PATERR.
           MOVE "795-WRITE-PATERR" TO PARA-NAME.
           MOVE INPATIENT-DAILY-REC TO REST-OF-PAT-REC.
           WRITE INPATIENT-DAILY-REC-ERR.
           ADD +1 TO PAT-RECORDS-IN-ERROR.
       795-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT PATSRCH, PATPERSN, PATMSTR, TRMTDATA,
           OPEN OUTPUT WARDFILE, PATERR, SYSOUT,
                       TRMTERR.
           DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.

           CLOSE PATSRCH, WARDFILE,
                 SYSOUT, PATPERSN,
                 PATMSTR.
           DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       850-EXIT.
           EXIT.

       900-READ-TRMTDATA.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           INITIALIZE INPATIENT-TREATMENT-REC-DATA .
           READ TRMTDATA
               AT END MOVE "N" TO MORE-DATA-SW
               GO TO 900-T-EXIT
           END-READ
           MOVE "N" TO ERROR-FOUND-SW.
           ADD +1 TO INPUT-FILE-RECORDS-READ.
       900-T-EXIT.
           EXIT.

       900-READ-WARD-DATA.
      *  READ SORTED PATIENT TRANS FILE
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSRCH INTO INPATIENT-DAILY-REC
               AT END MOVE "N" TO MORE-WARD-DATA-SW
               GO TO 900-EXIT
           END-READ.

           ADD +1 TO PAT-RECORDS-READ.
       900-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF TRLR-REC-FOUND
               NEXT SENTENCE
           ELSE
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-DAILY-REC TO WS-TRAILER-REC.
           ADD +1 TO RECORDS-WRITTEN.
      *    IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
      *        MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
      *                              TO ABEND-REASON
      *        MOVE RECORDS-READ     TO ACTUAL-VAL
      *        MOVE IN-RECORD-COUNT  TO EXPECTED-VAL
      *        GO TO 1000-ABEND-RTN.
      *
      *    MOVE "T" TO PATIENT-RECORD-TYPE.
      *    MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
      *    MOVE WS-BASE-ROOM-CHARGE  TO IN-BASE-ROOM-CHARGE.
      *    MOVE WS-TOTAL-ROOM-CHARGE TO IN-TOTAL-ROOM-CHARGE.
      *    MOVE WS-EQUIPMENT-COST TO IN-EQUIPMENT-CHARGES.
      *    WRITE INPATIENT-DAILY-REC  FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "NORMAL END OF JOB".
       999-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB- DALYEDIT ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.

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
            GO TO 1000-ABEND-RTN.