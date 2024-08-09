       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BNCHS601.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
       SECURITY. CONFIDENTIAL PATIENT DATA.

      ******************************************************************
      *          LAST UPDATE date: 06/01/2010
      *
      *          THIS PROGRAM EDITS A DAILY TREATMENT TRANSACTION FILE
      *          PRODUCED BY DATA ENTRY OPERATORS FROM CICS SCREENS
      *
      *          IT CONTAINS EVERY TREATMENT FOR EVERY PATIENT IN THE
      *          HOSPITAL.
      *
      *          THE PROGRAM EDITS EACH RECORD AGAINST A NUMBER OF
      *          CRITERIA, BALANCES FINAL TOTALS AND WRITES GOOD
      *          RECORDS TO AN OUTPUT FILE
      *
      ******************************************************************

               INPUT FILE              -   DDS0001.TRMTDATA

               VSAM MASTER FILE        -   DDS0001.PATMASTR

               INPUT ERROR FILE        -   DDS0001.TRMTERR

               OUTPUT FILE PRODUCED    -   DDS001.TRMTEDIT

               DUMP FILE               -   SYSOUT

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT TRMTDATA
           ASSIGN TO UT-S-TRMTDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTEDIT
           ASSIGN TO UT-S-TRMTEDIT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTERR
           ASSIGN TO UT-S-TRMTERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       TO PATMSTR
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS PATIENT-KEY
                  FILE STATUS  IS PATMSTR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-REC.
       01  SYSOUT-REC  PIC X(130).

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

      ****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS
      ****** THE PROGRAM'S EDIT ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  TRMTEDIT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 1101 CHARACTERS
           DATA RECORD IS INPATIENT-TREATMENT-REC-EDIT.
       01  INPATIENT-TREATMENT-REC-EDIT PIC X(1101).

       FD  TRMTERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1141 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.
       01  INPATIENT-TREATMENT-REC-ERR.
           05  ERR-MSG                     PIC X(40) VALUE 'abc'.
           05  REST-OF-REC                 PIC X(1101).

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

      ** QSAM FILE
       WORKING-STORAGE SECTION.
       01  FILLER                     PIC X(32) VALUE
              '* WORKING STORAGE BEGINS HERE *'.

       01  FILLER                     PIC X(32) VALUE
                   '****** DUMP MSG ****************'.
      *****************************************************************
      *    DUMP POINTER AREA
      *        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    *
      *                      AS EACH PARAGRAPH IS ENTERED. DO NOT     *
      *                      MOVE PARAGRAPH NUMBERS OF COMMON
      *                      PARAGRAPHS (USE COMM POINTER).
      *                                                               *
      *        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       *
      *                       ITS PARAGRAPH NUMBER TO THIS POINTER    *
      *                       AT IT INCEPTION.
      *                                                               *
      *****************************************************************
       01  DUMP-LOCATOR.
           05 FILLER             PIC X(32)
               VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.
           05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.
           05 PARA-POINTER       PIC X(8)    VALUE SPACES.
           05 FILLER             PIC X(8)    VALUE '       Z'.
           05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.
           05 COMM-POINTER       PIC X(8)    VALUE SPACES.
           05 FILLER             PIC X(32)
                   VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.

       01  DUMP-DISPLAY.
           05 DUMP-STATUS               PIC X(3)  VALUE SPACES.
           05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.

       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND         VALUE "00".
               88 PATMSTR-NOT-FOUND    VALUE "23".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

      ** DAILY PATIENT/TREATMENTS FILE
       01  INPATIENT-TREATMENT-REC.
           05  RECORD-TYPE             PIC X(01).
               88  TRAILER-REC        VALUE "T".
           05  PATIENT-ID              PIC 9(6).
           05  TREATMENT-DATE-TIME.
               10 TREATMENT-DATE       PIC X(08).
               10 FILLER               PIC X.
               10 TREATMENT-TIME       PIC X(08).
               10 FILLER               PIC X(09).
           05  BED-IDENTITY            PIC X(4).
           05  PRIMARY-DIAGNOSTIC-CODE PIC X(5).
           05  MEDICATION-ID           PIC X(8).
           05  TREATMENT-MODE          PIC X(03).
               88  ORAL-ADMIN          VALUE "0RA".
               88  INTRAVENOUS-ADMIN   VALUE "INV".
               88  INJECTION           VALUE "INJ".
               88  MRI                 VALUE "MRI".
               88  CAT                 VALUE "CAT".
               88  CHEMO-THERAPY       VALUE "CHM".
               88  RADIATION-THERAPY   VALUE "RAD".
               88  SURGERY             VALUE "SUR".
               88  PHYSIO-THERAPY      VALUE "PHY".
               88  EQUIPMENT           VALUE "EQP".
               88  LAB-TESTS           VALUE "LAB".
               88  VENIPUNCTURE        VALUE "VEN".                     022904MN
               88  OTHER-TREATMENT     VALUE "OTH".
               88  VALID-TRTMNT-MODES VALUES ARE
                  "ORA", "INV", "INJ", "MRI", "CAT"
                  "SUR", "PHY", "EQP", "LAB", "VEN"
                  "MRI", "CAT", "CHM", "RAD", "OTH".
           05  BILLABLE-TREATMENT-IND   PIC X(01).
               88  NON-BILLABLE         VALUE "N".
               88  BILLABLE             VALUE "B".
               88 VALID-BILLABLE-TYPES
                   VALUES ARE "N", "B".
           05  MEDICATION-COST         PIC 9(5)V99.
           05  ATTENDING-PHYS-ID       PIC X(08).
           05  PRESCRIBING-PHYS-ID     PIC X(08).
           05  SUPERVISOR-NURSE-ID     PIC X(08).
           05  TREATMENT-NURSE-ID      PIC X(08).
           05  PHARMACY-COST           PIC 9(3)V99.
           05  ANCILLARY-CHARGE        PIC 9(3)V99.
           05  LAB-CHARGES OCCURS 12 TIMES.
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
           05  TREATMENT-COMMENTS      PIC X(254).

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1).
           05  IN-RECORD-COUNT         PIC 9(9).
           05  FILLER                  PIC X(1).
           05  IN-MEDICATION-CHARGES   PIC S9(9)V99.
           05  IN-PHARMACY-CHARGES     PIC S9(7)V99.
           05  IN-ANCILLARY-CHARGES    PIC S9(5)V99.

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

      * COPY PTMSTR.
      ** VSAM FILE
       01  PATIENT-MASTER-REC.
           05  PATIENT-ID                      PIC X(6).
           05  PATIENT-TYPE                    PIC X(1).
               88 INPATIENT   VALUE "I".
               88 OUTPATIENT  VALUE "0".
               88 VALID-TYPE  VALUES ARE "I", "O".
           05  PREVIOUS-PATIENT-IND            PIC X(01).
               88 PREV-PATIENT         VALUE "Y".
               88 NOT-PREVE-PATIENT    VALUE "N".
               88 VALID-PREV-IND  VALUES ARE "Y", "N".
           05  PRIMARY-STAY-WARD-NBR           PIC X(4).
               88  INTENSIVE-CARE  VALUE "0010".
               88  OBSTETRICS      VALUE "2010".
               88  PEDIATRICS      VALUE "1010".
               88  ONCOLOGY        VALUE "0011".
               88  CARDIO-THORACIC VALUE "0110".
               88  GENERAL         VALUE "0000".
               88  VALID-WARD VALUES ARE
                   "0010", "2010", "1010", "0011", "0110", "0000".
           05  BED-IDENTITY-PRIMARY            PIC 9(4).
           05  DATE-ADMIT                      PIC X(10).
           05  DATE-DISCHARGE                  PIC X(10).
           05  ATTENDING-PHYSICIAN             PIC X(08).
           05  DIAGNOSTIC-CODE-PRIMARY         PIC X(05).
           05  DIAGNOSTIC-CODE-SECONDARY       PIC X(05).
           05  DIAGNOSTIC-CODE-TERTIARY        PIC X(05).
           05  INS-TYPE                        PIC X(3).
               88 VALID-INS-TYPE VALUES ARE "HMO", "PPO", "POS" "MAN".
               88 MANAGED-CARE VALUE "MAN".
           05  HOSPITAL-STAY-LTH               PIC 999.
           05  PATIENT-TOT-AMT                 PIC 9(7)V99.
           05  PRIMARY-CARE-PHYSICIAN-ID       PIC X(8).
           05  IN-OUT-NETWORK                  PIC X(1).
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                           PIC S9(3).
           05  REMAINING-DEDUCTIBLE            PIC S9(4).
           05  HIPAA-FORM-SIGNED-IND           PIC X(01).
               88 HIPAA-SIGNED       VALUE "Y".
               88 HIPAA-UNSIGNED     VALUE "N".
           05  PATIENT-ADMIT-COMMENTS          PIC X(254).
           05  DAILY-LAB-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  LAB-TEST-S-ID             PIC X(08).
               10  LAB-TEST-DATE             PIC X(08).
               10  TEST-SHORT-S-DESC         PIC X(25).
               10  TEST-DIAG-CODE            PIC X(5).
               10  TEST-CHARGES              PIC 9(7)V99.
               10  PRESCRIBING-S-PHYS-ID     PIC X(08).
           05  EQUIPMENT-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  EQUIPMENT-S-ID            PIC X(08).
               10  EQUIPMENT-CHARGE-DATE     PIC X(08).
               10  EQUIP-DIAG-CODE           PIC X(5).
               10  EQUIPMENT-S-SHORT-DESC    PIC X(30).
               10  EQUIPMENT-CHARGES         PIC 9(7)V99.
               10  EQUIPMENT-PRES-PHYS-ID    PIC X(08).
      ** VSAM FILE

       01  WS-SYSOUT-REC.
           05  MSG                     PIC X(80).

       77  WS-DATE                     PIC 9(6).

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-WRITTEN          PIC 9(7) COMP.
           05 RECORDS-IN-ERROR         PIC 9(7) COMP.
           05 RECORDS-READ             PIC 9(7) COMP.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.

       01  MISC-WS-FLDS.
           05 STR-LTH                  PIC 9(04) VALUE 0.
           05 RETURN-CD                PIC S9(04) VALUE 0.
           05 ROW-SUB                  PIC 9(02).
           05 ERROR-MSG-WS.
              10  ERROR-MSG-FIRST      PIC X(20).
              10  ERROR-MSG-REST       PIC X(60).

       01  FLAGS-AND-SWITCHES.
           05 MORE-DATA-SW             PIC X(01) VALUE "Y".
               88 NO-MORE-DATA VALUE "N".
           05 ERROR-FOUND-SW           PIC X(01) VALUE "N".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".
           05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".
               88 NO-MORE-TABLE-ROWS VALUE "N".

      * COPY ABENDREC.
      ** QSAM FILE
      ** QSAM FILE
       01  ABEND-REC.
           05  FILLER             PIC X(12) VALUE "ABEND PARA: ".
           05  PARA-NAME          PIC X(20).
           05  ABEND-REASON       PIC X(40).
           05  FILLER             PIC X(10) VALUE " EXPECTED:".
           05  EXPECTED-VAL       PIC 9(6).
           05  FILLER             PIC X(8) VALUE " ACTUAL:".
           05  ACTUAL-VAL         PIC 9(6).
           05  FILLER             PIC X(9) VALUE " VALUE-3:".
           05  VALUE-3            PIC 9(6).
           05  FILLER             PIC X(9) VALUE "S0CB VALS".
           05  ONE-VAL            PIC 9 VALUE 1.
           05  ZERO-VAL           PIC 9 VALUE 0.
      * VSAM FILES
       01  PATIENT-INSURANCE.
           05  INS-COMPANY-PRIMARY.
               10  PATIENT-ID              PIC X(6).
               10  INS-COMPANY-PRIMARY-ID  PIC X(8).
               10  CARRIER-NAME            PIC X(30).
               10  CARRIER-PHONE           PIC X(10).
               10  CARRIER-FAX             PIC X(10).
               10  INSURED-NAME            PIC X(30).
               10  INSURED-GENDER          PIC X(01).
                   88  FEMALE          VALUE "F".
                   88  MALE            VALUE "M".
                   88  NOT-PROVIDED    VALUE "N".
                   88 VALID-GENDER
                       VALUES ARE "F", "M", "N".
               10  PATIENT-RELATIONSHIP    PIC X(02).
                   88  SPOUSE          VALUE "SP".
                   88  SELF-REL        VALUE "SE".
                   88  CHILD           VALUE "CH".
                   88  OTHER-REL       VALUE "OT".
                   88 VALID-RELS
                       VALUES ARE "SP", "SE", "SP", "OT".
               10  INS-IDENT-NBR           PIC X(20).
               10  GROUP-NBR               PIC X(10).
               10  ADDRESS-CLAIMS.
                   15  STREET              PIC X(30).
                   15  CITY                PIC X(20).
                   15  STATE               PIC X(02).
                   15  ZIP                 PIC X(9).
               10  RETIRED-IND    PIC X(01).
                   88 RETIRED          VALUE "Y".
                   88 NOT-RETIRED      VALUE "N".
                   88 VALID-RET-IND  VALUES ARE "Y", "N".
           05  INS-COMPANY-SECONDARY.
               10  CARRIER-ID              PIC X(8).
               10  CARRIER-NAME            PIC X(30).
               10  CARRIER-PHONE           PIC X(10).
               10  CARRIER-FAX             PIC X(10).
               10  INSUREDS-NAME           PIC X(30).
               10  PATIENT-RELATIONSHIP    PIC X(02).
                   88  SPOUSE          VALUE "SP".
                   88  SELF-REL        VALUE "SE".
                   88  CHILD           VALUE "CH".
                   88  OTHER-REL       VALUE "OT".
                   88 VALID-RELS
                       VALUES ARE "SP", "SE", "SP", "OT".
               10  INS-IDENT-NBR           PIC X(20).
               10  GROUP-NBR               PIC X(10).
               10  ADDRESS-CLAIMS.
                   15  STREET              PIC X(30).
                   15  CITY                PIC X(20).
                   15  STATE               PIC X(02).
                   15  ZIP                 PIC X(9).
           05  BENEFIT-ASSIGNMENT-DETAILS.
               10  MEDICARE-BENEFICIARY    PIC X(30).
               10  MEDICARE-CLAIM-NBR      PIC X(15).
               10  COMMERCIAL-BENEFICIARY  PIC X(30).
               10  COMMERCIAL-CLAIM-NBR    PIC X(15).
           05  PAT-INSURANCE-COMMENTS      PIC X(100).
           05  FILLER                      PIC X(142).
       01  PATIENT-PERSONAL-MASTER-REC.
           05  PATIENT-NBR-MR          PIC X(6).
           05  SSNBR                   PIC X(10).
           05  AGE                     PIC 9(03).
           05  DRIVERS-LICENSE-NO      PIC X(10).
           05  ISSUING-STATE           PIC X(02).
           05  OCCUPATION              PIC X(20).
           05  EMPLOYER.
               10  EMP-NAME   PIC X(30).
               10  EMP-ADDR   PIC X(30).
               10  EMP-CITY   PIC X(30).
               10  EMP-STATE  PIC X(02).
               10  EMP-ZIP    PIC X(09).
           05  MARITAL-STATUS PIC X(01).
               88 MARRIED      VALUE "M".
               88 SINGLE       VALUE "S".
               88 DIVORCED     VALUE "D".
               88 WIDOWED      VALUE "W".
               88 VALID-STATUS
                   VALUES ARE "M", "S", "W", "D".
           05  PATIENT-NAME.
               10 LAST-NAME   PIC X(15).
               10 MIDINIT     PIC X(01).
               10 FIRST-NAME  PIC X(20).
           05  PHONE-HOME     PIC X(10).
           05  PHONE-WORK    PIC X(10).
           05  PHONE-MOBILE  PIC X(10).
           05  HEIGHT        PIC 9(02).
           05  WEIGHT        PIC 9(03).
           05  GENDER        PIC X(01).
               88  FEMALE          VALUE "F".
               88  MALE            VALUE "M".
               88  NOT-PROVIDED    VALUE "N".
               88 VALID-GENDER
                   VALUES ARE "F", "M", "N".
           05  DOB                     PIC 9(05).
           05  FAMILY-CONTACT-PRIMARY  PIC X(30).
           05  FCON-RELATIONSHIP       PIC X(02).
               88  SPOUSE      VALUE "SP".
               88  SIBLING     VALUE "SI".
               88  CHILD       VALUE "CH".
               88  FRIEND      VALUE "FR".
               88 VALID-RELS
                   VALUES ARE "SP", "SI", "CH", "FR".
           05  MINOR-INDICATOR         PIC X(01) VALUE SPACES.
           05  RESPONSIBLE-PARTY.
               10  SSN           PIC X(10).
               10  OCCUPATION    PIC X(30).
               10  EMPLOYER      PIC X(30).
               10  CITY          PIC X(20).
               10  ST            PIC X(02).
               10  ZIP           PIC X(09).
           05  FCON-PHONE-H      PIC 9(10).
           05  FCON-PHONE-C        PIC X(10) VALUE SPACE.
           05  PAYMENT-METHOD-TYPE     PIC X(02).
               88 CREDIT-CARD      VALUE "CC".
               88 CHECK            VALUE "CH".
               88 CASH             VALUE "CA".
               88 VALID-PAYMENT-METHOD
                   VALUES ARE "CC", "CH", "CA".
           05  CREDIT-CARD-EXP-DATE.
               10  EXP-MONTH           PIC 9(02).
               10  EXP-YEAR            PIC 9(04).
           05  HOME-ADDRESS.
               10 APARTMENT-NBR PIC X(05).
               10 STREET        PIC X(30).
               10 CITY          PIC X(20).
               10 STATE         PIC X(02).
               10 POSTAL-CODE   PIC X(09).
               10 COUNTRY              PIC X(20).
           05  OCCUPATION              PIC X(30).
           05  EMPLOYER                PIC X(30).
           05  PATIENT-COMMENTS        PIC X(255).
      * COPY DIAGCODE.
      ******************************************************************
      ***** DB2 TABLE DCLGENS

      ******************************************************************
      * DCLGEN TABLE(DDS0001.WARD_DATA)                                *
      *        LIBRARY(DDS0001.TEST.COPYLIB(WARDDATA))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.WARD_DATA TABLE
           ( WARD_ID                        CHAR(4),
             PRIMARY_PHYSICIAN_ID           CHAR(8),
             SUPERVISE_NURSE_ID             CHAR(8),
             LOCATION                       CHAR(8),
             NUMBER_OF_BEDS                 SMALLINT,
             BASE_ROOM_CHARGE               DECIMAL(7, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.WARD_DATA                  *
      ******************************************************************
       01  DCLWARD-DATA.
           10 WARD-ID              PIC X(4).
           10 PRIMARY-PHYSICIAN-ID
              PIC X(8).
           10 SUPERVISE-NURSE-ID   PIC X(8).
           10 LOCATION             PIC X(8).
           10 NUMBER-OF-BEDS       PIC S9(4) USAGE COMP.
           10 BASE-ROOM-CHARGE     PIC S9(5)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
      ******************************************************************
      * DCLGEN TABLE(DDS0001.MEDICATION)                               *
      *        LIBRARY(DDS0001.TEST.COPYLIB(MEDICATN))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.MEDICATION TABLE
           ( MEDICATION_ID                  CHAR(8),
             MED_NAME                       CHAR(40),
             SHORT_DESCRIPTION              CHAR(100),
             COST                           DECIMAL(7, 2),
             PHARMACY_COST                  DECIMAL(5, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.MEDICATION                 *
      ******************************************************************
       01  DCLMEDICATION.
           10 MEDICATION-ID        PIC X(8).
           10 MED-NAME             PIC X(40).
           10 SHORT-DESCRIPTION    PIC X(100).
           10 COST                 PIC S9(5)V9(2) USAGE COMP-3.
           10 PHARMACY-COST        PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
      ******************************************************************
      * DCLGEN TABLE(DDS0001.HOSP_BED)                                 *
      *        LIBRARY(DDS0001.TEST.COPYLIB(HOSPBED))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.HOSP_BED TABLE
           ( BED_ID                         CHAR(4),
             ROOM_ID                        CHAR(4),
             WARD_ID                        CHAR(4),
             SPECIAL_CHARGES                DECIMAL(7, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.HOSP_BED                   *
      ******************************************************************
       01  DCLHOSP-BED.
           10 BED-ID               PIC X(4).
           10 ROOM-ID              PIC X(4).
           10 WARD-ID              PIC X(4).
           10 SPECIAL-CHARGES      PIC S9(5)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
      ******************************************************************
      * DCLGEN TABLE(DDS0001.DIAG_CODES)                               *
      *        LIBRARY(DDS0001.TEST.COPYLIB(DIAGCODE))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.DIAG_CODES TABLE
           ( DIAG_CODE                      CHAR(5) NOT NULL,
             INS_TYPE                       CHAR(3) NOT NULL,
             COPAY                          SMALLINT NOT NULL,
             DEDUCTIBLE                     SMALLINT NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.DIAG_CODES                 *
      ******************************************************************
       01  DCLDIAG-CODES.
           10 DIAG-CODE            PIC X(5).
           10 INS-TYPE             PIC X(3).
           10 COPAY                PIC S9(4) USAGE COMP.
           10 DEDUCTIBLE           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

       COPY SQLCA.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-DATA OR
      ******* Balancing logic put in by TGD 02/12/92
                   TRAILER-REC.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           INITIALIZE COUNTERS-AND-ACCUMULATORS.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
           PERFORM 900-READ-TRMTDATA THRU 900-EXIT.
           IF NO-MORE-DATA
               MOVE "EMPTY INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *     DISPLAY "100-MAINLINE".
      *  Validate patient type and insurance coverage
           PERFORM 300-FIELD-EDITS THRU 300-EXIT.

           IF RECORD-ERROR-FOUND
               ADD +1 TO RECORDS-IN-ERROR
               PERFORM 710-WRITE-TRMTERR THRU 710-EXIT
           ELSE
               PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.
           PERFORM 900-READ-TRMTDATA THRU 900-EXIT.
       100-EXIT.
           EXIT.

       300-FIELD-EDITS.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.
           MOVE "300-FIELD-EDITS" TO PARA-NAME.
      ******** non-numeric fields
           IF NOT VALID-BILLABLE-TYPES IN BILLABLE-TREATMENT-IND
              MOVE "*** INVALID BILLABLE TYPE" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF NOT VALID-TRTMNT-MODES IN TREATMENT-MODE
              MOVE "*** INVALID TREATMENT MODE" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC PATIENT-ID" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PATIENT-ID IN INPATIENT-TREATMENT-REC = ZERO
              MOVE "*** INVALID (000000) PATIENT-ID" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF BED-IDENTITY IN INPATIENT-TREATMENT-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC BED-IDENTITY" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF MEDICATION-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC MEDICATION-COST" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PHARMACY-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC PHARMACY COSTS" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC ANCILLARY-CHARGES" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF ATTENDING-PHYS-ID = SPACES
              MOVE "*** BLANK ATTENDING PHYSICIAN-ID" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF GROUP-NBR IN INS-COMPANY-PRIMARY = SPACES
              MOVE "*** BLANK INS GROUP NBR" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PRESCRIBING-PHYS-ID = SPACES
              MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           CALL 'DTEVAL' USING TREATMENT-DATE, RETURN-CD.
           IF RETURN-CD < 0
              MOVE "*** BAD DATE PORTION OF DATE-TIME" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           MOVE "Y" TO MORE-TABLE-ROWS.
           PERFORM 350-CHECK-LAB-TABLE THRU 350-EXIT VARYING ROW-SUB
                FROM 1 BY 1 UNTIL NO-MORE-TABLE-ROWS OR ROW-SUB = 12.

           IF VALID-RECORD
               PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.

      ****** VERIFY TABLE (JUST TYPES AND LAB-TEST-ID)

       300-EXIT.
           EXIT.

       350-CHECK-LAB-TABLE.
           IF LAB-TEST-ID(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 350-EXIT.

           IF NOT VALID-CATEGORY(ROW-SUB)
              MOVE "*** INVALID LAB-TEST CATEGORY" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

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
               GO TO 400-EXIT.


           IF  (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990
           OR  PHARMACY-COST IN INPATIENT-TREATMENT-REC < .99)
               MOVE "*** INVALID PHARMACY COSTS" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.


           IF  (ANCILLARY-CHARGE > 900
           OR  ANCILLARY-CHARGE < 1.01)
               MOVE "*** INVALID ANCILLARY CHARGES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.


           IF  (SSN IN RESPONSIBLE-PARTY > "999999999"
           OR  SSN IN RESPONSIBLE-PARTY < "0000000001")
               MOVE "*** INVALID SOCIAL SECURITY #" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.


           IF  (HOSPITAL-STAY-LTH > 365
           OR  HOSPITAL-STAY-LTH < 1)
               MOVE "*** INVALID HOSPITAL STAY LTH" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.

           IF  (EQUIPMENT-CHARGES(ROW-SUB) > 999.00
           OR  EQUIPMENT-CHARGES(ROW-SUB) < 99.99)
               MOVE "*** INVALID EQUIPMENT CHARGES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.


           IF  (EXP-MONTH > 12
           OR  EXP-MONTH < 1)
               MOVE "*** INVALID CREDIT-CARD EXP. DATE" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.

           IF VALID-RECORD
               PERFORM 450-CROSS-FIELD-EDITS THRU 450-EXIT.

           IF VALID-RECORD
               PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.

       400-EXIT.
           EXIT.

       450-CROSS-FIELD-EDITS.
           MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.
      ******** Specific requirements for certain procedures
           IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY
               OR SURGERY OR LAB-TESTS
               IF MEDICATION-COST = ZERO OR
                  ANCILLARY-CHARGE = ZERO
               MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  ORAL-ADMIN OR INTRAVENOUS-ADMIN OR INJECTION
               IF PHARMACY-COST IN INPATIENT-TREATMENT-REC = ZERO OR
                  ANCILLARY-CHARGE = ZERO
               MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  NOT OTHER-TREATMENT
               IF TREATMENT-NURSE-ID = SPACES OR
                  SUPERVISOR-NURSE-ID = SPACES
               MOVE "*** INVALID NURSING ENTRIES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  NOT (OTHER-TREATMENT AND LAB-TESTS)
               IF TREATMENT-COMMENTS = SPACES
               MOVE "*** INVALID TREATMENT COMMENTS" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  CHEMO-THERAPY OR RADIATION-THERAPY OR SURGERY
              MOVE +0 TO STR-LTH
              CALL 'STRLTH' USING TREATMENT-COMMENTS, STR-LTH
              IF STR-LTH < 25
               MOVE "*** INVALID TREATMENT COMMENT LENGTH" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF VALID-RECORD
               PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.

       450-EXIT.
           EXIT.

       500-CROSS-FILE-EDITS.
           MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.
      ******** Call to VSAM file to read record
           MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
                  PATIENT-KEY.
           READ PATMSTR INTO PATIENT-MASTER-REC.
           IF  NOT RECORD-FOUND
               MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 500-EXIT.

           IF VALID-RECORD
              PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.

       500-EXIT.
           EXIT.

       600-DB2-TABLE-EDITS.
           MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE ZERO TO  ANCILLARY-CHARGE.
           MOVE DIAGNOSTIC-CODE-PRIMARY IN PATIENT-MASTER-REC TO
                DIAG-CODE IN DCLDIAG-CODES.

      ****** CHECK FOR VALID DIAGNOSTIC CODE
           EXEC SQL
              SEECT DIAG_CODE INTO :DIAG-CODE
              FROM DDS0001.DIAG_CODES
              WHERE DIAG_CODE = :DIAG-CODE
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC
               MOVE DIAG-CODE IN DCLDIAG-CODES
                                  TO PRIMARY-DIAGNOSTIC-CODE
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "***  FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC
               MOVE DIAG-CODE IN DCLDIAG-CODES
                                  TO PRIMARY-DIAGNOSTIC-CODE
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID BED IDENTITY
           MOVE BED-IDENTITY TO BED-ID.
           EXEC SQL
              SELECT BED_ID INTO :BED-ID
              FROM DDS0001.HOSP_BED
              WHERE BED_ID = :BED-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** BED IDENT NOT-FOUND IN HOSP_BED" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "***  FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID PHYSICIAN-ID
           MOVE ATTENDING-PHYS-ID TO PRIMARY-PHYSICIAN-ID.
           EXEC SQL
              SELECT PRIMARY_PHYSICIAN_ID INTO :PRIMARY-PHYSICIAN-ID
              FROM DDS0001.WARD_DATA
              WHERE PRIMARY_PHYSICIAN_ID = :PRIMARY-PHYSICIAN-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** ATTENDING PHYSICIAN NOT FOUND IN TABLE" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
               MOVE "***  FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID MEDICATION-ID
           MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC TO
                  MEDICATION-ID IN DCLMEDICATION.

           EXEC SQL
              SELECT MEDICATION_ID
                             INTO :DCLMEDICATION.MEDICATION-ID
              FROM DDS0001.MEDICATION
              WHERE MEDICATION_ID = :DCLMEDICATION.MEDICATION-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** MEDICATION-ID NOT FOUND IN TABLE" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "***  FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID SUPERVISOR NURSE-ID
           MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
              SELECT SUPERVISE_NURSE_ID
                             INTO :SUPERVISE-NURSE-ID
              FROM DDS0001.WARD_DATA
              WHERE SUPERVISE_NURSE_ID = :SUPERVISE-NURSE-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** SUPERVISOR NURSE NOT FOUND" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.
      ****** CHECK FOR VALID SUPERVISOR NURSE-ID

           MOVE IN-PHARMACY-CHARGES  TO PHARMACY-COST IN DCLMEDICATION.
           EXEC SQL
              SELECT MEDICATION_ID
                             INTO :DCLMEDICATION.MEDICATION-ID
              FROM DDS0001.MEDICATION
              WHERE PHARMACY_COST >= :DCLMEDICATION.PHARMACY-COST
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** PHARMACY COST" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.
       600-EXIT.
           EXIT.

       700-WRITE-TRMTEDIT.
           MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.

           WRITE INPATIENT-TREATMENT-REC-EDIT
               FROM INPATIENT-TREATMENT-REC.
           ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.
           ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.
           ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC
                                TO WS-PHARMACY-CHARGES.
           ADD +1 TO RECORDS-WRITTEN.
       700-EXIT.
           EXIT.

       710-WRITE-TRMTERR.
           MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.
           WRITE INPATIENT-TREATMENT-REC-ERR.
           ADD +1 TO RECORDS-IN-ERROR.
       710-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT TRMTDATA.
           OPEN OUTPUT TRMTEDIT, SYSOUT, TRMTERR.
           OPEN I-O PATMSTR.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.
           CLOSE TRMTDATA,
                 TRMTEDIT, SYSOUT, TRMTERR,
                 PATMSTR.
       850-EXIT.
           EXIT.

       900-READ-TRMTDATA.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTDATA  INTO INPATIENT-TREATMENT-REC
               AT END MOVE "N" TO MORE-DATA-SW
               GO TO 900-EXIT
           END-READ
           MOVE "N" TO ERROR-FOUND-SW.
           ADD +1 TO RECORDS-READ.
       900-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-TREATMENT-REC-DATA TO WS-TRAILER-REC.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                                     TO ABEND-REASON
               GO TO 1000-ABEND-RTN.


           IF WS-ANCILLARY-CHARGES NOT EQUAL TO IN-ANCILLARY-CHARGES
               MOVE "** ANCILLARY CHARGES OUT OF BALANCE"
                                     TO ABEND-REASON
               MOVE WS-ANCILLARY-CHARGES TO EXPECTED-VAL
               MOVE IN-ANCILLARY-CHARGES TO ACTUAL-VAL
               DISPLAY "** ANCILLARY CHARGES IN **"
               DISPLAY WS-ANCILLARY-CHARGES
               DISPLAY "** ANCILLARY CHARGES EXPECTED **"
               DISPLAY  IN-ANCILLARY-CHARGES.

           IF WS-MEDICATION-CHARGES  NOT EQUAL TO IN-MEDICATION-CHARGES
               MOVE "** MEDICATION CHARGES OUT OF BALANCE"
                                     TO ABEND-REASON
               DISPLAY "** MEDICATION CHARGES IN **"
               DISPLAY WS-MEDICATION-CHARGES
               DISPLAY "** MEDICATION CHARGES EXPECTED **"
               DISPLAY  IN-MEDICATION-CHARGES.

           IF WS-PHARMACY-CHARGES  NOT EQUAL TO IN-PHARMACY-CHARGES
               MOVE "** PHARMACY CHARGES OUT OF BALANCE"
                                     TO ABEND-REASON
               DISPLAY "** PHARMACY CHARGES IN **"
               DISPLAY WS-PHARMACY-CHARGES
               DISPLAY "** PHARMACY CHARGES EXPECTED **"
               DISPLAY  IN-PHARMACY-CHARGES.

           MOVE "T" TO RECORD-TYPE.
           ADD +1 TO RECORDS-WRITTEN.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
           MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.
           MOVE WS-MEDICATION-CHARGES TO IN-MEDICATION-CHARGES.
           MOVE WS-PHARMACY-CHARGES TO IN-PHARMACY-CHARGES.
           WRITE INPATIENT-TREATMENT-REC-EDIT FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.

           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.
           DISPLAY  RECORDS-IN-ERROR.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB TRTMNT ********".
       999-EXIT.
           EXIT.


       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB - TRTMNT ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.

       1000-DB2-ERROR-RTN.
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************

            DISPLAY '**** DB2 ACCESS PROBLEM *****'.
            DISPLAY '999-ERROR-TRAP-RTN '.
            MOVE "*** DB2 PROBLEM ***" TO ERROR-MSG-FIRST.
            MOVE SQLCA TO ERROR-MSG-REST.
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
            DISPLAY 'SQLCODE ==> ' SQLCODE.
            DISPLAY SQLCA.
      *      DISPLAY SQLERRM.
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
            EXEC SQL ROLLBACK WORK END-EXEC.
            GO TO 1000-ABEND-RTN.