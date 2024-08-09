       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PATLIST.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
       SECURITY. NON-CONFIDENTIAL.

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

           SELECT TRMTSRCH
           ASSIGN TO UT-S-TRMTSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATSRCH
           ASSIGN TO UT-S-PATSRCH
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATRPT
           ASSIGN TO UT-S-PATRPT
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

           SELECT PATINS
                  ASSIGN       to PATINS
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATINS-KEY
                  FILE STATUS  is PATINS-STATUS.

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

       FD  PATRPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC.
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
           05  ERR-MSG-PAT                  PIC X(40).
           05  REST-OF-PAT-REC              PIC X(993).

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATMSTR-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

       FD  PATINS
           RECORD CONTAINS 702 CHARACTERS
           DATA RECORD IS PATINS-REC.
       01  PATINS-REC.
           05 PATINS-KEY      PIC X(06).
           05 FILLER           PIC X(696).

       FD  PATPERSN
           RECORD CONTAINS 800 CHARACTERS
           DATA RECORD IS PATPERSN-REC.
       01  PATPERSN-REC.
           05 PATPERSN-KEY      PIC X(06).
           05 FILLER           PIC X(794).

      ****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM
      ****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  TRMTSRCH
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1101 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-SRCH.
       01  INPATIENT-TREATMENT-REC-SRCH PIC X(1101).

       FD  TRMTERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1141 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.
       01  INPATIENT-TREATMENT-REC-ERR.
           05  ERR-MSG-PRMT                 PIC X(40).
           05  REST-OF-TRMTERR-REC          PIC X(1101).

      ** QSAM FILE
       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 PATMSTR-FOUND    VALUE "00".
           05  PATINS-STATUS          PIC X(2).
               88 PATINS-FOUND    VALUE "00".
           05  PATPERSN-STATUS          PIC X(2).
               88 PATPERSN-FOUND    VALUE "00".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       COPY PATDALY.
      ** QSAM FILE

       COPY TREATMNT.

       01  WS-HDR-REC.
           05  FILLER                  PIC X(1) VALUE " ".
           05  HDR-DATE.
               10  HDR-YY              PIC 9(4).
               10  DASH-1              PIC X(1) VALUE "-".
               10  HDR-MM              PIC 9(2).
               10  DASH-2              PIC X(1) VALUE "-".
               10  HDR-DD              PIC 9(2).
           05  FILLER                  PIC X(20) VALUE SPACE.
           05  FILLER                  PIC X(50) VALUE
           "Patient Detailed Treatments and Charges List".
           05  FILLER         PIC X(26)
                         VALUE "Page Number:" Justified Right.
           05  PAGE-NBR-O              PIC ZZ9.

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1).
           05  IN-RECORD-COUNT         PIC 9(9).
           05  FILLER                  PIC X(1).
           05  IN-TOTAL-ROOM-CHARGE    PIC S9(9)V99.
           05  IN-BASE-ROOM-CHARGE     PIC S9(9)V99.
           05  IN-EQUIPMENT-CHARGES    PIC S9(9)V99.

       01  WS-COLM-HDR-REC.
           05  FILLER            PIC X(8)  VALUE "PAT-ID".
           05  FILLER            PIC X(26) VALUE "PATIENT NAME".
           05  FILLER            PIC X(12) VALUE "PAT PHONE".
           05  FILLER            PIC X(4)  VALUE "TY".
           05  FILLER            PIC X(6)  VALUE "WARD".
           05  FILLER            PIC X(6) VALUE "ROOM".
           05  FILLER            PIC X(6) VALUE "BED#".
           05  FILLER            PIC X(8) VALUE "ADMIT".
           05  FILLER            PIC X(6) VALUE "DIAG".
           05  FILLER            PIC X(13) VALUE "DAY CHARGES".
           05  FILLER            PIC X(5) VALUE "COP".
           05  FILLER            PIC X(32) VALUE "PCP".
           05  FILLER            PIC X(48) VALUE "INS".

       01  WS-PATIENT-RPT-REC.
           05  PATIENT-ID-O            PIC 9(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-O.
              15  LAST-NAME-O   PIC X(11).
              15  FILLER        PIC X(1) VALUE SPACES.
              15  MIDINIT-O     PIC X(1).
              15  FILLER        PIC X(2) VALUE ". ".
              15  FIRST-NAME-O  PIC X(11).
           05  PATIENT-PHONE-O         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(2).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WARD-NUMBER             PIC 9999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  ROOM-NUMBER             PIC 9999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  BED-IDENTITY-O          PIC 9999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  ADMIT-DATE-O            PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PRIMARY-DIAG-CODE-O     PIC X(8).
           05  PATIENT-DAILY-CHARGES   PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
      **** DO DB2 TABLE LOOK-UP TO GET NAME AS ENHANCEMENT?
           05  PRIMARY-PHYS-O          PIC X(30).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-O              PIC X(4).
           05  FILLER                  PIC X(27) VALUE SPACES.

       01  WS-EQUIP-HDR.
           05  FILLER     PIC X(133)
                     VALUE "DAILY EQUIPMENT CHARGES:".

       01  WS-TRMT-HDR.
           05  FILLER     PIC X(133)
                     VALUE "DAILY LAB TREATMENT CHARGES".

       01  WS-BLANK-LINE.
           05  FILLER     PIC X(130) VALUE SPACES.

       01  WS-EQUIP-RPT-REC.
           05  FILLER     PIC X(3) VALUE SPACES.
           05  FILLER     PIC X(14) VALUE "EQUIPMENT-ID:".
           05  EQUIP-ID-O               PIC X(8).
           05  FILLER     PIC X(17) VALUE "  EQUIPMENT COST:".
           05  EQUIP-COST               PIC $,$$$,$$9.99.
           05  FILLER     PIC X(19) VALUE "  EQUIPMENT CATEG:".
           05  EQUIP-CATEGORY           PIC 999.
           05  FILLER     PIC X(7) VALUE " DESC:".
           05  EQUIP-SHORT-DESC-O       PIC X(70) VALUE SPACES.

       01  WS-TRMT-RPT-REC.
           05  FILLER     PIC X(2) VALUE SPACES.
           05  FILLER     PIC X(12) VALUE "MEDICATION:".
           05  MEDICATION-ID-O         PIC X(8).
           05  FILLER     PIC X(18) VALUE "  MEDICATION COST:".
           05  MEDICATION-COST-O       PIC $$,$$9.99.
           05  FILLER     PIC X(25) VALUE "  PRESCRIBING PHYSICIAN:".
           05  PRESCRIBING-PHYS-ID-O   PIC X(08).
           05  FILLER     PIC X(11) VALUE "  COMMENTS:".
           05  TRMT-COMMENTS-O         PIC X(70) VALUE SPACES.

       01  WS-LABTEST-DETAIL.
           05  FILLER     PIC X(5) VALUE SPACES.
           05  FILLER     PIC X(12) VALUE "LABTEST-ID:".
           05  PATLISTEST-ID-O           PIC X(8).
           05  FILLER     PIC X(15) VALUE "  LABTEST COST:".
           05  TEST-COST-O             PIC $$$,$$9.99.
           05  FILLER     PIC X(17) VALUE "  LABTEST CATEG:".
           05  TEST-CATEGORY-O           PIC 999.
           05  FILLER     PIC X(17) VALUE "  VENIPUNCT COST:".
           05  VENIPUNCTION-COST-O     PIC $$9.99.
           05  FILLER     PIC X(8) VALUE "  DESC:".
           05  TEST-SHORT-DESC-O       PIC X(70) VALUE SPACES.

       01  PATIENT-TREATMENT-SUMMARY-LINE.
           05  FILLER                  PIC X(32) VALUE
               'SUMMARY LINE OF CHARGES FOR: '.
           05  PATIENT-NAME            PIC X(30).
           05  TOTAL-TREATMENT-CHARGES PIC $$$,$$$,$$9.99.

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
           05  PATIENT-NBR             PIC 9(6).
           05  SSN                     PIC X(10).
           05  AGE                     PIC 9(03).
           05  DRIVERS-LICENSE-NO      PIC X(10).
           05  ISSUING-STATE           PIC X(02).
           05  OCCUPATION              PIC X(20).
           05  EMPLOYER.
               10  EMP-NAME            PIC X(30).
               10  EMP-ADDRESS         PIC X(30).
               10  EMP-CITY            PIC X(30).
               10  EMP-STATE           PIC X(02).
               10  EMP-ZIP             PIC X(09).
           05  MARITAL-STATUS          PIC X(01).
               88 MARRIED      VALUE "M".
               88 SINGLE       VALUE "S".
               88 DIVORCED     VALUE "D".
               88 WIDOWED      VALUE "W".
               88 VALID-STATUS
                   VALUES ARE "M", "S", "W", "D".
           05  PATIENT-NAME.
               10 LAST-NAME            PIC X(15).
               10 MIDINIT              PIC X(01).
               10 FIRST-NAME           PIC X(20).
           05  PHONE-HOME              PIC X(10).
           05  PHONE-WORK              PIC X(10).
           05  PHONE-MOBILE            PIC X(10).
           05  HEIGHT                  PIC 9(02).
           05  WEIGHT                  PIC 9(03).
           05  GENDER                  PIC X(01).
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
           05  MINOR-INDICATOR         PIC X(01).
           05  RESPONSIBLE-PARTY.
               10  SSN                 PIC X(10).
               10  OCCUPATION          PIC X(30).
               10  EMPLOYER            PIC X(30).
               10  CITY                PIC X(20).
               10  ST                  PIC X(02).
               10  ZIP                 PIC X(09).
           05  FCON-PHONE-H            PIC X(10).
           05  FCON-PHONE-C            PIC X(10).
           05  PAYMENT-METHOD-TYPE     PIC X(02).
               88 CREDIT-CARD      VALUE "CC".
               88 CHECK            VALUE "CH".
               88 CASH             VALUE "CA".
               88 VALID-PAYMENT-METHOD
                   VALUES ARE "CC", "CH", "CA".
           05  CREDIT-CARD-EXP-DATE.
               10  EXP-MONTH           PIC X(02).
               10  EXP-YEAR            PIC X(04).
           05  HOME-ADDRESS.
               10 APARTMENT-NBR        PIC X(05).
               10 STREET               PIC X(30).
               10 CITY                 PIC X(20).
               10 STATE                PIC X(02).
               10 POSTAL-CODE          PIC X(9).
               10 COUNTRY              PIC X(20).
           05  OCCUPATION              PIC X(30).
           05  EMPLOYER                PIC X(30).
           05  PATIENT-COMMENTS        PIC X(262).


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
               88 Managed-Care value "MAN".
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
           05  PATIENT-ADMIT-COMMENTS          PIC X(253).
           05  DAILY-LAB-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  PATLISTEST-S-ID             PIC X(08).
               10  PATLISTEST-DATE             PIC X(08).
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
           05 RECORDS-READ             PIC 9(7) COMP.
           05 PAT-RECORDS-IN-ERROR     PIC 9(7) COMP.
           05 TRMT-RECORDS-IN-ERROR    PIC 9(7) COMP.
           05 TRMT-RECORDS-READ        PIC 9(9) COMP.
           05 PAT-RECORDS-READ         PIC 9(9) COMP.
           05 WS-BASE-ROOM-CHARGE      PIC S9(9)V99 COMP-3.
           05 WS-TOTAL-ROOM-CHARGE     PIC S9(9)V99 COMP-3.
           05 WS-EQUIPMENT-COST        PIC S9(7)V99 COMP-3.
           05 HOLD-EQUIPMENT-COST      PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(2).
           05 WS-LINES                 PIC 9(01) VALUE 2.
           05 WS-PAGES                 PIC 9(01) VALUE 1.
           05 HOLD-TRMT-PATIENT-ID     PIC 9(06).

       01  MISC-WS-FLDS.
           05 STR-LTH                  PIC 9(04) VALUE 0.
           05 RETURN-CD                PIC S9(04) VALUE 0.
           05 TABLE-SIZE               PIC 9(02) VALUE 12.
           05 MORE-TABLE-ROWS          PIC X(01).
              88 MORE-TABLE-ROWS     VALUE "Y".
              88 NO-MORE-TABLE-ROWS  VALUE "N".


       01  FLAGS-AND-SWITCHES.
           05 MORE-PATDATA-SW          PIC X(01) VALUE "Y".
               88 NO-MORE-PATIENTS VALUE "N".
               88 MORE-PATIENTS VALUE "Y".
           05 MORE-TRMT-SW          PIC X(01) VALUE "Y".
               88 NO-MORE-TREATMENTS VALUE "N".
               88 MORE-TREATMENTS VALUE "Y".
           05 MORE-EQUIP-SW          PIC X(01) VALUE "Y".
               88 NO-MORE-EQUIPMENT VALUE "N".
               88 MORE-EQUIPMENT VALUE "Y".
           05 MORE-LABS-SW          PIC X(01) VALUE "Y".
               88 NO-MORE-LABS VALUE "N".
               88 MORE-LABS VALUE "Y".
           05 ERROR-FOUND-SW           PIC X(01) VALUE "Y".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".
           05 FIRST-TIME-IN-SW           PIC X(01) VALUE "Y".
               88 FIRST-TREATMENT-READ VALUE "Y".
               88 NOT-FIRST-TIME  VALUE "N".
           05 PATIENT-TRMT-SW           PIC X(01) VALUE "N".
               88 NEW-PATIENT VALUE "Y".

       COPY ABENDREC.
      ** QSAM FILE

      * COPY DIAGCODE.
      ******************************************************************
      ***** DB2 TABLE DCLGENS
       01  DCLDIAG-CODES.
           10 DIAG-CODE                      PIC X(05).
           10 INS-TYPE                       PIC X(03).
           10 COPAY                          PIC S9(4) COMP.
           10 DEDUCTIBLE                     PIC S9(4) COMP.

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

       01  DCLMEDICATION.
           10 MEDICATION-ID                  PIC X(04).
           10 MED-NAME                       PIC X(08).
           10 SHORT-DESCRIPTION              PIC X(08).
           10 COST                           PIC S9(5)V99 COMP-3.
           10 PHARMACY-COST                  PIC S9(3)V99 COMP-3.

       COPY SQLCA.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-PATIENTS OR
      ******* Balancing logic put in by TGD 02/12/92
                   TRAILER-REC IN INPATIENT-DAILY-REC.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "******** BEGIN JOB DALYEDIT ********".
      *  DATE VALUES
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR  TO HDR-YY.
           MOVE WS-CURRENT-MONTH  TO HDR-MM.
           MOVE WS-CURRENT-DAY  TO HDR-DD.

           INITIALIZE COUNTERS-IDXS-AND-ACCUMULATORS, WS-TRAILER-REC.
           MOVE +1 TO WS-LINES.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
           PERFORM 900-READ-PATDATA THRU 900-EXIT.

           IF NO-MORE-PATIENTS
               MOVE "EMPTY PATIENT INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.
           PERFORM 920-READ-TRMTSRCH THRU 920-EXIT.

           IF NO-MORE-TREATMENTS
               MOVE "EMPTY TREATMENT INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
           IF MORE-PATIENTS
               PERFORM 200-NEW-PATIENT THRU 200-EXIT
               PERFORM 900-READ-PATDATA THRU 900-EXIT.
      *  Validate patient type and insurance coverage

      *    IF RECORD-ERROR-FOUND
      *        ADD +1 TO RECORDS-IN-ERROR
      *        PERFORM 710-WRITE-PATERR THRU 710-EXIT
      *    ELSE
      *        ADD +1 TO RECORDS-WRITTEN
      *        PERFORM 700-WRITE-PATEDIT THRU 700-EXIT.
       100-EXIT.
           EXIT.

       200-NEW-PATIENT.
           MOVE "200-NEW-PATIENT" TO PARA-NAME.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES,
                       PATIENT-TRMT-SW.
      *** SET UP PAGE HEADERS
           PERFORM 700-WRITE-PAGE-HDR   THRU 700-EXIT.
           PERFORM 720-WRITE-COLM-HDR   THRU 720-EXIT.
           PERFORM 740-WRITE-PAT-DETAIL THRU 740-EXIT.
           PERFORM 750-WRITE-PAT-EQUIP-DETAIL THRU 750-EXIT.


      ***PROCESS PATIENT TREATMENTS - CTRL+M TECHNIQUE
           IF ( (PATIENT-ID IN INPATIENT-TREATMENT-REC =
                 PATIENT-ID IN INPATIENT-DAILY-REC AND
                      PATIENT-ADMIT-COMMENTS > SPACES) OR
                 (PATIENT-ID IN INPATIENT-TREATMENT-REC =
                      PATIENT-ID IN INPATIENT-DAILY-REC AND
                      TEST-CATEGORY IN LAB-CHARGES (ROW-SUB) = 0 ) )
              MOVE "N" TO PATIENT-TRMT-SW
              PERFORM 300-PAT-TREATMENTS THRU 300-EXIT
              UNTIL NEW-PATIENT OR NO-MORE-TREATMENTS
           ELSE
           IF PATIENT-ID IN INPATIENT-TREATMENT-REC >
              PATIENT-ID IN INPATIENT-DAILY-REC
              GO TO 200-EXIT
           ELSE
              MOVE "** PAT-ID ERROR BETWEEN TRTMENT"
                 TO ERR-MSG-PAT
              PERFORM 799-WRITE-TRMTERR THRU 799-EXIT.

       200-EXIT.
           EXIT.

       300-PAT-TREATMENTS.
           MOVE "300-PAT-TREATMENTS" TO PARA-NAME.

           WRITE RPT-REC FROM WS-TRMT-HDR.
           MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC
                      TO MEDICATION-ID-O.
           MOVE MEDICATION-COST  TO MEDICATION-COST-O.
           MOVE PRESCRIBING-PHYS-ID IN INPATIENT-TREATMENT-REC
                 TO PRESCRIBING-PHYS-ID-O
           MOVE TREATMENT-COMMENTS TO TRMT-COMMENTS-O.
           PERFORM 760-WRITE-TRMT-DETAIL THRU 760-EXIT.

           MOVE "Y" TO MORE-LABS-SW.
           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
               ROW-SUB > 12 OR NO-MORE-LABS
               IF LAB-TEST-ID IN INPATIENT-TREATMENT-REC(ROW-SUB)
                          = SPACES
                MOVE "N" TO MORE-LABS-SW
               ELSE
                MOVE LAB-TEST-ID IN INPATIENT-TREATMENT-REC(ROW-SUB)
                             TO PATLISTEST-ID-O
                MOVE TEST-CATEGORY IN LAB-CHARGES (ROW-SUB)
                             TO TEST-CATEGORY-O
                MOVE TEST-COST IN LAB-CHARGES (ROW-SUB)
                            TO TEST-COST-O
                MOVE VENIPUNCTURE-COST (ROW-SUB) TO VENIPUNCTION-COST-O
                MOVE TEST-SHORT-DESC(ROW-SUB) TO TEST-SHORT-DESC-O
                 PERFORM 780-WRITE-LABTEST-DETAIL THRU 780-EXIT
               END-IF
           END-PERFORM.


      ****** WHEN PATIENT-IDS MATCH, PROCESS TREATMENTS FOR THIS PATIENT
           PERFORM 920-READ-TRMTSRCH THRU 920-EXIT.
           IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT =
              PATIENT-ID IN INPATIENT-DAILY-REC
              MOVE "Y" TO PATIENT-TRMT-SW.

       300-EXIT.
           EXIT.

       600-PAGE-BREAK.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           WRITE RPT-REC FROM WS-BLANK-LINE.
       600-EXIT.
           EXIT.

       700-WRITE-PAGE-HDR.
           MOVE "700-WRITE-PAGE-HDR" TO PARA-NAME.
           WRITE RPT-REC FROM WS-BLANK-LINE
               AFTER ADVANCING 1.
           MOVE WS-PAGES TO PAGE-NBR-O.
           WRITE RPT-REC FROM WS-HDR-REC
               AFTER ADVANCING NEXT-PAGE.
           MOVE ZERO TO WS-LINES.
           ADD +1 TO WS-PAGES.
           WRITE RPT-REC FROM WS-BLANK-LINE
               AFTER ADVANCING 1.
       700-EXIT.
           EXIT.

       720-WRITE-COLM-HDR.
           MOVE "720-WRITE-COLM-HDR" TO PARA-NAME.
           WRITE RPT-REC FROM WS-COLM-HDR-REC
               AFTER ADVANCING 2.

           ADD +1 TO WS-LINES.
       720-EXIT.
           EXIT.

       740-WRITE-PAT-DETAIL.
           MOVE "740-WRITE-PAT-DETAIL" TO PARA-NAME.
           IF WS-LINES > 45
              PERFORM 600-PAGE-BREAK THRU 600-EXIT.

           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
               PATIENT-ID-O, PATMSTR-KEY,
               PATINS-KEY, PATPERSN-KEY.

           READ PATMSTR INTO PATIENT-MASTER-REC.
           IF NOT PATMSTR-FOUND
              MOVE "** PATIENT NOT ON PATMSTR" TO ERR-MSG-PAT
              MOVE INPATIENT-DAILY-REC-SRCH TO
                   REST-OF-PAT-REC
              PERFORM 795-WRITE-PATERR THRU 795-EXIT
              GO TO 740-EXIT.

           READ PATINS INTO PATIENT-INSURANCE .
           IF NOT PATINS-FOUND
              MOVE "** PATIENT NOT ON PATINS" TO ERR-MSG-PAT
              MOVE INPATIENT-DAILY-REC-SRCH TO
                   REST-OF-PAT-REC
              PERFORM 795-WRITE-PATERR THRU 795-EXIT
              GO TO 740-EXIT.

           READ PATPERSN INTO PATIENT-PERSONAL-MASTER-REC.
           IF NOT PATPERSN-FOUND
              MOVE "** PATIENT NOT ON PATPERSN" TO ERR-MSG-PAT
              MOVE INPATIENT-DAILY-REC-SRCH TO
                   REST-OF-PAT-REC
              PERFORM 795-WRITE-PATERR THRU 795-EXIT
              GO TO 740-EXIT.

           MOVE LAST-NAME TO LAST-NAME-O
           MOVE MIDINIT TO MIDINIT-O
           MOVE FIRST-NAME TO FIRST-NAME-O

           MOVE PHONE-HOME TO PATIENT-PHONE-O.
           MOVE WARD-NBR IN INPATIENT-DAILY-REC TO
                WARD-NUMBER.
           MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC TO
                ROOM-NUMBER.
           MOVE BED-IDENTITY IN INPATIENT-DAILY-REC TO
                BED-IDENTITY-O.
           IF INPATIENT MOVE "IN" TO PATIENT-TYPE-O
           ELSE MOVE "OT" TO PATIENT-TYPE-O.
           MOVE DATE-ADMIT(1:2) TO ADMIT-DATE-O.
           MOVE DATE-ADMIT(4:2) TO ADMIT-DATE-O(3:2).
           MOVE DATE-ADMIT(9:2) TO ADMIT-DATE-O(5:2).
           MOVE DIAGNOSTIC-CODE-PRIMARY TO PRIMARY-DIAG-CODE-O.
           MOVE TOTAL-ROOM-CHARGE TO PATIENT-DAILY-CHARGES.
           MOVE COPAY IN PATIENT-MASTER-REC TO INS-COVERAGE-PERC-O.
           MOVE PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC TO
                PRIMARY-PHYS-O.
           MOVE INS-TYPE IN PATIENT-MASTER-REC TO
                INS-TYPE-O.

           WRITE RPT-REC FROM WS-PATIENT-RPT-REC
               AFTER ADVANCING 1.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +1 TO WS-LINES.
       740-EXIT.
           EXIT.

       750-WRITE-PAT-EQUIP-DETAIL.
           MOVE "750-WRITE-PAT-EQUIP-DETAIL" TO PARA-NAME.
           IF WS-LINES > 45
              PERFORM 600-PAGE-BREAK THRU 600-EXIT.

           WRITE RPT-REC FROM WS-EQUIP-HDR.

           MOVE "Y" TO MORE-EQUIP-SW.
           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
              ROW-SUB > 12 OR NO-MORE-EQUIPMENT
              IF EQUIPMENT-ID(ROW-SUB) = SPACES
               MOVE "N" TO MORE-EQUIP-SW
              ELSE
               MOVE EQUIPMENT-ID(ROW-SUB) TO EQUIP-ID-O
               MOVE EQUIPMENT-CATEGORY(ROW-SUB) TO EQUIP-CATEGORY
               MOVE EQUIPMENT-COST(ROW-SUB) TO EQUIP-COST
               MOVE EQUIPMENT-SHORT-DESC(ROW-SUB) TO EQUIP-SHORT-DESC-O
               WRITE RPT-REC FROM WS-EQUIP-RPT-REC
                        AFTER ADVANCING 1
                 ADD +1 TO WS-LINES
               END-IF
           END-PERFORM.

           WRITE RPT-REC FROM WS-BLANK-LINE
               AFTER ADVANCING 1.
           ADD +1 TO WS-LINES.
       750-EXIT.
           EXIT.

       760-WRITE-TRMT-DETAIL.
           MOVE "760-WRITE-TRMT-DETAIL" TO PARA-NAME.
           WRITE RPT-REC FROM WS-TRMT-RPT-REC
               AFTER ADVANCING 2.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           ADD +2 TO WS-LINES.
      ***     PERF

       760-EXIT.
           EXIT.

       780-WRITE-LABTEST-DETAIL.
           MOVE "780-WRITE-LABTEST-DETAIL" TO PARA-NAME.
           WRITE RPT-REC FROM WS-LABTEST-DETAIL
               AFTER ADVANCING 1.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           ADD +1 TO WS-LINES.
       780-EXIT.
           EXIT.

       790-CHECK-PAGINATION.
           MOVE "790-CHECK-PAGINATION" TO PARA-NAME.
           IF WS-LINES > 50
              PERFORM 600-PAGE-BREAK THRU 600-EXIT
              MOVE ZERO TO WS-LINES
              ADD +1 TO WS-LINES.
       790-EXIT.
           EXIT.

       795-WRITE-PATERR.
           MOVE "795-WRITE-PATERR" TO PARA-NAME.
           MOVE INPATIENT-DAILY-REC TO REST-OF-PAT-REC.
           WRITE INPATIENT-DAILY-REC-ERR.
           ADD +1 TO PAT-RECORDS-IN-ERROR.
       795-EXIT.
           EXIT.

       799-WRITE-TRMTERR.
           MOVE "799-WRITE-PATERR" TO PARA-NAME.
           MOVE INPATIENT-DAILY-REC TO REST-OF-TRMTERR-REC.
           WRITE INPATIENT-TREATMENT-REC-ERR.
           ADD +1 TO TRMT-RECORDS-IN-ERROR.
       799-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT PATSRCH, PATPERSN, PATINS, TRMTSRCH, PATMSTR.
           OPEN OUTPUT PATRPT, PATERR, TRMTERR, SYSOUT.
           DISPLAY PATMSTR-STATUS, PATINS-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.

           CLOSE PATSRCH, TRMTSRCH, PATRPT,
                 SYSOUT, PATERR, PATPERSN,
                 PATMSTR, PATINS, TRMTERR.
           DISPLAY PATMSTR-STATUS, PATINS-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       850-EXIT.
           EXIT.

       900-READ-PATDATA.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSRCH INTO INPATIENT-DAILY-REC
               AT END MOVE "N" TO MORE-PATDATA-SW
               GO TO 900-EXIT
           END-READ.

           ADD +1 TO PAT-RECORDS-READ.
       900-EXIT.
           EXIT.

       920-READ-TRMTSRCH.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTSRCH  INTO INPATIENT-TREATMENT-REC
               AT END MOVE "N" TO MORE-TRMT-SW
               GO TO 920-EXIT
           END-READ.

           IF NO-MORE-TREATMENTS AND TRMT-RECORDS-READ = 0
               MOVE "EMPTY TREATMENT INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
                    HOLD-TRMT-PATIENT-ID.

           ADD +1 TO TRMT-RECORDS-READ.
       920-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC IN PATIENT-RECORD-TYPE
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


           DISPLAY "** PATIENT RECORDS READ **".
           DISPLAY PAT-RECORDS-READ.
           DISPLAY "** PATIENT TREATMENT RECORDS READ **".
           DISPLAY  TRMT-RECORDS-READ.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB PATLIST ********".
       999-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB-PATLIST ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.