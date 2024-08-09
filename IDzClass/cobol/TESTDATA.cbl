       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTDATA.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
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

           SELECT PATDATA
           ASSIGN TO UT-S-PATDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTDATA
           ASSIGN TO UT-S-TRMTDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       to PATMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is DYNAMIC
                  RECORD KEY   is PATIENT-KEY
                  FILE STATUS  is PATMSTR-STATUS.

           SELECT PATINS
                  ASSIGN       to PATINS
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is DYNAMIC
                  RECORD KEY   is PATIENT-INS-KEY
                  FILE STATUS  is PATINS-STATUS.

           SELECT PRSNMSTR
                  ASSIGN       to PRSNMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is DYNAMIC
                  RECORD KEY   is PRSN-KEY
                  FILE STATUS  is PRSN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(120).

       FD  PATDATA
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-DATA.
       01  INPATIENT-DAILY-REC-DATA PIC X(993).

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

       FD  PATMSTR
           DATA RECORD IS PATMSTR-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

       FD  PRSNMSTR
           DATA RECORD IS PRSNMSTR-REC.
       01  PRSNMSTR-REC.
           05 PRSN-KEY      PIC X(06).
           05 FILLER           PIC X(794).

       FD  PATINS
           DATA RECORD IS PATINS-REC.
       01  PATINS-REC.
           05 PATIENT-INS-KEY      PIC X(06).
           05 FILLER               PIC X(696).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND    VALUE "00".
           05  PRSN-STATUS          PIC X(2).
               88 PRSN-FOUND    VALUE "00".
           05  PATINS-STATUS          PIC X(2).
               88 PATINS-FOUND    VALUE "00".
           05  OFCODE                  PIC X(2).
              88 CODE-WRITE    VALUE SPACES.

       01  TABLES.
           05 LNAMES-DOM.
              10 LINE1 PIC X(50) VALUE
           "SMITH     JONES     ADAMS     WEBSTER   HUDAK     ".
              10 LINE1 PIC X(50) VALUE
           "TREMBLAY  ROBERTO   WASHINGTONJEFFERSON LINCOLN   ".
              10 LINE1 PIC X(50) VALUE
           "BILLINGSLYGREENBERG COHEN     ODOM      BRITTON   ".
              10 LINE1 PIC X(50) VALUE
           "CHANDRU   NELSON    DELMONACO YOUNG     JETER     ".
              10 LINE1 PIC X(50) VALUE
           "RODDICK   WILLIAMS  FEDERER   GONAZALES TILDEN    ".
              10 LINE1 PIC X(50) VALUE
           "HOFFMAN   WILLIAMS  BROWN     JONES     DAVIS     ".
              10 LINE1 PIC X(50) VALUE
           "MILLER    WILSON    MOORE     TAYLOR    ANDERSON  ".
              10 LINE1 PIC X(50) VALUE
           "THOMAS    JACKSON   WHITE     HARRIS    MARTIN    ".
              10 LINE1 PIC X(50) VALUE
           "THOMPSON  GARCIA    MARTINEZ  ROBINSON  CLARK     ".
              10 LINE1 PIC X(50) VALUE
           "LEWIS     LEE       WALKER    HALL      ALLEN     ".
           05 LNAMES-DOM-RDF REDEFINES LNAMES-DOM.
              10 LN PIC X(10) OCCURS 50 TIMES.

           05 FNAMES-DOM.
              10 LINE1 PIC X(50) VALUE
           "JAMES     MICHELLE  KEN       SANJAY    WILLIAM   ".
              10 LINE2 PIC X(50) VALUE
           "BRIAN     GEORGE    LINDA     KIRK      DAVID     ".
              10 LINE3 PIC X(50) VALUE
           "EVERETT   NANCI     CHRIS     JAMISON   MARYELLEN ".
              10 LINE4 PIC X(50) VALUE
           "MARTHA    MATTHEW   CHARLES   PETER     JUSTINE   ".
              10 LINE5 PIC X(50) VALUE
           "IGOR      SOPHIA    MARIA     HUGO      MATHEUS   ".
              10 LINE6 PIC X(50) VALUE
           "BILLIE    LISA      VIJAY     JAMISON   EDWARD    ".
              10 LINE7 PIC X(50) VALUE
           "KELLY     KIRK      TYLER     AMANDA    GEORGETTE ".
              10 LINE8 PIC X(50) VALUE
           "KEVIN     MICHAEL   LEROY     HAYDEN    TODD      ".
              10 LINE9 PIC X(50) VALUE
           "HENRY     PAUL      BRUCE     DIANA     MEREDITH  ".
              10 LINE1A PIC X(50) VALUE
           "TIMOTHY   ANDRE     JOSEPH    PHILLIP   FRIEDA    ".
           05 FNAMES-DOM-RDF REDEFINES FNAMES-DOM.
              10 FN PIC X(10) OCCURS 50 TIMES.

           05 ADDRESS-DOM.
              10 LINE1 PIC X(45) VALUE
           "133 DEAN STREET122 BROOKFLD CT5940 TWO PINES ".
              10 LINE1 PIC X(45) VALUE
           "114 SOUTH MAIN 322 MIAMI BLVD 14 BELMONT AVE.".
              10 LINE1 PIC X(45) VALUE
           "3 ORCHARD PLACE980 BARHAM ROAD8 EAST MICHIGAN".
              10 LINE1 PIC X(45) VALUE
           "54 BELVEDERE ST764 ROUTE 44 NO5843 SOUTH 16TH".
              10 LINE1 PIC X(45) VALUE
           "84 WILLAMNTIC N643 CAPITAL AVE25-983 BROADWAY".
           05 ADDR-DOM-RDF REDEFINES ADDRESS-DOM.
              10 ADDR PIC X(15) OCCURS 15 TIMES.

           05 CITY-DOM.
              10 LINE1 PIC X(45) VALUE
           "RIDGEWOOD      GLEN ROCK      WAKE FOREST    ".
              10 LINE1 PIC X(45) VALUE
           "NEW YOUR CITY  SAN FRANCISCO  CHICAGO        ".
              10 LINE1 PIC X(45) VALUE
           "PHILADELPHIA   EAST LANSING   BOSTON         ".
              10 LINE1 PIC X(45) VALUE
           "LONDON         BANGALORE      MUMBAI         ".
              10 LINE1 PIC X(45) VALUE
           "BUENOS AIRES   MEXICO CITY    TORONTO        ".
           05 CITY-DOM-RDF REDEFINES CITY-DOM.
              10 CI PIC X(15) OCCURS 15 TIMES.

           05 TEST-DOM.
              10 LINE1 PIC X(48) VALUE
           "PULMBLODSPNLH1N1GASTLUNGNUCLRNALMISCPULMBLODSPNL".
              10 LINE2 PIC X(12) VALUE
           "PULMBLODSPNL".
           05 TEST-DOM-RDF REDEFINES TEST-DOM.
              10 TE PIC X(4) OCCURS 15 TIMES.

           05 STATE-DOM.
              10 LINE1 PIC X(56) VALUE
           "ALAKARCTNJNYDEWAHIORILLAAZMINMNDCADCPANHMAMERIVANCSCTXOK".
           05 STATE-DOM-RDF REDEFINES STATE-DOM.
              10 STAT1 PIC X(2) OCCURS 28 TIMES.

           05 DATE10-FR-DOM.
              10 LINE1 PIC X(50) VALUE
           "03-02-200803-17-200806-19-200806-24-200809-28-2008".
              10 LINE2 PIC X(50) VALUE
           "04-02-200804-17-200807-19-200807-24-200810-28-2008".
              10 LINE3 PIC X(50) VALUE
           "05-02-200805-17-200808-19-200808-24-200811-28-2008".
           05 DATE10-FR-DOM-RDF REDEFINES DATE10-FR-DOM.
              10 DF10 PIC X(10) OCCURS 15 TIMES.

           05 DATE10-TO-DOM.
              10 LINE1 PIC X(50) VALUE
           "03-04-200803-18-200806-22-200806-27-200809-29-2008".
              10 LINE2 PIC X(50) VALUE
           "04-05-200804-19-200807-24-200807-28-200810-30-2008".
              10 LINE3 PIC X(50) VALUE
           "05-06-200805-21-200808-25-200808-29-200811-30-2008".
           05 DATE10-TO-DOM-RDF REDEFINES DATE10-TO-DOM.
              10 DT10 PIC X(10) OCCURS 15 TIMES.

           05 DATE10-TO-DOM-DISCHARGE.
              10 LINE1 PIC X(50) VALUE
           "          03-18-2008          06-27-2008          ".
              10 LINE2 PIC X(50) VALUE
           "04-05-2008          07-24-2008          10-30-2008".
              10 LINE3 PIC X(50) VALUE
           "05-06-2008          08-25-2008                    ".
           05 DATE10-TO-DOM-RDF REDEFINES DATE10-TO-DOM-DISCHARGE.
              10 DT1D PIC X(10) OCCURS 15 TIMES.

           05 PHONE-DOM.
              10 LINE1 PIC X(50) VALUE
           "2123437897919232789720197843246097894324".
              10 LINE1 PIC X(50) VALUE
           "6758437647382765784736578438756843576876".
              10 LINE1 PIC X(50) VALUE
           "9875876548376598759877643287649874384097".
           05 PHONE-DOM-RDF REDEFINES PHONE-DOM.
              10 PHONE PIC 9(10) OCCURS 15 TIMES.

           05 DATE8-FR-DOM.
              10 LINE1 PIC X(40) VALUE
           "0102200802172008051920080824200810282008".
              10 LINE1 PIC X(40) VALUE
           "0202200803172008061920080924200811282008".
              10 LINE1 PIC X(40) VALUE
           "0302200804172008071920080824200812282008".
           05 DATE8-FR-DOM-RDF REDEFINES DATE8-FR-DOM.
              10 DF8 PIC X(08) OCCURS 15 TIMES.

           05 DATE8-TO-DOM.
              10 LINE1 PIC X(40) VALUE
           "0104200802232008053120080831200811028008".
              10 LINE1 PIC X(40) VALUE
           "0204200803272008062420080930200812029008".
              10 LINE1 PIC X(40) VALUE
           "0304200804232008071420080830200801004009".
           05 DATE8-TO-DOM-RDF REDEFINES DATE8-TO-DOM.
              10 DT8 PIC X(08) OCCURS 15 TIMES.

           05 ZIP-DOM.
              10 LINE1 PIC X(55) VALUE
           "9021006484074522758743243098438743229874764337823435792".
              10 LINE2 PIC X(55) VALUE
           "8756784736278567493878432678483927894678324563234388769".
           05 ZIP-DOM-RDF REDEFINES ZIP-DOM.
              10 ZI PIC X(5) OCCURS 22 TIMES.

           05 PLAN-DOM.
              10 LINE1 PIC X(45) VALUE
           "HMOPPOPOSHMOPPOPOSHMOPPOPOSHMOPPOPOSHMOPPOPOS".
           05 TEST-DOM-RDF REDEFINES PLAN-DOM.
              10 PL PIC X(3) OCCURS 15 TIMES.

           05 MODE-DOM.
              10 LINE1 PIC X(45) VALUE
           "ORAINVINJMRICATCHMRADSURPHYEQPLABVENOTHORAINV".
           05 MODE-DOM-RDF REDEFINES MODE-DOM.
              10 MO PIC X(3) OCCURS 15 TIMES.


           05 INS-DOM.
              10 LINE1 PIC X(60) VALUE
           "CIGNA                         TRAVELERS INSURANCE CORP".
              10 LINE2 PIC X(60) VALUE
           "AETNA                         MASSACHUSETTS MUTUAL".
              10 LINE3 PIC X(60) VALUE
           "PHOENIX HOME/LIFE/AUTO/HEALTH PROGRESSIVE INSURANCE".
              10 LINE4 PIC X(60) VALUE
           "KEMPER                        ALLSTATE INSURANCE".
              10 LINE5 PIC X(60) VALUE
           "NEW YORK LIFE & HEALTH        PENSACOLA MUTUAL".
              10 LINE6 PIC X(60) VALUE
           "MEDICARE                      SENTRY INSURANCE    ".
              10 LINE7 PIC X(60) VALUE
           "LIBERTY MUTUAL                FIDELITY HEALTH INS. ".
              10 LINE8 PIC X(60) VALUE
           "BANK OF BOSTON                PHILADELPHIA MUTUAL".
           05 INS-DOM-RDF REDEFINES INS-DOM.
              10 INS PIC X(30) OCCURS 16 TIMES.

      ****** Note - use WARD-DOM for Room and Bed-ID
           05 WARD-DOM.
              10 LINE1 PIC X(52) VALUE
           "0010201010100011000011112222333344445555666677778888".
              10 LINE2 PIC X(8) VALUE
           "00220033".
           05 WARD-DOM-RDF REDEFINES WARD-DOM.
              10 WA PIC X(4) OCCURS 15 TIMES.

           05 REASON-CODE-DOM.
              10 LINE1 PIC X(56) VALUE
           "AF67GHVCBDS-9F78DKF8I84-MF-M4-8A3-CVRE5TG-K83F-09RUR3-0O".
              10 LINE2 PIC X(4) VALUE  "87UY".
           05 REASON-CDE-DOM-RDF REDEFINES REASON-CODE-DOM.
              10 RESN PIC X(4) OCCURS 15 TIMES.

           05 EQUIP-DOM.
              10 LINE1 PIC X(56) VALUE
           "HEATAUTOSCOPDRIPMON SHNTMISCHEATAUTOSCOPDRIPMON SHNTMISC".
              10 LINE1 PIC X(4) VALUE
           "HEAT".
           05 EQUIP-DOM-RDF REDEFINES EQUIP-DOM.
              10 EQ PIC X(4) OCCURS 15 TIMES.

           05 REL-DOM.
              10 LINE1 PIC X(30) VALUE
           "SPSECHOTSPSECHOTSPSECHOTSPSECH".
           05 REL-DOM-RDF REDEFINES REL-DOM.
              10 RE PIC X(2) OCCURS 15 TIMES.

           05 OCCUPATION-DOM.
              10 LINE1 PIC X(50) VALUE
           "TEACHER   TAILOR    BAKER     SOFTWARE  ARTIST".
              10 LINE2 PIC X(50) VALUE
           "DATA ADMINGRAPHICS  EXECUTIVE PROJECT MNCHIROPRACT".
              10 LINE3 PIC X(50) VALUE
           "SOCIAL WRKPSYCHOLOGYADMINSTRATWRITER    STOCK BRKR".
           05 OCC-DOM-RDF REDEFINES OCCUPATION-DOM.
              10 OC PIC X(10) OCCURS 15 TIMES.


       01  PATIENT-PERSONAL-MASTER-REC.
           05  PATIENT-NBR-MR          PIC 9(6) VALUE 000001.
           05  SSNBR             PIC 9(10) VALUE 1238974324.
           05  AGE                     PIC 9(03) VALUE 28.
           05  DRIVERS-LICENSE-NO.
           15  DRIVERS-LICENSE-NO      PIC X(04) VALUE "DR-L".
           15  DL1                     PIC 9(2) VALUE 0.
           15  DRIVERS-LICENSE-NO2     PIC X(01) VALUE "-".
           15  DL2                     PIC 9(02) VALUE 0.
           15  FILLER                  PIC X(1) VALUE "T".
           05  ISSUING-STATE           PIC X(02) value "AL".
           05  OCCUPATON              PIC X(20) VALUE "TEACHER".
           05  EMPLOYER.
               10  EMP-NAME   PIC X(30) value "Royal Globe Insurance".
               10  EMP-ADDR   PIC X(30) value "15 Belmont Rd.".
               10  EMP-CITY   PIC X(30) value "Wake Forest".
               10  EMP-STATE  PIC X(02) value "NC".
               10  EMP-ZIP    PIC 9(09) value 243780832.
           05  MARITAL-STATUS PIC X(01) VALUE "M".
               88 MARRIED      VALUE "M".
               88 SINGLE       VALUE "S".
               88 DIVORCED     VALUE "D".
               88 WIDOWED      VALUE "W".
               88 VALID-STATUS
                   VALUES ARE "M", "S", "W", "D".
           05  PATIENT-NAME.
               10 FIRST-NAME   PIC X(15) VALUE "SMITH".
               10 MIDINIT      PIC X(02) VALUE  " ".
               10 LAST-NAME    PIC X(19) VALUE  "BRUCE".
           05  PHONE-HOME     PIC 9(10) VALUE  934089323.
           05  PHONE-WORK    PIC 9(10) VALUE 10894324.
           05  PHONE-MOBILE  PIC 9(10) VALUE 10897524.
           05  HEIGHT        PIC 9(02) VALUE 48.
           05  WEIGHT        PIC 9(03) VALUE 104.
           05  GENDER        PIC X(01) VALUE "F".
               88  FEMALE          VALUE "F".
               88  MALE            VALUE "M".
               88  NOT-PROVIDED    VALUE "N".
               88 VALID-GENDER
                   VALUES ARE "F", "M", "N".
           05  DOB                     PIC 9(05) VALUE 59178.
           05  FAMILY-CONTACT-PRIMARY  PIC X(30) VALUE "SPOUSE".
           05  FCON-RELATIONSHIP       PIC X(02) VALUE "SP".
               88  SPOUSE      VALUE "SP".
               88  SIBLING     VALUE "SI".
               88  CHILD       VALUE "CH".
               88  FRIEND      VALUE "FR".
               88 VALID-RELS
                   VALUES ARE "SP", "SI", "CH", "FR".
           05  MINOR-INDICATOR         PIC X(01) VALUE SPACES.
           05  RESPONSIBLE-PARTY.
               10  SSN           PIC 9(10) VALUE 578934324.
               10  OCCUPATION    PIC X(30) VALUE "TEACHER".
               10  EMPLOYER      PIC X(30) VALUE "RALEIGH SCHOOLS".
               10  CITY          PIC X(20) VALUE "GLEN ROCK".
               10  ST            PIC X(02) VALUE "OK".
               10  ZIP           PIC 9(09) VALUE 78943247.
           05  FCON-PHONE-H      PIC 9(10) VALUE 90843209.
           05  FCON-PHONE-C        PIC X(10) VALUE SPACE.
           05  PAYMENT-METHOD-TYPE     PIC X(02)  VALUE "CC".
               88 CREDIT-CARD      VALUE "CC".
               88 CHECK            VALUE "CH".
               88 CASH             VALUE "CA".
               88 VALID-PAYMENT-METHOD
                   VALUES ARE "CC", "CH", "CA".
           05  CREDIT-CARD-EXP-DATE.
               10  EXP-MONTH           PIC 9(02) VALUE 01.
               10  EXP-YEAR            PIC 9(04) VALUE 10.
           05  HOME-ADDRESS.
               10 APARTMENT-NBR PIC X(05).
               10 STREET        PIC X(30) VALUE "108 SPRING ST.".
               10 CITY          PIC X(20) VALUE "MURFREESBORO".
               10 STATE         PIC X(02) VALUE "TN".
               10 POSTAL-CODE   PIC 9(9) VALUE 194328904.
               10 COUNTRY              PIC X(20) VALUE "LATVIA".
           05  OCCUPATION              PIC X(30) VALUE "CUSTODIAN".
           05  EMPLOYER                PIC X(30) VALUE "LATVIAN GOVT".
           05  PATIENT-COMMENTS        PIC X(255) VALUE
             "THIS IS A PATIENT PERSONAL COMMENT....".

      * COPY TREATMNT.
       01  INPATIENT-TREATMENT-REC.
           05  RECORD-TYPE             PIC X(01) value " ".
               88  TRAILER-REC        VALUE "T".
           05  PATIENT-ID-t      PIC 9(6) value 000001.
           05  TREATMENT-DATE-TIME.
               10 TREATMENT-DATE       PIC X(08) VALUE "01011998".
               10 FILLER               PIC X     VALUE "-".
               10 TREATMENT-TIME       PIC X(08) VALUE "01.02.03".
               10 FILLER               PIC X(09) VALUE "-00000001".
           05  BED-IDENTITY            PIC 9(4)  VALUE 1111.
           05  PRIMARY-DIAGNOSTIC-CODE PIC X(4)  VALUE "DIAG".
           05  PRIMARY-DIAGNOSTIC-CDE-n PIC 9  VALUE 1.
           05  MEDICATION-ID           PIC X(7)  VALUE "MEDI-00".
           05  MEDICATION-ID-N         PIC 9(1)  VALUE 1.
           05  TREATMENT-MODE          PIC X(03) VALUE "ORA".
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
           05  BILLABLE-TREATMENT-IND   PIC X(01) VALUE "N".
               88  NON-BILLABLE         VALUE "N".
               88  BILLABLE             VALUE "B".
               88 VALID-BILLABLE-TYPES
                   VALUES ARE "N", "B".
           05  MEDICATION-COST         PIC 9(5)V99 VALUE 0002.09.
           05  ATTENDING-PHYS-ID       PIC X(07) VALUE "PHYS-00".
           05  ATTENDING-PHYS-ID-N     PIC 9(01) VALUE 2.
           05  PRESCRIBING-PHYS-ID     PIC X(07) VALUE "PHYS-00".
           05  PRESCRIBING-PHYS-ID-N   PIC 9(01) VALUE 002.
           05  SUPERVISOR-NURSE-ID     PIC X(07) VALUE "NURS-00".
           05  SUPERVISOR-NURSE-ID-N   PIC 9(01) VALUE 007.
           05  TREATMENT-NURSE-ID      PIC X(07) VALUE "NURS-00".
           05  TREATMENT-NURSE-ID-N    PIC 9(01) VALUE 002.
           05  PHARMACY-COST           PIC 9(3)V99 VALUE 01.41.
           05  ANCILLARY-CHARGE        PIC 9(3)V99 VALUE 21.05.
           05  LAB-CHARGES OCCURS 12 TIMES.
               10  LAB-TEST-ID         PIC X(07) VALUE "LABT-00".
               10  LAB-TEST-ID-N       PIC 9(01) VALUE 003.
               10  TEST-CATEGORY       PIC X(04) VALUE "PULM".
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
               10  TEST-SHORT-DESC         PIC X(25)
                    VALUE "Test short description".
               10  TEST-COST               PIC 9(5)V99 value 00219.03.
               10  VENIPUNCTURE-COST       PIC 9(3)V99 value 012.31.
               10  PRESCRIBING-PHYS        PIC X(07) VALUE "PHYS-00".
               10  PRESCRIBING-PHYS-n      PIC 9(01) value 002.
               10  DIAG-CDE                PIC X(04) value "DIAG".
               10  DIAG-CDE-n              PIC 9 value 1.
           05  TREATMENT-COMMENTS      PIC X(254)
           Value "Detailed Patient Treatment and Lab test Comments...".

       01  INPATIENT-DAILY-REC.
           05  PATIENT-RECORD-TYPE     PIC X(01) value " ".
               88  TRAILER-REC     VALUE "T".
           05  PATIENT-ID-d            PIC 9(6) value 000001.
           05  CURR-DTE                PIC X(08) value "12222008".
           05  BED-IDENTITY             PIC 9(4) value 1111.
           05  ROOM-IDENTITY           PIC 9(4) value 2222.
           05  TOTAL-ROOM-CHARGE       PIC 9(7)V99 value 00001894.28.
           05  BASE-ROOM-CHARGE        PIC 9(7)V99 value 0000668.00.
           05  ROOM-DATE-FROM          PIC X(08) value "12132008".
           05  ROOM-DATE-TO            PIC X(08) value "12182008".
           05  PRIMARY-DIAGNOSTIC-CODE  PIC X(4) value "DIAG".
           05  PRIMARY-DIAGNOSTIC-CODE-n PIC 9 value 1.
           05  WARD-NBR                PIC X(4) value "0011".
               88  INTENSIVE-CARE  VALUE "0010".
               88  OBSTETRICS      VALUE "2010".
               88  PEDIATRICS      VALUE "1010".
               88  ONCOLOGY        VALUE "0011".
               88  CARDIO-THORACIC VALUE "0110".
               88  GENERAL         VALUE "0000".
               88  VALID-WARD VALUES ARE
               "0010", "2010", "1010", "0011", "0110", "0000".
           05  ADDITIONAL-EQUIP-CHARGES OCCURS 12 TIMES.
               10  EQUIPMENT-ID            PIC X(07) VALUE "EQUIP-0".
               10  EQUIPMENT-ID-n          PIC 9(01) VALUE 001.
               10  EQUIPMENT-CATEGORY      PIC X(04) value "MISC".
                   88 HEATING-PAD   VALUE "HEAT".
                   88 AUTOCLAVE     VALUE "AUTO".
                   88 SCOPE         VALUE "SCOP".
                   88 DRIP          VALUE "DRIP".
                   88 MONITOR       VALUE "MON ".
                   88 SHUNT         VALUE "SHNT".
                   88 MISCELLANEOUS VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "HEAT", "AUTO",
                      "SCOP", "DRIP", "MON ", "SHNT", "MISC".
               10  EQUIPMENT-SHORT-DESC    PIC X(30)
                    VALUE "Equipment short description..." .
               10  EQUIPMENT-COST          PIC 9(5)V99 value 00111.48.
               10  EQUIPMENT-PRES-PHYS     PIC X(07) value "PHYS-00".
               10  EQUIPMENT-PRES-PHYS-N   PIC 9(01) value 005.
               10  EQUIPMENT-REASON-CDE    PIC X(04) value "AI7J".
           05  DAILY-CHARGES-COMMENTS      PIC X(255) value
            "These are detailed equipment daily charges comments.....".

      * COPY PTMSTR.
      ** VSAM FILE
       01  PATIENT-MASTER-REC.
           05  PATIENT-ID-M                    PIC 9(6) value 000001.
           05  PATIENT-TYPE                    PIC X(1) value "I".
               88 INPATIENT   VALUE "I".
               88 OUTPATIENT  VALUE "0".
               88 VALID-TYPE  VALUES ARE "I", "O".
           05  PREVIOUS-PATIENT-IND            PIC X(01) value "Y".
               88 PREV-PATIENT         VALUE "Y".
               88 NOT-PREVE-PATIENT    VALUE "N".
               88 VALID-PREV-IND  VALUES ARE "Y", "N".
           05  PRIMARY-STAY-WARD-NBR           PIC X(4) value "2010".
               88  INTENSIVE-CARE  VALUE "0010".
               88  OBSTETRICS      VALUE "2010".
               88  PEDIATRICS      VALUE "1010".
               88  ONCOLOGY        VALUE "0011".
               88  CARDIO-THORACIC VALUE "0110".
               88  GENERAL         VALUE "0000".
               88  VALID-WARD VALUES ARE
                   "0010", "2010", "1010", "0011", "0110", "0000".
           05  BED-IDENTITY-PRIMARY            PIC 9(4) value 1111.
           05  DATE-ADMIT             PIC X(10) value "01-02-2008".
           05  DATE-DISCHARGE         PIC X(10)  value "01-09-2008".
           05  ATTENDING-PHYSICIAN          PIC X(07) value "PHYS-00".
           05  ATTENDING-PHYSICIAN-n           PIC 9(01) value 003.
           05  DIAGNOSTIC-CODE-PRIMARY         PIC X(04) VALUE "DIAG".
           05  DIAGNOSTIC-CODE-PRIMARY-n       PIC 9(01) value 1.
           05  DIAGNOSTIC-CODE-SECONDARY       PIC X(05) value "DGG87".
           05  DIAGNOSTIC-CODE-TERTIARY        PIC X(05) value "KJD89".
           05  INS-TYPE                        PIC X(3) value "MAJ".
               88 VALID-INS-TYPE VALUES ARE "HMO", "PPO", "POS" "MAJ".
           05  HOSPITAL-STAY-LTH               PIC 999 value 111.
           05  PATIENT-TOT-AMT            PIC 9(7)V99 value 0001041.83.
           05  PRIMARY-CARE-PHYSICIAN-ID   PIC X(7) value "PHYS-00".
           05  PRIMARY-CARE-PHYSICIAN-ID-n       PIC 9(1) value 001.
           05  IN-OUT-NETWORK                  PIC X(1) value "N".
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                           PIC S9(3) value 0011.
           05  REMAINING-DEDUCTIBLE            PIC S9(4) value 0013.
           05  HIPAA-FORM-SIGNED-IND           PIC X(01) value "Y".
               88 HIPAA-SIGNED       VALUE "Y".
               88 HIPAA-UNSIGNED     VALUE "N".
           05  PATIENT-ADMIT-COMMENTS          PIC X(254) value
            "Patient admit comments on VSAM file for all Patients".
           05  DAILY-LAB-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  LAB-TEST-S-ID          PIC X(07) value "LABT-00".
               10  LAB-TEST-S-ID-n             PIC 9(01) value 002.
               10  LAB-TEST-DATE         PIC X(08) value "01022009".
               10  TEST-SHORT-S-DESC         PIC X(25)
                   value "Lab test short descript. ".
               10  TEST-DIAG-CODE            PIC X(4) value "DIAG".
               10  TEST-DIAG-CODE-n            PIC 9(1) value 1.
               10  TEST-CHARGES              PIC 9(7)V99.
               10  PRESCRIBING-S-PHYS-ID   PIC X(07) value "PHYS-00".
               10  PRESCRIBING-S-PHYS-ID-n     PIC 9(01) value 002.
           05  EQUIPMENT-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  EQUIPMENT-S-ID         PIC X(07) value "EQUIP".
               10  EQUIPMENT-S-ID-n            PIC 9(01) value 004.
               10  EQUIPMENT-CHARGE-DATE PIC X(08) value "01022009".
               10  EQUIP-DIAG-CODE           PIC X(4) value "DIAG".
               10  EQUIP-DIAG-CODE-n           PIC 9(1) value 1.
               10  EQUIPMENT-S-SHORT-DESC    PIC X(30)
                   value "Equipment short description...".
               10  EQUIPMENT-CHARGES      PIC 9(7)V99 value 000174.17.
               10  EQUIPMENT-PRES-PHYS-ID  PIC X(07) value "PHYS-00".
               10  EQUIPMENT-PRES-PHYS-ID-n    PIC 9(01) value 007.

       01  WS-SYSOUT-REC.
           05  MSG                     PIC X(80).

       77  WS-DATE                     PIC 9(6).
       77  WS-TIMEstamp                PIC x(21).

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-WRITTEN          PIC 9(7) COMP.
           05 RECORDS-IN-ERROR         PIC 9(7) COMP.
           05 RECORDS-READ             PIC 9(7) COMP.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 TMP-VAL1                 PIC S9(3)V99 COMP-3 VALUE 1.38.
           05 TMP-VAL2                 PIC S9(3)V99 COMP-3.
           05 TMP-VAL5                 PIC S9(4)V99 COMP-3.
           05 TMP-VAL4                 PIC S9(3)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.
           05 TMP-VAL3                 PIC 9 VALUE 0.
           05 APT-TMP                  PIC 9.
           05 TMP-PICX                 PIC X(30).
           05 TMP-ID1                  PIC 999 VALUE 0.
           05 TMP-ID2                  PIC 999 VALUE 0.
           05 TMP-ID3                  PIC 9.
           05 TMP-ID4                  PIC 9.
           05 TMP-ID5                  PIC 99.

       01  MISC-WS-FLDS.
           05 STR-LTH                  PIC 9(04) VALUE 0.
           05 RETURN-CD                PIC S9(04) VALUE 0.
           05 ROW-SUB                  PIC 9(04) COMP value 0.
           05 HOLD-SUB2                PIC 9(04) COMP value 15.
           05 EQUIP-SUB                PIC 9(04) COMP.
           05 EQUIP-SUB2               PIC 9(04) COMP VALUE 16.
           05 PMAST-SUB                PIC 9(04) COMP VALUE 0.
           05 HOLD-TRMT                PIC 9(04) COMP VALUE 0.
           05 NAME-SUB                 PIC 9(04) COMP VALUE 0.
           05 NAME-SUB2                PIC 9(04) COMP VALUE 51.

       01  FLAGS-AND-SWITCHES.
           05 MORE-DATA-SW             PIC X(01) VALUE "Y".
               88 NO-MORE-DATA VALUE "N".
           05 ERROR-FOUND-SW           PIC X(01) VALUE "N".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".
           05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".
               88 NO-MORE-TABLE-ROWS VALUE "N".
       01  PATIENT-INSURANCE.
           05  INS-COMPANY-PRIMARY.
               10  PATIENT-ID-I            PIC 9(6) VALUE 000001.
               10  INS-COMPANY-PRIMARY-ID  PIC X(7) VALUE "INS-000".
               10  INS-COMPANY-PRIMARY-ID-N  PIC 9(1) VALUE 1.
               10  CARRIER-NAME            PIC X(30) VALUE "CIGNA".
               10  CARRIER-PHONE-I         PIC 9(10) VALUE 129011209.
               10  CARRIER-FAX-I           PIC 9(10) VALUE 132432419.
               10  INSURED-NAME.
                   15 INS-F                PIC X(10).
                   15 FILLER               PIC X(1) VALUE SPACE.
                   15 INS-M                PIC X(1).
                   15 FILLER               PIC X(1) VALUE SPACE.
                   15 INS-L                PIC X(17).
               10  INSURED-GENDER          PIC X(01) VALUE "M".
                   88  FEMALE          VALUE "F".
                   88  MALE            VALUE "M".
                   88  NOT-PROVIDED    VALUE "N".
                   88 VALID-GENDER
                       VALUES ARE "F", "M", "N".
               10  PATIENT-RELATIONSHIP    PIC X(02) VALUE "SE".
                   88  SPOUSE          VALUE "SP".
                   88  SELF-REL        VALUE "SE".
                   88  CHILD           VALUE "CH".
                   88  OTHER-REL       VALUE "OT".
                   88 VALID-RELS
                       VALUES ARE "SP", "SE", "SP", "OT".
               10  INS-GRP.
               15  INS-IDENT-NBR   PIC X(12) VALUE "GBINS-2FD-T-".
               15  INSI1           PIC 9.
               15  INSI2           PIC X(6) VALUE "IX8I-A".
               15  INSI3           PIC 9.
               10  GRP-GRP.
               15  GROUP-NBR          PIC X(4) VALUE "GRP-".
               15  GN1                PIC 9(3).
               15  GN2                PIC X(2) VALUE "D-".
               15  GN3                PIC 9.
               10  ADDRESS-CLAIMS.
                   15  STREET        PIC X(30) VALUE "11 MAIN STREET".
                   15  CITY       PIC X(20) VALUE "RIDGEWOOD".
                   15  STATE               PIC X(02) VALUE "NJ".
                   15  ZIP-INS              PIC 9(5).
                   15  ZIN                  PIC 9(4).
               10  RETIRED-IND    PIC X(01) VALUE "N".
                   88 RETIRED          VALUE "Y".
                   88 NOT-RETIRED      VALUE "N".
                   88 VALID-RET-IND  VALUES ARE "Y", "N".
           05  INS-COMPANY-SECONDARY VALUE SPACES.
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
           05  BENEFIT-ASSIGNMENT-DETAILS VALUE SPACES.
               10  MEDICARE-BENEFICIARY    PIC X(30).
               10  MEDICARE-CLAIM-NBR      PIC X(15).
               10  COMMERCIAL-BENEFICIARY  PIC X(30).
               10  COMMERCIAL-CLAIM-NBR    PIC 9(15).
           05  PAT-INSURANCE-COMMENTS      PIC X(100) VALUE
              "THIS IS AN INSURANCE PATIENT COMMENT...".
           05  FILLER           PIC X(142) VALUE SPACES.

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1) VALUE "T".
           05  IN-RECORD-COUNT         PIC 9(9) VALUE 2001.
           05  FILLER                  PIC X(1) VALUE " ".
           05  IN-MEDICATION-CHARGES PIC S9(9)V99
                      VALUE 0.
           05  IN-PHARMACY-CHARGES  PIC S9(7)V99
                      VALUE 0.
           05  IN-ANCILLARY-CHARGES PIC S9(5)V99
                      VALUE 0.

       77  HOLD-SUB                    PIC S9(4) COMP VALUE 0.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           DISPLAY "BEGIN TRTMNT RECORDS ROUTINE".
           PERFORM 300-TRTMNT THRU 300-EXIT
                varying row-sub from 1 by 1 until row-sub > 2000.
           DISPLAY "BEGIN PATDAILY RECORDS ROUTINE".
           PERFORM 400-PATDATA  THRU 400-EXIT
                varying row-sub from 1 by 1 until row-sub > 1000.
           DISPLAY "BEGIN PATIENT MASTER RECORDS ROUTINE".
           PERFORM 500-PATMASTR  THRU 500-EXIT
                varying row-sub from 1 by 1 until row-sub > 1000.
           DISPLAY "BEGIN PAT INSURANCE RECORDS ROUTINE".
           PERFORM 600-PATINS  THRU 600-EXIT
                varying row-sub from 1 by 1 until row-sub > 1000.
           DISPLAY "BEGIN PRSNMSTR RECORDS ROUTINE".
           PERFORM 700-PATPERSN THRU 700-EXIT
                varying row-sub from 1 by 1 until row-sub > 1000.

           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           open output  TRMTDATA, PATDATA.
      ****** NOTE EXPECTS EMPTY VSAM FILES
           OPEN OUTPUT PATINS.
           DISPLAY "STATUS-FOR VSAM FILES...".
           DISPLAY PATINS-STATUS.
           OPEN OUTPUT    PATMSTR.
           OPEN OUTPUT PRSNMSTR.
           DISPLAY PATMSTR-STATUS.
           DISPLAY PRSN-STATUS.
           DISPLAY "END OF VSAM OPEN STATUS".
           IF PATMSTR-STATUS GREATER "00"
              OR PATINS-STATUS GREATER "00"
              OR PRSN-STATUS GREATER "00"
              GOBACK.
       000-EXIT.
           EXIT.

       300-TRTMNT.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.
           SUBTRACT 1 FROM HOLD-SUB2.
           IF HOLD-SUB2 < 1 MOVE +15 TO HOLD-SUB2.
           ADD +1 TO HOLD-SUB.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.

           IF ROW-SUB > 1000
               SUBTRACT 1000 FROM ROW-SUB GIVING HOLD-TRMT
           ELSE
               MOVE ROW-SUB TO HOLD-TRMT.

           MOVE HOLD-TRMT TO PATIENT-ID-T, PRIMARY-DIAGNOSTIC-CDE-n,
           MEDICATION-ID-N ATTENDING-PHYS-ID-N  PRESCRIBING-PHYS-ID-N
           SUPERVISOR-NURSE-ID-N  TREATMENT-NURSE-ID-N
           LAB-TEST-ID-N(1)
           DIAG-CDE-n(1)
           PRESCRIBING-PHYS-n(1).
           MOVE HOLD-SUB TO TMP-ID3.
           MOVE TMP-ID3 TO
           LAB-TEST-ID-N(2)
           DIAG-CDE-n(2)
           PRESCRIBING-PHYS-n(2).
           MOVE HOLD-SUB2 TO TMP-ID4.
           MOVE TMP-ID4 TO
           LAB-TEST-ID-N(3)
           DIAG-CDE-n(3)
           PRESCRIBING-PHYS-n(3).
           MOVE HOLD-SUB2 TO TMP-ID5.
           DIVIDE 3 INTO TMP-ID5 GIVING TMP-ID4 REMAINDER TMP-ID3.
           MOVE TMP-ID3 TO
           LAB-TEST-ID-N(4)
           DIAG-CDE-n(4)
           PRESCRIBING-PHYS-n(4).
           DIVIDE 2 INTO HOLD-SUB2 GIVING TMP-ID4 REMAINDER TMP-ID3.
           MOVE TMP-ID4 TO
           LAB-TEST-ID-N(5)
           DIAG-CDE-n(5)
           PRESCRIBING-PHYS-n(5).
           ADD +7 TO TMP-ID5.
           DIVIDE 3 INTO TMP-ID5 GIVING TMP-ID4 REMAINDER TMP-ID3.
           MOVE TMP-ID3 TO
           LAB-TEST-ID-N(6)
           DIAG-CDE-n(6)
           PRESCRIBING-PHYS-n(6).
           MOVE TMP-ID4 TO
           LAB-TEST-ID-N(7)
           DIAG-CDE-n(7)
           PRESCRIBING-PHYS-n(7).
           MOVE HOLD-SUB TO TMP-ID3.
           MOVE TMP-ID3 TO
           LAB-TEST-ID-N(8)
           DIAG-CDE-n(8)
           PRESCRIBING-PHYS-n(8).

           MOVE SPACES TO  LAB-CHARGES(9), LAB-CHARGES(10),
                  LAB-CHARGES(11), LAB-CHARGES(12).

           COMPUTE TMP-VAL2 = TMP-VAL1 * ROW-SUB.
           ADD TMP-VAL2 TO
           MEDICATION-COST PHARMACY-COST ANCILLARY-CHARGE
           TEST-COST(1)
           VENIPUNCTURE-COST(1)
           TEST-COST(2)
           VENIPUNCTURE-COST(2)
           TEST-COST(3)
           VENIPUNCTURE-COST(3)
           TEST-COST(4)
           VENIPUNCTURE-COST(4)
           TEST-COST(5)
           VENIPUNCTURE-COST(5)
           TEST-COST(6)
           VENIPUNCTURE-COST(6)
           TEST-COST(7)
           VENIPUNCTURE-COST(7)
           TEST-COST(8)
           VENIPUNCTURE-COST(8).

           COMPUTE TMP-VAL4 = ( TMP-VAL2 * .19 ).

           SUBTRACT TMP-VAL4 FROM
           TEST-COST(1)
           VENIPUNCTURE-COST(1)
           TEST-COST(5)
           VENIPUNCTURE-COST(5).

           COMPUTE TMP-VAL4 = ( TMP-VAL2 * 1.03 ).

           SUBTRACT TMP-VAL4 FROM
           TEST-COST(2)
           VENIPUNCTURE-COST(2)
           TEST-COST(7)
           VENIPUNCTURE-COST(7).

           MOVE DF8(HOLD-SUB) TO  TREATMENT-DATE.
           MOVE MO(HOLD-SUB) TO TREATMENT-MODE.
           MOVE WA(HOLD-SUB) TO BED-IDENTITY IN INPATIENT-TREATMENT-REC

           IF HOLD-SUB < 13 MOVE "B" TO BILLABLE-TREATMENT-IND
           ELSE MOVE "N" TO BILLABLE-TREATMENT-IND.
           MOVE TE(HOLD-SUB) TO
           TEST-CATEGORY(1),
           TEST-CATEGORY(3)
           TEST-CATEGORY(5)
           TEST-CATEGORY(7).
           MOVE TE(HOLD-SUB2) TO
           TEST-CATEGORY(2)
           TEST-CATEGORY(4)
           TEST-CATEGORY(6)
           TEST-CATEGORY(8).

           IF ANCILLARY-CHARGE > 500
               COMPUTE ANCILLARY-CHARGE = 6.14 + TMP-VAL2 / 17.

           IF PHARMACY-COST > 400
               COMPUTE PHARMACY-COST = 7.82 + TMP-VAL2 / 17.

           IF MEDICATION-COST > 8000
               COMPUTE MEDICATION-COST = 7.31 + TMP-VAL2 / 17.

           IF ANCILLARY-CHARGE < 1.01
               COMPUTE ANCILLARY-CHARGE = 3.69 + TMP-VAL2.

           IF PHARMACY-COST < 1.01
               COMPUTE PHARMACY-COST = 17.28 + TMP-VAL2.

           IF MEDICATION-COST < 1.01
               COMPUTE MEDICATION-COST = 5.62 + TMP-VAL2.

           ADD MEDICATION-COST IN INPATIENT-TREATMENT-REC
                    TO IN-MEDICATION-CHARGES.
           ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC
                    TO IN-PHARMACY-CHARGES.
           ADD ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC
                    TO IN-ANCILLARY-CHARGES.

           if TEST-COST(1) > 1001 compute test-cost(1)
                  = VENIPUNCTURE-COST(8) + ANCILLARY-CHARGE.
           if TEST-COST(2) > 599 compute test-cost(2)
                  = VENIPUNCTURE-COST(7) + MEDICATION-COST.
           if TEST-COST(3) > 200 compute test-cost(3)
                  = VENIPUNCTURE-COST(6) + PHARMACY-COST - HOLD-SUB2.
           if TEST-COST(4) > 299 compute test-cost(4)
                  = VENIPUNCTURE-COST(5) + ANCILLARY-CHARGE - HOLD-SUB.
           if TEST-COST(5) > 400 compute test-cost(5)
                  = VENIPUNCTURE-COST(4) + MEDICATION-COST.
           if TEST-COST(6) > 311 compute test-cost(6)
                 = VENIPUNCTURE-COST(3) + ANCILLARY-CHARGE - HOLD-SUB2.
           if TEST-COST(7) > 400 compute test-cost(7)
                 = VENIPUNCTURE-COST(2) + MEDICATION-COST.
           if TEST-COST(8) > 175 compute test-cost(8)
                     = VENIPUNCTURE-COST(1) + PHARMACY-COST - HOLD-SUB.



           PERFORM 730-WRITE-TRMT THRU 730-EXIT.
       300-EXIT.
           EXIT.
       400-PATDATA.
           ADD +1 TO HOLD-SUB.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.
           SUBTRACT 1 FROM HOLD-SUB2.
           IF HOLD-SUB2 < 1 MOVE +15 TO HOLD-SUB2.
           MOVE ROW-SUB TO PATIENT-ID-d, PRIMARY-DIAGNOSTIC-CODE-n,
           EQUIPMENT-ID-n(1)
           EQUIPMENT-PRES-PHYS-N(1).
           MOVE TMP-ID3 TO
           EQUIPMENT-ID-n(2)
           EQUIPMENT-PRES-PHYS-N(2).
           MOVE TMP-ID4 TO
           EQUIPMENT-ID-n(3)
           EQUIPMENT-PRES-PHYS-N(3)
           MOVE HOLD-SUB2 TO TMP-ID3.
           MOVE TMP-ID3 TO
           EQUIPMENT-ID-n(4)
           EQUIPMENT-PRES-PHYS-N(4).
           MOVE HOLD-SUB2 TO TMP-ID4.
           MOVE TMP-ID4 TO
           EQUIPMENT-ID-n(5)
           EQUIPMENT-PRES-PHYS-N(5).
           ADD +7 TO TMP-ID5.
           DIVIDE 3 INTO TMP-ID5 GIVING TMP-ID4 REMAINDER TMP-ID3.
           MOVE TMP-ID3 TO
           EQUIPMENT-ID-n(6)
           EQUIPMENT-PRES-PHYS-N(6).
           MOVE HOLD-SUB2 TO TMP-ID4.
           MOVE TMP-ID4 TO
           EQUIPMENT-ID-n(7)
           EQUIPMENT-PRES-PHYS-N(7).
           MOVE HOLD-SUB TO TMP-ID3.
           MOVE TMP-ID3 TO
           EQUIPMENT-ID-n(8)
           EQUIPMENT-PRES-PHYS-N(8).

           MOVE SPACES TO  ADDITIONAL-EQUIP-CHARGES(9),
           ADDITIONAL-EQUIP-CHARGES(10),
           ADDITIONAL-EQUIP-CHARGES(11), ADDITIONAL-EQUIP-CHARGES(12).

           MOVE ZERO TO tmp-val1, tmp-val2
                   TOTAL-ROOM-CHARGE, BASE-ROOM-CHARGE.

           MOVE ROW-SUB TO EQUIP-SUB.

           IF EQUIP-SUB2 > 80 MOVE 9 TO EQUIP-SUB2.

           IF EQUIP-SUB > 80
              ADD +1 TO EQUIP-SUB2
              MOVE EQUIP-SUB2 TO EQUIP-SUB.

           COMPUTE TMP-VAL2 = TMP-VAL1 + EQUIP-SUB + 43.13.

           MOVE TMP-VAL2 to
           EQUIPMENT-COST(1)
           EQUIPMENT-COST(5).

           SUBTRACT 1.97 FROM TMP-VAL2.
           move TMP-VAL2  to
           EQUIPMENT-COST(2)
           EQUIPMENT-COST(8) .

           add 3.49 to tmp-val2.
           MOVE TMP-VAL2 to
           EQUIPMENT-COST(3)
           EQUIPMENT-COST(6).

           SUBTRACT 9.64 FROM TMP-VAL2.
           move TMP-VAL2  to
           EQUIPMENT-COST(4)
           EQUIPMENT-COST(7).

           COMPUTE TOTAL-ROOM-CHARGE =
           ( BASE-ROOM-CHARGE +
           EQUIPMENT-COST(1) +
           EQUIPMENT-COST(2) +
           EQUIPMENT-COST(3) +
           EQUIPMENT-COST(4) +
           EQUIPMENT-COST(5) +
           EQUIPMENT-COST(6) +
           EQUIPMENT-COST(7) +
           EQUIPMENT-COST(8) ).

           if EQUIPMENT-COST(1) > 1001 compute EQUIPMENT-COST(1)
                     = BASE-ROOM-CHARGE + hold-sub + .19.
           if EQUIPMENT-COST(2) > 990 compute EQUIPMENT-COST(2)
                     = BASE-ROOM-CHARGE + hold-sub2 + .74.
           if EQUIPMENT-COST(3) > 1200 compute EQUIPMENT-COST(3)
                     = BASE-ROOM-CHARGE - (hold-sub + 1.09).
           if EQUIPMENT-COST(4) > 600 compute EQUIPMENT-COST(4)
                     = BASE-ROOM-CHARGE - (hold-sub2 + 6.98).
           if EQUIPMENT-COST(5) > 1001 compute EQUIPMENT-COST(5)
                     = BASE-ROOM-CHARGE + (hold-sub * 5) + .19.
           if EQUIPMENT-COST(6) > 990 compute EQUIPMENT-COST(6)
                     = BASE-ROOM-CHARGE + (hold-sub2 * 4.2) + 12.
           if EQUIPMENT-COST(7) > 499 compute EQUIPMENT-COST(7)
                     = BASE-ROOM-CHARGE - (hold-sub * 1.13) + 1.09.
           if EQUIPMENT-COST(8) > 295 compute EQUIPMENT-COST(8)
                     = BASE-ROOM-CHARGE - (hold-sub2 + 6.98).

           IF BASE-ROOM-CHARGE > 800 OR
              BASE-ROOM-CHARGE < 89.01
              MOVE 99.05  TO BASE-ROOM-CHARGE.

           IF TOTAL-ROOM-CHARGE > 9999.00 OR
              TOTAL-ROOM-CHARGE < 99.01
              MOVE 127.63  TO TOTAL-ROOM-CHARGE.

           MOVE DF8(HOLD-SUB) TO  ROOM-DATE-FROM.
           MOVE DF8(HOLD-SUB) TO  ROOM-DATE-TO.

           MOVE WA(HOLD-SUB) TO BED-IDENTITY
                     IN INPATIENT-DAILY-REC.
           MOVE WA(HOLD-SUB2) TO ROOM-IDENTITY.
           IF HOLD-SUB > 11
               MOVE WA(HOLD-SUB2) TO WARD-NBR IN INPATIENT-DAILY-REC
           ELSE IF HOLD-SUB > 7
               MOVE WA(HOLD-SUB) TO WARD-NBR IN INPATIENT-DAILY-REC
           ELSE MOVE 0011 TO WARD-NBR IN INPATIENT-DAILY-REC.


           IF HOLD-SUB < 13 MOVE "B" TO BILLABLE-TREATMENT-IND
           ELSE MOVE "N" TO BILLABLE-TREATMENT-IND.

           MOVE EQ(HOLD-SUB) TO
           EQUIPMENT-CATEGORY(1),
           EQUIPMENT-CATEGORY(3)
           EQUIPMENT-CATEGORY(5)
           EQUIPMENT-CATEGORY(7).

           MOVE EQ(HOLD-SUB2) TO
           EQUIPMENT-CATEGORY(2)
           EQUIPMENT-CATEGORY(4)
           EQUIPMENT-CATEGORY(6)
           EQUIPMENT-CATEGORY(8).

           MOVE RESN(HOLD-SUB) TO
           EQUIPMENT-REASON-CDE(1),
           EQUIPMENT-REASON-CDE(3)
           EQUIPMENT-REASON-CDE(5)
           EQUIPMENT-REASON-CDE(7).

           MOVE RESN(HOLD-SUB2) TO
           EQUIPMENT-REASON-CDE(2)
           EQUIPMENT-REASON-CDE(4)
           EQUIPMENT-REASON-CDE(6)
           EQUIPMENT-REASON-CDE(8).

           PERFORM 740-WRITE-PATDATA THRU 740-EXIT.
       400-EXIT.
           EXIT.

       500-PATMASTR.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.
           SUBTRACT 1 FROM HOLD-SUB2.
           IF HOLD-SUB2 < 1 MOVE +15 TO HOLD-SUB2.
           ADD +1 TO HOLD-SUB.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.

           MOVE ROW-SUB TO PATIENT-ID-M, ATTENDING-PHYSICIAN-n,
           DIAGNOSTIC-CODE-PRIMARY-n PRIMARY-CARE-PHYSICIAN-ID-n
           LAB-TEST-S-ID-n(1)
           PRESCRIBING-S-PHYS-ID-n(1)
           TEST-DIAG-CODE-n(1)
           EQUIPMENT-S-ID-n(1)
           EQUIP-DIAG-CODE-n(1)
           EQUIPMENT-PRES-PHYS-ID-n(1)
           LAB-TEST-S-ID-n(2)
           PRESCRIBING-S-PHYS-ID-n(2)
           TEST-DIAG-CODE-n(2)
           EQUIPMENT-S-ID-n(2)
           EQUIP-DIAG-CODE-n(2)
           EQUIPMENT-PRES-PHYS-ID-n(2)
           LAB-TEST-S-ID-n(3)
           PRESCRIBING-S-PHYS-ID-n(3)
           TEST-DIAG-CODE-n(3)
           EQUIPMENT-S-ID-n(3)
           EQUIP-DIAG-CODE-n(3)
           EQUIPMENT-PRES-PHYS-ID-n(3)
           LAB-TEST-S-ID-n(4)
           PRESCRIBING-S-PHYS-ID-n(4)
           TEST-DIAG-CODE-n(4)
           EQUIPMENT-S-ID-n(4)
           EQUIP-DIAG-CODE-n(4)
           EQUIPMENT-PRES-PHYS-ID-n(4)
           LAB-TEST-S-ID-n(5)
           PRESCRIBING-S-PHYS-ID-n(5)
           TEST-DIAG-CODE-n(5)
           EQUIPMENT-S-ID-n(5)
           EQUIP-DIAG-CODE-n(5)
           EQUIPMENT-PRES-PHYS-ID-n(5)
           LAB-TEST-S-ID-n(6)
           PRESCRIBING-S-PHYS-ID-n(6)
           TEST-DIAG-CODE-n(6)
           EQUIPMENT-S-ID-n(6)
           EQUIP-DIAG-CODE-n(6)
           EQUIPMENT-PRES-PHYS-ID-n(6)
           LAB-TEST-S-ID-n(7)
           PRESCRIBING-S-PHYS-ID-n(7)
           TEST-DIAG-CODE-n(7)
           EQUIPMENT-S-ID-n(7)
           EQUIP-DIAG-CODE-n(7)
           EQUIPMENT-PRES-PHYS-ID-n(7)
           LAB-TEST-S-ID-n(8)
           PRESCRIBING-S-PHYS-ID-n(8)
           TEST-DIAG-CODE-n(8)
           EQUIPMENT-S-ID-n(8)
           EQUIP-DIAG-CODE-n(8)
           EQUIPMENT-PRES-PHYS-ID-n(8).

           MOVE WA(HOLD-SUB) TO BED-IDENTITY-PRIMARY .
           MOVE WA(HOLD-SUB2) TO PRIMARY-STAY-WARD-NBR.

           move 0 to  tmp-val5
           MEDICATION-COST PHARMACY-COST ANCILLARY-CHARGE
           COPAY REMAINING-DEDUCTIBLE
           PATIENT-TOT-AMT
           TEST-CHARGES(1)
           EQUIPMENT-CHARGES(1)
           TEST-CHARGES(2)
           EQUIPMENT-CHARGES(2)
           TEST-CHARGES(3)
           EQUIPMENT-CHARGES(3)
           TEST-CHARGES(4)
           EQUIPMENT-CHARGES(4)
           TEST-CHARGES(5)
           EQUIPMENT-CHARGES(5)
           TEST-CHARGES(6)
           EQUIPMENT-CHARGES(6)
           TEST-CHARGES(7)
           VENIPUNCTURE-COST(7)
           TEST-CHARGES(8)
           EQUIPMENT-CHARGES(8).

      ****** $$$ amounts
           if row-sub > 500
           COMPUTE TMP-VAL5 =
                  (987.17 + hold-sub - hold-sub2) / (row-sub * 1.12)
           else
           if row-sub > 300
           COMPUTE TMP-VAL5 =
                  (113.9 + hold-sub - hold-sub2) / (row-sub * 1.29)
           else
           COMPUTE TMP-VAL5 =
                  (242.79 + hold-sub - hold-sub2) / (row-sub * 1.02).

           ADD TMP-VAL5 TO
           MEDICATION-COST PHARMACY-COST ANCILLARY-CHARGE
           COPAY REMAINING-DEDUCTIBLE
           PATIENT-TOT-AMT
           TEST-CHARGES(1)
           EQUIPMENT-CHARGES(1)
           TEST-CHARGES(2)
           EQUIPMENT-CHARGES(2)
           TEST-CHARGES(3)
           EQUIPMENT-CHARGES(3)
           TEST-CHARGES(4)
           EQUIPMENT-CHARGES(4)
           TEST-CHARGES(5)
           EQUIPMENT-CHARGES(5)
           TEST-CHARGES(6)
           EQUIPMENT-CHARGES(6)
           TEST-CHARGES(7)
           VENIPUNCTURE-COST(7)
           TEST-CHARGES(8)
           EQUIPMENT-CHARGES(8).

           IF COPAY > 40
               MOVE 10 TO COPAY.
           IF REMAINING-DEDUCTIBLE > 500
               MOVE 317.54 TO REMAINING-DEDUCTIBLE.

      ******** domain values
           MOVE PL(HOLD-SUB) TO  INS-TYPE IN PATIENT-MASTER-REC.
           MOVE DF10(HOLD-SUB) TO  DATE-ADMIT.
           MOVE DT1D(HOLD-SUB) TO  DATE-DISCHARGE.

           MOVE MO(HOLD-SUB) TO TREATMENT-MODE.
           MOVE WA(HOLD-SUB) TO BED-IDENTITY IN INPATIENT-TREATMENT-REC

           MOVE DF8(HOLD-SUB) TO
             LAB-TEST-DATE(1),
             EQUIPMENT-CHARGE-DATE(1)
             LAB-TEST-DATE(3)
             EQUIPMENT-CHARGE-DATE(3)
             LAB-TEST-DATE(5)
             EQUIPMENT-CHARGE-DATE(5)
             EQUIPMENT-CHARGE-DATE(7)
             LAB-TEST-DATE(7).

           MOVE DF8(HOLD-SUB2) TO
             LAB-TEST-DATE(2)
             EQUIPMENT-CHARGE-DATE(2)
             LAB-TEST-DATE(4)
             EQUIPMENT-CHARGE-DATE(4)
             LAB-TEST-DATE(6)
             EQUIPMENT-CHARGE-DATE(6)
             EQUIPMENT-CHARGE-DATE(8)
             LAB-TEST-DATE(8).

           DIVIDE ROW-SUB BY 2 GIVING TMP-VAL3.
           IF TMP-VAL3 = 0
             MOVE "N" TO HIPAA-FORM-SIGNED-IND
             MOVE "Y" TO PREVIOUS-PATIENT-IND
             MOVE "N" TO IN-OUT-NETWORK
             MOVE "I" TO PATIENT-TYPE
           ELSE
             MOVE FN(HOLD-SUB2)  TO  INS-F
             MOVE "M" TO IN-OUT-NETWORK
             MOVE "O" TO HIPAA-FORM-SIGNED-IND
             MOVE "O" TO PATIENT-TYPE
             MOVE "N" TO PREVIOUS-PATIENT-IND.

           IF HOSPITAL-STAY-LTH > 20
               MOVE 3 TO HOSPITAL-STAY-LTH.

      ****** space out final array elements
           Perform varying PMAST-SUB FROM 9 BY 1 UNTIL PMAST-SUB > 20
             MOVE SPACES TO
               EQUIPMENT-CHARGES-SUMMARY(PMAST-SUB),
               DAILY-LAB-CHARGES-SUMMARY(PMAST-SUB)
           END-PERFORM.

           PERFORM 750-WRITE-PATMASTR THRU 750-EXIT.

       500-EXIT.
           EXIT.
       600-PATINS.
           ADD +1 TO HOLD-SUB, NAME-SUB.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.
           IF NAME-SUB > 50 MOVE +1 TO NAME-SUB.
           SUBTRACT 1 FROM HOLD-SUB2, NAME-SUB2.
           IF HOLD-SUB2 < 1 MOVE +15 TO HOLD-SUB2.
           IF NAME-SUB2 < 1 MOVE +50 TO NAME-SUB2.

           MOVE ROW-SUB TO PATIENT-ID-I,
             INSI1, INSI3, GN1, GN3
                  INS-COMPANY-PRIMARY-ID-N.

           MOVE INS(HOLD-SUB) TO  CARRIER-NAME IN INS-COMPANY-PRIMARY.
           MOVE PHONE(HOLD-SUB) TO  CARRIER-PHONE-I.
           MOVE PHONE(HOLD-SUB2) TO  CARRIER-FAX-I.

           DIVIDE ROW-SUB BY 2 GIVING TMP-VAL3.
           IF TMP-VAL3 = 0
             MOVE FN(NAME-SUB)  TO  INS-F
             MOVE "N" TO RETIRED-IND
             MOVE LN(NAME-SUB) TO INS-L
             MOVE "F" TO INSURED-GENDER
           ELSE
             MOVE FN(NAME-SUB2)  TO  INS-F
             MOVE "M" TO INSURED-GENDER
             MOVE "Y" TO RETIRED-IND
             MOVE LN(NAME-SUB2) TO INS-L.

           COMPUTE TMP-VAL2 = TMP-VAL1 * ROW-SUB.
           MOVE ZEROS TO INSI1, INSI3, GN1, GN3 ZIN.
           ADD TMP-VAL2 TO
             INSI1, INSI3, GN1 GN3 ZIN.

           MOVE FN(NAME-SUB2) TO INS-M.

           MOVE RE(HOLD-SUB) TO PATIENT-RELATIONSHIP
                    IN INS-COMPANY-PRIMARY

           MOVE ADDR(HOLD-SUB) TO STREET IN INS-COMPANY-PRIMARY.
           MOVE CI(HOLD-SUB) TO CITY IN INS-COMPANY-PRIMARY.
           MOVE STAT1(HOLD-SUB) TO STATE IN INS-COMPANY-PRIMARY.
           MOVE ZI(HOLD-SUB) TO ZIP-INS IN INS-COMPANY-PRIMARY.

           MOVE SPACES TO INS-COMPANY-SECONDARY.
           PERFORM 760-WRITE-PATINS THRU 760-EXIT.
           MOVE INS-GRP TO MEDICARE-BENEFICIARY.
           MOVE GRP-GRP TO MEDICARE-CLAIM-NBR.
           MULTIPLY TMP-VAL2 BY 1034.4 GIVING COMMERCIAL-CLAIM-NBR.
       600-EXIT.
           EXIT.

       700-PATPERSN.
           ADD +1 TO HOLD-SUB, NAME-SUB.
           IF HOLD-SUB > 15 MOVE +1 TO HOLD-SUB.
           IF NAME-SUB > 50 MOVE +1 TO NAME-SUB.
           SUBTRACT 1 FROM HOLD-SUB2, NAME-SUB2.
           IF HOLD-SUB2 < 1 MOVE +15 TO HOLD-SUB2.
           IF NAME-SUB2 < 1 MOVE +50 TO NAME-SUB2.

           MOVE ROW-SUB TO PATIENT-NBR-MR, DL1, DL2.

           MOVE INS(HOLD-SUB) TO
                    EMP-NAME IN PATIENT-PERSONAL-MASTER-REC
           MOVE INS(HOLD-SUB2) TO
                    EMPLOYER IN RESPONSIBLE-PARTY.

           MOVE ADDR(HOLD-SUB) TO EMP-ADDR.
           MOVE CI(HOLD-SUB) TO EMP-CITY.
           MOVE STAT1(HOLD-SUB) TO EMP-STATE.
           MOVE ZI(HOLD-SUB) TO EMP-ZIP.

           MOVE ADDR(HOLD-SUB2) TO STREET IN HOME-ADDRESS.
           MOVE CI(HOLD-SUB2) TO CITY IN HOME-ADDRESS.
           MOVE STAT1(HOLD-SUB2) TO STATE IN HOME-ADDRESS
                                    ISSUING-STATE.
           MOVE ZI(HOLD-SUB2) TO POSTAL-CODE IN HOME-ADDRESS.

           MOVE PHONE(HOLD-SUB) TO  PHONE-HOME, FCON-PHONE-H.
           MOVE PHONE(HOLD-SUB2) TO  PHONE-WORK, PHONE-MOBILE
                                         FCON-PHONE-C.

           MOVE "SPOUSE" TO FAMILY-CONTACT-PRIMARY.
           MOVE "UNITED STATUES" TO COUNTRY.

           DIVIDE ROW-SUB BY 9 GIVING APT-TMP.
           IF APT-TMP = 0 MOVE "APT 8B" TO APARTMENT-NBR
           ELSE MOVE "  " TO APARTMENT-NBR.

           DIVIDE ROW-SUB BY 2 GIVING TMP-VAL3.

           IF TMP-VAL3 = 0
             MOVE FN(NAME-SUB)  TO  FIRST-NAME
             MOVE "M" TO MARITAL-STATUS
             MOVE LN(NAME-SUB) TO LAST-NAME
             MOVE 03 TO EXP-MONTH
             MOVE 2015 TO EXP-YEAR
             MOVE "B" TO MIDINIT
             MOVE "CC" TO PAYMENT-METHOD-TYPE
           ELSE
             MOVE FN(NAME-SUB2)  TO  FIRST-NAME
             MOVE "S" TO MARITAL-STATUS
             MOVE 07 TO EXP-MONTH
             MOVE 2014 TO EXP-YEAR
             MOVE "K" TO MIDINIT
             MOVE "CH" TO PAYMENT-METHOD-TYPE
             MOVE LN(NAME-SUB2) TO LAST-NAME.

           DIVIDE ROW-SUB BY 3 GIVING TMP-VAL3.
           IF TMP-VAL3 = 0
               MOVE "D" TO MARITAL-STATUS
               MOVE 09 TO EXP-MONTH
               MOVE 2012 TO EXP-YEAR
               MOVE "U" TO MIDINIT
               MOVE "GREAT BRITAIN" TO COUNTRY
               MOVE "SPOUSE" TO FAMILY-CONTACT-PRIMARY
               MOVE "CA" TO  PAYMENT-METHOD-TYPE

           IF HOLD-SUB < 15
               SUBTRACT 2 FROM HOLD-SUB GIVING PMAST-SUB.
           MOVE PATIENT-NAME TO TMP-PICX.
           MOVE FN(PMAST-SUB) TO  TMP-PICX(1:12).
           MOVE TMP-PICX TO  FAMILY-CONTACT-PRIMARY.

           COMPUTE TMP-VAL2 = TMP-VAL1 * ROW-SUB.
           MOVE ZEROS TO INSI1, INSI3, GN1, GN3 ZIN.
           ADD TMP-VAL2 TO
             HEIGHT, WEIGHT.

           MOVE FN(HOLD-SUB2) TO INS-M.

           MOVE RE(HOLD-SUB) TO PATIENT-RELATIONSHIP
                    IN INS-COMPANY-PRIMARY

           IF HEIGHT > 80
               MOVE 56 TO HEIGHT.

           IF WEIGHT > 300
               MOVE 109 TO WEIGHT.

           ADD ROW-SUB TO DOB.

           MOVE OC(HOLD-SUB) TO OCCUPATON
                     IN PATIENT-PERSONAL-MASTER-REC.
           MOVE OC(HOLD-SUB2) TO OCCUPATION IN RESPONSIBLE-PARTY.

           PERFORM 770-WRITE-PRSNMSTR THRU 770-EXIT.

       700-EXIT.
           EXIT.

       730-WRITE-TRMT.
           WRITE INPATIENT-TREATMENT-REC-DATA
              FROM INPATIENT-TREATMENT-REC.
       730-EXIT.
           EXIT.

       740-WRITE-PATDATA.
           write INPATIENT-DAILY-REC-DATA
              from INPATIENT-DAILY-REC.
       740-EXIT.
           EXIT.

       750-WRITE-PATMASTR.
           move PATIENT-MASTER-REC to PATMSTR-REC.
           write PATMSTR-REC.
           DISPLAY PATMSTR-STATUS.
       750-EXIT.
           EXIT.

       760-WRITE-PATINS.
           MOVE PATIENT-INSURANCE TO PATINS-REC.
           write PATINS-REC.
           DISPLAY PATINS-STATUS.
       760-EXIT.
           EXIT.

       770-WRITE-PRSNMSTR.
           MOVE PATIENT-PERSONAL-MASTER-REC  TO PRSNMSTR-REC.
           write PRSNMSTR-REC.
           DISPLAY PRSN-STATUS.
       770-EXIT.
           EXIT.

       850-CLOSE-FILES.
            CLOSE TRMTDATA, PATDATA, PATMSTR, PATINS, PRSNMSTR.
       850-EXIT.
           EXIT.

       999-CLEANUP.
      *  Code the statement to close all files
           WRITE INPATIENT-TREATMENT-REC-DATA FROM WS-TRAILER-REC.
           move 1001 to IN-RECORD-COUNT.
           WRITE INPATIENT-DAILY-REC-DATA FROM WS-TRAILER-REC.

           PERFORM 850-CLOSE-FILES THRU 850-EXIT.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "NORMAL END OF JOB".
       999-EXIT.
           EXIT.