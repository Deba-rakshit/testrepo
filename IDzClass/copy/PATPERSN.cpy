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