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