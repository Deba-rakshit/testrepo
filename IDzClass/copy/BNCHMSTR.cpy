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
           05  DIAGNOSTIC-CODE-PRIMARY               PIC X(05).
           05  DIAGNOSTIC-CODE-SECONDARY                   PIC X(05).
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