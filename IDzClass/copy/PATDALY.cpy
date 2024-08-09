      ** QSAM FILE
       01  INPATIENT-DAILY-REC.
           05  PATIENT-RECORD-TYPE     PIC X(01).
               88  TRAILER-REC     VALUE "T".
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
               "0010", "2010", "1010", "0011", "0110", "0000"
               "3333" "4444" "5555" "6666" "7777" "0033".
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