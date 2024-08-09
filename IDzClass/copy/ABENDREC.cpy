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