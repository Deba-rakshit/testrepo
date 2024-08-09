       01  SEGY1-CURRENT-PERIOD.
           03 SEGY-CURR-PERIOD    PIC X(6).
           03 SEGY-TYPE-IND1      PIC X(4)  VALUE 'CURR'.
           03 SEGY-CURR-BILL      PIC 9(9)V99 COMP.
           03 SEGY-END-BILL-DATE.
              05 END-BILL-MM      PIC 99.
              05 FILLER           PIC X VALUE '\'.
              05 END-BILL-DD      PIC 99.
              05 FILLER           PIC X VALUE '\'.
              05 END-BILL-YYYY    PIC 9(4).
           03 SEGY-GRACE-PERIOD   PIC XX.
           03 SEGY-COMMENTS       PIC X(100).
           03 FILLER              PIC X(40).
       01  SEGY2-PAST-PERIODS.
           03  SEGY-PAST-PERIOD   PIC X(6).
           03  SEGY-TYPE-IND2     PIC X(4)   VALUE 'PAST'.
           03  SEGY-CURR-DUE      PIC 9(9)V99 COMP.
           03  SEGY-ORIG-DUE-DATE.
               05 SEGY-ORIG-MM    PIC X(2).
               05 SEGY-ORIG-DD    PIC X(2).
               05 SEGY-ORIG-YYYY  PIC X(4).
           03  DAYS-PAST-DUE      PIC 9(3)    COMP.
           03  ORIGINAL-DUE       PIC 9(9)V99 COMP.
           03  LAST-PAYMENT-DATE.
               05 SEGY-LAST-MM    PIC X(2).
               05 SEGY-LAST-DD    PIC X(2).
               05 SEGY-LAST-YYYY  PIC X(4).
           03  AMOUNT-PAYED       PIC 9(9)V99 COMP.
           03  OVERDUE-RATE       PIC V9999   COMP.
           03  ORIG-AMOUNT-DUE    PIC 9(9)V99 COMP.
           03  TOTAL-PENALTY      PIC 9(5)V99 COMP.
           03  SEGY-LATE-REASON   PIC X(101).