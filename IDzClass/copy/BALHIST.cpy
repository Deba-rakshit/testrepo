       01  BALHIST.
           03  SEGY1-CURRENT-PERIOD.
               05 SEGY-CURR-PERIOD    PIC X(6).
               05 SEGY-TYPE-IND1      PIC X(4).
                   88 BILL-CURRENT   VALUE 'CURR'.
                   88 BILL-PAST-DUE  VALUE 'PAST'.
               05 SEGY-CURR-BILL      PIC 9(9)V99 COMP.
               05 SEGY-END-BILL-DATE.
                  10 END-BILL-MM      PIC 99.
                  10 FILLER           PIC X   VALUE '/'.
                  10 END-BILL-DD      PIC 99.
                  10 FILLER           PIC X   VALUE '/'.
                  10 END-BILL-YYYY    PIC 9(4).
               05 SEGY-GRACE-PERIOD   PIC XX.
               05 SEGY-COMMENTS       PIC X(100).
               05 FILLER              PIC X(40).
           03  SEGY2-PAST-PERIODS REDEFINES SEGY1-CURRENT-PERIOD.
               05  SEGY-PAST-PERIOD   PIC X(6).
               05  SEGY-TYPE-IND2     PIC X(4).
               05  SEGY-CURR-DUE      PIC 9(9)V99 COMP.
               05  SEGY-ORIG-DUE-DATE.
                   10 SEGY-ORIG-MM    PIC X(2).
                   10 SEGY-ORIG-DD    PIC X(2).
                   10 SEGY-ORIG-YYYY  PIC X(4).
               05  DAYS-PAST-DUE      PIC 9(3)    COMP.
               05  ORIGINAL-DUE       PIC 9(9)V99 COMP.
               05  LAST-PAYMENT-DATE.
                   10 SEGY-LAST-MM    PIC X(2).
                   10 SEGY-LAST-DD    PIC X(2).
                   10 SEGY-LAST-YYYY  PIC X(4).
               05  AMOUNT-PAYED       PIC 9(9)V99 COMP.
               05  OVERDUE-RATE       PIC V9999   COMP.
               05  ORIG-AMOUNT-DUE    PIC 9(9)V99 COMP.
               05  TOTAL-PENALTY      PIC 9(5)V99 COMP.
               05  SEGY-LATE-REASON   PIC X(101).