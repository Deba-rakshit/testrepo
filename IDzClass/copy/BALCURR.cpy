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