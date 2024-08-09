      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Describes file <userid>.ADLAB.FILES(CUST2)
      *   ---------------------------------------------------
       01  :TAG:-REC.
           05  :TAG:-KEY.
               10  :TAG:-ID              PIC X(5).
               10  :TAG:-RECORD-TYPE     PIC X.
               10  FILLER                PIC X(7).
           05  :TAG:-NAME              PIC X(17).
           05  :TAG:-ACCT-BALANCE      PIC S9(7)V99  COMP-3.
           05  :TAG:-ORDERS-YTD        PIC S9(4)     COMP.
           05  :TAG:-CITY              PIC X(15).
           05  :TAG:-OCCUPATION        PIC X(28).