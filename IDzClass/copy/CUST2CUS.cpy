      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Describes Customer Records in <userid>.ADLAB.FILES(CUST2)
      *   Use this Copybook in conjunction with CUST2PRO
      *   ---------------------------------------------------
       01  CUSTOMER-RECORD.
           05  CUSTOMER-KEY.
               10  CUST-ID               PIC X(5).
               10  RECORD-TYPE           PIC X.
               10  FILLER                PIC X(7).
           05  NAME                  PIC X(17).
           05  CUST-ACCT-BALANCE     PIC S9(7)V99  COMP-3.
           05  ORDERS-YTD            PIC S9(4)     COMP.
           05  CITY                  PIC X(15).
           05  OCCUPATION            PIC X(28).