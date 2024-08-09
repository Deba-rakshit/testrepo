      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Describes file <userid>.ADLAB.CUST2SEG
      *   ---------------------------------------------------
       01  CUSTOMER-SEGMENT.
           05  CUSTOMER-KEY.
               10  CUST-ID               PIC X(5).
               10  RECORD-TYPE           PIC X.
               10  FILLER                PIC X(7).
           05  NAME                  PIC X(17).
           05  ACCT-BALANCE          PIC S9(7)V99  COMP-3.
           05  ORDERS-YTD            PIC S9(4)     COMP.
           05  CITY                  PIC X(15).
           05  OCCUPATION            PIC X(28).
       01  PRODUCT-SEGMENT.
           05  PRODUCT-KEY.
               10  CUST-ID               PIC X(5).
               10  RECORD-TYPE           PIC X.
               10  PRODUCT-ID            PIC X(7).
           05  PRODUCT-NAME          PIC X(25).
           05  DATE-PURCHASED        PIC X(10).
           05  SERVICE-CALLS         PIC S9(4)     COMP.
           05  LAST-SERVICE-CALL     PIC X(10).
           05  FILLER                PIC X(20).