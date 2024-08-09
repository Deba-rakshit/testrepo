      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Describes Product Records in <userid>.ADLAB.FILES(CUST2)
      *   Use this Copybook in conjunction with CUST2CUS
      *   ---------------------------------------------------
       01  PRODUCT-RECORD.
           05  PRODUCT-KEY.
               10  CUST-ID               PIC X(5).
               10  RECORD-TYPE           PIC X.
               10  PRODUCT-ID            PIC X(7).
           05  PRODUCT-NAME          PIC X(25).
           05  DATE-PURCHASED        PIC X(10).
           05  SERVICE-CALLS         PIC S9(4)     COMP.
           05  LAST-SERVICE-CALL     PIC X(10).
           05  FILLER                PIC X(20).