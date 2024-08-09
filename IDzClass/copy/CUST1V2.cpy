      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Modified version of CUST1 for copy with reformat example
      *   ---------------------------------------------------
       01  CUST-REC.
           05  CUSTOMER-KEY.
               10  CUST-ID         PIC X(5).
           05  NAME                PIC X(17).
           05  ACCT-BALANCE        PIC S9(7)V99  COMP-3.
           05  ORDERS-YTD          PIC S9(4)     COMP.
           05  ADDR                PIC X(20).
           05  CITY                PIC X(16).
           05  STATE               PIC X(02).
           05  MONTH               PIC S9(7)V99  COMP-3  OCCURS 14.
           05  JOB                 PIC X(30).
           05  LAB-DATA-1          PIC S9(7)     COMP-3.
           05  NEW-ALPHA-FIELD     PIC X(10).
           05  NEW-NUM-FIELD       PIC S9(5).
           05  LAB-DATA-2          PIC X(40).
           05  LAB-DATA-2-RDF    REDEFINES LAB-DATA-2.
               10  LAB-RDF1        PIC X(05).
               10  LAB-RDF2        PIC X(05).
               10  LAB-RDF3        PIC X(30).
           05  NOTES               PIC X(150).