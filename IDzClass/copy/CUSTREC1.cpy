      *** +++++++++++++++++++++++++++++++++++++++++++++++++++
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *
      *   The sample data described by this copybook
      *       is <USERID>.ADLAB.CUSTFILE
      *** +++++++++++++++++++++++++++++++++++++++++++++++++++
           05  CUSTOMER-KEY.
               10  CUST-ID               PIC X(5).
               10  REC-TYPE              PIC X.
           05  NAME                  PIC X(17).
           05  ACCT-BALANCE          PIC S9(7)V99  COMP-3.
           05  ORDERS-YTD            PIC S9(5)     COMP.
           05  ADDR                  PIC X(20).
           05  CITY                  PIC X(14).
           05  STATE                 PIC X(02).
           05  COUNTRY               PIC X(11).
           05  MONTH                 PIC S9(7)V99   COMP-3  OCCURS 12.
           05  OCCUPATION            PIC X(30).
           05  NOTES                 PIC X(120).
           05  LAB-DATA-1            PIC X(05).
           05  LAB-DATA-2            PIC X(40).