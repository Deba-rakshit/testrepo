       01  ORDER-RECORD.
           05  ORD-NUMBER                 PIC 9(5).
           05  FILLER  REDEFINES ORD-NUMBER.
               10  ORDER-REC-COMMENT          PIC X.
               10  FILLER                     PIC X(4).
           05  FILLER                     PIC XX.
           05  ORD-LOCATION-CODE          PIC 9.
           05  FILLER                     PIC XX.
           05  ORD-ITEM-CODE              PIC 9(4).
           05  FILLER                     PIC X.
           05  ORD-CUST-ID                PIC X(10).
           05  ORD-NUM-ITEMS              PIC 9(5).
           05  FILLER                     PIC X.
           05  ORD-SALE-AMOUNT            PIC 9(5)V99.