       01  TRANSACTION-RECORD.
           05  TRAN-CODE              PIC X(6).
           05  FILLER  REDEFINES TRAN-CODE.
               10  TRAN-COMMENT       PIC X.
               10  FILLER             PIC X(5).
           05  FILLER                 PIC X.
           05  TRAN-PARMS.
               10  TRAN-KEY               PIC X(13).
               10  FILLER                 PIC X.
               10  TRAN-ACTION            PIC X(8).
               10  FILLER                 PIC X.
               10  TRAN-FIELD-NAME        PIC X(10).
               10  FILLER                 PIC X.
               10  TRAN-FIELD-SS          PIC 99.
               10  FILLER                 PIC X.
               10  TRAN-UPDATE-DATA       PIC X(36).
               10  FILLER            REDEFINES TRAN-UPDATE-DATA.
                   15  TRAN-UPDATE-NUM        PIC 9(7)V99.
                   15  FILLER                 PIC X(27).
               10  FILLER            REDEFINES TRAN-UPDATE-DATA.
                   15  TRAN-UPDATE-NUMX.
                       20  TRAN-UPDATE-NUMX1      PIC X.
                       20  TRAN-UPDATE-NUMX2-7    PIC X(6).
                   15  TRAN-UPDATE-NUM-HH     PIC 99.
                   15  FILLER                 PIC X(27).
       05  CRUNCH-PARMS   REDEFINES TRAN-PARMS.
           10  CRUNCH-IO-LOOPS        PIC 9(5).
           10  FILLER                 PIC X.
           10  CRUNCH-CPU-LOOPS       PIC 9(9).
           10  FILLER                 PIC X(55).