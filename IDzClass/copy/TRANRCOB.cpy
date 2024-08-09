       01  TRAN-RECORD.
           05  TRAN-CODE              PIC X(6).
           05  FILLER  REDEFINES TRAN-CODE.
               10  TRAN-COMMENT       PIC X.
               10  FILLER             PIC X(5).
           05  FILLER                 PIC X.
           05  TRAN-PARMS             PIC X(73).
           05  CRUNCH-PARMS   REDEFINES TRAN-PARMS.
               10  CRUNCH-CPU-PARM-ALPHA.
                   15  CRUNCH-CPU-PARM        PIC 99.
               10  FILLER                     PIC X.
               10  CRUNCH-IO-PARM-ALPHA.
                   15  CRUNCH-IO-PARM         PIC 99.
               10  FILLER                     PIC X(68).