       01 SQLCA.
           03 SQLCAID          PIC X(8).
           03 SQLCABC          PIC S9(9) COMP.
           03 SQLCODE          PIC S9(9) COMP.
           03 SQLERRM.
               49 SQLERRML     PIC S9(4) COMP.
               49 SQLERRMC     PIC X(70).
           03 SQLERRP          PIC X(8).
           03 SQLERRD OCCURS 6  PIC S9(9) COMP.
           03 SQLWARN.
               05 SQLWARN0       PIC X.
               05 SQLWARN1       PIC X.
               05 SQLWARN2       PIC X.
               05 SQLWARN3       PIC X.
               05 SQLWARN4       PIC X.
               05 SQLWARN5       PIC X.
               05 SQLWARN6       PIC X.
               05 SQLWARN7       PIC X.
           03 SQLEXT             PIC X(8).