       01  FEEDBACK.
         10  FB-SEV                 PIC S9(4) COMP.
         10  FB-MSGNO               PIC S9(4) COMP.
         10  FB-CASE-SEV            PIC X.
         10  FB-FAC-ID              PIC X(3).
         10  FB-ISINFO              PIC S9(8) COMP.
       01  CEETEST-VSTRING.
            05  COMMAND-LTH         PIC S9(4) COMP VALUE +80.
            05  COMMAND             PIC X(80) VALUE SPACES.