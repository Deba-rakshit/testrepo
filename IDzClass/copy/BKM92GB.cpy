       01  DETAILI.
           02  FILLER PIC X(12).
           02  TITLEL    COMP  PIC  S9(4).
           02  TITLEF    PICTURE X.
           02  FILLER REDEFINES TITLEF.
             03 TITLEA    PICTURE X.
           02  TITLEI  PIC X(20).
           02  NUMBL    COMP  PIC  S9(4).
           02  NUMBF    PICTURE X.
           02  FILLER REDEFINES NUMBF.
             03 NUMBA    PICTURE X.
           02  NUMBI  PIC X(6).
           02  NAMEL    COMP  PIC  S9(4).
           02  NAMEF    PICTURE X.
           02  FILLER REDEFINES NAMEF.
             03 NAMEA    PICTURE X.
           02  NAMEI  PIC X(20).
           02  ADDRL    COMP  PIC  S9(4).
           02  ADDRF    PICTURE X.
           02  FILLER REDEFINES ADDRF.
             03 ADDRA    PICTURE X.
           02  ADDRI  PIC X(20).
           02  PHONEL    COMP  PIC  S9(4).
           02  PHONEF    PICTURE X.
           02  FILLER REDEFINES PHONEF.
             03 PHONEA    PICTURE X.
           02  PHONEI  PIC X(8).
           02  DATEL    COMP  PIC  S9(4).
           02  DATEF    PICTURE X.
           02  FILLER REDEFINES DATEF.
             03 DATEA    PICTURE X.
           02  DATEI  PIC X(8).
           02  AMOUNTL    COMP  PIC  S9(4).
           02  AMOUNTF    PICTURE X.
           02  FILLER REDEFINES AMOUNTF.
             03 AMOUNTA    PICTURE X.
           02  AMOUNTI  PIC X(8).
           02  COMMENTL    COMP  PIC  S9(4).
           02  COMMENTF    PICTURE X.
           02  FILLER REDEFINES COMMENTF.
             03 COMMENTA    PICTURE X.
           02  COMMENTI  PIC X(9).
           02  MSG1L    COMP  PIC  S9(4).
           02  MSG1F    PICTURE X.
           02  FILLER REDEFINES MSG1F.
             03 MSG1A    PICTURE X.
           02  MSG1I  PIC X(39).
           02  MSG3L    COMP  PIC  S9(4).
           02  MSG3F    PICTURE X.
           02  FILLER REDEFINES MSG3F.
             03 MSG3A    PICTURE X.
           02  MSG3I  PIC X(39).
       01  DETAILO REDEFINES DETAILI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TITLEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  NUMBO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  NAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ADDRO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  PHONEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  DATEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  AMOUNTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  COMMENTO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  MSG1O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  MSG3O  PIC X(39).