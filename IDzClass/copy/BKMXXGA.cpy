       01  MENUI.
           02  FILLER PIC X(12).
           02  FUNCTIONL    COMP  PIC  S9(4).
           02  FUNCTIONF    PICTURE X.
           02  FILLER REDEFINES FUNCTIONF.
             03 FUNCTIONA    PICTURE X.
           02  FUNCTIONI  PIC X(1).
           02  KEYL    COMP  PIC  S9(4).
           02  KEYF    PICTURE X.
           02  FILLER REDEFINES KEYF.
             03 KEYA    PICTURE X.
           02  KEYI  PIC X(6).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  MSGI  PIC X(31).
       01  MENUO REDEFINES MENUI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  FUNCTIONO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  KEYO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  MSGO  PIC X(31).