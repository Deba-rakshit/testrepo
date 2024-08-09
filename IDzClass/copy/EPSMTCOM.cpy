          10  PROCESS-INDICATOR               PIC X.
      * INPUT
          10 EPSPCOM-PRINCIPLE-DATA   PIC S9(9)V99 COMP.
          10 EPSPCOM-NUMBER-OF-YEARS  PIC S9(4)    COMP.
          10 EPSPCOM-NUMBER-OF-MONTHS PIC S9(4)    COMP.
          10 EPSPCOM-QUOTED-INTEREST-RATE
                                        PIC S9(2)v9(3) COMP.
          10 EPSPCOM-YEAR-MONTH-IND   PIC X.
      * OUTPUT
          10 EPSPCOM-RETURN-MONTH-PAYMENT
                                      PIC S9(7)V99 COMP.
          10 EPSPCOM-ERRMSG           PIC X(80).
          10 EPSPCOM-PROGRAM-RETCODE  PIC 9(4).
             88 EPS02-REQUEST-SUCCESS VALUE 0.
          10 EPSPCOM-PROGRAM-RETCODE-RDF
                  REDEFINES EPSPCOM-PROGRAM-RETCODE
                                      PIC X(4).
