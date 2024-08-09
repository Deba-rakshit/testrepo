           10 LINK-COMM.                                                00000101
               20  USER-REQUEST    PIC X.                               00000208
                 88 ACCOUNT-OPEN     VALUE 'O'.                         00000308
                 88 ACCOUNT-VIEW     VALUE 'V'.                         00000408
                 88 ACCOUNT-CHANGE   VALUE 'U'.                         00000510
                 88 ACCOUNT-CLOSE    VALUE 'C'.                         00000600
               20  SERVER-REQUEST  PIC X.                               00000708
                 88 ACCOUNT-CREATE   VALUE 'C'.                         00000808
                 88 ACCOUNT-READ     VALUE 'R'.                         00000908
                 88 ACCOUNT-UPDATE   VALUE 'U'.                         00001008
                 88 ACCOUNT-DELETE   VALUE 'D'.                         00001108
                 88 ACCOUNT-BROWSE   VALUE 'B'.                         00001208
               20  KEYNUM          PIC 9(6).                            00001408
               20  TEAMID          PIC X(2).                            00001508
               20  RCODE           PIC 9(9)  COMP.                      00001608
               20  ERR-RESP        PIC 99.                              00001708