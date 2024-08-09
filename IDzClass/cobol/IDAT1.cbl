       ID DIVISION.
       PROGRAM-ID. IDAT1.
      *
      *    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO
      *    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM
      *    CURRENT DATE.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-FLEX-ES.
       OBJECT-COMPUTER. IBM-FLEX-ES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  W-FLAGS.
           10  MSG-FLAG-IND                     PIC X.
               88  END-OF-MSG                   VALUE 'Y'.

       01 MSG-DATA.
          10 MSG-LGTH                         PIC 9(9) COMP.
          10 MSG-CMD                          PIC X(8).
          10 MSG-DATE                         PIC X(8).
          10 MSG-IND                          PIC X.

       01 MSG-OUT-DATA.
          10 MO-MSG-LGTH                      PIC 9(4) COMP.
          10 MO-MSG-LGTH2                     PIC 9(4) COMP.
          10 MO-CMD                           PIC X(8).
          10 MO-DATE                          PIC X(8).
          10 MO-DATE-IND                      PIC X.
          10 MO-FORMATTED-DAYS-LIT            PIC X(45).
          10 MO-DAYS-LIT                      PIC X(23).
          10 MO-FMT-DATE                      PIC X(30).
          10 MO-DAYS-ALIVE-LIT                PIC X(23).
          10 MO-DAYS                          PIC 9(10).
          10 MO-DAYS-CHAR                     REDEFINES MO-DAYS
                                              PIC X(10).
          10 MO-DAYS2-LIT                     PIC X(4).
          10 MO-RET-TITLE                     PIC X(30).
          10 MO-RET-LITERAL                   PIC X(26).
          10 MO-RET-DATE                      PIC X(30).
          10 MO-ERROR-MESSAGE                 PIC X(71).

       01 WS-MSG-DATA.
          10   WS-MSG-DATE                    PIC X(10).
          10   WS-MSG-IND                     PIC X.

       01 INTERFACE-AREA.
          05 L-INPUT-DATE.
             10 L-YYYY                        PIC 9(4) VALUE 0.
             10 L-MM                          PIC 9(2) VALUE 0.
             10 L-DD                          PIC 9(2) VALUE 0.
          05 L-DAYS-OLD.
             10 L-DAY-DIFFERENCE                 PIC 9(9).
             10 L-DATE-FORMATTED                 PIC X(29).
             10 L-PROGRAM-RETCODE                PIC 9(4).
             10 L-FILLER                         PIC X(72).
          05 L-RETIREMENT REDEFINES L-DAYS-OLD.
             10 L-RETIREMENT-DATE                PIC X(80).
             10 L-RET-PROGRAM-RETCODE            PIC 9(4).
             10 L-RETIREMENT-ERRMSG              PIC X(30).

       01 INTERFACE-OVERLAY.
          05 OL-INPUT-DATE                    PIC 9(8).
          05 FILLER                           PIC X(10).


       01 GET-NEXT-LIT                        PIC X(4)
                                              VALUE 'GN'.
       01 GET-UNIQUE-LIT                      PIC X(4)
                                              VALUE 'GU'.
       01 ISRT-LIT                            PIC X(4)
                                              VALUE 'ISRT'.

       01  W-INPUT-DATE.
           10 W-INPUT-DATE-CCYY               PIC 9(4).
           10 W-INPUT-DATE-MM                 PIC 99.
           10 W-INPUT-DATE-DD                 PIC 99.

       01  DATE-ROUTINE                       PIC X(8).

       01  S0C7-WORK-AREAS.
           05  SOME-NUMBER            PIC 9(7)   VALUE 0.
           05  BAD-DATA-ALPHA         PIC X(5).
           05  BAD-NUMBER       REDEFINES BAD-DATA-ALPHA
                                      PIC S9(9)  COMP-3.

       01  MESSAGE-LITERALS.
           03 INVALID-PROC-IND                PIC X(71) VALUE
           'Please enter a 1 or 2 to indicate the correct processing'.
           03 INVALID-DATE                    PIC X(71) VALUE
           'Please enter a valid date or an X to terminate'.
           03 NON-NUMERIC-DATE                PIC X(71) VALUE
           'Please enter all numbers in the format of yyyymmdd'.

       LINKAGE SECTION.
       01 IOPCB.
           02 LTERM-NAME                      PIC X(8).
           02      FILLER                     PIC XX.
           02      TPSTATUS                   PIC XX.
           02      TERM-PREFIX.
               03  FILLER                     PIC X.
               03  JULIAN-DATE                PIC S9(5) COMPUTATIONAL-3.
               03  TIME-O-DAY                 PIC S9(7) COMPUTATIONAL-3.
               03  FILLER                     PIC XXXX.

       01  DATABASE.
           02      DBASE-NAME           PIC X(8).
           02      SEGMENT-INDR         PIC XX.
           02      DBASE-STATUS         PIC XX.
           02      PROC-OPTIONS         PIC XXXX.
           02      DLI-RESERVED         PIC XXXX.
           02      SEG-FEEDBACK         PIC X(8).
           02      KEY-FEEDBACK-LENGTH  PIC XXXX.
           02      NO-OF-SENSEG-TYPES   PIC XXXX.
           02      KEY-FEEDBACK.
               03  ROOT-KEY.
                   04  FILLER           PIC XX.
                   04  PARTNUM          PIC X(15).
               03  STANINFO-KEY         PIC XX.
           02      PART-SEGNAME         PIC X(8).
           02      STAN-SEGNAME         PIC X(8).

       PROCEDURE DIVISION USING IOPCB, DATABASE.

      *    ENTRY 'DLITCBL' USING IOPCB, DATABASE.
           PERFORM IDAT1-MAINLINE.

       IDAT1-MAINLINE.
           PERFORM IDAT1-GET-MSG.
           MOVE MSG-DATE TO WS-MSG-DATE.
           MOVE MSG-IND  TO WS-MSG-IND.
           MOVE 'N'      TO MSG-FLAG-IND.
           MOVE SPACES   TO MO-ERROR-MESSAGE.
           PERFORM IDAT1-INPUT-VALIDATION.
      * Loop through all input messages to ensure no other
      *      messages are left in the queue
           IF MO-ERROR-MESSAGE = SPACES
              PERFORM IDAT1-COMPUTE-DATE
           END-IF.
           PERFORM IDAT1-NEXT-GET-MSG UNTIL END-OF-MSG.
           PERFORM IDAT1-SEND-MSG.
           GOBACK.

       IDAT1-GET-MSG.
           CALL 'CBLTDLI' USING GET-UNIQUE-LIT, IOPCB, MSG-DATA.
           IF TPSTATUS NOT = '  '
              MOVE 'Y' TO MSG-FLAG-IND.

       IDAT1-INPUT-VALIDATION.
           IF MSG-DATE = 'X'
              GOBACK
           END-IF.
           IF MSG-IND  NOT = '1' AND
              MSG-IND  NOT = '2' AND
              MSG-IND  NOT = '@'
              MOVE INVALID-PROC-IND TO MO-ERROR-MESSAGE
           ELSE
      *       OK- GOOD PROCESSING INDICATOR
              IF MSG-DATE NUMERIC
                MOVE MSG-DATE TO W-INPUT-DATE
                EVALUATE TRUE
                  WHEN W-INPUT-DATE-CCYY < 1582
                  WHEN W-INPUT-DATE-MM < 01
                  WHEN W-INPUT-DATE-MM > 12
                  WHEN W-INPUT-DATE-DD < 01
                  WHEN W-INPUT-DATE-DD > 31
                  WHEN W-INPUT-DATE > FUNCTION CURRENT-DATE(1:8)
                    MOVE INVALID-DATE TO MO-ERROR-MESSAGE
                END-EVALUATE
              ELSE
                MOVE NON-NUMERIC-DATE TO MO-ERROR-MESSAGE
              END-IF
           END-IF
           .

       IDAT1-COMPUTE-DATE.
           MOVE W-INPUT-DATE TO L-INPUT-DATE.
           MOVE 0            TO L-DAY-DIFFERENCE
           IF MSG-IND = '1'
              MOVE 'IDAT2   '   TO DATE-ROUTINE
           END-IF.
           IF MSG-IND = '2'
              MOVE 'IDAT3   '   TO DATE-ROUTINE
           END-IF.
           IF MSG-IND = '@'
      *       ABEND WITH S0C7
              MOVE '!@#$%'  TO BAD-DATA-ALPHA
              COMPUTE SOME-NUMBER = BAD-NUMBER + 1
           END-IF.
           CALL DATE-ROUTINE USING INTERFACE-AREA.

       IDAT1-NEXT-GET-MSG.
           CALL 'CBLTDLI' USING GET-NEXT-LIT, IOPCB, MSG-DATA.
           IF TPSTATUS NOT = '  '
              MOVE 'Y' TO MSG-FLAG-IND.

       IDAT1-SEND-MSG.
           MOVE WS-MSG-DATE      TO MO-DATE.
           MOVE WS-MSG-IND       TO MO-DATE-IND.
           MOVE MSG-CMD          TO MO-CMD .
           MOVE 307              TO MO-MSG-LGTH.
           IF MO-ERROR-MESSAGE = SPACES
             IF WS-MSG-IND = '1'
               MOVE 'HERE IS YOUR BIRTHDATE AND NO DAYS FROM CURR'
                             TO MO-FORMATTED-DAYS-LIT
               MOVE 'YOUR BIRTHDATE AND DAY:'
                             TO MO-DAYS-LIT
               MOVE L-DATE-FORMATTED
                             TO MO-FMT-DATE
               MOVE 'YOUR BIRTHDATE AND DAY:'
                             TO MO-DAYS-ALIVE-LIT
               MOVE L-DAY-DIFFERENCE
                             TO MO-DAYS
               MOVE SPACES    TO MO-RET-TITLE
               MOVE SPACES    TO MO-RET-LITERAL
               MOVE SPACES    TO MO-RET-DATE

             ELSE
               MOVE SPACES    TO MO-FORMATTED-DAYS-LIT
               MOVE SPACES    TO MO-DAYS-LIT
               MOVE SPACES    TO MO-FMT-DATE
               MOVE SPACES    TO MO-DAYS-ALIVE-LIT
               MOVE SPACES    TO MO-DAYS-CHAR
               MOVE SPACES    TO MO-DAYS2-LIT
               MOVE 'IF YOU WANT TO RETIRE AT 65'
                             TO MO-RET-TITLE
               MOVE L-RETIREMENT-DATE
                             TO MO-RET-DATE
               MOVE 'YOU WILL REACH AGE 65 ON:'
                             TO MO-RET-LITERAL
             END-IF
           ELSE
               MOVE SPACES    TO MO-FORMATTED-DAYS-LIT
               MOVE SPACES    TO MO-DAYS-LIT
               MOVE SPACES    TO MO-FMT-DATE
               MOVE SPACES    TO MO-DAYS-ALIVE-LIT
               MOVE SPACES    TO MO-DAYS-CHAR
               MOVE SPACES    TO MO-DAYS2-LIT
               MOVE SPACES    TO MO-RET-TITLE
               MOVE SPACES    TO MO-RET-LITERAL
               MOVE SPACES    TO MO-RET-DATE
           END-IF.
           CALL 'CBLTDLI' USING ISRT-LIT, IOPCB,  MSG-OUT-DATA.