       ID DIVISION.
       PROGRAM-ID. CDAT1.
      *    CICS / DEBUG TOOL DEMO PROGRAM
      *
      *    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO
      *    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM
      *    CURRENT DATE.
      *
      *    (C) 2004 IBM - KEVIN J. CUMMINGS RESERVED.
      *    CONVERTED FOR CICS BY JANICE WINCHELL AND DOUG STOUT
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-FLEX-ES.
       OBJECT-COMPUTER. IBM-FLEX-ES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  W-DATE-VALID-SW                     PIC XX.

       01  CDAT2-INTERFACE-AREA.
           05 L-INPUT-DATE.
              10 L-YYYY                        PIC 9(4) VALUE 0.
              10 L-MM                          PIC 9(2) VALUE 0.
              10 L-DD                          PIC 9(2) VALUE 0.
           05 L-DAY-DIFFERENCE                 PIC 9(9).
           05 L-DATE-FORMATTED                 PIC X(29).
           05 L-PROGRAM-RETCODE                PIC 9(4).

       01  WS-COMM-AREA.
           05  W-COM-TRAN-EXEC-COUNT           PIC 9999  VALUE 0.
           05  W-COM-PROGRAM-AREA.
               10  W-COM-USER-REQUEST          PIC S9.
               10  W-COM-DATE-INDICATOR        PIC 99.
               10  W-COM-DAY-DIFFERENCE        PIC 9(9).
               10  W-COM-DATE-FORMATTED        PIC X(29).
               10  W-COM-RETIRE-DATE-OK        PIC X.
               10  W-COM-RETIRE-DATE           PIC X(29).
               10  W-COM-INPUT-BIRTHDATE.
                   15  W-COM-INPUT-DATE-CCYY   PIC X(4).
                   15  W-COM-INPUT-DATE-MM     PIC X(2).
                   15  W-COM-INPUT-DATE-DD     PIC X(2).
               10  W-COM-CDAT2-RETCODE         PIC 9(4).
                   88 W-COM-CDAT2-SUCCESS         VALUE 0.

       01 W-CALL-PROGRAM                       PIC X(8).

       COPY CDAT3L.

       01  MESSAGE-WORK-AREAS.
           05  END-OF-TRANS-MSG       PIC X(30)
                      VALUE 'APPLICATION ENDED'.
           05  MSG-COUNT              PIC S9(4) COMP    VALUE +0.
           05  MSG-INIT               PIC X(300)        VALUE SPACES.
           05  MSG-LINE               PIC X(60).

       01  S0C7-WORK-AREAS.
           05  SOME-NUMBER            PIC 9(7)          VALUE 0.
           05  BAD-DATA-ALPHA         PIC X(5).
           05  BAD-NUMBER       REDEFINES BAD-DATA-ALPHA
                                      PIC S9(9)  COMP-3.

       01  MESSAGE-VALUES.
           05  ERR-MSG-BAD-KEY       PIC X(50)
                 VALUE 'INVALID KEY PRESSED.  PLEASE TRY AGAIN'.
           05  BDATE-MSG1            PIC X(50)
                 VALUE 'HERE IS YOUR BIRTHDATE AND # OF DAYS ELAPSED'.
           05  BDATE-MSG2.
               10  FILLER            PIC X(24)
                       VALUE 'YOUR BIRTHDATE AND DAY: '.
               10  BDATE-BIRTHDATE     PIC X(36).
           05  BDATE-MSG3.
               10  FILLER            PIC X(25)
                       VALUE 'HOW LONG AGO WAS THIS?   '.
               10  BDATE-NUMBER-OF-DAYS     PIC ZZZ,ZZZ,ZZ9.
               10  FILLER            PIC X(6)
                       VALUE ' DAYS '.
           05  RETIRE-MSG1           PIC X(50)
                       VALUE 'IF YOU WANT TO RETIRE AT 65 '.
           05  RETIRE-MSG2.
               10  FILLER            PIC X(26)
                       VALUE 'YOU WILL REACH AGE 65 ON: '.
               10  RETIRE-DATE       PIC X(30).


           COPY DFHAID.
           COPY CDATMAP.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           05  COM-TRAN-EXEC-COUNT             PIC 9999.
           05  COM-AREA-TRAN-PROGRAM.
               10  COM-PROCESS-INDICATOR       PIC X.
               10  COM-DATE-INDICATOR          PIC X.
               10  COM-DAY-DIFFERENCE          PIC 9(9).
               10  COM-DATE-FORMATTED          PIC X(29).
               10  COM-RETIRE-INDICATOR        PIC X.
               10  COM-RETIRE-DATE             PIC X(29).
               10  COM-INPUT-DATE.
                   15  COM-INPUT-DATE-CCYY     PIC X(4).
                   15  COM-INPUT-DATE-MM       PIC X(2).
                   15  COM-INPUT-DATE-DD       PIC X(2).
               10  COM-CDAT2-RETCODE           PIC 9(4).
                   88 CDAT2-REQUEST-SUCCESS VALUE 0.

       PROCEDURE DIVISION.

       0000-MAINLINE.
           COMPUTE W-COM-TRAN-EXEC-COUNT = W-COM-TRAN-EXEC-COUNT + 1
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   INITIALIZE W-COM-PROGRAM-AREA
                   PERFORM 0300-SEND-ERASE
                   PERFORM 0900-RETURN-PSEUDO
               WHEN EIBAID = DFHENTER
                   MOVE DFHCOMMAREA TO WS-COMM-AREA
                   PERFORM 0100-PROCESS-REQUEST
                   PERFORM 0310-SEND-DATAONLY
                   PERFORM 0900-RETURN-PSEUDO
               WHEN EIBAID = DFHPF3 OR DFHPF12 OR DFHCLEAR
                   PERFORM 0910-RETURN-FINAL
               WHEN OTHER
                   MOVE DFHCOMMAREA TO WS-COMM-AREA
                   MOVE ERR-MSG-BAD-KEY TO MSG-LINE
                   PERFORM 0350-ADD-MESSAGE-LINE
                   PERFORM 0310-SEND-DATAONLY
                   PERFORM 0900-RETURN-PSEUDO
           END-EVALUATE
           .

       0100-PROCESS-REQUEST.
           PERFORM 0400-RECEIVE-MAP.
           MOVE FUNCTION UPPER-CASE(MAPREQI) TO W-COM-USER-REQUEST
      *
      *               B = BIRTHDAY INFO (LINK TO PROGRAM CDAT2)
      *               R = RETIREMENT    (CALL PROGRAM CDAT3)
      *               C = CLEAR
      *               @ = ABEND 0C7
      *
           EVALUATE W-COM-USER-REQUEST
             WHEN 'C'
                 PERFORM 0300-SEND-ERASE
                 PERFORM 0900-RETURN-PSEUDO
             WHEN 'B'
                 MOVE MAPDATI TO W-COM-INPUT-BIRTHDATE
                 PERFORM 0500-VERIFY-INPUT-DATE
                 EVALUATE W-DATE-VALID-SW
                    WHEN 'OK'
                      PERFORM 0600-CALC-DAY-DIFFERENCE
                      MOVE BDATE-MSG1         TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE SPACES             TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE L-DATE-FORMATTED   TO BDATE-BIRTHDATE
                      MOVE BDATE-MSG2         TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE L-DAY-DIFFERENCE   TO BDATE-NUMBER-OF-DAYS
                      MOVE BDATE-MSG3         TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE -1 TO MAPREQL
                    WHEN OTHER
                      MOVE -1 TO MAPDATL
                 END-EVALUATE
            WHEN 'R'
                 MOVE MAPDATI TO W-CDAT3-DATE-IN, W-COM-INPUT-BIRTHDATE
                 PERFORM 0500-VERIFY-INPUT-DATE
                 EVALUATE W-DATE-VALID-SW
                    WHEN 'OK'
                      PERFORM 0650-CALCULATE-RETIREMENT
                      MOVE RETIRE-MSG1        TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE SPACES             TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE W-CDAT3-RETIRE-DATE TO RETIRE-DATE
                      MOVE RETIRE-MSG2        TO MSG-LINE
                      PERFORM 0350-ADD-MESSAGE-LINE
                      MOVE -1 TO MAPREQL
                    WHEN OTHER
                      MOVE -1 TO MAPDATL
                   END-EVALUATE
            WHEN '@'
      *          ABEND WITH S0C7
                 MOVE '!@#$%'  TO BAD-DATA-ALPHA
                 COMPUTE SOME-NUMBER = BAD-NUMBER + 1
                 PERFORM 0910-RETURN-FINAL
            WHEN OTHER
                 MOVE 'UNRECOGNIZED REQUEST' TO MSG-LINE
                 PERFORM 0350-ADD-MESSAGE-LINE
                 MOVE -1 TO MAPREQL
                 PERFORM 0310-SEND-DATAONLY
           END-EVALUATE
               .

       0300-SEND-ERASE.
           MOVE -1 TO MAPDATL.
           EXEC CICS
             SEND MAP ('ADMENU')
                 MAPSET('CDATMAP')
                 MAPONLY
                 ERASE
           END-EXEC.

       0310-SEND-DATAONLY.
           EXEC CICS
             SEND MAP ('ADMENU')
                 MAPSET('CDATMAP')
                 FROM(ADMENUO)
                 DATAONLY
                 CURSOR
           END-EXEC.

       0350-ADD-MESSAGE-LINE.
           COMPUTE MSG-COUNT = MSG-COUNT + 1.
           IF MSG-COUNT = 1
               MOVE MSG-LINE TO MAPMSG1O
               MOVE SPACES TO MAPMSG2O MAPMSG3O
                              MAPMSG4O MAPMSG5O
           END-IF.
           IF MSG-COUNT = 2 THEN MOVE MSG-LINE TO MAPMSG2O.
           IF MSG-COUNT = 3 THEN MOVE MSG-LINE TO MAPMSG3O.
           IF MSG-COUNT = 4 THEN MOVE MSG-LINE TO MAPMSG4O.
           IF MSG-COUNT = 5 THEN MOVE MSG-LINE TO MAPMSG5O.

       0400-RECEIVE-MAP.
           EXEC CICS
                RECEIVE MAP('ADMENU')
                   MAPSET('CDATMAP')
                   INTO (ADMENUI)
           END-EXEC.

       0500-VERIFY-INPUT-DATE.
           IF W-COM-INPUT-BIRTHDATE NUMERIC
              MOVE W-COM-INPUT-BIRTHDATE TO COM-INPUT-DATE, L-INPUT-DATE
              MOVE 'OK' TO W-DATE-VALID-SW
              EVALUATE TRUE
                 WHEN W-COM-INPUT-DATE-CCYY < 1582
                 WHEN W-COM-INPUT-DATE-MM < 01
                 WHEN W-COM-INPUT-DATE-MM > 12
                 WHEN W-COM-INPUT-DATE-DD < 01
                 WHEN W-COM-INPUT-DATE-DD > 31
                 WHEN W-COM-INPUT-BIRTHDATE > FUNCTION CURRENT-DATE(1:8)
                   MOVE 'INPUT DATE INVALID. ENTER IN YYYYMMDD FORMAT'
                      TO MSG-LINE
                   PERFORM 0350-ADD-MESSAGE-LINE
                   MOVE 'XX' TO W-DATE-VALID-SW
              END-EVALUATE
           ELSE
              MOVE 'INPUT DATE NOT NUMERIC - PLEASE REENTER'
                 TO MSG-LINE
              PERFORM 0350-ADD-MESSAGE-LINE
              MOVE 'XX' TO W-DATE-VALID-SW
           END-IF.
      *
       0600-CALC-DAY-DIFFERENCE.
           MOVE 'CDAT2' TO W-CALL-PROGRAM
           MOVE 0        TO W-COM-DAY-DIFFERENCE
           MOVE 0        TO W-COM-CDAT2-RETCODE

           EXEC CICS LINK PROGRAM(W-CALL-PROGRAM)
               COMMAREA(CDAT2-INTERFACE-AREA)
               LENGTH(LENGTH OF CDAT2-INTERFACE-AREA)
           END-EXEC

           IF W-COM-CDAT2-SUCCESS
              MOVE 'Y' TO W-COM-DATE-INDICATOR
           ELSE
              MOVE 'PROBLEMS IN CALL OF CDAT2' TO MSG-LINE
              PERFORM 0350-ADD-MESSAGE-LINE
           END-IF.
      *
       0650-CALCULATE-RETIREMENT.
           MOVE SPACES   TO W-CDAT3-RETIRE-DATE
           MOVE 0        TO W-CDAT3-PROGRAM-RETCODE

           CALL 'CDAT3' USING W-CDAT3-LINKAGE-AREA

           IF W-CDAT3-REQUEST-SUCCESS
              MOVE W-CDAT3-RETIRE-DATE TO W-COM-RETIRE-DATE
              MOVE 'Y' TO W-COM-RETIRE-DATE-OK
           ELSE
              MOVE 'PROBLEMS IN CALCULATING RETIREMENT' TO MSG-LINE
              PERFORM 0350-ADD-MESSAGE-LINE
           END-IF.

       0900-RETURN-PSEUDO.
           EXEC CICS
               RETURN TRANSID('CDAT')
               COMMAREA(WS-COMM-AREA)
           END-EXEC.

       0910-RETURN-FINAL.
           EXEC CICS SEND TEXT FROM (END-OF-TRANS-MSG)
              ERASE
              FREEKB
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.