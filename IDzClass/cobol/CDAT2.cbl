   CBL NODYNAM,APOST,LIB,OBJECT,NOOPT,NODECK,NOTERM,NONAME
       ID DIVISION.
       PROGRAM-ID. CDAT2.
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
      *
      *    THIS PROGRAM WILL BE CALLED BY ANOTHER, RECEIVE A
      *    DATE(YYYYMMDD) AND DETERMINE THE NUMBER OF DAYS
      *    SINCE CURRENT DATE.
      *
      *    (C) 2003 IBM - KEVIN J. CUMMINGS RESERVED.
      *    CONVERTED FOR CICS BY DOUG STOUT
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. FLEX-ES.
       OBJECT-COMPUTER. FLEX-ES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 W-INPUT-DATE            PIC 9(8).
       01 W-INPUT-DATE-INT        PIC 9(9) COMP.

       01 W-CURRENT-DATE          PIC 9(8).
       01 W-CURRENT-DATE-INT      PIC 9(9).

       01 W-DAY-DIFFERENCE        PIC 9(9).

       01 W-PICSTR-IN.
          10  W-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
          10  W-PICSTR-STR-IN     PIC X(8)
               value 'YYYYMMDD'.

       01 W-DATE-IN-CEE.
          10  W-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
          10  W-DATE-IN-STR-CEE   PIC X(8).

       01 FC.
          10  FC-SEV              PIC S9(4) COMP.
          10  FC-MSG              PIC S9(4) COMP.
          10  FC-CTW              PIC X.
          10  FC-FAC              PIC X(3).
          10  FC-ISI              PIC S9(8) COMP.
       01 W-OUT-DATE              PIC X(80).
       01 W-PICSTR.
          10  W-PICSTR-LTH        PIC S9(4) COMP VALUE 29.
          10  W-PICSTR-STR        PIC X(29)
                 VALUE 'Wwwwwwwwwz DD Mmmmmmmmmz YYYY'.
      *
       01  INTERFACE-AREA.
           05  L-INPUT-DATE.
               10 L-YYYY            PIC 9(4).
               10 L-MM              PIC 9(2).
               10 L-DD              PIC 9(2).
           05  L-DAY-DIFFERENCE     PIC 9(9).
           05  L-DATE-FORMATTED     PIC X(29).
           05  L-PROGRAM-RETCODE    PIC 9(4).
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA              PIC X(50).

       PROCEDURE DIVISION.
      *
       A000-MAINLINE.
           MOVE DFHCOMMAREA TO INTERFACE-AREA.
           PERFORM A100-OBTAIN-CURRENT-DATE
           PERFORM A200-CALCULATE-DAY-DIFFERENCE
           PERFORM A300-FORMAT-DATE
           MOVE INTERFACE-AREA TO DFHCOMMAREA.
           EXEC CICS
               RETURN
           END-EXEC
           .
      *
       A100-OBTAIN-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO W-CURRENT-DATE
           COMPUTE W-CURRENT-DATE-INT = ,
              FUNCTION INTEGER-OF-DATE(W-CURRENT-DATE)
           .
      *
       A200-CALCULATE-DAY-DIFFERENCE.
           MOVE L-INPUT-DATE TO W-INPUT-DATE

           COMPUTE W-INPUT-DATE-INT = ,
              FUNCTION INTEGER-OF-DATE(W-INPUT-DATE)

           COMPUTE W-DAY-DIFFERENCE = ,
              W-CURRENT-DATE-INT - W-INPUT-DATE-INT

           MOVE W-DAY-DIFFERENCE TO L-DAY-DIFFERENCE
           MOVE 0                TO L-PROGRAM-RETCODE
           MOVE 0                TO L-DATE-FORMATTED
           .
       A300-FORMAT-DATE.

           MOVE W-INPUT-DATE TO W-DATE-IN-STR-CEE

           CALL 'CEEDAYS' USING W-DATE-IN-CEE
               W-PICSTR-IN, W-INPUT-DATE-INT, FC

           IF FC-SEV NOT = ZERO
              MOVE 'BAD DATE' to L-DATE-FORMATTED
              MOVE FC-MSG TO L-PROGRAM-RETCODE
           ELSE
              MOVE W-OUT-DATE TO L-DATE-FORMATTED
              MOVE 0 TO L-PROGRAM-RETCODE
           END-IF

           CALL 'CEEDATE' USING W-INPUT-DATE-INT,
                W-PICSTR, W-OUT-DATE, FC.

           IF FC-SEV NOT = ZERO
              MOVE 'BAD DATE' TO L-DATE-FORMATTED
              MOVE FC-MSG TO L-PROGRAM-RETCODE
           ELSE
              MOVE W-OUT-DATE TO L-DATE-FORMATTED
              MOVE 0 TO L-PROGRAM-RETCODE
           END-IF.