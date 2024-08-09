   CBL NODYNAM,APOST,LIB,OBJECT,NOOPT,NODECK,NOTERM,NONAME
       ID DIVISION.
       PROGRAM-ID. CDAT3.
      *    THIS IS A SAMPLE PROGRAM FOR DEMONSTRATION 2004
      *
      *    THIS PROGRAM WILL BE CALLED BY ANOTHER, RECEIVE A
      *    DATE(YYMMDD) AND DETERMINE A PROPER FORMATTED
      *    RETIREMENT DATE.
      *
      *    (C) 2004 IBM - KEVIN J. CUMMINGS RESERVED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. FLEX-ES.
       OBJECT-COMPUTER. FLEX-ES.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  W-WORK-DATE                       PIC S9(9) COMP.
       01  LILIAN                            PIC S9(9) COMP.
       01  CHRDATE                           PIC X(80).

       01  IN-DATE.
           02  IN-DATE-LENGTH                PIC S9(4) COMP.
           02  IN-DATE-CHAR                  PIC X(50).

       01  PICSTR.
           02  PICSTR-LENGTH                 PIC S9(4) COMP.
           02  PICSTR-CHAR                   PIC X(50).

       01  FC.
          10  FC-SEV                         PIC S9(4) COMP.
          10  FC-MSG                         PIC S9(4) COMP.
          10  FC-CTW                         PIC X.
          10  FC-FAC                         PIC XXX.
          10  FC-ISINFO                      PIC S9(9) COMP.

      *
       LINKAGE SECTION.

       COPY CDAT3L.

      *

       PROCEDURE DIVISION USING W-CDAT3-LINKAGE-AREA.
      *
       A000-MAINLINE.
           PERFORM A100-DETERMINE-RETIREMENT
           IF W-PROGRAM-RETCODE = 0
              PERFORM A200-FORMAT-DATE
           GOBACK
           .

       A100-DETERMINE-RETIREMENT.
      ****************************************************
      ** ADD 65 TO BIRTY DATE AND CALL CEEDAYS TO       **
      ** GET LILIAN DATE (NO DAYS FROM 1582/08/14)      **
      ****************************************************

           ADD +65 TO W-BD-YYYY
           MOVE 8 TO IN-DATE-LENGTH
           MOVE W-BIRTHDATE-IN TO
              IN-DATE-CHAR(1:8)
           MOVE 8 TO PICSTR-LENGTH
           MOVE "YYYYMMDD" TO PICSTR-CHAR
           CALL "CEEDAYS" USING IN-DATE, PICSTR,
                                LILIAN, FC.


      *************************************************
      ** IF CEEDAYS RUNS SUCCESSFULLY, THEN ADD +65  **
      ** TO BIRTHDATE TO DETERMINE RETIREMENT DATE   **
      *************************************************
           IF  FC-SEV = 0    THEN
               MOVE 0 TO W-PROGRAM-RETCODE
           ELSE
               MOVE 'ERROR IN CALL TO CEEDAYS' TO W-RETIREMENT-ERRMSG
               MOVE FC-MSG TO W-PROGRAM-RETCODE
           END-IF
           .
      *

       A200-FORMAT-DATE.
      *************************************************
      ** SPECIFY PICTURE STRING THAT DESCRIBES THE   **
      **  DESIRED FORMAT OF THE OUTPUT FROM CEEDATE, **
      **  AND THE PICTURE STRING'S LENGTH.           **
      *************************************************
           MOVE 37 TO PICSTR-LENGTH
           MOVE "Wwwwwwwwwwz, ZD Mmmmmmmmmmmmmmz YYYY" TO
                        PICSTR-CHAR

      *************************************************
      ** CALL CEEDATE TO CONVERT THE LILIAN DATE     **
      **     TO  A PICTURE STRING.                   **
      *************************************************
           CALL "CEEDATE" USING LILIAN, PICSTR,
                                CHRDATE, FC.


      *************************************************
      ** IF CEEDATE RUNS SUCCESSFULLY, DISPLAY RESULT**
      *************************************************
           IF FC-SEV = 0        THEN
               MOVE CHRDATE TO W-RETIREMENT-DATE
           ELSE
               MOVE 'ERROR IN CALL TO CEEDATE' TO W-RETIREMENT-ERRMSG
               MOVE FC-MSG TO W-PROGRAM-RETCODE
           END-IF
           .
      * END OF PROGRAM