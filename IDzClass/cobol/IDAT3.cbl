   CBL FLAG(I,W),NUMPROC(MIG),DYN
       ID DIVISION.
       PROGRAM-ID. IDAT3.
      *
      *    THIS PROGRAM WILL BE CALLED BY ANOTHER, RECEIVE A
      *    DATE(YYMMDD) AND DETERMINE A PROPER FORMATTED
      *    RETIREMENT DATE.
      *
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

       01 W-ADC03-LINKAGE-AREA.
          10 W-BIRTHDATE-IN.
             15 W-BD-YYYY                     PIC 9(4).
             15 W-BD-MM                       PIC 9(2).
             15 W-BD-DD                       PIC 9(2).
          10 W-RETIREMENT-DATE                PIC X(80).
          10 W-PROGRAM-RETCODE                PIC 9(4) VALUE 0.
             88 W-ADC03-REQUEST-SUCCESS VALUE 0.
          10 W-RETIREMENT-ERRMSG              PIC X(30).
          10 W-FORCE-ABEND                    PIC 9(3) COMP-3.
      *

       PROCEDURE DIVISION USING W-ADC03-LINKAGE-AREA.
      *
       A000-MAINLINE.
           PERFORM A100-DETERMINE-RETIREMENT
           IF W-PROGRAM-RETCODE = 0
              PERFORM A200-FORMAT-DATE
           GOBACK
           .

       A100-DETERMINE-RETIREMENT.
      ****************************************************
      ** ADD 65 TO BIRTH DATE AND CALL CEEDAYS TO       **
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
      * FOLLOWING IS SIMPLY TO FORCE ABEND FOR
      * DEMONSTRATION OF FAULT ANALYZER UNDER CICS

      *    ADD 1 TO W-FORCE-ABEND
           .
      * END OF PROGRAM