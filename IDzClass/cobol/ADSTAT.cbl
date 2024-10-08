       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADSTAT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WORK-VARIABLES.
           05  PROGRAM-STATUS PIC X(20) VALUE SPACES.
           05  ACCUM-A        PIC S9(4) VALUE +0.
           05  SUB-A          PIC S9(8) COMP SYNC.
           05  SIGNX          PIC S9(4) COMP SYNC  VALUE +1.
           05  WORK-NUMBER    PIC S9(8) COMP SYNC.
           05  WORK-REM       PIC S9(8) COMP SYNC.
           05  WORK-MIN       PIC S9(8) COMP SYNC.
           05  WORK-MAX       PIC S9(8) COMP SYNC.
           05  ARRAY-SIZE     PIC S9(8) COMP SYNC.
       01  ARRAY-OF-NUMBERS.
           05  NUM            PIC S9(8) COMP OCCURS 5.
       01  RESULTS.
           05  RESULT-SUM     PIC S9(10).
           05  RESULT-AVERAGE PIC ++,+++,++9.99.
           05  RESULT-MIN     PIC S9(10).
           05  RESULT-MAX     PIC S9(10).
           05  RESULT-RANGE   PIC S9(10).
           05  RESULT-MEDIAN  PIC ++,+++,++9.99.

       PROCEDURE DIVISION.

           MOVE  'PROGRAM STARTING' TO PROGRAM-STATUS.
           DISPLAY 'HELLO'.
           PERFORM 100-GENERATE-ARRAY.
           PERFORM 200-CALC-AVG-AND-SUM.
           PERFORM 300-CALC-MIN-MAX-AND-RANGE.
           PERFORM 400-SORT-ARRAY.
           PERFORM 500-CALC-MEDIAN.
           DISPLAY 'GOODBYE'.
           GOBACK.

       100-GENERATE-ARRAY.
           MOVE  'GENERATING NUMBERS INTO ARRAY' TO PROGRAM-STATUS.
           MOVE 5  TO ARRAY-SIZE.
           MOVE 1  TO SUB-A.
           PERFORM UNTIL SUB-A > ARRAY-SIZE
               COMPUTE  WORK-NUMBER = 100 + (SUB-A * 3 * SIGNX)
               MOVE     WORK-NUMBER TO NUM(SUB-A)
               COMPUTE  SIGNX       = SIGNX * -1
               COMPUTE  SUB-A       = SUB-A + 1
           END-PERFORM.

       200-CALC-AVG-AND-SUM.
           MOVE  'CALCULATING AVERAGE AND SUM' TO PROGRAM-STATUS.
           MOVE 0 TO ACCUM-A.
           PERFORM VARYING SUB-A FROM 1 BY 1
                   UNTIL SUB-A > ARRAY-SIZE
               COMPUTE  ACCUM-A  = ACCUM-A + NUM(SUB-A)
           END-PERFORM.
           MOVE ACCUM-A TO  RESULT-SUM.
           COMPUTE RESULT-AVERAGE = ACCUM-A / ARRAY-SIZE.

       300-CALC-MIN-MAX-AND-RANGE.
           MOVE  'CALCULATING MIN, MAX, AND RANGE' TO PROGRAM-STATUS.
           MOVE NUM(1) TO WORK-MIN.
           MOVE NUM(1) TO WORK-MAX.
           PERFORM VARYING SUB-A FROM 2 BY 1
                   UNTIL SUB-A > ARRAY-SIZE
               IF NUM(SUB-A) > WORK-MAX THEN
                   MOVE NUM(SUB-A) TO WORK-MAX
               END-IF
               IF NUM(SUB-A) < WORK-MIN THEN
                   MOVE NUM(SUB-A) TO WORK-MIN
               END-IF
           END-PERFORM.
           MOVE WORK-MIN TO  RESULT-MIN.
           MOVE WORK-MAX TO  RESULT-MAX.
           COMPUTE RESULT-RANGE = WORK-MAX - WORK-MIN.

       400-SORT-ARRAY.
           MOVE 'SORTING ARRAY' TO PROGRAM-STATUS.
      *    ***CALL THE ADSORT ROUTINE TO DO A SIMPLE INSERTION SORT
           CALL 'ADSORT' USING ARRAY-SIZE, ARRAY-OF-NUMBERS.

       500-CALC-MEDIAN.
           MOVE  'CALCULATING MEDIAN VALUE' TO PROGRAM-STATUS.
           DIVIDE ARRAY-SIZE BY 2
               GIVING WORK-NUMBER REMAINDER WORK-REM.
           IF WORK-REM = 0 THEN
      *        *** CALC FOR EVEN NUMBER OF ARRAY ENTRIES
               COMPUTE SUB-A = ARRAY-SIZE / 2
               COMPUTE RESULT-MEDIAN =
                           ( NUM(SUB-A) + NUM(SUB-A + 1) ) / 2
           ELSE
      *        *** CALC FOR ODD NUMBER OF ARRAY ENTRIES
               COMPUTE SUB-A = (ARRAY-SIZE + 1 ) / 2
               COMPUTE RESULT-MEDIAN = NUM(SUB-A)
           END-IF.