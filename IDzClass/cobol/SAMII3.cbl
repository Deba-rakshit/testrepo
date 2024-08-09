      ****************************************************************
      * PROGRAM:  SAMII3
      *           Sample program for the VS COBOL II Compiler
      *
      * AUTHOR :  Doug Stout
      *           IBM PD Tools
      *
      * Part of a sample application used as a teaching aid for
      * Debug Tool, Fault Analyzer, and APA workshops.
      *
      * SUBROUTINE TO CALCULATE PRODUCT STATISTICS
      *   - Called by program SAMII1
      *
      *****************************************************************
      * Linkage:
      *      parameters:
      *        1: Product Record     (passed and not changed)
      *        2: Statistics area    (passed and modified)
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMII3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *****************************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *
       01  WS-FIELDS.
           05  WS-PROGRAM-STATUS    PIC X(30)     VALUE SPACES.
           05  WS-FIRST-TIME-SW     PIC X         VALUE 'Y'.
           05  WS-WORK-NUM-1        PIC S9(7)     COMP-3  VALUE +0.
           05  WS-WORK-NUM-2        PIC S9(7)     COMP-3  VALUE +0.
           05  WS-WORK-NUM-3        PIC S9(7)     COMP-3  VALUE +0.
           05  WS-WORK-NUM-4        PIC S9(7)     COMP-3  VALUE +0.
           05  WS-WORK-NUM-5        PIC S9(7)     COMP-3  VALUE +0.

      *****************************************************************
       LINKAGE SECTION.

       COPY CUST2PRO.

       01  PRODUCT-STATS.
           05  SERV-CALLS-COUNT      PIC S9(7)    COMP-3.
           05  SERV-CALLS-TOTAL      PIC S9(7)    COMP-3.
           05  SERV-CALLS-MIN        PIC S9(7)    COMP-3.
           05  SERV-CALLS-MAX        PIC S9(7)    COMP-3.
           05  SERV-CALLS-RANGE      PIC S9(7)    COMP-3.
           05  SERV-CALLS-AVG        PIC S9(7)V99 COMP-3.

      *****************************************************************
       PROCEDURE DIVISION USING PRODUCT-RECORD, PRODUCT-STATS.

       000-MAIN.
           MOVE 'PROGRAM STARTED' TO WS-PROGRAM-STATUS.
           IF WS-FIRST-TIME-SW = 'Y'
               PERFORM 500-INIT-STATISTICS.
           PERFORM 100-CALC-PRODUCT-STATISTICS.
           MOVE 'N' TO WS-FIRST-TIME-SW
           MOVE 'PROGRAM ENDED' TO WS-PROGRAM-STATUS.
           GOBACK.

       100-CALC-PRODUCT-STATISTICS.
           MOVE  'CALCULATING PRODUCT STATS' TO WS-PROGRAM-STATUS.
      *    *** Increment Record Count ***
           ADD +1 TO SERV-CALLS-COUNT
      *    *** Add this customer's SERV-CALL to the grand total ***
           COMPUTE SERV-CALLS-TOTAL =
              SERV-CALLS-TOTAL + SERVICE-CALLS
      *    *** Calculate Average ***
           COMPUTE SERV-CALLS-AVG =
              SERV-CALLS-TOTAL / SERV-CALLS-COUNT
      *    *** Calculate Minimum ***
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE SERVICE-CALLS TO SERV-CALLS-MIN.
           IF SERVICE-CALLS < SERV-CALLS-MIN
              MOVE SERVICE-CALLS TO SERV-CALLS-MIN.
      *    *** Calculate Maximum ***
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE SERVICE-CALLS TO SERV-CALLS-MAX.
           IF SERVICE-CALLS > SERV-CALLS-MAX
              MOVE SERVICE-CALLS TO SERV-CALLS-MAX.
      *    *** CALCULATE RANGE ***
           COMPUTE SERV-CALLS-RANGE = SERV-CALLS-MAX - SERV-CALLS-MIN.

       500-INIT-STATISTICS.
           MOVE 'ZEROING STATS VARIABLES' TO WS-PROGRAM-STATUS.
           MOVE 0  TO SERV-CALLS-COUNT.
           MOVE 0  TO SERV-CALLS-TOTAL.
           MOVE 0  TO SERV-CALLS-MIN.
           MOVE 0  TO SERV-CALLS-MAX.
           MOVE 0  TO SERV-CALLS-RANGE.
           MOVE 0  TO SERV-CALLS-AVG.

      *  END OF PROGRAM SAMII3