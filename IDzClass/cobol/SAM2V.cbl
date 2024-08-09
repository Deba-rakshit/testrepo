       CBL NOOPT
      ****************************************************************
      * PROGRAM:  SAM2V
      *           Sample program for the ENTERPRISE COBOL Compiler
      *
      * AUTHOR :  Doug Stout
      *           IBM PD Tools
      *
      * THIS PROGRAM IS SIMILAR TO SAM2, EXCEPT:
      *   - IT ACCEPTS A PARMS TO CONTROL CPU LOOPS, FOR
      *     USE WITH APA HANDS-ON LABS
      *
      *
      * Part of a sample application used as a teaching aid for
      * Debug Tool, Fault Analyzer, and APA workshops.
      *
      * SUBROUTINE TO CALCULATE CUSTOMER STATISTICS
      *   - Called by program SAM1
      *
      * This program has a bug that can be analyzed during the
      * DEBUG TOOL workshop:
      *  -The variable BALANCE-MAX results with an incorrect value.
      *   BALANCE-MAX *should, but does not* contain the highest
      *   value found in variable cust-acct-balance from all
      *   customer records that are passed from the calling program.
      *
      *****************************************************************
      * Linkage:
      *      parameters:
      *        1: Customer Record    (passed and not changed)
      *        2: Statistics area    (passed and modified)
      *        3: LOOP COUNT         (PASSED AND NOT CHANGED)
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAM2V.
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
           05  LOOP-COUNT           PIC S9(9)     COMP-3  VALUE +0.

      *****************************************************************
       LINKAGE SECTION.

       COPY CUST2CPY REPLACING ==:TAG:== BY ==CUST==.
      *
       01  CUST-BALANCE-STATS.
           05  BALANCE-COUNT         PIC S9(13)V99.
           05  BALANCE-TOTAL         PIC S9(15)V99.
           05  BALANCE-MIN           PIC S9(7)V99     COMP-3.
           05  BALANCE-MAX           PIC S9(7)V99     COMP-3.
           05  BALANCE-RANGE         PIC S9(7)V99     COMP-3.
           05  BALANCE-AVERAGE       PIC S9(15)V99.
      *
       01  CRUNCH-CPU-LOOPS          PIC S9(9)  COMP-3.

      *****************************************************************
       PROCEDURE DIVISION USING CUST-REC, CUST-BALANCE-STATS,
                                     CRUNCH-CPU-LOOPS.
       000-MAIN.
           MOVE 'PROGRAM STARTED' TO WS-PROGRAM-STATUS.
           IF WS-FIRST-TIME-SW = 'Y'
               PERFORM 500-INIT-STATISTICS.
           PERFORM 050-CALC-BALANCE-STATISTICS.
           MOVE 'N' TO WS-FIRST-TIME-SW
           MOVE 'PROGRAM ENDED' TO WS-PROGRAM-STATUS.
           GOBACK.

       050-CALC-BALANCE-STATISTICS.
           MOVE 0 TO LOOP-COUNT.
           PERFORM 100-CRUNCH-LOOP
               UNTIL LOOP-COUNT > CRUNCH-CPU-LOOPS .

       100-CRUNCH-LOOP.
           MOVE  'CALCULATING BALANCE STATS' TO WS-PROGRAM-STATUS.
      *    *** Increment Record Count ***
           ADD +1 TO BALANCE-COUNT
      *    *** Add this customer's BALANCE to the grand total ***
           COMPUTE BALANCE-TOTAL =
              BALANCE-TOTAL + CUST-ACCT-BALANCE
      *    *** Calculate Average ***
           COMPUTE BALANCE-AVERAGE =
              BALANCE-TOTAL / BALANCE-COUNT
      *    *** Calculate Minimum ***
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE CUST-ACCT-BALANCE TO BALANCE-MIN.
           IF CUST-ACCT-BALANCE < BALANCE-MIN
              MOVE CUST-ACCT-BALANCE TO BALANCE-MIN.
      *    *** Calculate Maximum ***
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE CUST-ACCT-BALANCE TO BALANCE-MAX.
           IF CUST-ACCT-BALANCE > BALANCE-MAX
              MOVE CUST-ACCT-BALANCE TO BALANCE-MAX.
      *    *** CALCULATE RANGE ***
           COMPUTE BALANCE-RANGE = BALANCE-MAX - BALANCE-MIN.
           ADD 1 TO LOOP-COUNT.

       500-INIT-STATISTICS.
           MOVE 'ZEROING STATS VARIABLES' TO WS-PROGRAM-STATUS.
           MOVE 0  TO BALANCE-COUNT.
           MOVE 0  TO BALANCE-TOTAL.
           MOVE 0  TO BALANCE-MIN.
           MOVE 0  TO BALANCE-MAX.
           MOVE 0  TO BALANCE-RANGE.
           MOVE 0  TO BALANCE-AVERAGE.

      *  END OF PROGRAM SAM2