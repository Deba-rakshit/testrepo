      ****************************************************************
      * PROGRAM:  SAMOS2
      *           Sample program for the OS/VS COBOL Compiler
      *
      * AUTHOR :  Doug Stout
      *           IBM PD Tools
      *
      * Part of a sample application used as a teaching aid for
      * Debug Tool, Fault Analyzer, and APA workshops.
      *
      * SUBROUTINE TO CALCULATE CUSTOMER STATISTICS
      *   - Called by program SAMOS1
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
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMOS2.
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

      **** 01  COPY CUST2COB REPLACING ==CUST== BY ==CUST==.
      *   ---------------------------------------------------
      *   Sample COBOL Copybook for IBM PD Tools Workshops
      *   Describes file <userid>.ADLAB.FILES(CUST2)
      *   ---------------------------------------------------
       01  CUST-REC.
           05  CUST-KEY.
               10  CUST-ID               PIC X(5).
               10  CUST-RECORD-TYPE      PIC X.
               10  FILLER                PIC X(7).
           05  CUST-NAME               PIC X(17).
           05  CUST-ACCT-BALANCE       PIC S9(7)V99  COMP-3.
           05  CUST-ORDERS-YTD         PIC S9(4)     COMP.
           05  CUST-CITY               PIC X(15).
           05  CUST-OCCUPATION         PIC X(28).

       01  CUST-BALANCE-STATS.
           05  BALANCE-COUNT        PIC S9(7)V99  COMP-3.
           05  BALANCE-TOTAL        PIC S9(7)V99  COMP-3.
           05  BALANCE-MIN          PIC S9(7)V99  COMP-3.
           05  BALANCE-MAX          PIC S9(7)V99  COMP-3.
           05  BALANCE-RANGE        PIC S9(7)V99  COMP-3.
           05  BALANCE-AVERAGE      PIC S9(7)V99  COMP-3.

      *****************************************************************
       PROCEDURE DIVISION USING CUST-REC, CUST-BALANCE-STATS.

       000-MAIN.
           MOVE 'PROGRAM STARTED' TO WS-PROGRAM-STATUS.
           IF WS-FIRST-TIME-SW = 'Y'
               PERFORM 500-INIT-STATISTICS.
           PERFORM 100-CALC-BALANCE-STATISTICS.
           MOVE 'N' TO WS-FIRST-TIME-SW
           MOVE 'PROGRAM ENDED' TO WS-PROGRAM-STATUS.
           GOBACK.

       100-CALC-BALANCE-STATISTICS.
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
      *    ***   There is a bug calculating the maximum.  ***
      *    ***            Can you find it?                ***
           IF WS-FIRST-TIME-SW = 'Y'
              MOVE CUST-ACCT-BALANCE TO BALANCE-MAX
           IF CUST-ACCT-BALANCE > BALANCE-MAX
              MOVE CUST-ACCT-BALANCE TO BALANCE-MAX.
      *    *** CALCULATE RANGE ***
           COMPUTE BALANCE-RANGE = BALANCE-MAX - BALANCE-MIN.

       500-INIT-STATISTICS.
           MOVE 'ZEROING STATS VARIABLES' TO WS-PROGRAM-STATUS.
           MOVE 0  TO BALANCE-COUNT.
           MOVE 0  TO BALANCE-TOTAL.
           MOVE 0  TO BALANCE-MIN.
           MOVE 0  TO BALANCE-MAX.
           MOVE 0  TO BALANCE-RANGE.
           MOVE 0  TO BALANCE-AVERAGE.

      *  END OF PROGRAM SAMOS2