       IDENTIFICATION DIVISION.
      ******************************************************
      *  PROGRAM NAME: DTDEM2  VERSION TDM                 *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL                  *
      *                                                    *
      ******************************************************
       PROGRAM-ID.             ATCDEM2.
       AUTHOR.                 TIM MAGEE.
           DATE-WRITTEN.       03/22/02.
           DATE-COMPILED.      CURRENT-DATE.
           INSTALLATION.       IBM LEXINGTON.
           REMARKS.
              PURPOSE.
              THIS PROGRAM IS DEFINED TO TEST A NUMBER OF THE
              APPLICATION TESTING COLLECTION AND DEBUG TOOL FUNCTIONS
      *    SKIP3
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-370.
       OBJECT-COMPUTER.        IBM-370.
      *    EJECT

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 TAPARM1      PIC 99 VALUE 5.
       01 TAPARM2      PIC 99 VALUE 2.
       01 ATCDEM3      PIC X(7) VALUE 'ATCDEM3'.
       01 P1PARM1      PIC 99 VALUE 0.

       01 TASTRUCT.
         05 LOC-ID.
           10 STATE    PIC X(2).
           10 CITY     PIC X(3).
         05 OP-SYS     PIC X(3).

       PROCEDURE DIVISION.

      * THE FOLLOWING ALWAYS PERFORMED

      * ACCESS BY TOP LEVEL QUALIFIER
           MOVE 'ILCHIMVS' TO TASTRUCT.

      * ACCESS BY MID LEVEL QUALIFIERS
           MOVE 'ILSPR' TO LOC-ID.
           MOVE 'AIX' TO OP-SYS.

      * ACCESS BY LOW LEVEL QUALIFIERS
           MOVE 'KY' TO STATE.
           MOVE 'LEX' TO CITY.
           MOVE 'VM ' TO OP-SYS.

       PROGA.

      * THIS PERFORM EXECUTED
           PERFORM WITH TEST BEFORE UNTIL TAPARM1 = 0
             SUBTRACT 1 FROM TAPARM1
             CALL 'ATCDEM3'
           END-PERFORM

      * THIS IF ALWAYS FALSE
           IF TAPARM2 = 0
             PERFORM PROCA
           END-IF

      * THIS PERFORM EXECUTED
           PERFORM WITH TEST BEFORE UNTIL TAPARM2 = 0
             SUBTRACT 1 FROM TAPARM2
           END-PERFORM
      *    STOP RUN
           .

       PROCA.
      * PROCA NEVER CALLED
           MOVE 10 TO P1PARM1
           .

      * START OF ATCDEM3 NESTED IN ATCDEM2

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATCDEM3.
      ******************************************************************
      *                                                                *
      * COBOL FOR MVS & VM TEST.                                       *
      *                                                                *
      * ATCDEM3, CALLED BY ATCDEM2.                                    *
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 TBPARM1      PIC 99 VALUE 5.
       01 TBPARM2      PIC 99 VALUE 0.
       01 ATCDEM4      PIC X(7) VALUE 'ATCDEM4'.
       01 P1PARM1      PIC 99 VALUE 0.

       PROCEDURE DIVISION.

       PROGB.
      * THIS PERFORM EXECUTED
           PERFORM WITH TEST BEFORE UNTIL TBPARM1 = 0
             SUBTRACT 1 FROM TBPARM1
             CALL 'ATCDEM4'
           END-PERFORM

      * THIS IF EXECUTED
           IF TBPARM2 = 0
             PERFORM PROCB
           END-IF

      * THIS PERFORM NOT EXECUTED
           PERFORM WITH TEST BEFORE UNTIL TBPARM2 = 0
             SUBTRACT 1 FROM TBPARM2
           END-PERFORM
           .

       PROCB.
      * PROCB EXECUTED
           MOVE 10 TO P1PARM1
           .

      *    EXIT PROGRAM.

       END PROGRAM ATCDEM3.
       END PROGRAM ATCDEM2.