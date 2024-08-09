       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSVSAM.
       AUTHOR. R. Barosa.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POTVSAM-FILE
                  ASSIGN       to POTVSAM
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is CUST-NO
                  FILE STATUS  is POTVSAM-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  POTVSAM-FILE.
       COPY POTVSAM.
       WORKING-STORAGE SECTION.
      * --------------------------------------------------------------- -----
       01  WS-PROGRAM    PIC X(08)  VALUE 'CUSVSAM'.
       01  WS-LITERAL-WS PIC X(48)  VALUE
            '        WORKING STORAGE STARTS HERE'.
       01  W-POTVSAM-RECORD.
           03 W-CUST-NO            PIC 999.
           03 W-CUST-LN            PIC X(25).
           03 W-CUST-FN            PIC X(15).
           03 W-CUST-ADDR1         PIC X(20).
           03 W-CUST-CITY          PIC X(20).
           03 W-CUST-ST            PIC X(5).
           03 W-CUST-CTRY          PIC X(15).
       01  POTVSAM-STATUS        PIC XX.
       01 WORK-FIELDS.
                03 INPUT-NAME               Pic x(30).
                03 PROGRAM-TO-CALL          PIC X(07).
                03 RECEIVED-FROM-CALLED     PIC 99.
                03 VALUE1                   PIC 99.
                03 FIELD-A                  PIC X(6).
                03 FIELD-B                  PIC X(6).
                03 FIELD-C                  PIC X(6).
                03 WHICH-LAB                PIC X(4).
                03 RESULT                   PIC 99.
                03 BRANCHFLAG               PIC 99.
      * ==========================POTVSAM-==============================
       PROCEDURE DIVISION.
           DISPLAY "Program CUSVSAM starting.RS.  "
           OPEN INPUT  POTVSAM-FILE
           IF POTVSAM-STATUS  = '00'
               CONTINUE
           ELSE
               DISPLAY 'POTVSAM: FAILURE-OPEN-!!!'
               GOBACK.
           MOVE 1 to W-CUST-NO.
           PERFORM 0100-POTVSAM-READ THRU
                   0100-EXIT UNTIL W-CUST-NO = 010.
           CLOSE POTVSAM-FILE.
           GO TO 0150-SECOND-PART.
       0100-POTVSAM-READ.
      *     MOVE W-CUST-NO
           READ POTVSAM-FILE INTO W-POTVSAM-RECORD  KEY IS CUST-NO
               INVALID KEY
               DISPLAY 'KEY ' CUST-NO  'ERROR READING VSAM FILE'
               GOBACK.
               DISPLAY 'CUST NO: ' W-CUST-NO ' '   W-CUST-FN  W-CUST-LN
                   W-CUST-ADDR1 W-CUST-CITY W-CUST-CTRY
           ADD 1 TO  W-CUST-NO.
       0100-EXIT.
           EXIT.
      *  --------------------------------------------------------------------
       0150-SECOND-PART.
              MOVE 2 TO BRANCHFLAG.
              MOVE 'AAAAAA' to FIELD-A.
              MOVE 'BBBBBB' to FIELD-B.
              MOVE 'CCCCCC' to FIELD-C.
              MOVE "LAB2" to WHICH-LAB.
       0200-LOGIC.
           IF WHICH-LAB = 'LAB2'
      * If is LAB2 lets do a dynamic CALL.. and force a divide by ZERO
              MOVE "REGI0B" TO PROGRAM-TO-CALL
              CALL  PROGRAM-TO-CALL USING RECEIVED-FROM-CALLED
              MOVE  66 TO VALUE1
              DIVIDE VALUE1 BY RECEIVED-FROM-CALLED GIVING RESULT
              DISPLAY "The result is ... " RESULT
           END-IF
              IF BRANCHFLAG > 1
                   CALL 'REGI0C' USING Input-name
                   DISPLAY "BRANCHFLAG GREATER THAN 1"
                   PERFORM 0300-SEEYA
              ELSE
                   DISPLAY "BRANCHFLAG <= 1 no STATIC CALL"
                   PERFORM 0400-GOODBYE.
       0300-SEEYA.
              DISPLAY "EXECUTED SEEYA PARAGRAPH".
       0400-GOODBYE.
              DISPLAY "EXECUTED GOODBYE PARAGRAPH".
