       IDENTIFICATION DIVISION.
       PROGRAM-ID. PHONEC1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CMPTMPA PIC S9(9) COMP-5.
       01  TEMP         PIC X(6).
       01  TEMP2         PIC X(6).
          EXEC SQL INCLUDE SQLDA END-EXEC.
          EXEC SQL INCLUDE SQLCA END-EXEC.
          COPY IDzDCLS.
       LINKAGE SECTION.
      *****************************************************
      * STRUCTURE FOR INPUT                               *
      *****************************************************
       01  PHONEDATA.
          02  ACTION             PIC X(01).
          02  INDATA.
             03 INFNAME            PIC X(10).
             03 NEWPHONE           PIC X(10).
          02 RESULT PIC X(1).
             88 ACTION-SUCCEED  VALUE 'Y'.
             88 ACTION-FAILED VALUE 'N'.
             88 INPUT-ERROR VALUE 'E'.
          02 STATUS-MSG-LEN PIC 9(3).
          02 STATUS-MSG PIC X(128).
       PROCEDURE DIVISION USING PHONEDATA.
       MAINLINE SECTION.
      *     EXEC SQL CONNECT TO DBAG END-EXEC.
           DISPLAY 'PHONEC1 STARTED...'
           PERFORM VALIDATE-INPUT
           PERFORM TELEPHONE-UPDATE
           DISPLAY 'PHONEC1 SUCCESSFUL'
           GOBACK
           .
       VALIDATE-INPUT.
           IF ACTION = 'U'
             CONTINUE
           ELSE
             MOVE 1 TO CMPTMPA
             STRING 'PHONEC1 ERROR, ACTION IS NOT U.'
               DELIMITED BY SIZE INTO STATUS-MSG
               WITH POINTER CMPTMPA
             END-STRING
             COMPUTE STATUS-MSG-LEN = CMPTMPA - 1
             SET INPUT-ERROR TO TRUE
             MOVE 16 TO RETURN-CODE
             GOBACK
           END-IF.

      *****************************************************
      * UPDATES PHONE NUMBERS FOR EMPLOYEES               *
      *****************************************************
       TELEPHONE-UPDATE.
           DISPLAY 'TELEPHONE-UPDATE STARTED...'
           EXEC SQL UPDATE IBMUSER.FRIENDZ
                SET   PHONE    = :NEWPHONE
                WHERE FNAME = :INFNAME END-EXEC.
           IF SQLCODE = ZERO
      *                                         **EMPLOYEE FOUND
      *                                         **UPDATE SUCCESSFUL
             CONTINUE
           ELSE
      *                                           **NO EMPLOYEE FOUND
      *                                           **UPDATE FAILED
      *                                           **PRINT ERROR MESSAGE
             MOVE 1 TO CMPTMPA
             DISPLAY "UPDATE ERROR:" SQLCODE
             DISPLAY "INFNAME:" INFNAME
             STRING 'PHONEC1 ERROR, SQLCODE IS NOT ZERO.'
               DELIMITED BY SIZE INTO STATUS-MSG
               WITH POINTER CMPTMPA
             END-STRING
             COMPUTE STATUS-MSG-LEN = CMPTMPA - 1
             SET ACTION-FAILED TO TRUE
             MOVE 16 TO RETURN-CODE
      *       GOBACK
           END-IF.
           DISPLAY "SQL SELECT....."
           EXEC SQL
             SELECT FNAME, LNAME, PHONE, EMAIL
             INTO  :FNAME,
                   :LNAME,
                   :PHONE,
                   :EMAIL
             FROM IBMUSER.FRIENDZ
             WHERE FNAME = :INFNAME
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
             DISPLAY "SELECT ERROR:" SQLCODE
           END-IF

           DISPLAY "FIRSTNAME:".
           DISPLAY FNAME.
           DISPLAY "LASTNAME:".
           DISPLAY LNAME.
           DISPLAY "PHONE:".
           DISPLAY PHONE.
           DISPLAY "EMAIL:".
           DISPLAY EMAIL.
           EXEC SQL ROLLBACK WORK END-EXEC.
       END PROGRAM 'PHONEC1'.