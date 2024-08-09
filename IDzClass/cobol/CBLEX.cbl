       IDENTIFICATION DIVISION.

       PROGRAM-ID.  CBLEX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT PRINTFILE ASSIGN TO PRINTER-QPRINT
              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  PRINTFILE
           BLOCK CONTAINS 1 RECORDS
           LABEL RECORDS ARE OMITTED.
       01  PRINT-RECORD PIC X(132).

       WORKING-STORAGE SECTION.
       77  WORK-DAYS PIC S9(4) BINARY VALUE 253.
       77  RAISE-DATE PIC X(11) VALUE "1982-06-01".
       77  PERCENTAGE PIC S999V99 PACKED-DECIMAL.
       77  COMMISSION PIC S99999V99 PACKED-DECIMAL VALUE 2000.00.

      ***************************************************************
      *  Structure for report 1.                                    *
      ***************************************************************

       01  RPT1.
           05  PROJNO    PIC X(6).
           05  EMPNO     PIC X(6).
           05  EMPNO1     PIC X(6).
           05  NAME      PIC X(30).
           05  SALARY    PIC S9(6)V99 PACKED-DECIMAL.


      ***************************************************************
      *  Structure for report 2.                                    *
      ***************************************************************

       01  RPT2.
           15  PROJNO PIC X(6).
           15  PROJECT-NAME PIC X(36).
           15  EMPLOYEE-COUNT PIC S9(4) BINARY.
           15  TOTAL-PROJ-COST PIC S9(10)V99 PACKED-DECIMAL.

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
       77  CODE-EDIT PIC ---99.

      ***************************************************************
      *  Headers for reports.                                       *
      ***************************************************************

       01  RPT1-HEADERS.
           05  RPT1-HEADER1.
               10  FILLER PIC X(21) VALUE SPACES.
               10  FILLER PIC X(111)
                     VALUE "REPORT OF PROJECTS AFFECTED BY RAISES".
           05  RPT1-HEADER2.
               10  FILLER PIC X(9) VALUE "PROJECT".
               10  FILLER PIC X(10) VALUE "EMPID".
               10  FILLER PIC X(35) VALUE "EMPLOYEE NAME".
               10  FILLER PIC X(40) VALUE "SALARY".
       01  RPT2-HEADERS.
           05  RPT2-HEADER1.
               10  FILLER PIC X(21) VALUE SPACES.
               10  FILLER PIC X(111)
                       VALUE "ACCUMULATED STATISTICS BY PROJECT".
           05  RPT2-HEADER2.
               10  FILLER PIC X(9) VALUE "PROJECT".
               10  FILLER PIC X(38) VALUE SPACES.
               10  FILLER PIC X(16) VALUE "NUMBER OF".
               10  FILLER PIC X(10) VALUE "TOTAL".
           05  RPT2-HEADER3.
               10  FILLER PIC X(9) VALUE "NUMBER".
               10  FILLER PIC X(38) VALUE "PROJECT NAME".
               10  FILLER PIC X(16) VALUE "EMPLOYEES".
               10  FILLER PIC X(65) VALUE "COST".
       01  RPT1-DATA.
           05  PROJNO    PIC X(6).
           05  FILLER    PIC XXX VALUE SPACES.
           05  EMPNO     PIC X(6).
           05  FILLER    PIC X(4) VALUE SPACES.
           05  NAME      PIC X(30).
           05  FILLER    PIC X(3) VALUE SPACES.
           05  SALARY    PIC ZZZZZ9.99.
           05  FILLER    PIC X(96) VALUE SPACES.
       01  RPT2-DATA.
           05  PROJNO PIC X(6).
           05  FILLER PIC XXX VALUE SPACES.
           05  PROJECT-NAME PIC X(36).
           05  FILLER PIC X(4) VALUE SPACES.
           05  EMPLOYEE-COUNT PIC ZZZ9.
           05  FILLER PIC X(5) VALUE SPACES.
           05  TOTAL-PROJ-COST PIC ZZZZZZZZ9.99.
           05  FILLER PIC X(56) VALUE SPACES.

       PROCEDURE DIVISION.

       A000-MAIN.
           MOVE 1.04 TO PERCENTAGE.
           OPEN OUTPUT PRINTFILE.

      ***************************************************************
      * Update the selected employees by the new percentage. If an  *
      * error occurs during the updat, ROLLBACK the changes,        *
      ***************************************************************

           EXEC SQL
                WHENEVER SQLERROR GO TO E010-UPDATE-ERROR
           END-EXEC.

           EXEC SQL
                UPDATE CORPDATA.EMPLOYEE
                  SET SALARY = SALARY * :PERCENTAGE
                  WHERE COMM >= :COMMISSION
           END-EXEC.

      ***************************************************************
      *  Commit changes.                                            *
      ***************************************************************

           EXEC SQL
                COMMIT
           END-EXEC.

           EXEC SQL
                WHENEVER SQLERROR GO TO E020-REPORT-ERROR
           END-EXEC.

      ***************************************************************
      *  Report the updated statistics for each employee receiving  *
      *  a raise and the projects that s/he participates in         *
      ***************************************************************

      ***************************************************************
      *  Write out the header for Report 1.                         *
      ***************************************************************

           write print-record from rpt1-header1
                 before advancing 2 lines.

           write print-record from rpt1-header2
                 before advancing 1 line.

           exec sql
                declare c1 cursor for
                  SELECT DISTINCT projno, emp_act.empno,
                          lastname||", "||firstnme ,salary
                  from corpdata.emp_act, corpdata.employee
                  where emp_act.empno = employee.empno and
                        comm >= :commission
                  order by projno, empno
           end-exec.

           EXEC SQL
                OPEN C1
           END-EXEC.

           PERFORM B000-GENERATE-REPORT1 THRU B010-GENERATE-REPORT1-EXIT
               UNTIL SQLCODE NOT EQUAL TO ZERO.

       A100-DONE1.
           EXEC SQL
                CLOSE C1
           END-EXEC.

      *************************************************************
      *  For all projects anding at a date later than the RAISE-  *
      *  DATE ( i.e. those projects potentially affected by the   *
      *  salary raises generate a report containing the project   *
      *  project number, project name, the count of employees     *
      *  participating in the project and the total salary cost   *
      *  for the project                                          *
      *************************************************************


      ***************************************************************
      *  Write out the header for Report 2.                         *
      ***************************************************************

           MOVE SPACES TO PRINT-RECORD.
           WRITE PRINT-RECORD BEFORE ADVANCING 2 LINES.
           WRITE PRINT-RECORD FROM RPT2-HEADER1
                 BEFORE ADVANCING 2 LINES.
           WRITE PRINT-RECORD FROM RPT2-HEADER2
                 BEFORE ADVANCING 1 LINE.
           WRITE PRINT-RECORD FROM RPT2-HEADER3
                 BEFORE ADVANCING 2 LINES.

           EXEC SQL
                DECLARE C2 CURSOR FOR
                  SELECT EMP_ACT.PROJNO, PROJNAME, COUNT(*),
                         SUM ( (DAYS(EMENDATE)-DAYS(EMSTDATE)) *
                         EMPTIME * DECIMAL((SALARY / :WORK-DAYS),8,2))
                  FROM CORPDATA.EMP_ACT, CORPDATA.PROJECT,
                       CORPDATA.EMPLOYEE
                  WHERE EMP_ACT.PROJNO=PROJECT.PROJNO AND
                        EMP_ACT.EMPNO = EMPLOYEE.EMPNO AND
                        PRENDATE > :RAISE-DATE
                  GROUP BY EMP_ACT.PROJNO, PROJNAME
                  ORDER BY 1
           END-EXEC.

           EXEC SQL
                OPEN C2
           END-EXEC.

           PERFORM C000-GENERATE-REPORT2 THRU
              C010-GENERATE-REPORT2-EXIT
                UNTIL SQLCODE NOT EQUAL TO ZERO.

       A200-DONE2.
           EXEC SQL
                CLOSE C2
           END-EXEC

      ***************************************************************
      *  All done.                                                  *
      ***************************************************************

       A900-MAIN-EXIT.
           CLOSE PRINTFILE.
           STOP RUN.

      ***************************************************************
      *  Fetch and write the rows to PRINTFILE.                     *
      ***************************************************************

       B000-GENERATE-REPORT1.
           EXEC SQL
                WHENEVER NOT FOUND GO TO A100-DONE1
           END-EXEC.

           EXEC SQL
                FETCH C1 INTO :RPT1.PROJNO, :RPT1.EMPNO,
                              :RPT1.NAME, :RPT1.SALARY
           END-EXEC.

           MOVE CORRESPONDING RPT1 TO RPT1-DATA.
           MOVE PROJNO OF RPT1 TO PROJNO OF RPT1-DATA.
           WRITE PRINT-RECORD FROM RPT1-DATA
                 BEFORE ADVANCING 1 LINE.

       B010-GENERATE-REPORT1-EXIT.
           EXIT.

      ***************************************************************
      *  Fetch and write the rows to PRINTFILE.                     *
      ***************************************************************

       C000-GENERATE-REPORT2.
           EXEC SQL
                WHENEVER NOT FOUND GO TO A200-DONE2
           END-EXEC.

           EXEC SQL
                FETCH C2 INTO :RPT2
           END-EXEC.

           MOVE CORRESPONDING RPT2 TO RPT2-DATA.
           WRITE PRINT-RECORD FROM RPT2-DATA
                BEFORE ADVANCING 1 LINE.

       C010-GENERATE-REPORT2-EXIT.
           EXIT.

      ***************************************************************
      *  Error occurred while updating table.  Inform user and      *
      *  rollback changes.                                          *
      ***************************************************************

       E010-UPDATE-ERROR.
           EXEC SQL
                WHENEVER SQLERROR CONTINUE
           END-EXEC.
           MOVE SQLCODE TO CODE-EDIT.
           STRING "*** ERROR Occurred while updating table.  SQLCODE="
                 CODE-EDIT DELIMITED BY SIZE INTO PRINT-RECORD.
           WRITE PRINT-RECORD.
           EXEC SQL
                ROLLBACK
           END-EXEC.
           STOP RUN.

      ***************************************************************
      *  Error occurred while generating reports.  Inform user and  *
      *  exit.                                                      *
      ***************************************************************

       E020-REPORT-ERROR.
           MOVE SQLCODE TO CODE-EDIT.
           STRING "*** ERROR Occurred while generating reports. SQLCODE
      -           "=" CODE-EDIT DELIMITED BY SIZE INTO PRINT-RECORD.
           WRITE PRINT-RECORD.
           STOP RUN. 