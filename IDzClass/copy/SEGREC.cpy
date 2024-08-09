      ******************************************************************
      *  SETMENTED RECORD BUFFER: WIDGET EMPLOYEE INFORMATION          *
      *                                                                *
      *  PROGRAMMER: TIMOTHY DAVID MAGEE                               *
      *              LEXINGTON, KENTUCKY                               *
      *                                                                *
      *  DATE: 01/09/2006                                              *
      ******************************************************************
       01 WIDGET-DEPT.
           03 REC-TYPE                 PIC X(2).
           03 WIDGET-DEPARTMENT        PIC X(3).
           03 WIDGET-JOB               PIC X(20).
       01 WIDGET-EMPLOYEE.
           03 REC-TYPE                 PIC X(2).
           03 WIDGET-EMP-ID            PIC 9999.
           03 EMP-FIRST-NAME           PIC X(10).
           03 EMP-LAST-NAME            PIC X(10).
           03 WIDGET-EXTENSION         PIC 9999.
           03 EMP-SALARY               PIC 99999.
           03 EMP-APPRASAL             PIC X(20).