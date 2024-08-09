       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  HOSPDRVR.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-INPUT-REC  PIC x(100).
       copy patbill.
       copy patdaly.
       copy patins.
       copy patmstr.
       copy patrpt.
       copy ptrmtsum.
       copy treatmnt.
       copy trmntrpt.
       copy wardrpt.


       PROCEDURE DIVISION.
           CALL "HOSPEDIT" using WS-INPUT-REC.
           IF RETURN-CODE = ZERO
           CALL "HOSPSORT".
           MOVE ZERO TO RETURN-CODE.
           CALL "HOSPSRCH" using WS-INPUT-REC.
           IF RETURN-CODE = ZERO
           CALL "HOSPCALC" using WS-INPUT-REC.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.
