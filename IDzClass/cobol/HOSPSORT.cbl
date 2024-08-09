       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPSORT.
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
       FILE-CONTROL.
           SELECT INFILE
           ASSIGN TO "d:\IDzProjectNew\HospProject\dat\hospout.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS IFCODE.

           SELECT SRTFILE
           ASSIGN TO "d:\IDzProjectNew\HospProject\dat\hosptmp.dat".

           SELECT OUTFILE
           ASSIGN TO "d:\IDzProjectNew\HospProject\dat\hospsrt.dat"
           ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS EFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC  PIC X(100).

       FD  OUTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OUT-Rec.
       01  OUT-REC  PIC X(100).

       SD  SRTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS SRT-Rec.
       01  SRT-REC.
           05 SRT-KEY                  PIC X(5).
           05 FILLER                   PIC X(95).


       WORKING-STORAGE SECTION.
           COPY PATIENT.
       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  RFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       PROCEDURE DIVISION.
           SORT SRTFILE ON ASCENDING SRT-KEY
               USING  INFILE
               GIVING OUTFILE.

           MOVE ZERO TO RETURN-CODE.
           GOBACK.
