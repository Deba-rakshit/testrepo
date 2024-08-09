      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  calc2.
       AUTHOR. calc.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

      *****************************************************************
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ISERIES.
       OBJECT-COMPUTER. IBM-ISERIES.
       SPECIAL-NAMES. C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT INFILE ASSIGN TO "c:\infile.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS IFCODE.
             SELECT OUTFILE ASSIGN TO "c:\outputFile.dat" ORGANIZATION
             IS LINE SEQUENTIAL ACCESS MODE IS SEQUENTIAL FILE STATUS IS
             OFCODE.
      *****************************************************************
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS InputRec
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  InputRec.
           05 Field1 PIC X(40).
           05 Field2 PIC X(40).
       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS OutputRec
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OutputRec.
           05 Field1 PIC X(40).
           05 Field2 PIC X(40).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  IFCODE           PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE           PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       01  COUNTERS-AND-ACCUMULATORS.
           05  REC-KTR        PIC S9(4)     COMP.
           05  TOTAL-AMOUNT   PIC S9(3)V99  COMP-3.

       01  Calc-Fields.
           05 Top-Number       pic s9(5)v99.
           05 Bottom-Number    pic s9(5)v99.
           05 Result           pic s9(7)v99.
           05 remnder          pic 99.

      * Convert Fahrenheit to Celsius and back - start with PIC 9(3).
       01  temp-converter.
           05 fahrenheit        pic 9(3)v999.
           05 celsius           pic 9(3)v999.

      * Fields for calculating simple interest
       01  simple-interest.
           05 amount           PIC 9(7)v99 value 0.
           05 principal        PIC 9(7)v99 value 100000.
           05 interest  PIC v99 value .05.
           05 nbrYears  pic 9(4) comp value 10.

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 000-calc THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           GOBACK.

      ******************************************************************
      *  This routine should perform file open and initial(priming) reads
      ******************************************************************
       000-calc.
           move 111.11 to Top-Number.
           move 222.33 to Bottom-Number.
           add Top-Number to Bottom-Number GIVING Result.

           subtract bottom-number from result giving top-number.
           multiply top-number by bottom-number giving result.
           divide bottom-number by top-number giving result rounded
               remainder remnder.

           move 98 to fahrenheit.
           Compute celsius rounded =
             ( 5 * (fahrenheit - 32) / 9).

           move 98 to celsius.
           Compute fahrenheit rounded =
             (( fahrenheit * 9) / 5 ) + 32.

           compute amount =
               ( principal * ( 1 + ( nbrYears * interest )) ).


       000-EXIT.
           EXIT.
      ******************************************************************
      *  This routine contains the business logic for the program
      ******************************************************************
       100-MAINLINE.
       100-EXIT.

      ******************************************************************
      *  This routine should perform file close operations
      ******************************************************************
       200-CLEANUP.
       200-EXIT.