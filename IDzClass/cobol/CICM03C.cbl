      ******************************************************
      *  TRANSACTION: M03C                                 *            $CI01410
      *  VERIFICATION: CHECK COMPILER LISTING AND          *            $CI01420
      *                TERMINAL MESSAGES                   *            $CI01430
      *  OBJECTIVES OF TESTCASE:                           *            $CI01440
      *                                                    *            $CI01450
      *       INVOKE DEBUG TOOL IN FULL SCREEN MODE.       *            $CI01460
      *       USING CEEUOPT SPECIFYING TEST.               *            $CI01470
      *       RUN IN BOTH MFI AND PWS                      *            $CI01560
      *  XOPTS(COBOL2)- I removed this from CBL            *
      ******************************************************            $CI01580
                                                                        $CI01590
      ******************************************************            $CI01600
      *      CRITERIA FOR  SUCCESS                         *            $CI01610
      *                                                    *            $CI01620
      ******************************************************            $CI01680
      *************************************************************     CBL00010
       IDENTIFICATION DIVISION.                                         CBL00020
      *************************************************************     CBL00030
       PROGRAM-ID.    CICM03C.                                          CBL00040
       AUTHOR.        COBOL Productivity Suite.                         CBL00050
                                                                        CBL00060
      *************************************************************     CBL00070
      *NAME:     CICM03C                                        ***     CBL00080
      *LANGUAGE: IBM COBOL/370                                  ***     CBL00090
      *                                                         ***     CBL00100
      *FUNCTION: Employee Lookup Application: CICS Version      ***     CBL00110
      *          MVS Host Resident                              ***     CBL00120
      *          (Last update 01/21/94)                         ***     CBL00130
      *                                                         ***     CBL00140
      *          This program displays a list of employees      ***     CBL00150
      *          with associated data) based on the user's      ***     CBL00160
      *          request.  The user's requests include:         ***     CBL00170
      *            o display all employees                      ***     CBL00180
      *            o display all employees that match the       ***     CBL00190
      *              search criteria exactly                    ***     CBL00200
      *            o display all employees that match the       ***     CBL00210
      *              search criteria partially, that is, if the ***     CBL00220
      *              user specifies ABC for last name, display  ***     CBL00230
      *              all employees with last names beginning    ***     CBL00240
      *              with the letters ABC                       ***     CBL00250
      *          The search criteria which the user can specify ***     CBL00260
      *          includes the employee's last name, department, ***     CBL00270
      *          or a combination of both.                      ***     CBL00280
      *                                                         ***     CBL00290
      *External Subroutine: CSM03C (Message Handler)            ***     CBL00300
      *COPY Members: CSM03MP (BMS Map fields)                   ***     CBL00310
      *              CBLACC3 (Program flags)                    ***     CBL00320
      *BMS Panel:    CSM03MP                                    ***     CBL00330
      *                                                         ***     CBL00340
      *************************************************************     CBL00350
                                                                        CBL00360
      *************************************************************     CBL00370
       ENVIRONMENT DIVISION.                                            CBL00380
      *************************************************************     CBL00390
       CONFIGURATION SECTION.                                           CBL00400
       SOURCE-COMPUTER.  IBM-370.                                       CBL00410
       OBJECT-COMPUTER.  IBM-370.                                       CBL00420
                                                                        CBL00430
      *************************************************************     CBL00440
       DATA DIVISION.                                                   CBL00450
      *************************************************************     CBL00460
                                                                        CBL00470
       WORKING-STORAGE SECTION.                                         CBL00480
                                                                        CBL00490
      ****************************************************************  CBL00500
      *  CBL Menu Screen                                             *  CBL00510
      ****************************************************************  CBL00520
                                                                        CBL00530
      *    COPY CSM03MP.                                                CBL00540
       01  CSM03MPI.
           02  FILLER PIC X(12).
           02  DATETIML    COMP  PIC  S9(4).
           02  DATETIMF    PICTURE X.
           02  FILLER REDEFINES DATETIMF.
             03 DATETIMA    PICTURE X.
           02  DATETIMI  PIC X(14).
           02  ACTIONL    COMP  PIC  S9(4).
           02  ACTIONF    PICTURE X.
           02  FILLER REDEFINES ACTIONF.
             03 ACTIONA    PICTURE X.
           02  ACTIONI  PIC X(1).
           02  LASTNAML    COMP  PIC  S9(4).
           02  LASTNAMF    PICTURE X.
           02  FILLER REDEFINES LASTNAMF.
             03 LASTNAMA    PICTURE X.
           02  LASTNAMI  PIC X(15).
           02  DEPMTL    COMP  PIC  S9(4).
           02  DEPMTF    PICTURE X.
           02  FILLER REDEFINES DEPMTF.
             03 DEPMTA    PICTURE X.
           02  DEPMTI  PIC X(3).
           02  LINE1L    COMP  PIC  S9(4).
           02  LINE1F    PICTURE X.
           02  FILLER REDEFINES LINE1F.
             03 LINE1A    PICTURE X.
           02  LINE1I  PIC X(74).
           02  LINE2L    COMP  PIC  S9(4).
           02  LINE2F    PICTURE X.
           02  FILLER REDEFINES LINE2F.
             03 LINE2A    PICTURE X.
           02  LINE2I  PIC X(74).
           02  LINE3L    COMP  PIC  S9(4).
           02  LINE3F    PICTURE X.
           02  FILLER REDEFINES LINE3F.
             03 LINE3A    PICTURE X.
           02  LINE3I  PIC X(74).
           02  LINE4L    COMP  PIC  S9(4).
           02  LINE4F    PICTURE X.
           02  FILLER REDEFINES LINE4F.
             03 LINE4A    PICTURE X.
           02  LINE4I  PIC X(74).
           02  LINE5L    COMP  PIC  S9(4).
           02  LINE5F    PICTURE X.
           02  FILLER REDEFINES LINE5F.
             03 LINE5A    PICTURE X.
           02  LINE5I  PIC X(74).
           02  LINE6L    COMP  PIC  S9(4).
           02  LINE6F    PICTURE X.
           02  FILLER REDEFINES LINE6F.
             03 LINE6A    PICTURE X.
           02  LINE6I  PIC X(74).
           02  LINE7L    COMP  PIC  S9(4).
           02  LINE7F    PICTURE X.
           02  FILLER REDEFINES LINE7F.
             03 LINE7A    PICTURE X.
           02  LINE7I  PIC X(74).
           02  LINE8L    COMP  PIC  S9(4).
           02  LINE8F    PICTURE X.
           02  FILLER REDEFINES LINE8F.
             03 LINE8A    PICTURE X.
           02  LINE8I  PIC X(74).
           02  LINE9L    COMP  PIC  S9(4).
           02  LINE9F    PICTURE X.
           02  FILLER REDEFINES LINE9F.
             03 LINE9A    PICTURE X.
           02  LINE9I  PIC X(74).
           02  LINE10L    COMP  PIC  S9(4).
           02  LINE10F    PICTURE X.
           02  FILLER REDEFINES LINE10F.
             03 LINE10A    PICTURE X.
           02  LINE10I  PIC X(74).
           02  LINE11L    COMP  PIC  S9(4).
           02  LINE11F    PICTURE X.
           02  FILLER REDEFINES LINE11F.
             03 LINE11A    PICTURE X.
           02  LINE11I  PIC X(74).
           02  LINE12L    COMP  PIC  S9(4).
           02  LINE12F    PICTURE X.
           02  FILLER REDEFINES LINE12F.
             03 LINE12A    PICTURE X.
           02  LINE12I  PIC X(74).
           02  MSGLINEL    COMP  PIC  S9(4).
           02  MSGLINEF    PICTURE X.
           02  FILLER REDEFINES MSGLINEF.
             03 MSGLINEA    PICTURE X.
           02  MSGLINEI  PIC X(68).
       01  CSM03MPO REDEFINES CSM03MPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  DATETIMO  PIC X(14).
           02  FILLER PICTURE X(3).
           02  ACTIONO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  LASTNAMO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  DEPMTO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  LINE1O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE2O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE3O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE4O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE5O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE6O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE7O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE8O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE9O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE10O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE11O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  LINE12O  PIC X(74).
           02  FILLER PICTURE X(3).
           02  MSGLINEO  PIC X(68).
                                                                        CBL00550
      ****************************************************************  CBL00560
      * Redefinitions of screen input/output areas                   *  CBL00570
      ****************************************************************  CBL00580
                                                                        CBL00590
       01  CSM03MPI-REDEF               REDEFINES CSM03MPI.             CBL00600
           05  FILLER                       PIC X(36).                  CBL00610
           05  LNAME-FORMAT.                                            CBL00620
               10  LNAMCHRS             OCCURS 15 TIMES                 CBL00630
                                            PIC X(01).                  CBL00640
           05  FILLER                       PIC X(03).                  CBL00650
           05  DEPT-FORMAT.                                             CBL00660
               10  DEPTCHRS             OCCURS 3 TIMES                  CBL00670
                                            PIC X(01).                  CBL00680
           05  FILLER                       PIC X(995).                 CBL00690
                                                                        CBL00700
       01  CSM03MPO-REDEF               REDEFINES CSM03MPO.             CBL00710
           05  FILLER                       PIC X(57).                  CBL00720
           05  SCREEN-ARRAY.                                            CBL00730
               10  SCREEN-ENTRY         OCCURS 12 TIMES.                CBL00740
                   15  FILLER               PIC X(03).                  CBL00750
                   15  SCREEN-LINE.                                     CBL00760
                       20  SCREEN-NAME.                                 CBL00770
                           25  SCREEN-NAME-CHAR                         CBL00780
                                        OCCURS 30 TIMES                 CBL00790
                                            PIC X(01).                  CBL00800
                       20  FILLER           PIC X(02).                  CBL00810
                       20  SCREEN-DEPT      PIC X(03).                  CBL00820
                       20  FILLER           PIC X(03).                  CBL00830
                       20  SCREEN-PHONE     PIC X(12).                  CBL00840
                       20  FILLER           PIC X(02).                  CBL00850
                       20  SCREEN-HIRE-DATE                             CBL00860
                                            PIC X(10).                  CBL00870
                       20  FILLER           PIC X(03).                  CBL00880
                       20  SCREEN-SERVICE-LENGTH                        CBL00890
                                            PIC X(05).                  CBL00900
                       20  FILLER           PIC X(04).                  CBL00910
           05  FILLER                       PIC X(71).                  CBL00920
                                                                        CBL00930
      ****************************************************************  CBL00940
      * CICS RETURN Command Communications Area.                     *  CBL00950
      ****************************************************************  CBL00960
                                                                        CBL00970
       01  CICM03C-COMMAREA                 PIC X(01).                  CBL00980
                                                                        CBL00990
                                                                        CBL01000
      ************************************************************      CBL01010
      *   EMP-ARRAY:    Employee Database                        *      CBL01020
      ************************************************************      CBL01030
                                                                        CBL01040
       01  EMP-ARRAY.                                                   CBL01050
           05  ENTRY1.                                                  CBL01060
               10  FILLER     PIC X(15)  VALUE 'DOE            '.       CBL01070
               10  FILLER     PIC X(10)  VALUE 'BILL      '.            CBL01080
               10  FILLER     PIC X(1)   VALUE 'D'.                     CBL01090
               10  FILLER     PIC X(3)   VALUE 'D35'.                   CBL01100
               10  FILLER     PIC X(12)  VALUE '408-555-9995'.          CBL01110
               10  FILLER     PIC 9(6)   VALUE 781206.                  CBL01120
                                                                        CBL01130
           05  ENTRY2.                                                  CBL01140
               10  FILLER     PIC X(15)  VALUE 'DOE            '.       CBL01150
               10  FILLER     PIC X(10)  VALUE 'JANE      '.            CBL01160
               10  FILLER     PIC X(1)   VALUE 'A'.                     CBL01170
               10  FILLER     PIC X(3)   VALUE 'A55'.                   CBL01180
               10  FILLER     PIC X(12)  VALUE '212-555-9950'.          CBL01190
               10  FILLER     PIC 9(6)   VALUE 890726.                  CBL01200
                                                                        CBL01210
           05  ENTRY3.                                                  CBL01220
               10  FILLER     PIC X(15)  VALUE 'DOE            '.       CBL01230
               10  FILLER     PIC X(10)  VALUE 'JOHN      '.            CBL01240
               10  FILLER     PIC X(1)   VALUE 'B'.                     CBL01250
               10  FILLER     PIC X(3)   VALUE 'B41'.                   CBL01260
               10  FILLER     PIC X(12)  VALUE '202-555-4500'.          CBL01270
               10  FILLER     PIC 9(6)   VALUE 750716.                  CBL01280
                                                                        CBL01290
           05  ENTRY4.                                                  CBL01300
               10  FILLER     PIC X(15)  VALUE 'DOWE           '.       CBL01310
               10  FILLER     PIC X(10)  VALUE 'SUE       '.            CBL01320
               10  FILLER     PIC X(1)   VALUE 'T'.                     CBL01330
               10  FILLER     PIC X(3)   VALUE 'D35'.                   CBL01340
               10  FILLER     PIC X(12)  VALUE '408-555-1990'.          CBL01350
               10  FILLER     PIC 9(6)   VALUE 921127.                  CBL01360
                                                                        CBL01370
           05  ENTRY5.                                                  CBL01380
               10  FILLER     PIC X(15)  VALUE 'JOHNSON        '.       CBL01390
               10  FILLER     PIC X(10)  VALUE 'JANE      '.            CBL01400
               10  FILLER     PIC X(1)   VALUE 'T'.                     CBL01410
               10  FILLER     PIC X(3)   VALUE 'B44'.                   CBL01420
               10  FILLER     PIC X(12)  VALUE '202-555-8575'.          CBL01430
               10  FILLER     PIC 9(6)   VALUE 911123.                  CBL01440
                                                                        CBL01450
           05  ENTRY6.                                                  CBL01460
               10  FILLER     PIC X(15)  VALUE 'JONES          '.       CBL01470
               10  FILLER     PIC X(10)  VALUE 'BOB       '.            CBL01480
               10  FILLER     PIC X(1)   VALUE 'F'.                     CBL01490
               10  FILLER     PIC X(3)   VALUE 'B41'.                   CBL01500
               10  FILLER     PIC X(12)  VALUE '202-555-3250'.          CBL01510
               10  FILLER     PIC 9(6)   VALUE 820215.                  CBL01520
                                                                        CBL01530
           05  ENTRY7.                                                  CBL01540
               10  FILLER     PIC X(15)  VALUE 'JONES          '.       CBL01550
               10  FILLER     PIC X(10)  VALUE 'BILL      '.            CBL01560
               10  FILLER     PIC X(1)   VALUE 'N'.                     CBL01570
               10  FILLER     PIC X(3)   VALUE 'A55'.                   CBL01580
               10  FILLER     PIC X(12)  VALUE '212-555-8960'.          CBL01590
               10  FILLER     PIC 9(6)   VALUE 921204.                  CBL01600
                                                                        CBL01610
           05  ENTRY8.                                                  CBL01620
               10  FILLER     PIC X(15)  VALUE 'JONES          '.       CBL01630
               10  FILLER     PIC X(10)  VALUE 'MARY      '.            CBL01640
               10  FILLER     PIC X(1)   VALUE 'C'.                     CBL01650
               10  FILLER     PIC X(3)   VALUE 'D35'.                   CBL01660
               10  FILLER     PIC X(12)  VALUE '408-555-9999'.          CBL01670
               10  FILLER     PIC 9(6)   VALUE 890521.                  CBL01680
                                                                        CBL01690
           05  ENTRY9.                                                  CBL01700
               10  FILLER     PIC X(15)  VALUE 'SMITH          '.       CBL01710
               10  FILLER     PIC X(10)  VALUE 'BOB       '.            CBL01720
               10  FILLER     PIC X(1)   VALUE 'M'.                     CBL01730
               10  FILLER     PIC X(3)   VALUE 'B44'.                   CBL01740
               10  FILLER     PIC X(12)  VALUE '202-555-8555'.          CBL01750
               10  FILLER     PIC 9(6)   VALUE 901219.                  CBL01760
                                                                        CBL01770
           05  ENTRY10.                                                 CBL01780
               10  FILLER     PIC X(15)  VALUE 'SMITH          '.       CBL01790
               10  FILLER     PIC X(10)  VALUE 'MARY      '.            CBL01800
               10  FILLER     PIC X(1)   VALUE 'S'.                     CBL01810
               10  FILLER     PIC X(3)   VALUE 'A55'.                   CBL01820
               10  FILLER     PIC X(12)  VALUE '212-555-9080'.          CBL01830
               10  FILLER     PIC 9(6)   VALUE 880822.                  CBL01840
                                                                        CBL01850
           05  ENTRY11.                                                 CBL01860
               10  FILLER     PIC X(15)  VALUE 'SMITH          '.       CBL01870
               10  FILLER     PIC X(10)  VALUE 'SUE       '.            CBL01880
               10  FILLER     PIC X(1)   VALUE 'R'.                     CBL01890
               10  FILLER     PIC X(3)   VALUE 'B41'.                   CBL01900
               10  FILLER     PIC X(12)  VALUE '202-555-8989'.          CBL01910
               10  FILLER     PIC 9(6)   VALUE 740701.                  CBL01920
                                                                        CBL01930
           05  ENTRY12.                                                 CBL01940
               10  FILLER     PIC X(15)  VALUE 'SMITHE         '.       CBL01950
               10  FILLER     PIC X(10)  VALUE 'BILL      '.            CBL01960
               10  FILLER     PIC X(1)   VALUE 'E'.                     CBL01970
               10  FILLER     PIC X(3)   VALUE 'A55'.                   CBL01980
               10  FILLER     PIC X(12)  VALUE '212-555-7535'.          CBL01990
               10  FILLER     PIC 9(6)   VALUE 821229.                  CBL02000
                                                                        CBL02010
      **************************************************                CBL02020
      *    Processing Template for Employee-Database   *                CBL02030
      **************************************************                CBL02040
                                                                        CBL02050
       01  EMP-ARRAY-R              REDEFINES EMP-ARRAY.                CBL02060
           05  EMP-ENTRY            OCCURS 12 TIMES.                    CBL02070
               10 EMP-LAST-NAME         PIC X(15).                      CBL02080
               10 EMP-FIRST-NAME        PIC X(10).                      CBL02090
               10 EMP-MIDDLE-INITIAL    PIC X(1).                       CBL02100
               10 EMP-DEPT              PIC X(3).                       CBL02110
               10 EMP-PHONE             PIC X(12).                      CBL02120
               10 EMP-HIRE-DATE         PIC 9(6).                       CBL02130
                                                                        CBL02140
      **************************************************                CBL02150
      *    RESULTS-ARRAY                               *                CBL02160
      *      Receives data for internal processing     *                CBL02170
      **************************************************                CBL02180
                                                                        CBL02190
       01  RESULTS-ARRAY.                                               CBL02200
           05  RESULTS-ENTRY                 OCCURS 12 TIMES.           CBL02210
               10 RESULTS-LAST-NAME              PIC X(15).             CBL02220
               10 RESULTS-FIRST-NAME             PIC X(10).             CBL02230
               10 RESULTS-MIDDLE-INITIAL         PIC X(1).              CBL02240
               10 RESULTS-DEPT                   PIC X(3).              CBL02250
               10 RESULTS-PHONE                  PIC X(12).             CBL02260
               10 RESULTS-HIRE-DATE              PIC 9(6).              CBL02270
               10 RESULTS-HIRE-DATE-R REDEFINES RESULTS-HIRE-DATE.      CBL02280
                  15 RESULTS-HIRE-DATE-YEAR      PIC 9(2).              CBL02290
                  15 RESULTS-HIRE-DATE-MONTH     PIC 9(2).              CBL02300
                  15 RESULTS-HIRE-DATE-DAY       PIC 9(2).              CBL02310
                                                                        CBL02320
      *************************************************************     CBL02330
      *  Internal variables                                       *     CBL02340
      *************************************************************     CBL02350
                                                                        CBL02360
       01  ARRAY-MAX-ENTRIES.                                           CBL02370
           05  EMP-ARRAY-MAX          PIC 9(2) VALUE 12.                CBL02380
           05  RESULTS-ARRAY-MAX      PIC 9(2) VALUE 0.                 CBL02390
                                                                        CBL02400
       01  PROGRAM-WORK-FIELDS.                                         CBL02410
           05  RESULTS-PTR            PIC 9(2).                         CBL02420
           05  SCREEN-PTR             PIC 9(2).                         CBL02430
           05  EMP-PTR                PIC 9(2).                         CBL02440
                                                                        CBL02450
           05  BLANK-COUNT            PIC 9(2).                         CBL02460
           05  LINE-COUNT             PIC 9(2).                         CBL02470
           05  LASTNAME-LENGTH        PIC 9(2).                         CBL02480
           05  DEPT-LENGTH            PIC 9(2).                         CBL02490
           05  TODAYS-DATE-LILIAN     PIC 9(9)  COMP VALUE 0.           CBL02500
           05  TODAYS-DATE-ARG        PIC X(8).                         CBL02510
           05  HIRE-DATE-CHAR         PIC X(8).                         CBL02520
           05  HIRE-DATE-CHAR-R   REDEFINES HIRE-DATE-CHAR.             CBL02530
               10  HIRE-DATE-INT      PIC 9(8).                         CBL02540
           05  TODAYS-DATE-INT        PIC 9(8).                         CBL02550
                                                                        CBL02560
       01  CICS-BMS-SUPPORT-FIELDS.                                     CBL02570
           05  LOCAL-RESPONSE         PIC S9(8) COMP.                   CBL02580
           05  LOCAL-ACTION-FLAG      PIC X(1).                         CBL02590
           05  BMS-DATE.                                                CBL02600
               10  BMS-DATE-YEAR1     PIC X(2).                         CBL02610
               10  BMS-DATE-YEAR2     PIC X(2).                         CBL02620
               10  BMS-DATE-MONTH     PIC X(2).                         CBL02630
               10  BMS-DATE-DAY       PIC X(2).                         CBL02640
               10  BMS-TIME-HOUR      PIC X(2).                         CBL02650
               10  BMS-TIME-MINUTES   PIC X(2).                         CBL02660
               10  FILLER             PIC X(10).                        CBL02670
           05  CHARPTR                PIC 9(2).                         CBL02680
           05  WORKPTR                PIC 9(2).                         CBL02690
           05  LJUST-FIELD-1.                                           CBL02700
               10  LJUST-LASTNAME-1.                                    CBL02710
                   15  LJUST-DEPT-1   PIC X(3).                         CBL02720
                   15  FILLER         PIC X(12).                        CBL02730
           05  LJUST-FIELD-2.                                           CBL02740
               10  LJUST-LASTNAME-2.                                    CBL02750
                   15  LJUST-DEPT-2   PIC X(3).                         CBL02760
                   15  FILLER         PIC X(12).                        CBL02770
                                                                        CBL02780
                                                                        CBL02790
       01  CEE-PARAMETERS.                                              CBL02800
      *   05  HIRE-DATE-LILIAN             PIC 9(9)  COMP VALUE 0.      CBL02810
          05  HIRE-DATE-LILIAN             PIC S9(9) COMP.              CBL02820
          05  CEE-CALL-FEEDBACK            PIC X(12).                   CBL02830
          05  CEEDAYS-INPUT.                                            CBL02840
              10  CEEDAYS-INPUT-LENGTH     PIC S9(4) COMP.              CBL02850
              10  CEEDAYS-INPUT-VALUE      PIC X(6)  VALUE SPACES.      CBL02860
          05  CEEDAYS-PICSTRING.                                        CBL02870
              10  CEEDAYS-PICSTRING-LENGTH PIC S9(4) COMP.              CBL02880
              10  CEEDAYS-PICSTRING-FORMAT PIC X(10) VALUE SPACES.      CBL02890
          05  CEEDATE-OUTPUT               PIC X(80) VALUE SPACES.      CBL02900
          05  CEEDATE-OUTPUT-R   REDEFINES CEEDATE-OUTPUT.              CBL02910
              10  CEEDATE-OUTPUT-MM        PIC X(2).                    CBL02920
              10  FILLER                   PIC X.                       CBL02930
              10  CEEDATE-OUTPUT-DD        PIC X(2).                    CBL02940
              10  FILLER                   PIC X.                       CBL02950
              10  CEEDATE-OUTPUT-YYYY      PIC X(4).                    CBL02960
              10  FILLER                   PIC X(70).                   CBL02970
          05  CEEDATE-PICSTRING.                                        CBL02980
              10  CEEDATE-PICSTRING-LENGTH PIC S9(4) COMP.              CBL02990
              10  CEEDATE-PICSTRING-FORMAT PIC X(10) VALUE SPACES.      CBL03000
                                                                        CBL03010
      *  Internal Program Flags                                         CBL03020
      *   COPY CBLACC3.                                                 CBL03030

      *************************************************************
      *NAME:     CBLACC3                                        ***
      *LANGUAGE: IBM COBOL/370                                  ***
      *FUNCTION: Copy member for internal program flag fields   ***
      *          for Employee Lookup Application CICM03C        ***
      *Last Update: 11-15-93                                    ***
      *                                                         ***
      *************************************************************


       01  PROGRAM-FLAGS.
           05  LOOP-FLAG            PIC X VALUE SPACE.
               88  START-LOOP       VALUE 'Y'.
               88  STOP-LOOP        VALUE 'N'.
           05  CHECK-INPUTS         PIC X VALUE SPACE.
               88  INPUTS-VALID     VALUE 'Y'.
               88  INPUTS-NOT-VALID VALUE 'N'.
           05  OUTPUT-FLAG          PIC X VALUE SPACE.

                                                                        CBL03040
       01  CICM03C-TRAN-ID          PIC X(4).                           CBL03050
                                                                        CBL03060
       01  TODAYS-DATE-C            PIC X(8).                           CBL03070
       01  SERVICE-LENGTH-WKAREA.                                       CBL03080
           05  TODAYS-DATE          PIC 9(8).                           CBL03090
           05  LILIAN-DIF           PIC 9(9) VALUE 0.                   CBL03100
           05  SERV-YEAR-REMAINDER  PIC 9(4) VALUE 0.                   CBL03110
           05  SERV-YEAR            PIC 9(2) VALUE 0.                   CBL03120
           05  SERVICE-FORMAT.                                          CBL03130
               10  SERV-YEAR-F      PIC Z9.                             CBL03140
               10  FILLER           PIC X(3) VALUE ' YR'.               CBL03150
                                                                        CBL03160
      ******************************************************            CBL03170
      *     Parameters for CSM03C External Routine         *            CBL03180
      ******************************************************            CBL03190
                                                                        CBL03200
       01  CSM03C-PARMS.                                                CBL03210
           05  GM-MSG-ID     PIC X(3).                                  CBL03220
           05  GM-MSG-TXT    PIC X(68).                                 CBL03230
                                                                        CBL03240
      ****************************************************************  CBL03250
      * CICS Attention Identifiers                                   *  CBL03260
      ****************************************************************  CBL03270
                                                                        CBL03280
           COPY DFHAID.                                                 CBL03290
                                                                        CBL03300
       LINKAGE SECTION.                                                 CBL03310
      ******************************************************            CBL03320
      *     Communication Area for program invocation      *            CBL03330
      *     (CICS-BMS support)                             *            CBL03340
      ******************************************************            CBL03350
                                                                        CBL03360
       01  DFHCOMMAREA                            PIC X(01).            CBL03370
                                                                        CBL03380
      *************************************************************     CBL03390
       PROCEDURE DIVISION.                                              CBL03400
      *************************************************************     CBL03410
                                                                        CBL03420
                                                                        CBL03430
      ****************************************************************  CBL03440
      *0000-LOOKUP.                                                  *  CBL03450
      *                                                              *  CBL03460
      *  Initialize the communication area, the map input area and   *  CBL03470
      *  the message line on the screen.                             *  CBL03480
      *                                                              *  CBL03490
      *  If this is the first time the program is being executed,    *  CBL03500
      *  display the panel with the appropriate message              *  CBL03510
      *                                 (C100-DISPLAY-SCREEN)        *  CBL03520
      *  Otherwise, process the input from the panel (1000-MAIN).    *  CBL03530
      *                                                              *  CBL03540
      *  If an error occurs processing the screen, or if PF 3 (Exit) *  CBL03550
      *  was pressed, exit the program without planning to return.   *  CBL03560
      *  Otherwise, exit the program planning to return.             *  CBL03570
      *                                                              *  CBL03580
      ****************************************************************  CBL03590
                                                                        CBL03600
       0000-LOOKUP.                                                     CBL03610
                                                                        CBL03620
                                                                        CBL03630
      * Initialize the RETURN command communication area                CBL03640
           MOVE 'I' TO CICM03C-COMMAREA.                                CBL03650
      * Initialize the screen map input area                            CBL03660
           MOVE LOW-VALUES TO CSM03MPI.                                 CBL03670
      * Initialize the transaction identifier                           CBL03680
           MOVE EIBTRNID TO CICM03C-TRAN-ID.                            CBL03690
      * Initialize the screen message output area                       CBL03700
           MOVE SPACES TO MSGLINEO.                                     CBL03710
                                                                        CBL03720
      *  First time executing this program, that is, there is           CBL03730
      *  no communications area                                         CBL03740
           IF EIBCALEN = 0                                              CBL03750
      * Panel will initially display Message:                           CBL03760
      *  '01I: Specify Action and Search data.'                         CBL03770
             MOVE '01I' TO GM-MSG-ID                                    CBL03780
      *  Indicate first display of the panel                            CBL03790
             MOVE 'F' TO OUTPUT-FLAG                                    CBL03800
      * First display of panel                                          CBL03810
             PERFORM C100-DISPLAY-SCREEN THRU                           CBL03820
                     C100-DISPLAY-SCREEN-EXIT                           CBL03830
           ELSE                                                         CBL03840
      * Subsequent display of panel                                     CBL03850
             MOVE SPACE TO OUTPUT-FLAG                                  CBL03860
      * Process the panel input/output                                  CBL03870
             PERFORM 1000-MAIN THRU 1000-MAIN-EXIT                      CBL03880
           END-IF.                                                      CBL03890
                                                                        CBL03900
      *  If error with screen or F3 pressed,                            CBL03910
      *  return control to CICS and end program (no return).            CBL03920
      *  Exit program. Pseudo-conversational transaction completed.     CBL03930
           IF CICM03C-COMMAREA = SPACE                                  CBL03940
             EXEC CICS                                                  CBL03950
               RETURN                                                   CBL03960
             END-EXEC                                                   CBL03970
           ELSE                                                         CBL03980
      *  Exit program. Prepare for return.                              CBL03990
      *  Pseudo-conversational transaction will resume.                 CBL04000
             EXEC CICS                                                  CBL04010
               RETURN                                                   CBL04020
               TRANSID(CICM03C-TRAN-ID)                                 CBL04030
               COMMAREA(CICM03C-COMMAREA)                               CBL04040
               LENGTH(1)                                                CBL04050
             END-EXEC                                                   CBL04060
           END-IF.                                                      CBL04070
                                                                        CBL04080
           STOP RUN.                                                    CBL04090
                                                                        CBL04100
        0000-LOOKUP-EXIT. EXIT.                                         CBL04110
                                                                        CBL04120
                                                                        CBL04130
      *************************************************************     CBL04140
      * 1000-MAIN: Main Processing                                      CBL04150
      *                                                                 CBL04160
      *   The Action and search fields (Lastname and Dept) are          CBL04170
      *   validated (1100-USER-INPUT). If errors are found,             CBL04180
      *   messages will be set (CSM03C external subroutine).            CBL04190
      *                                                                 CBL04200
      *   If user inputs are valid, process each action code:           CBL04210
      *     D: Display all entries (1200-DISPLAY-ALL)                   CBL04220
      *     P: Display entries that match search data that is fully     CBL04230
      *        qualified or consists of leading characters.             CBL04240
      *        (1300-PARTIAL-MATCH)                                     CBL04250
      *     E: Display entries that match fully qualified search data.  CBL04260
      *        (1400-EXACT-MATCH)                                       CBL04270
      *     If matching entries are found, the data will be moved       CBL04280
      *     from the employee database (EMP-ARRAY) to an internal       CBL04290
      *     work area (RESULTS-ARRAY).                                  CBL04300
      *                                                                 CBL04310
      *   After all matches have been found, the entries in the         CBL04320
      *   the internal work area RESULTS-ARRAY are formatted and        CBL04330
      *   moved to the output array SCREEN-ARRAY(1500-LOAD-SCREEN).     CBL04340
      *   Messages will be set if matches were found or not found.      CBL04350
      *                                 (CSM03C external subroutine)    CBL04360
      *                                                                 CBL04370
      *   Finally, the screen is redisplayed (C100-DISPLAY-SCREEN).     CBL04380
      *************************************************************     CBL04390
                                                                        CBL04400
        1000-MAIN.                                                      CBL04410
                                                                        CBL04420
      *   Process and Validate User Input                               CBL04430
      *   If inputs invalid, error message will be set.                 CBL04440
                PERFORM 1100-USER-INPUT THRU 1100-USER-INPUT-EXIT       CBL04450
                                                                        CBL04460
      *   If user inputs valid, process actions and search fields.      CBL04470
                IF INPUTS-VALID                                         CBL04480
                                                                        CBL04490
      * Initialize RESULTS-ARRAY                                        CBL04500
                  INITIALIZE RESULTS-ARRAY                              CBL04510
                  MOVE 0 TO RESULTS-ARRAY-MAX                           CBL04520
                                                                        CBL04530
      *   Load matched entries from EMP-ARRAY to RESULTS-ARRAY.         CBL04540
      *   Set messages if match is found or not found.                  CBL04550
                  EVALUATE TRUE                                         CBL04560
                    WHEN ACTIONI = 'D'                                  CBL04570
                      PERFORM 1200-DISPLAY-ALL THRU                     CBL04580
                                        1200-DISPLAY-ALL-EXIT           CBL04590
                    WHEN ACTIONI = 'P'                                  CBL04600
                      PERFORM 1300-PARTIAL-MATCH THRU                   CBL04610
                                        1300-PARTIAL-MATCH-EXIT         CBL04620
                    WHEN OTHER                                          CBL04630
                      PERFORM 1400-EXACT-MATCH THRU                     CBL04640
                                        1400-EXACT-MATCH-EXIT           CBL04650
                  END-EVALUATE                                          CBL04660
                                                                        CBL04670
      *  Load data from RESULTS-ARRAY into SCREEN-ARRAY                 CBL04680
                  PERFORM 1500-LOAD-SCREEN THRU                         CBL04690
                          1500-LOAD-SCREEN-EXIT                         CBL04700
      * User inputs invalid. Message already set.                       CBL04710
                ELSE                                                    CBL04720
                  CONTINUE                                              CBL04730
                END-IF.                                                 CBL04740
                                                                        CBL04750
      *  Redisplay the main screen                                      CBL04760
           PERFORM C100-DISPLAY-SCREEN THRU                             CBL04770
                   C100-DISPLAY-SCREEN-EXIT.                            CBL04780
                                                                        CBL04790
                                                                        CBL04800
        1000-MAIN-EXIT. EXIT.                                           CBL04810
                                                                        CBL04820
      ****************************************************              CBL04830
      * 1100-USER-INPUT                                                 CBL04840
      *   Test for PF1, PF3, and Enter. If Enter pressed,               CBL04850
      *   retrieve the map. If retrieval is successful,                 CBL04860
      *   validate the Action and search fields (Lastname,              CBL04870
      *   Department).             (1110-VALIDATE-INPUT)                CBL04880
      *   For any other key, set message.                               CBL04890
      *   For all keys, the OUTPUT-FLAG is set which will               CBL04900
      *   determine how the map is to be displayed.                     CBL04910
      *   INPUTS-NOT-VALID is set to avoid further                      CBL04920
      *   processing if certain keys are pressed or if                  CBL04930
      *   RECEIVE-MAP fails.                                            CBL04940
      ****************************************************              CBL04950
                                                                        CBL04960
       1100-USER-INPUT.                                                 CBL04970
                                                                        CBL04980
           EVALUATE TRUE                                                CBL04990
                                                                        CBL05000
      *  PF1=Help pressed; 'D'=Subsequent display                       CBL05010
             WHEN EIBAID = DFHPF1                                       CBL05020
               MOVE 'D' TO OUTPUT-FLAG                                  CBL05030
               SET INPUTS-NOT-VALID TO TRUE                             CBL05040
      *  '02I: Help Not Available.'                                     CBL05050
               MOVE '02I' TO GM-MSG-ID                                  CBL05060
                                                                        CBL05070
      *  PF3=Exit: 'X'=Exit the program                                 CBL05080
             WHEN EIBAID = DFHPF3                                       CBL05090
               MOVE 'X' TO OUTPUT-FLAG                                  CBL05100
               SET INPUTS-NOT-VALID TO TRUE                             CBL05110
                                                                        CBL05120
      *  Enter pressed; retrieve CICS panel                             CBL05130
             WHEN EIBAID = DFHENTER                                     CBL05140
               EXEC CICS                                                CBL05150
                 RECEIVE MAP('CSM03MP')                                 CBL05160
                 MAPSET('CSM03MP')                                      CBL05170
                 RESP(LOCAL-RESPONSE)                                   CBL05180
               END-EXEC                                                 CBL05190
                                                                        CBL05200
      * RECEIVE-MAP failed; Process like PF3                            CBL05210
               IF LOCAL-RESPONSE NOT = DFHRESP(NORMAL)                  CBL05220
                 MOVE 'X' TO OUTPUT-FLAG                                CBL05230
                 SET INPUTS-NOT-VALID TO TRUE                           CBL05240
               ELSE                                                     CBL05250
      *  Initialize flag to indicate all inputs are valid               CBL05260
                 MOVE 'D' TO OUTPUT-FLAG                                CBL05270
                 SET INPUTS-VALID TO TRUE                               CBL05280
                 PERFORM 1110-VALIDATE-INPUT THRU                       CBL05290
                         1110-VALIDATE-INPUT-EXIT                       CBL05300
               END-IF                                                   CBL05310
                                                                        CBL05320
      *  Any other key was pressed                                      CBL05330
             WHEN OTHER                                                 CBL05340
               MOVE 'D' TO OUTPUT-FLAG                                  CBL05350
               SET INPUTS-NOT-VALID TO TRUE                             CBL05360
      *  '03E: Key Not Defined.'                                        CBL05370
               MOVE '03E' TO GM-MSG-ID                                  CBL05380
                                                                        CBL05390
           END-EVALUATE.                                                CBL05400
                                                                        CBL05410
       1100-USER-INPUT-EXIT. EXIT.                                      CBL05420
                                                                        CBL05430
      *************************************************************     CBL05440
      *  1110-VALIDATE-INPUT                                            CBL05450
      *     Validate the Action and search fields (Lastname, Dept).     CBL05460
      *     If errors found, set the message id. If no errors are       CBL05470
      *     found, left-justify the data before processing. This        CBL05480
      *     is needed for BMS support.                                  CBL05490
      *************************************************************     CBL05500
                                                                        CBL05510
        1110-VALIDATE-INPUT.                                            CBL05520
                                                                        CBL05530
                                                                        CBL05540
      * 04E 'Action Invalid or blank.'                                  CBL05550
                IF (ACTIONI NOT = 'E' AND ACTIONI NOT = 'P' AND         CBL05560
                    ACTIONI NOT = 'D')                                  CBL05570
                  SET INPUTS-NOT-VALID TO TRUE                          CBL05580
                  MOVE '04E' TO GM-MSG-ID                               CBL05590
                                                                        CBL05600
                ELSE                                                    CBL05610
      * 05E 'Search Data Missing.'                                      CBL05620
                  IF (ACTIONI = 'E' OR ACTIONI = 'P') AND               CBL05630
                     (LASTNAMI = SPACES AND DEPMTI = SPACES)            CBL05640
                    SET INPUTS-NOT-VALID TO TRUE                        CBL05650
                    MOVE '05E' TO GM-MSG-ID                             CBL05660
                                                                        CBL05670
      * Inputs are valid. Message and INPUTS-VALID flag already set.    CBL05680
                  ELSE                                                  CBL05690
                    INITIALIZE LJUST-FIELD-1, LJUST-FIELD-2             CBL05700
      *   Left-justify the Last Name input (BMS support)                CBL05710
                    IF LASTNAMI NOT = SPACES                            CBL05720
                      INSPECT LASTNAMI REPLACING LEADING SPACES BY      CBL05730
                        HIGH-VALUES                                     CBL05740
                      UNSTRING LASTNAMI DELIMITED BY ALL HIGH-VALUES    CBL05750
                        INTO LJUST-FIELD-1, LJUST-FIELD-2               CBL05760
      *   Leading blanks found                                          CBL05770
                      IF LJUST-FIELD-1 = SPACES                         CBL05780
                        MOVE LJUST-LASTNAME-2 TO LASTNAMO               CBL05790
      *   No leading blanks found                                       CBL05800
                      ELSE                                              CBL05810
                        MOVE LJUST-LASTNAME-1 TO LASTNAMO               CBL05820
                      END-IF                                            CBL05830
                    ELSE                                                CBL05840
                      CONTINUE                                          CBL05850
                    END-IF                                              CBL05860
                                                                        CBL05870
      *   Left-justify the Department input (BMS support)               CBL05880
                    IF DEPMTI NOT = SPACES                              CBL05890
                      INSPECT DEPMTI REPLACING LEADING SPACES BY        CBL05900
                        HIGH-VALUES                                     CBL05910
                      UNSTRING DEPMTI DELIMITED BY ALL HIGH-VALUES      CBL05920
                        INTO LJUST-FIELD-1, LJUST-FIELD-2               CBL05930
      *   Leading blanks found                                          CBL05940
                      IF LJUST-FIELD-1 = SPACES                         CBL05950
                        MOVE LJUST-DEPT-2 TO DEPMTO                     CBL05960
                      ELSE                                              CBL05970
      *   No leading blanks found                                       CBL05980
                        MOVE LJUST-DEPT-1 TO DEPMTO                     CBL05990
                      END-IF                                            CBL06000
                    ELSE                                                CBL06010
                      CONTINUE                                          CBL06020
                    END-IF.                                             CBL06030
                                                                        CBL06040
        1110-VALIDATE-INPUT-EXIT. EXIT.                                 CBL06050
                                                                        CBL06060
      *************************************************************     CBL06070
      * 1200-DISPLAY-ALL:                                               CBL06080
      *   Display all of the entries in EMP-ARRAY.                      CBL06090
      *   Move all of the entries from EMP-DATA to RESULTS-ARRAY.       CBL06100
      *************************************************************     CBL06110
                                                                        CBL06120
        1200-DISPLAY-ALL.                                               CBL06130
                                                                        CBL06140
      * Initialize subscripts                                           CBL06150
            MOVE 1 TO RESULTS-PTR.                                      CBL06160
            MOVE 1 TO EMP-PTR.                                          CBL06170
                                                                        CBL06180
      * Move all entries to RESULTS-ARRAY                               CBL06190
            PERFORM UNTIL EMP-PTR > EMP-ARRAY-MAX                       CBL06200
              MOVE EMP-ENTRY(EMP-PTR) TO RESULTS-ENTRY(RESULTS-PTR)     CBL06210
              ADD 1 TO EMP-PTR                                          CBL06220
              ADD 1 TO RESULTS-PTR                                      CBL06230
            END-PERFORM.                                                CBL06240
                                                                        CBL06250
      * Indicate number of entries processed                            CBL06260
            MOVE EMP-ARRAY-MAX TO RESULTS-ARRAY-MAX.                    CBL06270
                                                                        CBL06280
         1200-DISPLAY-ALL-EXIT. EXIT.                                   CBL06290
                                                                        CBL06300
      *************************************************************     CBL06310
      * 1300-PARTIAL-MATCH:                                             CBL06320
      *   Process search fields that are fully qualified of             CBL06330
      *   consist of leading characters.                                CBL06340
      *   Determine the lengths of the Lastname and Department          CBL06350
      *   fields on the screen.                 (1310-FIND-LENGTHS)     CBL06360
      *   Move matching entries from EMP-DATA to RESULTS-ARRAY.         CBL06370
      *************************************************************     CBL06380
                                                                        CBL06390
        1300-PARTIAL-MATCH.                                             CBL06400
                                                                        CBL06410
      *   Determine the lengths of the user inputs: Lastname, Dept      CBL06420
            PERFORM 1310-FIND-LENGTHS THRU 1310-FIND-LENGTHS-EXIT.      CBL06430
                                                                        CBL06440
      *   Initialize subscripts                                         CBL06450
            MOVE 1 TO RESULTS-PTR.                                      CBL06460
            MOVE 1 TO EMP-PTR.                                          CBL06470
                                                                        CBL06480
      *   Search through Employee data                                  CBL06490
            PERFORM UNTIL EMP-PTR > EMP-ARRAY-MAX                       CBL06500
                                                                        CBL06510
      *   Lastname not specified                                        CBL06520
              IF LASTNAMI IS = SPACES                                   CBL06530
      *   Dept only is specified                                        CBL06540
                IF DEPMTI (1:DEPT-LENGTH) =                             CBL06550
                     EMP-DEPT (EMP-PTR)(1:DEPT-LENGTH)                  CBL06560
                  MOVE EMP-ENTRY(EMP-PTR) TO                            CBL06570
                       RESULTS-ENTRY(RESULTS-PTR)                       CBL06580
                  ADD 1 TO RESULTS-PTR                                  CBL06590
                ELSE                                                    CBL06600
                  CONTINUE                                              CBL06610
                END-IF                                                  CBL06620
                                                                        CBL06630
      *   Lastname specified                                            CBL06640
              ELSE                                                      CBL06650
                IF LASTNAMI (1:LASTNAME-LENGTH) =                       CBL06660
                   EMP-LAST-NAME (EMP-PTR)(1:LASTNAME-LENGTH)           CBL06670
                                                                        CBL06680
      *   Lastname only was specified                                   CBL06690
                  IF DEPMTI = SPACES                                    CBL06700
                    MOVE EMP-ENTRY(EMP-PTR) TO                          CBL06710
                       RESULTS-ENTRY(RESULTS-PTR)                       CBL06720
                    ADD 1 TO RESULTS-PTR                                CBL06730
                  ELSE                                                  CBL06740
                                                                        CBL06750
      *   Lastname and Dept both specified                              CBL06760
                    IF DEPMTI (1:DEPT-LENGTH) =                         CBL06770
                       EMP-DEPT (EMP-PTR)(1:DEPT-LENGTH)                CBL06780
                      MOVE EMP-ENTRY(EMP-PTR) TO                        CBL06790
                         RESULTS-ENTRY(RESULTS-PTR)                     CBL06800
                      ADD 1 TO RESULTS-PTR                              CBL06810
      *   Dept did not match. Continue search.                          CBL06820
                    ELSE                                                CBL06830
                      CONTINUE                                          CBL06840
                    END-IF                                              CBL06850
                  END-IF                                                CBL06860
      *   Lastname did not match. Continue search.                      CBL06870
                ELSE                                                    CBL06880
                  CONTINUE                                              CBL06890
                END-IF                                                  CBL06900
              END-IF                                                    CBL06910
              ADD 1 TO EMP-PTR                                          CBL06920
            END-PERFORM.                                                CBL06930
                                                                        CBL06940
      *   Set number of matches found.                                  CBL06950
            SUBTRACT 1 FROM RESULTS-PTR GIVING RESULTS-ARRAY-MAX.       CBL06960
                                                                        CBL06970
        1300-PARTIAL-MATCH-EXIT. EXIT.                                  CBL06980
                                                                        CBL06990
      *************************************************************     CBL07000
      * 1310-FIND-LENGTHS: Determine length of Last Name and Dept       CBL07010
      *   Determine length of Last Name and Dept fields entered         CBL07020
      *   by the user.                                                  CBL07030
      *   The backward search for the first non-blank character         CBL07040
      *   is done to allow imbedded blanks.                             CBL07050
      *   Currently, Lastname is up to 15 characters and Dept is        CBL07060
      *   up to 3 characters.                                           CBL07070
      *                                                                 CBL07080
      *   Upgrading Highlight:                                          CBL07090
      *   o REVERSE Intrinsic Function                                  CBL07100
      *************************************************************     CBL07110
                                                                        CBL07120
        1310-FIND-LENGTHS.                                              CBL07130
                                                                        CBL07140
      * Determine length of Last Name field from ISPF panel             CBL07150
            IF LASTNAMI = SPACES                                        CBL07160
      * Lastname not specified.                                         CBL07170
              MOVE 0 TO LASTNAME-LENGTH                                 CBL07180
            ELSE                                                        CBL07190
              INITIALIZE BLANK-COUNT                                    CBL07200
      * Facilitate search for trailing blanks by reversing the          CBL07210
      * input characters using COBOL/370 intrinsic function REVERSE     CBL07220
              INSPECT FUNCTION REVERSE(LASTNAMI)                        CBL07230
                TALLYING BLANK-COUNT FOR LEADING SPACES                 CBL07240
              COMPUTE LASTNAME-LENGTH = 15 - BLANK-COUNT                CBL07250
            END-IF.                                                     CBL07260
                                                                        CBL07270
      * Determine length of Department field from ISPF panel            CBL07280
            IF DEPMTI = SPACES                                          CBL07290
      * Dept not specified. Error message already set.                  CBL07300
              MOVE 0 TO DEPT-LENGTH                                     CBL07310
            ELSE                                                        CBL07320
              INITIALIZE BLANK-COUNT                                    CBL07330
      * Facilitate search for trailing blanks by reversing the          CBL07340
      * input characters using COBOL/370 intrinsic function REVERSE     CBL07350
              INSPECT FUNCTION REVERSE(DEPMTI)                          CBL07360
                TALLYING BLANK-COUNT FOR LEADING SPACES                 CBL07370
              COMPUTE DEPT-LENGTH = 3 - BLANK-COUNT                     CBL07380
            END-IF.                                                     CBL07390
                                                                        CBL07400
        1310-FIND-LENGTHS-EXIT. EXIT.                                   CBL07410
                                                                        CBL07420
      *************************************************************     CBL07430
      * 1400-EXACT-MATCH:                                               CBL07440
      *   Process search fields that are fully qualified.               CBL07450
      *   Move matching entries from EMP-DATA to RESULTS-ARRAY.         CBL07460
      *   This procedure uses a simple sequential search                CBL07470
      *   because there are currently only twelve records.              CBL07480
      *************************************************************     CBL07490
                                                                        CBL07500
        1400-EXACT-MATCH.                                               CBL07510
                                                                        CBL07520
      *  Initialize subscripts.                                         CBL07530
            MOVE 1 TO RESULTS-PTR.                                      CBL07540
            MOVE 1 TO EMP-PTR.                                          CBL07550
                                                                        CBL07560
            PERFORM UNTIL EMP-PTR > EMP-ARRAY-MAX                       CBL07570
      *  Lastname not specified                                         CBL07580
              IF LASTNAMI = SPACES                                      CBL07590
      *  Only Dept specified                                            CBL07600
                IF DEPMTI = EMP-DEPT(EMP-PTR)                           CBL07610
                  MOVE EMP-ENTRY(EMP-PTR) TO                            CBL07620
                       RESULTS-ENTRY(RESULTS-PTR)                       CBL07630
                  ADD 1 TO RESULTS-PTR                                  CBL07640
      *  Dept did not match. Continue search.                           CBL07650
                ELSE                                                    CBL07660
                  CONTINUE                                              CBL07670
                END-IF                                                  CBL07680
      *  Lastname specified                                             CBL07690
              ELSE                                                      CBL07700
                IF LASTNAMI = EMP-LAST-NAME(EMP-PTR)                    CBL07710
      *  Only Lastname specified                                        CBL07720
                  IF DEPMTI = SPACES                                    CBL07730
                    MOVE EMP-ENTRY(EMP-PTR) TO                          CBL07740
                         RESULTS-ENTRY(RESULTS-PTR)                     CBL07750
                    ADD 1 TO RESULTS-PTR                                CBL07760
                  ELSE                                                  CBL07770
      *  Lastname and Dept specified                                    CBL07780
                    IF DEPMTI = EMP-DEPT(EMP-PTR)                       CBL07790
                      MOVE EMP-ENTRY(EMP-PTR) TO                        CBL07800
                           RESULTS-ENTRY(RESULTS-PTR)                   CBL07810
                      ADD 1 TO RESULTS-PTR                              CBL07820
      *  Dept did not match. Continue search.                           CBL07830
                    ELSE                                                CBL07840
                      CONTINUE                                          CBL07850
                    END-IF                                              CBL07860
                  END-IF                                                CBL07870
      *  Lastname did not match. Continue search.                       CBL07880
                ELSE                                                    CBL07890
                  CONTINUE                                              CBL07900
                END-IF                                                  CBL07910
              END-IF                                                    CBL07920
                                                                        CBL07930
              ADD 1 TO EMP-PTR                                          CBL07940
            END-PERFORM                                                 CBL07950
                                                                        CBL07960
      *   Set number of matches found.                                  CBL07970
            SUBTRACT 1 FROM RESULTS-PTR GIVING RESULTS-ARRAY-MAX.       CBL07980
                                                                        CBL07990
        1400-EXACT-MATCH-EXIT. EXIT.                                    CBL08000
                                                                        CBL08010
      *************************************************************     CBL08020
      * 1500-LOAD-SCREEN:                                               CBL08030
      *   Format the data in the internal work area (RESULTS-ARRAY)     CBL08040
      *   then move it to the ouput area (SCREEN-ARRAY)                 CBL08050
      *   (1510-LOAD-SCREEN-ENTRY).                                     CBL08060
      *   Set the message if match was found or not found.              CBL08070
      *************************************************************     CBL08080
                                                                        CBL08090
        1500-LOAD-SCREEN.                                               CBL08100
                                                                        CBL08110
      * Reinitialize the BMS output lines.                              CBL08120
                SET START-LOOP TO TRUE                                  CBL08130
                PERFORM VARYING WORKPTR FROM 1 BY 1 UNTIL STOP-LOOP     CBL08140
                  MOVE SPACES TO SCREEN-LINE(WORKPTR)                   CBL08150
                  IF WORKPTR = EMP-ARRAY-MAX                            CBL08160
                    SET STOP-LOOP TO TRUE                               CBL08170
                  ELSE                                                  CBL08180
                    CONTINUE                                            CBL08190
                  END-IF                                                CBL08200
                END-PERFORM                                             CBL08210
                                                                        CBL08220
      * If at least one match was found, continue processing.           CBL08230
            IF RESULTS-ARRAY-MAX > 0                                    CBL08240
      * Initialize subscripts                                           CBL08250
              MOVE 1 TO RESULTS-PTR                                     CBL08260
              MOVE 1 TO SCREEN-PTR                                      CBL08270
                                                                        CBL08280
      * Format and load data from RESULTS-ARRAY into SCREEN-ARRAY       CBL08290
              PERFORM 1510-LOAD-SCREEN-ENTRY THRU                       CBL08300
                      1510-LOAD-SCREEN-ENTRY-EXIT                       CBL08310
                  UNTIL RESULTS-PTR > RESULTS-ARRAY-MAX                 CBL08320
      * 06I: Lookup Completed Successfully                              CBL08330
              MOVE '06I' TO GM-MSG-ID                                   CBL08340
            ELSE                                                        CBL08350
      * 07I: No match found                                             CBL08360
              MOVE '07I' TO GM-MSG-ID                                   CBL08370
            END-IF.                                                     CBL08380
                                                                        CBL08390
        1500-LOAD-SCREEN-EXIT. EXIT.                                    CBL08400
                                                                        CBL08410
      *************************************************************     CBL08420
      * 1510-LOAD-SCREEN-ENTRY:                                         CBL08430
      *   Format and Move data from RESULTS-ARRAY to SCREEN-ARRAY.      CBL08440
      *   The Service Length is calculated based on the Hire-Date       CBL08450
      *   and the current system date.                                  CBL08460
      *************************************************************     CBL08470
                                                                        CBL08480
        1510-LOAD-SCREEN-ENTRY.                                         CBL08490
                                                                        CBL08500
      * Set Employee name. Use HIGH-VALUE as separators because         CBL08510
      * STRING does not allow concatenation of blanks when              CBL08520
      * delimiting by SPACE                                             CBL08530
                                                                        CBL08540
            STRING   RESULTS-LAST-NAME(RESULTS-PTR)                     CBL08550
                     ','                                                CBL08560
                     HIGH-VALUE                                         CBL08570
                     RESULTS-FIRST-NAME(RESULTS-PTR)                    CBL08580
                     HIGH-VALUE                                         CBL08590
                     RESULTS-MIDDLE-INITIAL(RESULTS-PTR)                CBL08600
                     '.'                                                CBL08610
                DELIMITED BY SPACE                                      CBL08620
                INTO SCREEN-NAME(SCREEN-PTR)                            CBL08630
           END-STRING.                                                  CBL08640
                                                                        CBL08650
      * Change HIGH-VALUEs to blanks to complete formatting             CBL08660
            INSPECT SCREEN-NAME(SCREEN-PTR)                             CBL08670
              REPLACING ALL HIGH-VALUE BY SPACE.                        CBL08680
                                                                        CBL08690
      *  Set output Department and Phone                                CBL08700
           MOVE RESULTS-DEPT(RESULTS-PTR) TO SCREEN-DEPT(SCREEN-PTR).   CBL08710
           MOVE RESULTS-PHONE(RESULTS-PTR) TO SCREEN-PHONE(SCREEN-PTR). CBL08720
                                                                        CBL08730
      *  Format output Hire Date: mm/dd/yyyy:                           CBL08740
      *   Convert YYMMDD Hire date to Lilian format (integer)           CBL08750
      *   using LE/370 callable service CEEDAYS.                        CBL08760
           MOVE RESULTS-HIRE-DATE(RESULTS-PTR) TO                       CBL08770
                CEEDAYS-INPUT-VALUE.                                    CBL08780
           MOVE 6 TO CEEDAYS-INPUT-LENGTH.                              CBL08790
           MOVE 'YYMMDD' TO CEEDAYS-PICSTRING-FORMAT.                   CBL08800
           MOVE 6 TO CEEDAYS-PICSTRING-LENGTH.                          CBL08810
                                                                        CBL08820
           CALL 'CEEDAYS' USING CEEDAYS-INPUT,                          CBL08830
                                CEEDAYS-PICSTRING,                      CBL08840
                                HIRE-DATE-LILIAN,                       CBL08850
                                CEE-CALL-FEEDBACK.                      CBL08860
                                                                        CBL08870
      *   Convert Lilian format (integer) to MM/DD/YYYY                 CBL08880
      *   using LE/370 callable service CEEDATE.                        CBL08890
           MOVE 10 TO CEEDATE-PICSTRING-LENGTH.                         CBL08900
           MOVE 'MM/DD/YYYY' TO CEEDATE-PICSTRING-FORMAT.               CBL08910
                                                                        CBL08920
           CALL 'CEEDATE' USING HIRE-DATE-LILIAN,                       CBL08930
                                CEEDATE-PICSTRING,                      CBL08940
                                CEEDATE-OUTPUT,                         CBL08950
                                CEE-CALL-FEEDBACK.                      CBL08960
           MOVE CEEDATE-OUTPUT (1:10) TO SCREEN-HIRE-DATE(SCREEN-PTR).  CBL08970
                                                                        CBL08980
      * Calculate the Service Length:                                   CBL08990
      *   Convert mm/dd/yyyy to yyyymmdd:                               CBL09000
           STRING   CEEDATE-OUTPUT-YYYY                                 CBL09010
                    CEEDATE-OUTPUT-MM                                   CBL09020
                    CEEDATE-OUTPUT-DD                                   CBL09030
                 DELIMITED BY SIZE                                      CBL09040
                 INTO HIRE-DATE-CHAR                                    CBL09050
           END-STRING.                                                  CBL09060
                                                                        CBL09070
            COMPUTE HIRE-DATE-LILIAN =                                  CBL09080
                         FUNCTION INTEGER-OF-DATE(HIRE-DATE-INT).       CBL09090
      *   Retrieve the current date (yyyymmdd --> todays-date-int)      CBL09100
      *   Obtain system date/time using COBOL/370 intrinsic function    CBL09110
      *   CURRENT-DATE.                                                 CBL09120
            MOVE FUNCTION CURRENT-DATE(1:8) TO TODAYS-DATE-INT.         CBL09130
            COMPUTE TODAYS-DATE-LILIAN =                                CBL09140
                         FUNCTION INTEGER-OF-DATE(TODAYS-DATE-INT).     CBL09150
                                                                        CBL09160
      *   Calculate years of service                                    CBL09170
            SUBTRACT HIRE-DATE-LILIAN FROM TODAYS-DATE-LILIAN           CBL09180
                  GIVING LILIAN-DIF                                     CBL09190
            END-SUBTRACT.                                               CBL09200
            DIVIDE LILIAN-DIF BY 365 GIVING SERV-YEAR                   CBL09210
                    REMAINDER SERV-YEAR-REMAINDER                       CBL09220
            END-DIVIDE.                                                 CBL09230
                                                                        CBL09240
      * Format the Service Length output: yy YR                         CBL09250
            MOVE SERV-YEAR TO SERV-YEAR-F.                              CBL09260
            MOVE SERVICE-FORMAT TO SCREEN-SERVICE-LENGTH(SCREEN-PTR).   CBL09270
                                                                        CBL09280
            ADD 1 TO SCREEN-PTR.                                        CBL09290
            ADD 1 TO RESULTS-PTR.                                       CBL09300
                                                                        CBL09310
        1510-LOAD-SCREEN-ENTRY-EXIT. EXIT.                              CBL09320
                                                                        CBL09330
      ******************************************************            CBL09340
      * C100-DISPLAY-SCREEN:                                            CBL09350
      *   For BMS support, set the output date and time.                CBL09360
      *   The date and time is be refreshed every time the              CBL09370
      *   panel is displayed.                                           CBL09380
      *                                                                 CBL09390
      *   The OUTPUT-FLAG is tested as follows:                         CBL09400
      *                                                                 CBL09410
      *   If this is the first time to display the panel,               CBL09420
      *   call the message routine to obtain the message                CBL09430
      *   text '01I: Specify Action and Search data.'                   CBL09440
      *   The output lines are initialized at this point to             CBL09450
      *   insure that they are write-protected at the first             CBL09460
      *   display of the map.                                           CBL09470
      *   Send both the data (date/time) and the map.                   CBL09480
      *                                                                 CBL09490
      *   If PF3 (Exit), clear the terminal screen. The program         CBL09500
      *   will exit in 0000-LOOKUP.                                     CBL09510
      *                                                                 CBL09520
      *   For any other value of OUTPUT-FLAG, call the message          CBL09530
      *   routine to obtain the message text, then send both            CBL09540
      *   the data and the map.                                         CBL09550
      *                                                                 CBL09560
      ******************************************************            CBL09570
                                                                        CBL09580
       C100-DISPLAY-SCREEN.                                             CBL09590
                                                                        CBL09600
                                                                        CBL09610
      *  Initialize the Date/Time (mm/dd/yy HH:MM) for BMS screen       CBL09620
                                                                        CBL09630
      *   Obtain system date/time using COBOL/370 intrinsic function    CBL09640
      *   CURRENT-DATE.                                                 CBL09650
            MOVE FUNCTION CURRENT-DATE TO BMS-DATE.                     CBL09660
            STRING   BMS-DATE-MONTH                                     CBL09670
                     '/'                                                CBL09680
                     BMS-DATE-DAY                                       CBL09690
                     '/'                                                CBL09700
                     BMS-DATE-YEAR2                                     CBL09710
                     SPACE                                              CBL09720
                     BMS-TIME-HOUR                                      CBL09730
                     ':'                                                CBL09740
                     BMS-TIME-MINUTES                                   CBL09750
                DELIMITED BY SIZE                                       CBL09760
                INTO DATETIMO                                           CBL09770
            END-STRING.                                                 CBL09780
                                                                        CBL09790
      **************************************************                CBL09800
      *  Process keyboard inputs based on OUTPUT-FLAG  *                CBL09810
      **************************************************                CBL09820
                                                                        CBL09830
            EVALUATE OUTPUT-FLAG                                        CBL09840
                                                                        CBL09850
      * First time display: Retrieve message text, initialize the       CBL09860
      * screen fields, then send the MAP only                           CBL09870
              WHEN 'F'                                                  CBL09880
      *   Call Message routine to get message text                      CBL09890
                EXEC CICS LINK PROGRAM('CSM03C')                        CBL09900
                               COMMAREA(CSM03C-PARMS)                   CBL09910
                               LENGTH(71)                               CBL09920
                END-EXEC                                                CBL09930
                                                                        CBL09940
      *   Initialize the BMS screen with the message text               CBL09950
                MOVE GM-MSG-TXT TO MSGLINEO                             CBL09960
                                                                        CBL09970
      *   Initialize output fields                                      CBL09980
                MOVE SPACES TO ACTIONO                                  CBL09990
                MOVE SPACES TO LASTNAMO                                 CBL10000
                MOVE SPACES TO DEPMTO                                   CBL10010
                                                                        CBL10020
      * Initialize the BMS output lines.                                CBL10030
                SET START-LOOP TO TRUE                                  CBL10040
                PERFORM VARYING WORKPTR FROM 1 BY 1 UNTIL STOP-LOOP     CBL10050
                  MOVE SPACES TO SCREEN-LINE(WORKPTR)                   CBL10060
                  IF WORKPTR = EMP-ARRAY-MAX                            CBL10070
                    SET STOP-LOOP TO TRUE                               CBL10080
                  ELSE                                                  CBL10090
                    CONTINUE                                            CBL10100
                  END-IF                                                CBL10110
                END-PERFORM                                             CBL10120
                                                                        CBL10130
      *   Send map and data (date/time)                                 CBL10140
                EXEC CICS                                               CBL10150
                     SEND MAP('CSM03MP')                                CBL10160
                     MAPSET('CSM03MP')                                  CBL10170
                     ERASE FREEKB                                       CBL10180
                END-EXEC                                                CBL10190
                                                                        CBL10200
      * PF3 key pressed:clear the terminal screen.                      CBL10210
                                                                        CBL10220
              WHEN 'X'                                                  CBL10230
                MOVE SPACE TO CICM03C-COMMAREA                          CBL10240
                EXEC CICS                                               CBL10250
                      SEND CONTROL                                      CBL10260
                      ERASE FREEKB                                      CBL10270
                END-EXEC                                                CBL10280
                                                                        CBL10290
      * Subsequent Display:  send the DATA only                         CBL10300
              WHEN OTHER                                                CBL10310
      *   Call Message routine to get message text                      CBL10320
                EXEC CICS LINK PROGRAM('CSM03C')                        CBL10330
                               COMMAREA(CSM03C-PARMS)                   CBL10340
                               LENGTH(71)                               CBL10350
                END-EXEC                                                CBL10360
                                                                        CBL10370
                MOVE GM-MSG-TXT TO MSGLINEO                             CBL10380
                                                                        CBL10390
                EXEC CICS                                               CBL10400
                     SEND MAP('CSM03MP')                                CBL10410
                     MAPSET('CSM03MP')                                  CBL10420
                     ERASE FREEKB                                       CBL10430
                END-EXEC                                                CBL10440
              END-EVALUATE.                                             CBL10450
                                                                        CBL10460
       C100-DISPLAY-SCREEN-EXIT. EXIT.                                  CBL10470
                                                                        CBL10480