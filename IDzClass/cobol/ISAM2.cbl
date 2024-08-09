CBL  APOST
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ISAM2.
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SEGA-CUSTOMER-ACCOUNT.
           03 SEGA-ACCOUNT-NUM       PIC X(10).
           03 SEGA-CUSTOMER-PHONE-NB PIC X(10).
           03 SEGA-CUSTOMER-NAME     PIC X(30).
           03 SEGA-CUSTOMER-ADDRESS  PIC X(40).

       01  SEGB-TOTAL-USAGE.
           03 USAGE-PERIOD.
              05 USAGE-YEAR          PIC X(4).
              05 USAGE-MONTH         PIC X(2).
           03 ACCOUNT-INV-SEQ        PIC X.
           03 ACCOUNT-TOTAL-USAGE    PIC 9(9)V99 COMP-3.
           03 CURRENT-PERIOD-AMT-DUE PIC 9(7)V99 COMP-3.
           03 COMPLAINTS             PIC X(62).

       01  DL1-FUNCTION            PICTURE X(04).

       01  SEG00010-SSA.
           02 SEG-NAME-00010       PICTURE X(08) VALUE 'CUSTADDR'.
           02 BEGIN-OP-00010       PICTURE X(03) VALUE '*P('.
           02 KEY-NAME-00010       PICTURE X(08) VALUE 'AKEY    '.
           02 REL-OPER-00010       PICTURE X(02) VALUE ' ='.
           02 KEY-VALUE-00010      PICTURE X(10).
           02 END-OP-00010         PICTURE X(01) VALUE ')'.
       01  SEG00020-SSA.
           02 SEG-NAME-00020       PICTURE X(08) VALUE 'TOTUSE  '.
           02 BEGIN-OP-00020       PICTURE X(03) VALUE '   '.
           02 KEY-NAME-00020       PICTURE X(08) VALUE 'BKEY    '.
           02 REL-OPER-00020       PICTURE X(02) VALUE ' ='.
           02 KEY-VALUE-00020      PICTURE X(17).
           02 END-OP-00020         PICTURE X(01) VALUE ')'.


       01  MISC-ARITHMETIC-FIELDS.
           02 USAGE-TOTAL          PICTURE S9(11)V99 COMP-3.

       LINKAGE SECTION.
       01  PCB-AREA-1.
           02 DBD-NAME             PICTURE  X(08).
           02 SEGMENT-LEVEL        PICTURE  X(02).
           02 STATUS-CODES         PICTURE  X(02).
           02 PROCESS-OPTIONS      PICTURE X(04).
           02 FILLER               PICTURE S9(05)  COMPUTATIONAL.
           02 SEG-NAME-FEEDBACK    PICTURE X(08).
           02 KEY-LENGTH           PICTURE S9(05)  COMPUTATIONAL.
           02 NUMBER-SEGS          PICTURE S9(05)  COMPUTATIONAL.
           02 KEY-FEEDBACK-AREA.
              03 KFB-CUSTOMER-NUMBER PIC    X(10).
              03 KFB-USE-YYYYMM      PIC    X(06).

       01  CUST-ACCOUNT-NUM        PICTURE  X(10).

       01  TOTUSE-STATS.
           05  NUM-TOTUSE-SEGMENTS   PIC S9(9)   COMP-3.
           05  TOTAL-USAGE           PIC S9(9)   COMP-3.

       PROCEDURE DIVISION USING PCB-AREA-1,
                                CUST-ACCOUNT-NUM, TOTUSE-STATS.
      *    DISPLAY 'STARTING ISAM2'.
           MOVE CUST-ACCOUNT-NUM TO KEY-VALUE-00010.
           MOVE 'GU  '           TO DL1-FUNCTION.
           CALL 'CBLTDLI' USING  DL1-FUNCTION,
                                 PCB-AREA-1,
                                 SEGB-TOTAL-USAGE
                                 SEG00010-SSA,
                                 SEG00020-SSA.


           IF STATUS-CODES = '  '
              ADD  1  TO  NUM-TOTUSE-SEGMENTS
              ADD  ACCOUNT-TOTAL-USAGE  TO  TOTAL-USAGE
              MOVE 'GNP '        TO DL1-FUNCTION
              PERFORM SUM-USAGE UNTIL STATUS-CODES NOT = '  '
           END-IF.

           GOBACK.

       SUM-USAGE.
           CALL 'CBLTDLI' USING DL1-FUNCTION,
                                PCB-AREA-1,
                                SEGB-TOTAL-USAGE,
                                SEG00020-SSA.
           IF STATUS-CODES = '  '
              ADD  1  TO  NUM-TOTUSE-SEGMENTS
              ADD ACCOUNT-TOTAL-USAGE TO TOTAL-USAGE.