       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPCRFL.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT OUTFILE
           ASSIGN TO UT-S-OUTFILE
           ORGANIZATION IS SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(80).

       FD  OUTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OUT-Rec.
       01  OUT-REC  PIC X(100).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       01  WS-OUTPUT-REC               PIC X(100).

       01  WS-SYSOUT-REC.
           05  MSG                     PIC X(80).

       77  WS-DATE                     PIC 9(6).

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-WRITTEN          PIC 9(4)  value 0.

       01  Record-Table.
           03 Tab-data.
           05 Table-Rec1.
               10 filler pic x(50) value
             "11111Smith     Joanne    2016521111I000112/12/200".
               10 filler pic x(50) value
             "50001123010HMO012012345678A1O4H9I0100015".
           05 Table-Rec2.
               10 filler pic x(50) value
             "22222Martin    Ricky     2124441212O000210/01/195".
               10 filler pic x(50) value
             "20012345623PPO006001231421B2K5T8N0900025".
           05 Table-Rec3.
               10 filler pic x(50) value
             "63333Jones     James     2025551212I000311/23/200".
               10 filler pic x(50) value
             "40000873547HMO020071232321C3J6F7I0800035".
           05 Table-Rec4.
               10 filler pic x(50) value
             "74444McAllisterBillieJoe 8762341234I023409/23/200".
               10 filler pic x(50) value
             "80234323483MAN011045321321D4H7D6I0700045".
           05 Table-Rec5.
               10 filler pic x(50) value
             "55555Luthor    Lex       7645234324I098412/31/200".
               10 filler pic x(50) value
             "80434356746POS000000123786E5G8S5N0600055".
           05 Table-Rec6.
               10 filler pic x(50) value
             "66666Barnum    P.T.      7854354354O078321/09/200".
               10 filler pic x(50) value
             "60000432439HMO025070176321F6F9A4N0500065".
           05 Table-Rec7.
               10 filler pic x(50) value
             "98765Clampett  E.M.      5464432432I001308/12/200".
               10 filler pic x(50) value
             "50002342138POS030004536179G7D0Q6I0400075".
           05 Table-Rec8.
               10 filler pic x(50) value
             "88888Watson    Thomas    2127643483O002207/25/200".
               10 filler pic x(50) value
             "50012112329PPO010003546321H8S1W3I0300075".
           05 Table-Rec9.
               10 filler pic x(50) value
             "99999Hamilton  Alex      9098883213I000206/30/200".
               10 filler pic x(50) value
             "60002323483HMO020000047574I9D2E2I0200085".
           05 Table-Rec10.
               10 filler pic x(50) value
             "10021Welch     John      2036520980O000105/31/200".
               10 filler pic x(50) value
             "70001123010MAN040097812143J0F3F1N0100014".
           05 Table-Rec11.
               10 filler pic x(50) value
             "12901Thomas    Frank     2036520980O000105/31/200".
               10 filler pic x(50) value
             "70001123010MAN050097812143J0F3F1N      ".
           05 Table-Rec12.
               10 filler pic x(50) value
             "88888Bad Pat.  Type                X             ".
               10 filler pic x(50) value
             "           GOV                          ".
          03 recs redefines tab-data OCCURS 12 TIMES
                     indexed by r-idx.
               10  rec-data                Pic x(100).

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT 12 times.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "Starting program: HOSPCRFL" to msg.
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           OPEN OUTPUT OUTFILE, SYSOUT.
           move spaces to sysout-rec.
           Write SYSOUT-REC from WS-SYSOUT-REC.
           move ws-date to msg.
           write sysout-rec from WS-SYSOUT-REC.
           set r-idx to 1.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           Move rec-data(r-idx) to WS-OUTPUT-REC.
           WRITE OUT-REC FROM WS-OUTPUT-REC.
           move ws-output-rec to msg.
           write sysout-rec from WS-SYSOUT-REC.
           ADD +1 TO RECORDS-WRITTEN.
           set r-idx up by 1.
       100-EXIT.
           EXIT.

       200-CLEANUP.
           MOVE "Ending program: HOSPCRFL" to msg.
      *  Code the statement write the final output record
           WRITE SYSOUT-REC FROM WS-SYSOUT-REC .
           move RECORDS-WRITTEN to MSG.
           write sysout-rec from WS-SYSOUT-REC.
      *  Code the statement to close all files
           CLOSE OUTFILE, SYSOUT.
       200-EXIT.
           EXIT.