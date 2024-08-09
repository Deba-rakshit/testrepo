       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CPAT400.
       AUTHOR.        .
       DATE-COMPILED. .

      ***************************************************************
      * PATIENT SUMMARY INFORMATION LOOK-UP ROUTINE                 *
      ***************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-CICS-WORK-VARIABLES.
           03  WS-RESP                     PIC S9(08)  COMP VALUE ZEROS.
           03  WS-LENGTH                   PIC S9(08)  COMP VALUE ZEROS.
           03  WS-COMMAREA                 PIC X(01)  VALUE SPACE.
           03  WS-LOGOFF-MESSAGE           PIC X(35)  VALUE
               '*** APPLICATION COMPLETED. BYE-BYE.'.
           03  WS-LOGOFF-MESSAGE-LEN       PIC S9(08)  COMP VALUE +35.
       01  WS-ERR-LINE.
           03  FILLER                      PIC X(23) VALUE
               '*** ABEND *** TRAN ID: '.
           03  WS-ERR-TRAN-ID              PIC X(04) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE
               ' PROGRAM: '.
           03  WS-ERR-PROG-ID              PIC X(08) VALUE SPACES.
           03  FILLER                      PIC X(16) VALUE
               ' LAST FUNCTION: '.
           03  WS-ERR-FUNCTION             PIC 9(04) VALUE ZEROS.
           03  FILLER                      PIC X(09) VALUE
               ' RESULT: '.
           03  WS-ERR-RESULT               PIC 9(04) VALUE ZEROS.
           03  FILLER                      PIC X(02) VALUE
               '  '.
       01  WS-ERR-LINE2                    PIC X(80) VALUE SPACES.

       01  WS-WORKING-VARIABLES.
           03  WS-SELECTION                PIC X(01) VALUE SPACES.
               88  WS-SELECTION-VALID      VALUE 'A', 'D', 'U', 'X'.
           03  WS-TIME                     PIC S9(08) COMP VALUE +0.

      *--- COPYLIB CONTAINING PFKEY DEFINITIONS
      *COPY DFHAID.
       01  DFHAID.
           02  DFHNULL         PIC X   VALUE IS ' '.
           02  DFHENTER        PIC X   VALUE IS "'".
           02  DFHCLEAR        PIC X   VALUE IS '_'.
           02  DFHPEN          PIC X   VALUE IS '='.
           02  DFHOPID         PIC X   VALUE IS 'W'.
           02  DFHMSRE         PIC X   VALUE IS 'X'.
           02  DFHSTRF         PIC X   VALUE IS ' '.
           02  DFHTRIG         PIC X   VALUE IS '"'.
           02  DFHPA1          PIC X   VALUE IS '%'.
           02  DFHPA2          PIC X   VALUE IS '>'.
           02  DFHPA3          PIC X   VALUE IS ','.
           02  DFHPF1          PIC X   VALUE IS '1'.
           02  DFHPF2          PIC X   VALUE IS '2'.
           02  DFHPF3          PIC X   VALUE IS '3'.
           02  DFHPF4          PIC X   VALUE IS '4'.
           02  DFHPF5          PIC X   VALUE IS '5'.
           02  DFHPF6          PIC X   VALUE IS '6'.
           02  DFHPF7          PIC X   VALUE IS '7'.
           02  DFHPF8          PIC X   VALUE IS '8'.
           02  DFHPF9          PIC X   VALUE IS '9'.
           02  DFHPF10         PIC X   VALUE IS ':'.
           02  DFHPF11         PIC X   VALUE IS '#'.
           02  DFHPF12         PIC X   VALUE IS '@'.
           02  DFHPF13         PIC X   VALUE IS 'A'.
           02  DFHPF14         PIC X   VALUE IS 'B'.
           02  DFHPF15         PIC X   VALUE IS 'C'.
           02  DFHPF16         PIC X   VALUE IS 'D'.
           02  DFHPF17         PIC X   VALUE IS 'E'.
           02  DFHPF18         PIC X   VALUE IS 'F'.
           02  DFHPF19         PIC X   VALUE IS 'G'.
           02  DFHPF20         PIC X   VALUE IS 'H'.
           02  DFHPF21         PIC X   VALUE IS 'I'.
      *
      * NOTE - The cent sign is not valid in ASCII which is the format of
      *        this source code.  X'4A' is the correct hex value, and is
      *        the value returned by CICSVS86
      *
           02  DFHPF22         PIC X   VALUE IS X'4A'.
           02  DFHPF23         PIC X   VALUE IS '.'.
           02  DFHPF24         PIC X   VALUE IS '<'.

      *--- COPY BMS COPYLIBS HERE.
      *COPY BPAT400.
       01  BPAT400I.
           05  FILLER                             PIC X(12).
           05  DATEL                             PIC S9(4) COMP.
           05  DATEF                             PIC X(01).
           05  FILLER REDEFINES DATEF.
               10  DATEA                         PIC X(01).
           05  DATEI                             PIC X(008).
           05  TIMEL                             PIC S9(4) COMP.
           05  TIMEF                             PIC X(01).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA                         PIC X(01).
           05  TIMEI                             PIC X(008).
           05  PNUML                             PIC S9(4) COMP.
           05  PNUMF                             PIC X(01).
           05  FILLER REDEFINES PNUMF.
               10  PNUMA                         PIC X(01).
           05  PNUMI                             PIC X(06).
           05  WARDL                            PIC S9(4) COMP.
           05  WARDF                            PIC X(01).
           05  FILLER REDEFINES WARDF.
               10  WARDA                        PIC X(01).
           05  WARDI                            PIC X(04).
           05  BEDL                            PIC S9(4) COMP.
           05  BEDF                            PIC X(01).
           05  FILLER REDEFINES BEDF.
               10  BEDA                        PIC X(01).
           05  BEDI                            PIC X(04).
           05  ADMITL                           PIC S9(4) COMP.
           05  ADMITF                           PIC X(01).
           05  FILLER REDEFINES ADMITF.
               10  ADMITA                       PIC X(01).
           05  ADMITI                           PIC X(10).
           05  DIAGL                             PIC S9(4) COMP.
           05  DIAGF                             PIC X(01).
           05  FILLER REDEFINES DIAGF.
               10  DIAGA                         PIC X(01).
           05  DIAGI                             PIC X(05).
           05  AMOUNTL                           PIC S9(4) COMP.
           05  AMOUNTF                           PIC X(01).
           05  FILLER REDEFINES AMOUNTF.
               10  AMOUNTA                       PIC X(01).
           05  AMOUNTI                           PIC 9(9).
           05  INSTRL                            PIC S9(4) COMP.
           05  INSTRF                            PIC X(01).
           05  FILLER REDEFINES INSTRF.
               10  INSTRA                        PIC X(01).
           05  INSTRI                            PIC X(040).
           05  MSGL                              PIC S9(4) COMP.
           05  MSGF                              PIC X(01).
           05  FILLER REDEFINES MSGF.
               10  MSGA                          PIC X(01).
           05  MSGI                              PIC X(040).
       01  BPAT400O  REDEFINES BPAT400I.
           05  FILLER                             PIC X(12).
           05  FILLER                             PIC X(03).
           05  DATEO                             PIC X(008).
           05  FILLER                             PIC X(03).
           05  TIMEO                             PIC X(008).
           05  FILLER                             PIC X(03).
           05  PNUMO                             PIC X(06).
           05  FILLER                             PIC X(03).
           05  WARDO                            PIC X(04).
           05  FILLER                             PIC X(03).
           05  BEDO                             PIC X(06).
           05  FILLER                             PIC X(03).
           05  ADMITO                              PIC X(06).
           05  FILLER                             PIC X(03).
           05  DIAGO                             PIC X(06).
           05  FILLER                             PIC X(03).
           05  AMOUNTO                           PIC 9(9).
           05  FILLER                             PIC X(03).
           05  INSTRO                            PIC X(040).
           05  FILLER                             PIC X(03).
           05  MSGO                              PIC X(040).

       77  PERSONNL    PIC s9(3).
       77  PERSONNi    PIC s9(3).
      *--- COPY DATASET COPYLIB HERE.
       COPY PATMSTR.
       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(01).

       PROCEDURE DIVISION.

       0000-INITIAL-LOOP.

              EXEC CICS HANDLE CONDITION
                        ERROR(9999-ABEND-ROUTINE)
              END-EXEC.
      *--- IF CLEAR KEY IS PRESSED, SEND LOGOFF MESSAGE AND RETURN TO
      *---     CICS.
           IF (EIBAID = DFHCLEAR)
              EXEC CICS SEND TEXT
                        FROM(WS-LOGOFF-MESSAGE)
                        LENGTH(WS-LOGOFF-MESSAGE-LEN)
                        ERASE
              END-EXEC
              EXEC CICS RETURN
              END-EXEC
           END-IF.

      *--- IF PF3 KEY IS PRESSED, TRANSFER CONTROL BACK TO MAIN MENU
      *---     PROGRAM.
           IF (EIBAID = DFHPF3)
              EXEC CICS XCTL
                        PROGRAM('CMEN400')
                        RESP(WS-RESP)
              END-EXEC
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'ERROR: XCTL TO MENU400 FAILED'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0000-EXIT
              END-IF
           END-IF.

      *--- BASIC PSUDOCONVERSATIONAL LOOP
      *---    CHECK COMMAREA LENGTH TO SEE IF THIS IS FIRST TIME IN
      *---    IF THIS IS FIRST TIME IN DO SEND LOOP
      *---    OTHERWISE, DO RECEIVE LOOP
           IF (EIBCALEN > 0)
              MOVE DFHCOMMAREA          TO WS-COMMAREA
              PERFORM 0100-RECEIVE-LOOP THRU 0100-EXIT
           ELSE
              PERFORM 0200-SEND-LOOP    THRU 0200-EXIT
           END-IF.

      *--- RETURN TO CICS WITH TRANSACTION ID AND COMMAREA
           EXEC CICS RETURN
                     TRANSID('A400')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(1)
           END-EXEC.

       0000-EXIT.
            EXIT.

       0100-RECEIVE-LOOP.

           EXEC CICS RECEIVE MAP('BPAT400')
                     MAPSET('BPAT400')
                     INTO(BPAT400I)
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'ERROR: NO DATA WAS ENTERED OR UPDATED'
                    TO MSGO
               PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
               GO TO 0100-EXIT
               MOVE HIGH-VALUES TO MSGI
           END-IF.

      *--- APPLICATION PROCESSING GOES HERE

           IF (PERSONNL > 0)
              NEXT SENTENCE
           ELSE
              MOVE LOW-VALUES                TO MSGO
              MOVE 'INVALID PERSON NUMBER. PLEASE TRY AGAIN.'
                      TO MSGO
              PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
              GO TO 0100-EXIT
           END-IF.

           INITIALIZE  PATIENT-MASTER-REC.
           MOVE PNUMI  TO PATIENT-ID.

           EXEC CICS READ
                     FILE('PATMSTR')
                     INTO(PAT)
                     LENGTH(2964)
                     RIDFLD(PATIENT-MASTER-REC)
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(DUPREC) OR
              WS-RESP = DFHRESP(DUPKEY)
              MOVE LOW-VALUES                TO MSGO
              MOVE 'RECORD ALREADY EXISTS. PLEASE TRY AGAIN.'
                      TO MSGO
              PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
              GO TO 0100-EXIT
           ELSE
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0000: DATASET WRITE; NOT NORMAL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

           MOVE PRIMARY-STAY-WARD-NBR TO WARDO.
           MOVE BED-IDENTITY-PRIMARY  TO BEDO.
           MOVE DIAGNOSTIC-CODE-PRIMARY TO DIAGO.
           MOVE DATE-ADMIT TO ADMITO.
           MOVE PATIENT-TOT-AMT TO AMOUNTO.

           MOVE '***  PATIENT FOUND. '
                   TO MSGO.
           PERFORM 0875-SEND-APPL-SCREEN      THRU 0875-EXIT.

       0100-EXIT.
            EXIT.

       0200-SEND-LOOP.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     LENGTH(1000)
                     MAP('BPAT400')
                     MAPSET('BPAT400')
                     FROM(BPAT400O)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0200: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0200-EXIT.
            EXIT.

       0850-SEND-ERROR-SCREEN.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     LENGTH(1000)
                     MAP('BPAT400')
                     MAPSET('BPAT400')
                     FROM(BPAT400O)
                     FREEKB
                     ALARM
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0850: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0850-EXIT.
            EXIT.

       0875-SEND-APPL-SCREEN.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     LENGTH(1000)
                     MAP('BPAT400')
                     MAPSET('BPAT400')
                     FROM(BPAT400O)
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0875: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0875-EXIT.
            EXIT.

       9999-ABEND-ROUTINE.
           MOVE EIBTRNID       TO WS-ERR-TRAN-ID.
           MOVE 'ADD '         TO WS-ERR-PROG-ID.
           MOVE EIBFN          TO WS-ERR-FUNCTION.
           MOVE EIBRESP        TO WS-ERR-RESULT.
           EXEC CICS SEND TEXT
                     FROM(WS-ERR-LINE)
                     LENGTH(80)
                     ERASE
           END-EXEC.
           EXEC CICS SEND TEXT
                     FROM(WS-ERR-LINE2)
                     LENGTH(80)
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.