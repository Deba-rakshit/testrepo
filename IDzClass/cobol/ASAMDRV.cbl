      * ---------------------------------------------------
      *    PROGRAM   ASAMDRV
      *
      *    SAMPLE PROGRAM TO TEST CALLS TO HEXCHARS (ASAM1) SUBROUTINE
      * ---------------------------------------------------
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ASAMDRV.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.

       01  PROGRAM-WORK-FIELDS.
           05  WS-START                    PIC X(29)
                             VALUE '<WORKING-STORAGE-STARTS-HERE>'.
           05  PARM1-CHAR-STRING-LEN       PIC S9(8)  COMP SYNC
                                           VALUE +10.
           05  PARM2-CHAR-STRING           PIC X(10)
                                           VALUE '1A2B3C4D5E'.
           05  PARM3-TOP-HEX-LINE          PIC X(10).
           05  PARM4-BOT-HEX-LINE          PIC X(10).
           05  PGM-STATUS                  PIC X(40).

       01  PROGRAM-CONSTANTS.
           05  ASAM1-PGM            PIC X(8)  VALUE 'ASAM1   '.
      *
       PROCEDURE DIVISION.

       PROGRAM-CONTROL.
           MOVE SPACES TO PARM3-TOP-HEX-LINE.
           MOVE SPACES TO PARM4-BOT-HEX-LINE.
           MOVE 'CALLING ' TO PGM-STATUS.
           CALL ASAM1-PGM.
      *
           MOVE 'RETURNED FROM SUBPGM  ' TO PGM-STATUS.
           MOVE 'TERMINATING           ' TO PGM-STATUS.
           GOBACK.