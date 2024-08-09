       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CADDDB2.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   THIS PROGRAM PERFORMS THE ADD RECORD FUNCTION.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   THIS PROGRAM IS WRITTEN FOR THE OS/VS COBOL COMPILER.       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RECORD-LENGTH              PIC S9(4) COMP  VALUE +80.
       01  MESSAGE-LENGTH             PIC S9(4) COMP.
       01  MESSAGE-AREA               PIC X(80).
       01  CA-COMMAREA.
           05  CA-COMMAREA-WRK                   PIC X.
           05  CA-COMMAREA-CA-IND                PIC X.
       01  MAP-MESSAGES.
           05  0910-INVALID-KEY-MSG.
               10  FILLER             PIC X(11) VALUE SPACES.
               10  FILLER             PIC X(29)
                   VALUE 'INVALID KEY PRESSED'.
           05  0930-DUPLICATE-RECORD-MSG.
               10  FILLER             PIC X(05) VALUE SPACES.
               10  FILLER             PIC X(35)
                   VALUE 'PERSON NUMBER ALREADY ASSIGNED'.
           05  RECORD-ADDED-MSG.
               10  FILLER             PIC X(06) VALUE SPACES.
               10  FILLER             PIC X(34)
                   VALUE 'RECORD HAS BEEN ADDED TO FILE'.
           05  HIGHLIGHT-ERROR-MSG.
               10  FILLER             PIC X(07) VALUE SPACES.
               10  FILLER             PIC X(33)
                   VALUE 'HIGHLIGHTED FIELDS IN ERROR'.
       01  DATE-TIME-WORK-AREAS.
           05  ABSOLUTE-TIME           PIC X(8).
           05  FORMATTED-DATE          PIC X(8).
           05  FORMATTED-TIME          PIC X(8).
       01  INPUT-EDIT-FLAG             PIC X    VALUE 'N'.
       01  INITIALIZATION-IMAGE        PIC X    VALUE LOW-VALUES.
       01  STATE-VALIDATION-AREA.
           05  STATE-CODE              PIC X(2).
           05  RESPONSE-CODE           PIC X.

      *    COPY BADDB2.
       01  BADD400I.
           05  FILLER                             PIC X(12).
           05  ADATEL                             PIC S9(4) COMP.
           05  ADATEF                             PIC X(01).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA                         PIC X(01).
           05  ADATEI                             PIC X(008).
           05  ATIMEL                             PIC S9(4) COMP.
           05  ATIMEF                             PIC X(01).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA                         PIC X(01).
           05  ATIMEI                             PIC X(008).
           05  APNUML                             PIC S9(4) COMP.
           05  APNUMF                             PIC X(01).
           05  FILLER REDEFINES APNUMF.
               10  APNUMA                         PIC X(01).
           05  APNUMI                             PIC X(010).
           05  AFNAMEL                            PIC S9(4) COMP.
           05  AFNAMEF                            PIC X(01).
           05  FILLER REDEFINES AFNAMEF.
               10  AFNAMEA                        PIC X(01).
           05  AFNAMEI                            PIC X(012).
           05  ALNAMEL                            PIC S9(4) COMP.
           05  ALNAMEF                            PIC X(01).
           05  FILLER REDEFINES ALNAMEF.
               10  ALNAMEA                        PIC X(01).
           05  ALNAMEI                            PIC X(015).
           05  ASTREETL                           PIC S9(4) COMP.
           05  ASTREETF                           PIC X(01).
           05  FILLER REDEFINES ASTREETF.
               10  ASTREETA                       PIC X(01).
           05  ASTREETI                           PIC X(016).
           05  ACITYL                             PIC S9(4) COMP.
           05  ACITYF                             PIC X(01).
           05  FILLER REDEFINES ACITYF.
               10  ACITYA                         PIC X(01).
           05  ACITYI                             PIC X(012).
           05  ASTATEL                            PIC S9(4) COMP.
           05  ASTATEF                            PIC X(01).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA                        PIC X(01).
           05  ASTATEI                            PIC X(002).
           05  ASALARYL                           PIC S9(4) COMP.
           05  ASALARYF                           PIC X(01).
           05  FILLER REDEFINES ASALARYF.
               10  ASALARYA                       PIC X(01).
           05  ASALARYI                           PIC 9(7).
           05  AINSTRL                            PIC S9(4) COMP.
           05  AINSTRF                            PIC X(01).
           05  FILLER REDEFINES AINSTRF.
               10  AINSTRA                        PIC X(01).
           05  AINSTRI                            PIC X(040).
           05  AMSGL                              PIC S9(4) COMP.
           05  AMSGF                              PIC X(01).
           05  FILLER REDEFINES AMSGF.
               10  AMSGA                          PIC X(01).
           05  AMSGI                              PIC X(040).
       01  BADD400O  REDEFINES BADD400I.
           05  FILLER                             PIC X(12).
           05  FILLER                             PIC X(03).
           05  ADATEO                             PIC X(008).
           05  FILLER                             PIC X(03).
           05  ATIMEO                             PIC X(008).
           05  FILLER                             PIC X(03).
           05  APNUMO                             PIC X(010).
           05  FILLER                             PIC X(03).
           05  AFNAMEO                            PIC X(012).
           05  FILLER                             PIC X(03).
           05  ALNAMEO                            PIC X(015).
           05  FILLER                             PIC X(03).
           05  ASTREETO                           PIC X(016).
           05  FILLER                             PIC X(03).
           05  ACITYO                             PIC X(012).
           05  FILLER                             PIC X(03).
           05  ASTATEO                            PIC X(002).
           05  FILLER                             PIC X(03).
           05  ASALARYO                           PIC 9(7).
           05  FILLER                             PIC X(03).
           05  AINSTRO                            PIC X(040).
           05  FILLER                             PIC X(03).
           05  AMSGO                              PIC X(040).

      *     COPY DFHBMSCA.
       01      DFHBMSCA.
         02    DFHBMPEM  PICTURE X   VALUE  IS  ' '.
         02    DFHBMASK  PICTURE X   VALUE  IS  '0'.
         02    DFHBMUNP  PICTURE X   VALUE  IS  ' '.
         02    DFHBMUNN  PICTURE X   VALUE  IS  '&'.
         02    DFHBMPRO  PICTURE X   VALUE  IS  '-'.
         02    DFHBMBRY  PICTURE X   VALUE  IS  'H'.
         02    DFHBMDAR  PICTURE X   VALUE  IS  '<'.
         02    DFHBMFSE  PICTURE X   VALUE  IS  'A'.
         02    DFHBMPRF  PICTURE X   VALUE  IS  '/'.
         02    DFHBMASF  PICTURE X   VALUE  IS  '1'.
         02    DFHBMASB  PICTURE X   VALUE  IS  '8'.
      *  02    DFHBMEOF  PICTURE X   VALUE  IS  ' '.
      *  02    DFHBMDET  PICTURE X   VALUE  IS  ' '.
         02    DFHBMPSO  PICTURE X   VALUE  IS  ' '.
         02    DFHBMPSI  PICTURE X   VALUE  IS  ' '.
      *  02    DFHSA     PICTURE X   VALUE  IS  ' '.
      *  02    DFHCOLOR  PICTURE X   VALUE  IS  ' '.
      *  02    DFHPS     PICTURE X   VALUE  IS  ' '.
      *  02    DFHHLT    PICTURE X   VALUE  IS  ' '.
         02    DFH3270   PICTURE X   VALUE  IS  '{'.
         02    DFHVAL    PICTURE X   VALUE  IS  'A'.
         02    DFHOUTLN  PICTURE X   VALUE  IS  'B'.

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

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.


           EXEC SQL DECLARE PERSONFL TABLE
           (
              PERSON_NUMBER                  CHAR (10)
                                             NOT NULL,
              PERSON_FIRST_NAME              CHAR (12)
                                             NOT NULL,
              PERSON_LAST_NAME               CHAR (15)
                                             NOT NULL,
              PERSON_STREET_ADDR             CHAR (16)
                                             NOT NULL,
              PERSON_CITY_ADDR               CHAR (12)
                                             NOT NULL,
              PERSON_STATE_ADDR              CHAR (2)
                                             NOT NULL,
              PERSON_SALARY                  SMALLINT
                                             NOT NULL
           )
           END-EXEC.


       01  DCL-PERSONFL.
           03 PERSON-NUMBER                  PIC X(10).
           03 PERSON-FIRST-NAME              PIC X(12).
           03 PERSON-LAST-NAME               PIC X(15).
           03 PERSON-STREET-ADDR             PIC X(16).
           03 PERSON-CITY-ADDR               PIC X(12).
           03 PERSON-STATE-ADDR              PIC X(2).
           03 PERSON-SALARY                  PIC S9(4) COMP.

       01  DCL-PATSEGM.
           03 PATNO                  PIC X(10).
           03 PATNO1                  PIC X(10).


       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  DFHCOMMAREA-WRK                   PIC X.
           05  DFHCOMMAREA-CA-IND                PIC X.
       PROCEDURE DIVISION.
       HOUSEKEEPING-ROUTINE.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   SET A GENERAL ERROR TRAP FOR EXCEPTIONAL CONDITIONS.        *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC CICS HANDLE CONDITION
                DUPREC(0930-DUPLICATE-RECORD-RECORD)
           END-EXEC.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   DETERMINE WHETHER TO SEND OR RECEIVE THE MAP.               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           IF  EIBCALEN EQUAL TO ZEROES
           THEN
      *        MOVE LOW-VALUES TO BADDB2O
               MOVE 'Y' TO CA-COMMAREA-CA-IND
               GO TO 0100-SEND-ADD-SCREEN
           ELSE
               MOVE DFHCOMMAREA TO CA-COMMAREA
               MOVE 'N' TO CA-COMMAREA-CA-IND.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   READ MAP INTO MAP COPYBOOK                                  *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           PERFORM 0300-RECEIVE-MAP THRU 0300-EXIT.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   EDIT SCREEN INPUT                                           *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           PERFORM 0500-EDIT-SCREEN THRU 0500-EXIT.

       0000-EXIT.
            EXIT.
       0100-SEND-ADD-SCREEN.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   OBTAIN AND FORMAT CURRENT DATE AND TIME.                    *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC CICS ASKTIME
                ABSTIME(ABSOLUTE-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                ABSTIME(ABSOLUTE-TIME)
                DATE(FORMATTED-DATE)
                DATESEP
                TIME(FORMATTED-TIME)
                TIMESEP
           END-EXEC.
           MOVE FORMATTED-TIME TO ATIMEO.
           MOVE FORMATTED-DATE TO ADATEO.
           MOVE -1 TO APNUML.
           MOVE +300 TO MESSAGE-LENGTH.

           IF CA-COMMAREA-CA-IND IS EQUAL TO 'Y'
           THEN
               EXEC CICS SEND
                   MAP('BADDB2')
                   FROM(BADD400O)
                   LENGTH(MESSAGE-LENGTH)
                   CURSOR
                   FREEKB
                   ERASE
               END-EXEC
           ELSE
               EXEC CICS SEND
                   MAP('BADDB2')
                   FROM(BADD400O)
                   LENGTH(MESSAGE-LENGTH)
                   CURSOR
                   FREEKB
                   DATAONLY
                   END-EXEC.

      *    MOVE 'N' TO CA-COMMAREA-CA-IND.

           EXEC CICS RETURN
                TRANSID('AXDB')
                COMMAREA(CA-COMMAREA)
                LENGTH(2)
           END-EXEC.
       0100-EXIT.
            EXIT.
       0300-RECEIVE-MAP.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   PREPARE TO INTERCEPT ATTENTION IDENTIFIER TO CONTROL        *
      *   PROCESSING.                                                 *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC CICS HANDLE AID
                PF3(0990-RETURN-TO-MAIN-MENU)
           END-EXEC.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   RECEIVE THE ADD SCREEN.  THE SET OPTION WILL ALLOW CICS TO  *
      *   ESTABLISH ADDRESSABILITY FOR THE PROGRAM TO ACCESS TO THE   *
      *   ACQUIRED AREA INTO WHICH THE MAP DATA WAS PLACED.           *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC CICS RECEIVE
                MAP('BADDB2')
                INTO(BADD400I)
           END-EXEC.


      *     EXEC DLI GU
      *        SEGMENT(PATIENT) INTO(PATSEGM) WHERE (PATNO=PATNO1);

       0300-EXIT.
            EXIT.
       0500-EDIT-SCREEN.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   AFTER RECEIVING THE MA, FIELDS MUST BE EXAMINED PRIOR TO    *
      *   CREATING A NEW RECORD FOR THE FILE.                         *
      *   EDIT CRITERIA:                                              *
      *     O PERSON NUMBER MUST BE PRESENT AND MUST BE 10 CHARACTERS *
      *     O FIRST NAME MUST BE PRESENT                              *
      *     O LAST NAME MUST BE PRESENT                               *
      *     O STREET MUST BE PRESENT                                  *
      *     O CITY MUST BE PRESENT                                    *
      *     O STATE MUST BE PRESENT AND MUST BE 2 CHARACTERS          *
      *       IF THESE CONDITIONS ARE MET THE VALUE IS PASSED TO      *
      *       A VALIDATION PROGRAM                                    *
      *     O SALARY MUST BE PRESENT AND GREATER THAN 0               *
      *                                                               *
      *   BEFORE EDITING THE INPUT, PRESERVE IT FOR DISPLAY IF AN     *
      *   EDIT FAILS FOR A FIELD.  DO THIS BY SETTING THE ATTRIBUTE   *
      *   FOR EACH FIELD INDICATING THAT THE FIELD HAS BEEN MODIFIED, *
      *   I.E., SET THE MDT ON.  THIS WILL AVOID THE USER HAVING TO   *
      *   RE-ENTER EVERY FIELD.                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           MOVE DFHBMFSE TO APNUMA, AFNAMEA, ALNAMEA, ASTREETA,
                            ACITYA, ASTATEA, ASALARYA.
           IF  APNUML NOT EQUAL TO 10
               THEN MOVE DFHBMBRY TO APNUMA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO APNUML.
           IF  AFNAMEL EQUAL TO 0
               THEN MOVE DFHBMBRY TO AFNAMEA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO AFNAMEL.
           IF  ALNAMEL EQUAL TO 0
               THEN MOVE DFHBMBRY TO ALNAMEA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ALNAMEL.
           IF  ASTREETL EQUAL TO 0
               THEN MOVE DFHBMBRY TO ASTREETA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ASTREETL.
           IF  ACITYL EQUAL TO 0
               THEN MOVE DFHBMBRY TO ACITYA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ACITYL.
      *    IF  ASTATEL NOT EQUAL TO 2
      *        THEN MOVE DFHBMBRY TO ASTATEA
      *             MOVE 'Y' TO INPUT-EDIT-FLAG
      *             MOVE -1 TO ASTATEL
      *    ELSE
      *        MOVE ASTATEI TO STATE-CODE
      *        EXEC CICS LINK
      *             PROGRAM('TSTATE')
      *             COMMAREA(STATE-VALIDATION-AREA)
      *             LENGTH(3)
      *        END-EXEC
      *        IF  RESPONSE-CODE EQUAL TO HIGH-VALUES
      *            THEN MOVE DFHBMBRY TO ASTATEA
      *                 MOVE 'Y' TO INPUT-EDIT-FLAG
      *                 MOVE -1 TO ASTATEL.
      *
           IF  ASALARYL EQUAL TO 0
               THEN MOVE DFHBMBRY TO ASALARYA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ASALARYL
           ELSE
           IF  ASALARYI NOT NUMERIC
               THEN MOVE DFHBMBRY TO ASALARYA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ASALARYL
           ELSE
           IF  ASALARYI NOT GREATER THAN 0
               THEN MOVE DFHBMBRY TO ASALARYA
                    MOVE 'Y' TO INPUT-EDIT-FLAG
                    MOVE -1 TO ASALARYL.
           IF  INPUT-EDIT-FLAG = 'Y'
               THEN MOVE HIGHLIGHT-ERROR-MSG TO AMSGO
                    MOVE DFHBMFSE TO APNUMA, AFNAMEA, ALNAMEA,
                                     ASTREETA, ACITYA, ASTATEA, ASALARYA
                    GO TO 0100-SEND-ADD-SCREEN.
           MOVE  APNUMI   TO PERSON-NUMBER.
           MOVE  AFNAMEI  TO PERSON-FIRST-NAME.
           MOVE  ALNAMEI  TO PERSON-LAST-NAME.
           MOVE  ASTREETI TO PERSON-STREET-ADDR.
           MOVE  ACITYI   TO PERSON-CITY-ADDR.
           MOVE  ASTATEI  TO PERSON-STATE-ADDR.
           MOVE  ASALARYI TO PERSON-SALARY.
           EXEC SQL
                INSERT INTO PERSONFL
                  (PERSON_NUMBER,
                   PERSON_FIRST_NAME,
                   PERSON_LAST_NAME,
                   PERSON_STREET_ADDR,
                   PERSON_CITY_ADDR,
                   PERSON_STATE_ADDR,
                   PERSON_SALARY)
                VALUES
                  (:PERSON-NUMBER,
                   :PERSON-FIRST-NAME,
                   :PERSON-LAST-NAME,
                   :PERSON-STREET-ADDR,
                   :PERSON-CITY-ADDR,
                   :PERSON-STATE-ADDR,
                   :PERSON-SALARY)
           END-EXEC.
           EVALUATE SQLCODE
           WHEN +0
               GO TO 0930-DUPLICATE-RECORD-RECORD
           WHEN -803
               GO TO 0930-DUPLICATE-RECORD-RECORD
           WHEN OTHER
               GO TO 0910-INVALID-KEY.
      *    EXEC CICS WRITE
      *         DATASET('XXXXXXXX')
      *         FROM(PERSON-MASTER-RECORD)
      *         RIDFLD(PERSON-NUMBER)
      *         LENGTH(RECORD-LENGTH)
      *    END-EXEC.
           MOVE RECORD-ADDED-MSG TO AMSGO.
           MOVE SPACES TO APNUMO, AFNAMEO, ALNAMEO, ASTREETO,
                          ACITYO, ASTATEO.
           MOVE ZEROES TO ASALARYO.
           MOVE -1 TO APNUML.
           GO TO 0100-SEND-ADD-SCREEN.
       0500-EXIT.
            EXIT.
       0900-ERROR-ROUTINE.
           EXEC CICS HANDLE CONDITION ERROR END-EXEC.
           MOVE 'PROGRAM TERMINATED: ENCOUNTERED ERROR' TO MESSAGE-AREA.
           MOVE +37 TO MESSAGE-LENGTH.
           EXEC CICS SEND
                FROM(MESSAGE-AREA)
                LENGTH(MESSAGE-LENGTH)
                ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
       0910-INVALID-KEY.
           MOVE 0910-INVALID-KEY-MSG TO AMSGO.
           MOVE -1 TO APNUML.
           IF  EIBAID = DFHCLEAR
               THEN MOVE DFHBMFSE TO APNUMA, AFNAMEA, ALNAMEA,
                                     ASTREETA, ACITYA, ASTATEA, ASALARYA
                MOVE 'Y' TO CA-COMMAREA-CA-IND
                    GO TO 0100-SEND-ADD-SCREEN
           ELSE
                MOVE 'N' TO CA-COMMAREA-CA-IND
                GO TO 0100-SEND-ADD-SCREEN.
       0910-EXIT.
            EXIT.
       0930-DUPLICATE-RECORD-RECORD.
           MOVE 0930-DUPLICATE-RECORD-MSG TO AMSGO.
           MOVE -1 TO APNUML.
           MOVE 'N' TO CA-COMMAREA-CA-IND.
           GO TO 0100-SEND-ADD-SCREEN.
       0930-EXIT.
            EXIT.
       0990-RETURN-TO-MAIN-MENU.
           EXEC CICS XCTL
                PROGRAM('CMENXDB')
           END-EXEC.
           GOBACK.
       0990-EXIT.
            EXIT.