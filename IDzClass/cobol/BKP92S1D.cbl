       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,CICS
      ******************************************************************
      * PRODUCT: IBM Rational Developer for System z V7.6
      * COMPONENT: Enterprise Service Tools
      * PROGRAM: Web Services for CICS TS Converter Driver
      * RUNTIME: Web Services for CICS
      * REQUIRED COMPILER: IBM Enterprise COBOL 4.1
      * XMLPARSE OPTION: COMPAT
      * XML2LS XML CCSID: 1140
      * LANGUAGE STRUCTURE CCSID: 1140
      * LS2XML XML CCSID: 1140
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1D'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-7 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020005300650072'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0076006900630065002000430061006C006C002000460061'.
       2 PIC N(4) USAGE NATIONAL
           VALUE NX'0069006C00650064'.
       1 CONVERTER-ERROR-8 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020004D00650073'.
       2 PIC N(11) USAGE NATIONAL
           VALUE NX'00730061006700650020004E0075006D006200650072'.
       1 CONVERTER-ERROR-9 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0058004D004C00200043006F006E00760065007200740065'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00720020004900730020005400650072006D0069006E0061'.
       2 PIC N(7) USAGE NATIONAL
           VALUE NX'00740069006E0067002E002E002E'.
      * **************************************************************
      * Vendor Program Container Definitions
      * **************************************************************
       1 DFH-BODY-CONTAINER PIC X(16) VALUE 'DFH-BODY'.
       1 DFH-DATA-CONTAINER PIC X(16) VALUE 'DFH-DATA'.
       LOCAL-STORAGE SECTION.
      * **************************************************************
      * Storage Items For LE Error Handling
      * **************************************************************
       1 CONVERTER-RETURN-CODE PIC S9(9) BINARY.
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 OPTIONAL-FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 ERROR-RESPONSE.
       2 ERROR-OCCURRED PIC X.
       2 ERROR-MESSAGE-NUMBER PIC 9(9).
       2 ERROR-REASON-LENGTH PIC 9(9) BINARY.
       2 ERROR-REASON PIC X(512).
      * **************************************************************
      * Converter Metadata Variables
      * **************************************************************
       1 XML2LS-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC S9(9) COMP.
       1 XML2LS-XML-CCSID PIC S9(9) COMP.
       1 HOST-LANG-CCSID PIC S9(9) COMP.
       1 LS2XML-XML-CCSID PIC S9(9) COMP.
      * **************************************************************
      * SOAP Pipeline Work Variables
      * **************************************************************
       1 SOAP-PIPELINE-WORK-VARIABLES.
       2 NEXT-CONTAINER PIC X(16).
       2 COMMAND-RESP PIC 9(9) BINARY.
       2 COMMAND-RESP2 PIC 9(9) BINARY.
       2 CONTAINER-BROWSE-TOKEN POINTER.
       2 DFH-BODY-PTR POINTER.
       2 DFH-BODY-LEN PIC 9(9) BINARY.
       2 DFH-DATA-PTR POINTER.
       2 DFH-DATA-LEN PIC 9(9) BINARY.
       2 WORK-AREA-PTR POINTER.
       2 WORK-AREA-LEN PIC 9(9) BINARY.
       1 WORK-AREA-VAL PIC X VALUE X'00'.
       LINKAGE SECTION.
       1 DFH-BODY PIC X.
       1 DFH-DATA PIC X.
      * **************************************************************
      * Business Program Binary Interfaces
      * **************************************************************
       01 X0000004B
           .
       05 FILLER-LINK
           .
       10 LINK-COMM
           .
       20 USER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-OPEN
           VALUE
           'O'
           .
       88 ACCOUNT-VIEW
           VALUE
           'V'
           .
       88 ACCOUNT-CHANGE
           VALUE
           'U'
           .
       88 ACCOUNT-CLOSE
           VALUE
           'C'
           .
       20 SERVER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-CREATE
           VALUE
           'C'
           .
       88 ACCOUNT-READ
           VALUE
           'R'
           .
       88 ACCOUNT-UPDATE
           VALUE
           'U'
           .
       88 ACCOUNT-DELETE
           VALUE
           'D'
           .
       88 ACCOUNT-BROWSE
           VALUE
           'B'
           .
       20 KEYNUM
           PICTURE 9(6)
           USAGE DISPLAY
           .
       20 TEAMID
           PICTURE X(2)
           USAGE DISPLAY
           .
       20 RCODE
           PICTURE 9(9)
           USAGE COMP
           .
       20 ERR-RESP
           PICTURE 9(2)
           USAGE DISPLAY
           .
       05 FILEA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       05 COMM-AREA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       PROCEDURE DIVISION.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Initialize Storage and Browse Channel
      * --------------------------------------------------------------
           PERFORM REGISTER-EXCEPTION-HANDLER
           INITIALIZE SOAP-PIPELINE-WORK-VARIABLES
           PERFORM GET-CONVERTER-METADATA
           PERFORM BROWSE-VENDOR-CHANNEL
      * --------------------------------------------------------------
      * Branch To Processing Logic For Container
      * --------------------------------------------------------------
           EVALUATE NEXT-CONTAINER
             WHEN DFH-BODY-CONTAINER
               PERFORM PROCESS-DFH-BODY
             WHEN DFH-DATA-CONTAINER
               PERFORM PROCESS-DFH-DATA
             WHEN OTHER
               EXEC CICS ABEND
               END-EXEC
           END-EVALUATE
      * --------------------------------------------------------------
      * Finished
      * --------------------------------------------------------------
           PERFORM FREE-WORK-AREA
           PERFORM UNREGISTER-EXCEPTION-HANDLER
           EXEC CICS RETURN
           END-EXEC
           .
       BROWSE-VENDOR-CHANNEL.
           EXEC CICS STARTBROWSE CONTAINER
             BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
           END-EXEC
           PERFORM TEST AFTER UNTIL
             NEXT-CONTAINER EQUAL DFH-BODY-CONTAINER OR
             NEXT-CONTAINER EQUAL DFH-DATA-CONTAINER OR
             COMMAND-RESP2 NOT EQUAL ZERO
             EXEC CICS GETNEXT CONTAINER (NEXT-CONTAINER)
               BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
               RESP(COMMAND-RESP)
               RESP2(COMMAND-RESP2)
             END-EXEC
           END-PERFORM
           .
       PROCESS-DFH-BODY.
           PERFORM RECEIVE-DFH-BODY
           PERFORM ALLOCATE-DFH-DATA-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM INVOKE-XML2LS-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM SEND-DFH-DATA
           END-IF
           .
       PROCESS-DFH-DATA.
           PERFORM RECEIVE-DFH-DATA
           PERFORM ALLOCATE-DFH-BODY-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM INVOKE-LS2XML-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM SEND-DFH-BODY
           END-IF
           .
       RECEIVE-DFH-BODY.
           MOVE 'DFHREQUEST' TO DFH-BODY-CONTAINER
           EXEC CICS GET CONTAINER(DFH-BODY-CONTAINER)
             SET(DFH-BODY-PTR)
             FLENGTH(DFH-BODY-LEN)
             INTOCCSID(1140)
           END-EXEC
           SET ADDRESS OF DFH-BODY
             TO DFH-BODY-PTR
           .
       SEND-DFH-BODY.
           EXEC CICS PUT CONTAINER(DFH-BODY-CONTAINER)
             FROM(DFH-BODY)
             FLENGTH(DFH-BODY-LEN)
             FROMCCSID(1140)
           END-EXEC
           .
       RECEIVE-DFH-DATA.
           EXEC CICS GET CONTAINER(DFH-DATA-CONTAINER)
             SET(DFH-DATA-PTR)
             FLENGTH(DFH-DATA-LEN)
           END-EXEC
           SET ADDRESS OF X0000004B
             TO DFH-DATA-PTR
           .
       SEND-DFH-DATA.
           COMPUTE DFH-DATA-LEN =
             LENGTH OF X0000004B
           EXEC CICS PUT CONTAINER(DFH-DATA-CONTAINER)
             FROM(X0000004B)
             FLENGTH(DFH-DATA-LEN)
           END-EXEC
           .
       ALLOCATE-DFH-BODY-WORK-AREA.
           MOVE LS2XML-XML-BUFFER-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR)
             FLENGTH(WORK-AREA-LEN)
           END-EXEC
           SET ADDRESS OF DFH-BODY
             TO WORK-AREA-PTR
           .
       ALLOCATE-DFH-DATA-WORK-AREA.
           MOVE XML2LS-LANG-BUFFER-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR)
             FLENGTH(WORK-AREA-LEN)
             INITIMG(WORK-AREA-VAL)
           END-EXEC
           SET ADDRESS OF X0000004B
             TO WORK-AREA-PTR
           .
       FREE-WORK-AREA.
           IF WORK-AREA-PTR NOT EQUAL NULL
             EXEC CICS FREEMAIN
               DATAPOINTER(WORK-AREA-PTR)
             END-EXEC
           END-IF
           .
       GET-CONVERTER-METADATA.
           CALL 'BKP92S1X' USING
             XML2LS-LANG-BUFFER-LENGTH LS2XML-LANG-BUFFER-LENGTH
             LS2XML-XML-BUFFER-LENGTH XML2LS-XML-CCSID
             HOST-LANG-CCSID LS2XML-XML-CCSID
             OMITTED OMITTED
           .
       SEND-SOAP-FAULT.
           EXEC CICS SOAPFAULT CREATE CLIENT
             FAULTSTRING(ERROR-REASON)
             FAULTSTRLEN(ERROR-REASON-LENGTH)
           END-EXEC
           .
       INVOKE-XML2LS-CONVERSION.
           CALL 'BKP92S1I'
             USING
               X0000004B
               DFH-BODY-LEN
               DFH-BODY
               OMITTED
      *   OPTIONAL-FEEDBACK-CODE
             RETURNING
               CONVERTER-RETURN-CODE
           .
       INVOKE-LS2XML-CONVERSION.
           CALL 'BKP92S1O'
             USING
               X0000004B
               DFH-BODY-LEN
               DFH-BODY
               OMITTED
      *   OPTIONAL-FEEDBACK-CODE
             RETURNING
               CONVERTER-RETURN-CODE
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE TO ENTRY 'BKP92S1F'
           SET TOKEN TO ADDRESS OF ERROR-RESPONSE
           CALL 'CEEHDLR' USING ROUTINE TOKEN FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING ROUTINE FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
             DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-7)
             DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-8) ' '
               FACILITY OF FEEDBACK-CODE
               MSG-NO OF FEEDBACK-CODE
             DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-9)
             STOP RUN
           END-IF
           .
       END PROGRAM 'BKP92S1D'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Exception Handler
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,NOCICS
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1F'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-5 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F002000720065007400720069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500760065002000740068006500200074006500780074'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0020006F0066002000610020004C0061006E006700750061'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0067006500200045006E007600690072006F006E006D0065'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006E0074002000720075006E00740069006D00650020006D'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500730073006100670065002E00200020004300680065'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0063006B0020007400680061007400200074006800650020'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E0074002000720075006E'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00740069006D00650020006D006500730073006100670065'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0020006D006F00640075006C006500200066006F00720020'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'0066006100630069006C006900740079002000490052005A'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00200069007300200069006E007300740061006C006C0065'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006400200069006E002000440046004800520050004C0020'.
       2 PIC N(11) USAGE NATIONAL
           VALUE NX'006F007200200053005400450050004C00490042002E'.
       LOCAL-STORAGE SECTION.
       1 MSG-PTR PIC S9(9) COMP.
       1 MSG-PART PIC X(80).
       1 MSG-OFFSET PIC 9(9) COMP.
       1 MSG-PART-LENGTH PIC 9(9) COMP.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 TOKEN POINTER.
       1 RESULT PIC S9(9) BINARY.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION PIC X(12).
       1 ERROR-CDATA-PTR PIC X(512).
       1 ERROR-RESPONSE.
       2 ERROR-OCCURRED PIC X.
       2 ERROR-MESSAGE-NUMBER PIC 9(9).
       2 ERROR-REASON-LENGTH PIC 9(9) BINARY.
       2 ERROR-REASON PIC X(512).
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Storage For Saving Exception Details
      * --------------------------------------------------------------
           SET ADDRESS OF ERROR-RESPONSE TO TOKEN
      * --------------------------------------------------------------
      * Get Exception Message
      * --------------------------------------------------------------
           PERFORM GET-MESSAGE-TEXT
      * --------------------------------------------------------------
      * Display Exception Message
      * --------------------------------------------------------------
           PERFORM DISPLAY-MESSAGE-TEXT
      * --------------------------------------------------------------
      * Recover From Exception To Produce XML Response
      * --------------------------------------------------------------
           MOVE 'Y' TO ERROR-OCCURRED
           SET RESUME TO TRUE
      * --------------------------------------------------------------
      * Finished
      * --------------------------------------------------------------
           GOBACK
           .
       GET-MESSAGE-TEXT.
           MOVE 0 TO MSG-PTR
           MOVE 512 TO ERROR-REASON-LENGTH
           MOVE SPACES TO MSG-PART ERROR-REASON
           CALL 'CEEMGET' USING
             CURRENT-CONDITION MSG-PART
             MSG-PTR FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE AND
              NOT CEE0E7 OF FEEDBACK-CODE
            COMPUTE ERROR-REASON-LENGTH =
              FUNCTION LENGTH(FUNCTION DISPLAY-OF(CONVERTER-ERROR-5))
            MOVE FUNCTION DISPLAY-OF(CONVERTER-ERROR-5)
              TO ERROR-REASON
           ELSE
            IF NOT CEE0E7 OF FEEDBACK-CODE
             PERFORM COMPUTE-PART-LENGTH
             MOVE MSG-PART-LENGTH TO ERROR-REASON-LENGTH
             MOVE MSG-PART TO ERROR-REASON
            ELSE
             MOVE MSG-PART TO ERROR-REASON
             MOVE MSG-PTR TO MSG-OFFSET
             PERFORM UNTIL MSG-PTR = 0
              MOVE SPACES TO MSG-PART
              CALL 'CEEMGET' USING
               CURRENT-CONDITION MSG-PART
               MSG-PTR FEEDBACK-CODE
              IF NOT CEE000 OF FEEDBACK-CODE AND
                 NOT CEE0E7 OF FEEDBACK-CODE
               DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-5)
              END-IF
              IF MSG-PTR NOT = 0
               MOVE MSG-PART TO
                ERROR-REASON(MSG-OFFSET + 1:MSG-PTR)
               ADD MSG-PTR TO MSG-OFFSET
              ELSE
               PERFORM COMPUTE-PART-LENGTH
               MOVE MSG-PART TO
                ERROR-REASON(MSG-OFFSET + 1:MSG-PART-LENGTH)
               ADD MSG-PART-LENGTH TO MSG-OFFSET
              END-IF
             END-PERFORM
            END-IF
            MOVE MSG-NO OF CURRENT-CONDITION TO
             ERROR-MESSAGE-NUMBER
            MOVE MSG-OFFSET TO ERROR-REASON-LENGTH
           END-IF
           .
       COMPUTE-PART-LENGTH.
           PERFORM VARYING MSG-PART-LENGTH FROM 80 BY -1
            UNTIL MSG-PART(MSG-PART-LENGTH:1) NOT = SPACE
            OR MSG-PART-LENGTH < 1
           END-PERFORM
           .
       DISPLAY-MESSAGE-TEXT.
           DISPLAY ERROR-REASON(1:ERROR-REASON-LENGTH)
           .
       END PROGRAM 'BKP92S1F'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Compiled XML Conversion Properties API
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1X'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 XML2LS-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-XML-CCSID PIC 9(9) COMP.
       1 HOST-LANG-CCSID PIC 9(9) COMP.
       1 LS2XML-XML-CCSID PIC 9(9) COMP.
       1 XML2LS-PROPERTIES PIC X.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER-LENGTH
           LS2XML-LANG-BUFFER-LENGTH
           LS2XML-XML-BUFFER-LENGTH
           XML2LS-XML-CCSID
           HOST-LANG-CCSID
           LS2XML-XML-CCSID
           XML2LS-PROPERTIES
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF XML2LS-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 176
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 176
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 834
              TO LS2XML-XML-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF XML2LS-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO XML2LS-XML-CCSID
           END-IF
           IF ADDRESS OF HOST-LANG-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO HOST-LANG-CCSID
           END-IF
           IF ADDRESS OF LS2XML-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO LS2XML-XML-CCSID
           END-IF
           IF ADDRESS OF XML2LS-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO XML2LS-PROPERTIES
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'BKP92S1X'.
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,NOCICS,XMLPARSE(COMPAT)
      ******************************************************************
      * PRODUCT: IBM Rational Developer for System z V7.6
      * COMPONENT: Enterprise Service Tools
      * PROGRAM: XML to Language Structure Converter
      * RUNTIME: Web Services for CICS
      * REQUIRED COMPILER: IBM Enterprise COBOL 4.1
      * XMLPARSE OPTION: COMPAT
      * XML2LS XML CCSID: 1140
      * LANGUAGE STRUCTURE CCSID: 1140
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1I'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 XML-ROOT-ELEMENT.
       2 PIC X(11) USAGE DISPLAY
           VALUE 'DFHCOMMAREA'.
       1 XPATH-HASH-ENTRIES.
       2 X00000019.
       3 PIC X(55) VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE '-'.
       2 X0000001A.
       3 PIC X(48) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/filler_link/link_comm/user_request/t'.
       3 PIC X(5) USAGE DISPLAY
           VALUE 'ext()'.
       3 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 2.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'X'.
       2 X0000001B.
       3 PIC X(47) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/filler_link/link_comm/teamid/text()'.
       3 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'X'.
       2 X0000001C.
       3 PIC X(48) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/filler_link/link_comm/server_request'.
       3 PIC X(7) USAGE DISPLAY
           VALUE '/text()'.
       3 PIC 9(4) USAGE COMP-5 VALUE 3.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'X'.
       2 X0000001D.
       3 PIC X(11) USAGE DISPLAY
           VALUE 'DFHCOMMAREA'.
       3 PIC X(44) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC X VALUE 'T'.
       2 X0000001E.
       3 PIC X(47) USAGE DISPLAY
           VALUE 'DFHCOMMAREA/filler_link/link_comm/keynum/text()'.
       3 PIC X(8) USAGE DISPLAY
           VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5 VALUE 4.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5 VALUE 1.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE 'N'.
       2 X0000001F.
       3 PIC X(55) VALUE SPACES.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC 9(4) USAGE COMP-5.
       3 PIC X VALUE '-'.
       1 XPATH-HASH-TABLE REDEFINES XPATH-HASH-ENTRIES.
       2 XPATH-HASH-TABLE-ENTRIES OCCURS 7 TIMES.
       3 XPATH-TEXT PIC X(55).
       3 XPATH-CON-MAP-NDX PIC 9(4) USAGE COMP-5.
       3 XPATH-NUM-TRG-NDX PIC 9(4) USAGE COMP-5.
       3 XPATH-NUM-RES-NDX PIC 9(4) USAGE COMP-5.
       3 XPATH-STRUCT-NDX PIC 9(4) USAGE COMP-5.
       3 XPATH-CON-TYPE PIC X.
       88 TYPE-REPEATING-GROUP VALUE 'R'.
       88 TYPE-GROUP VALUE 'G'.
       88 TYPE-NUMERIC VALUE 'N'.
       88 TYPE-ALPHANUMERIC VALUE 'X'.
       88 TYPE-PURE-DBCS VALUE 'D'.
       88 TYPE-UNICODE VALUE 'U'.
       88 TYPE-FLOAT VALUE 'F'.
       88 TYPE-DOUBLE VALUE 'B'.
       88 TYPE-LANG-STRUCT VALUE 'T'.
       88 TYPE-NONE VALUE '-'.
       1 NUMERIC-SOURCES.
       2 PIC X(1) USAGE DISPLAY
           VALUE '9'.
       2 PIC X(5) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(2) USAGE DISPLAY
           VALUE '99'.
       2 PIC X(4) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(3) USAGE DISPLAY
           VALUE '999'.
       2 PIC X(3) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(4) USAGE DISPLAY
           VALUE '9999'.
       2 PIC X(2) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(5) USAGE DISPLAY
           VALUE '99999'.
       2 PIC X(1) USAGE DISPLAY
           VALUE SPACES.
       2 PIC X(6) USAGE DISPLAY
           VALUE '999999'.
       1 NUMERIC-SOURCES-ARRAY REDEFINES NUMERIC-SOURCES.
       2 NPSA PIC X(6)
           OCCURS 6 TIMES
           ASCENDING NPSA INDEXED BY NPSA-NDX.
       1 NUMERIC-MOVE-RULES.
       2 X00000020.
       3 PIC S9(4) COMP VALUE 1.
       2 X00000021.
       3 PIC S9(4) COMP VALUE 2.
       2 X00000022.
       3 PIC S9(4) COMP VALUE 3.
       2 X00000023.
       3 PIC S9(4) COMP VALUE 4.
       2 X00000024.
       3 PIC S9(4) COMP VALUE 5.
       2 X00000025.
       3 PIC S9(4) COMP VALUE 6.
       1 NMAR-TABLE REDEFINES NUMERIC-MOVE-RULES.
       2 OCCURS 6 TIMES.
       3 NMAR-ENTRY PIC S9(4) COMP OCCURS 1 TIMES.
       1 ERROR-MESSAGES.
       2 CONVERTER-ERROR-3 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F002000720065006700690073'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'007400650072002000610020004C0061006E006700750061'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0067006500200045006E007600690072006F006E006D0065'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006E007400200065007800630065007000740069006F006E'.
       3 PIC N(9) USAGE NATIONAL
           VALUE NX'002000680061006E0064006C00650072002E'.
       2 CONVERTER-ERROR-4 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F00200075006E007200650067'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690073007400650072002000610020004C0061006E0067'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'007500610067006500200045006E007600690072006F006E'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006D0065006E007400200065007800630065007000740069'.
       3 PIC N(11) USAGE NATIONAL
           VALUE NX'006F006E002000680061006E0064006C00650072002E'.
       2 CONVERTER-ERROR-7 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020005300650072'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0076006900630065002000430061006C006C002000460061'.
       3 PIC N(4) USAGE NATIONAL
           VALUE NX'0069006C00650064'.
       2 CONVERTER-ERROR-8 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020004D00650073'.
       3 PIC N(11) USAGE NATIONAL
           VALUE NX'00730061006700650020004E0075006D006200650072'.
       2 CONVERTER-ERROR-9 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0058004D004C00200043006F006E00760065007200740065'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00720020004900730020005400650072006D0069006E0061'.
       3 PIC N(7) USAGE NATIONAL
           VALUE NX'00740069006E0067002E002E002E'.
       1 XPATH-DELIM PIC X VALUE '/'.
       1 QNAME-DELIM PIC X VALUE ':'.
       1 XPATH-TEXT-FUNC PIC X(6)
           VALUE 'text()'.
       1 XPATH-DELIM-ATT PIC X VALUE '@'.
       LOCAL-STORAGE SECTION.
       1 NUMERIC-PICTURE-STORAGE.
       2 X00000026 PIC X(6).
       2 X00000027 PIC X(1).
       2 X00000028 PIC X(2).
       2 X00000029 PIC X(3).
       2 X0000002A PIC X(4).
       2 X0000002B PIC X(5).
       2 X0000002C PIC X(6).
       1 NUMERIC-CHARACTER-STORAGE.
       2 X0000002D PIC X(1).
       2 X0000002E PIC X(2).
       2 X0000002F PIC X(3).
       2 X00000030 PIC X(4).
       2 X00000031 PIC X(5).
       2 X00000032 PIC X(6).
       1 CEECMI-STRING.
       2 CEECMI-DATA-LEN PIC S9(4) COMP.
       2 CEECMI-DATA   PIC X(254).
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 ARRAY-SUBSCRIPTS.
       2 X00000011 PIC 9(9) COMP VALUE 0.
       1 HASH-TOKEN PIC X(56).
       1 REDEFINES HASH-TOKEN.
       2 OCCURS 14 TIMES INDEXED BY HASH-DIGIT-NDX.
       3 HASH-DIGIT PIC S9(9) COMP.
       1 HASH-VALUE PIC S9(9) COMP-5.
       1 LANG-STRUCT-HASH-VALUE PIC S9(9) COMP-5 VALUE 0.
       1 HASH-DISCARD PIC S9(9) COMP-5.
       1 INTEGER-PART PIC S9(20) COMP-3.
       1 FRACTION-PART PIC SV9(20) COMP-3.
       1 LOCAL-NAME-LEN PIC 9(9) COMP VALUE 0.
       1 LOCAL-NAME PIC X(640).
       1 CHAR-CON-LEN PIC 9(9) COMP VALUE 0.
       1 CHAR-CON PIC X(256).
       1 CHAR-CON-NDX PIC 9(9) COMP.
       1 CHAR-CON-LIMIT PIC 9(9) COMP.
       1 CMP-TMPA PIC S9(9) COMP.
       1 CMP-TMPB PIC S9(9) COMP.
       1 NPSAN PIC 9(9) COMP.
       1 ERROR-CODE PIC S9(9) COMP.
       1 XPATH-MAPPINGS-FOUND PIC 9(9) COMP VALUE 0.
       1 SEV PIC S9(4) COMP.
       1 MSGNO PIC S9(4) COMP.
       1 CASE PIC S9(4) COMP.
       1 SEV2 PIC S9(4) COMP.
       1 CNTRL PIC S9(4) COMP.
       1 FACID PIC X(3) DISPLAY.
       1 ISINFO PIC S9(9) COMP.
       1 QDATA PIC S9(9) COMP.
       1 INSERTNO PIC S9(9) COMP.
       1 EEC PIC 9(9) DISPLAY.
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 XML2LS-LANG-BUFFER-POINTER POINTER.
       1 XML2LS-LANG-BUFFER-ADDRESS
           REDEFINES XML2LS-LANG-BUFFER-POINTER PIC 9(9) COMP-5.
       1 XML2LS-CONVERTED-LENGTH PIC 9(9) COMP VALUE 0.
       1 XPATH PIC X(550).
       1 XPOS PIC 9(9) COMP VALUE 1.
       1 XSTACK-DEPTH PIC 9(9) COMP VALUE 0.
       1 CEECMI-XPATH PIC X(254).
       1 CEECMI-XPATH-LEN PIC 9(9) COMP VALUE 0.
       1 LANG-STRUCT-NAME PIC X(30).
       1 LANG-STRUCT-NAME-LENGTH PIC 9(4) COMP.
       1 XML-NAT-CHAR PIC N USAGE NATIONAL.
       1 XML-ROOT-ELEMENT-STATE PIC X VALUE X'00'.
       88 XML-ROOT-NOT-FOUND VALUE X'00'.
       88 XML-ROOT-FOUND VALUE X'FF'.
       1 PROCESSING-PROCEDURE-STATE PIC X VALUE X'00'.
       88 PROCESS-NEXT-XML-EVENT VALUE X'00'.
       88 REPEAT-CURRENT-XML-EVENT VALUE X'FF'.
       1 XML-ATTRIBUTE-STATE PIC X VALUE X'00'.
       88 XML-ATTRIBUTE-NOT-MAPPED VALUE X'00'.
       88 XML-ATTRIBUTE-MAPPED VALUE X'FF'.
       1 XML-ELEMENT-STATE PIC X VALUE X'00'.
       88 XML-ELEMENT-NOT-MAPPED VALUE X'00'.
       88 XML-ELEMENT-MAPPED VALUE X'FF'.
       1 XML-ROOT-NMSP-STATE PIC X VALUE X'00'.
       88 XML-ROOT-NMSP-NOT-VALID VALUE X'00'.
       88 XML-ROOT-NMSP-VALID VALUE X'FF'.
       88 XML-ROOT-NMSP-FOUND VALUE X'FE'.
       1 P-XML-EVENT PIC X VALUE X'00'.
       88 P-EMPTY-EVENT VALUE X'00'.
       88 P-START-OF-DOCUMENT VALUE X'FF'.
       88 P-START-OF-ELEMENT VALUE X'FE'.
       88 P-ATTRIBUTE-NAME VALUE X'FD'.
       88 P-ATTRIBUTE-CHARACTERS VALUE X'FC'.
       88 P-ATTRIBUTE-CHARACTER VALUE X'FB'.
       88 P-ATTRIBUTE-NATIONAL-CHARACTER VALUE X'FA'.
       88 P-CONTENT-CHARACTERS VALUE X'F9'.
       88 P-CONTENT-CHARACTER VALUE X'F8'.
       88 P-CONTENT-NATIONAL-CHARACTER VALUE X'F7'.
       88 P-END-OF-ELEMENT VALUE X'F6'.
       88 P-PARSE-EXCEPTION VALUE X'F5'.
       88 P-END-OF-DOCUMENT VALUE X'F4'.
       1 N-XML-EVENT PIC X VALUE X'00'.
       88 N-EMPTY-EVENT VALUE X'00'.
       88 N-START-OF-DOCUMENT VALUE X'FF'.
       88 N-START-OF-ELEMENT VALUE X'FE'.
       88 N-ATTRIBUTE-NAME VALUE X'FD'.
       88 N-ATTRIBUTE-CHARACTERS VALUE X'FC'.
       88 N-ATTRIBUTE-CHARACTER VALUE X'FB'.
       88 N-ATTRIBUTE-NATIONAL-CHARACTER VALUE X'FA'.
       88 N-CONTENT-CHARACTERS VALUE X'F9'.
       88 N-CONTENT-CHARACTER VALUE X'F8'.
       88 N-CONTENT-NATIONAL-CHARACTER VALUE X'F7'.
       88 N-END-OF-ELEMENT VALUE X'F6'.
       88 N-PARSE-EXCEPTION VALUE X'F5'.
       88 N-END-OF-DOCUMENT VALUE X'F4'.
       LINKAGE SECTION.
       1 X00000013 PIC 9.
       1 X00000014 PIC 9(2).
       1 X00000015 PIC 9(3).
       1 X00000016 PIC 9(4).
       1 X00000017 PIC 9(5).
       1 X00000018 PIC 9(6).
       1 X00000012 DISPLAY PIC 9(6).
       01 DFHCOMMAREA
           .
       05 FILLER-LINK
           .
       10 LINK-COMM
           .
       20 USER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-OPEN
           VALUE
           'O'
           .
       88 ACCOUNT-VIEW
           VALUE
           'V'
           .
       88 ACCOUNT-CHANGE
           VALUE
           'U'
           .
       88 ACCOUNT-CLOSE
           VALUE
           'C'
           .
       20 SERVER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-CREATE
           VALUE
           'C'
           .
       88 ACCOUNT-READ
           VALUE
           'R'
           .
       88 ACCOUNT-UPDATE
           VALUE
           'U'
           .
       88 ACCOUNT-DELETE
           VALUE
           'D'
           .
       88 ACCOUNT-BROWSE
           VALUE
           'B'
           .
       20 KEYNUM
           PICTURE 9(6)
           USAGE DISPLAY
           .
       20 TEAMID
           PICTURE X(2)
           USAGE DISPLAY
           .
       20 RCODE
           PICTURE 9(9)
           USAGE COMP
           .
       20 ERR-RESP
           PICTURE 9(2)
           USAGE DISPLAY
           .
       05 FILEA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       05 COMM-AREA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       1 XML2LS-LANG-BUFFER PIC X(176).
       1 XML2LS-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-XML-BUFFER PIC X(33554436).
       1 OPTIONAL-FEEDBACK-CODE PIC X(12).
       1 CONVERTER-RETURN-CODE PIC 9(9) COMP.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER
           XML2LS-XML-BUFFER-LENGTH
           XML2LS-XML-BUFFER
           OPTIONAL-FEEDBACK-CODE
           RETURNING
           CONVERTER-RETURN-CODE.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
       MAINLINE SECTION.
           MOVE 'N'
             TO NUMVAL-ERROR UNICODE-ERROR OTHER-ERROR
           PERFORM CHECK-PARAMETERS
           PERFORM REGISTER-EXCEPTION-HANDLER
           PERFORM CHECK-INPUT-MESSAGE-LENGTH
           SET XML2LS-LANG-BUFFER-POINTER
            TO ADDRESS OF XML2LS-LANG-BUFFER
           XML PARSE XML2LS-XML-BUFFER (1:XML2LS-XML-BUFFER-LENGTH)
            PROCESSING PROCEDURE XML2LS-XML-PARSE-HANDLER
            THRU GENERAL-LOGIC-EXIT
            ON EXCEPTION
             PERFORM UNREGISTER-EXCEPTION-HANDLER
             PERFORM SIGNAL-CONDITION
            NOT ON EXCEPTION
             PERFORM UNREGISTER-EXCEPTION-HANDLER
             MOVE ZERO TO CONVERTER-RETURN-CODE
           END-XML
           GOBACK
           .
       CHECK-PARAMETERS.
           IF ADDRESS OF XML2LS-LANG-BUFFER EQUAL NULL AND
              ADDRESS OF XML2LS-XML-BUFFER-LENGTH NOT EQUAL NULL
            CALL 'BKP92S1J'
                 USING XML2LS-XML-BUFFER-LENGTH OMITTED
            GOBACK
           END-IF
           IF ADDRESS OF XML2LS-LANG-BUFFER EQUAL NULL OR
              ADDRESS OF XML2LS-XML-BUFFER-LENGTH EQUAL NULL OR
              ADDRESS OF XML2LS-XML-BUFFER EQUAL NULL
            MOVE 294 TO MSGNO
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       CHECK-INPUT-MESSAGE-LENGTH.
           IF XML2LS-XML-BUFFER-LENGTH > 33554436
            MOVE 285 TO MSGNO
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       XML2LS-XML-PARSE-HANDLER.
           EVALUATE XML-EVENT
            WHEN 'START-OF-ELEMENT'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-START-OF-ELEMENT TO TRUE
             END-IF
             SET N-START-OF-ELEMENT TO TRUE
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM PUSH-ELEMENT
             SET XML-ELEMENT-NOT-MAPPED TO TRUE
             IF XSTACK-DEPTH > 0 AND
                (XPOS - 2) <= 55
              PERFORM HASH-XPATH
              IF XPATH-TEXT (HASH-VALUE) EQUAL
                 XPATH(2:XPOS - 2)
               ADD 1 TO XPATH-MAPPINGS-FOUND
               SET XML-ELEMENT-MAPPED TO TRUE
               IF TYPE-REPEATING-GROUP (HASH-VALUE) OR
                  TYPE-LANG-STRUCT (HASH-VALUE)
                GO TO SELECT-CONTENT-MAPPING
               END-IF
              ELSE
               COMPUTE CMP-TMPA = (XPOS - 2) + 7
               IF CMP-TMPA <= 55
                MOVE XPATH-DELIM
                  TO XPATH (XPOS:1)
                ADD 1 TO XPOS
                MOVE XPATH-TEXT-FUNC
                  TO XPATH (XPOS:6)
                ADD 6 TO XPOS
                PERFORM HASH-XPATH
                IF XPATH-TEXT (HASH-VALUE) EQUAL
                   XPATH(2:XPOS - 2)
                 SET XML-ELEMENT-MAPPED TO TRUE
                END-IF
                SUBTRACT 7 FROM XPOS
               END-IF
              END-IF
             END-IF
            WHEN 'ATTRIBUTE-NAME'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-ATTRIBUTE-NAME TO TRUE
             END-IF
             SET N-ATTRIBUTE-NAME TO TRUE
             IF XML-ROOT-NOT-FOUND
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM PUSH-ATTRIBUTE
             SET XML-ATTRIBUTE-NOT-MAPPED TO TRUE
             IF XSTACK-DEPTH > 0 AND
                (XPOS - 2) <= 55
              PERFORM HASH-XPATH
              IF XPATH-TEXT (HASH-VALUE) EQUAL
                 XPATH (2:XPOS - 2)
               SET XML-ATTRIBUTE-MAPPED TO TRUE
               ADD 1 TO XPATH-MAPPINGS-FOUND
              END-IF
             END-IF
             PERFORM POP-ATTRIBUTE
            WHEN 'ATTRIBUTE-CHARACTERS'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-ATTRIBUTE-CHARACTERS TO TRUE
             END-IF
             SET N-ATTRIBUTE-CHARACTERS TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ATTRIBUTE-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM BUFFER-CHARACTER-CONTENT
            WHEN 'ATTRIBUTE-CHARACTER'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-ATTRIBUTE-CHARACTER TO TRUE
             END-IF
             SET N-ATTRIBUTE-CHARACTER TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ATTRIBUTE-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM BUFFER-CHARACTER-CONTENT
            WHEN 'ATTRIBUTE-NATIONAL-CHARACTER'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-ATTRIBUTE-NATIONAL-CHARACTER TO TRUE
             END-IF
             SET N-ATTRIBUTE-NATIONAL-CHARACTER TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ATTRIBUTE-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM BUFFER-NATIONAL-CHARACTER
            WHEN 'CONTENT-CHARACTERS'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-CONTENT-CHARACTERS TO TRUE
             END-IF
             SET N-CONTENT-CHARACTERS TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ELEMENT-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM BUFFER-CHARACTER-CONTENT
            WHEN 'CONTENT-CHARACTER'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-CONTENT-CHARACTER TO TRUE
             END-IF
             SET N-CONTENT-CHARACTER TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ELEMENT-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM BUFFER-CHARACTER-CONTENT
            WHEN 'CONTENT-NATIONAL-CHARACTER'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-CONTENT-NATIONAL-CHARACTER TO TRUE
             END-IF
             SET N-CONTENT-NATIONAL-CHARACTER TO TRUE
             IF XML-ROOT-NOT-FOUND OR
                XML-ELEMENT-NOT-MAPPED
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             PERFORM CHECK-CONVERT-ATTRIBUTE
             PERFORM BUFFER-NATIONAL-CHARACTER
            WHEN 'END-OF-ELEMENT'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-END-OF-ELEMENT TO TRUE
             END-IF
             SET N-END-OF-ELEMENT TO TRUE
             PERFORM CHECK-CONVERT-ATTRIBUTE
             IF XSTACK-DEPTH > 0
              IF LANG-STRUCT-HASH-VALUE > 0 AND
                 XPATH-TEXT (LANG-STRUCT-HASH-VALUE) EQUAL
                 XPATH (2:XPOS - 2)
               EVALUATE XPATH-STRUCT-NDX
                (LANG-STRUCT-HASH-VALUE)
      * 01 DFHCOMMAREA.
                WHEN 1
                 COMPUTE CMP-TMPA =
                  LENGTH OF DFHCOMMAREA
                 ADD CMP-TMPA
                  TO XML2LS-LANG-BUFFER-ADDRESS
                     XML2LS-CONVERTED-LENGTH
               END-EVALUATE
               INITIALIZE LANG-STRUCT-HASH-VALUE
               PERFORM POP-ELEMENT
               GO TO GENERAL-LOGIC-EXIT
              ELSE
               IF XML-ROOT-FOUND
                IF P-CONTENT-CHARACTERS OR
                   P-CONTENT-CHARACTER OR
                   P-CONTENT-NATIONAL-CHARACTER OR
                   P-START-OF-ELEMENT
                 COMPUTE CMP-TMPA = (XPOS - 2) + 7
                 IF CMP-TMPA <= 55
                  MOVE XPATH-DELIM
                    TO XPATH (XPOS:1)
                  ADD 1 TO XPOS
                  MOVE XPATH-TEXT-FUNC
                    TO XPATH (XPOS:6)
                  ADD 6 TO XPOS
                  PERFORM HASH-XPATH
                  IF XPATH-TEXT (HASH-VALUE) EQUAL
                     XPATH (2:XPOS - 2)
                   SUBTRACT 7 FROM XPOS
                   PERFORM POP-ELEMENT
                   GO TO SELECT-CONTENT-MAPPING
                  END-IF
                  SUBTRACT 7 FROM XPOS
                 END-IF
                END-IF
               END-IF
              END-IF
              PERFORM POP-ELEMENT
             END-IF
            WHEN 'START-OF-DOCUMENT'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-START-OF-DOCUMENT TO TRUE
             END-IF
             SET N-START-OF-DOCUMENT TO TRUE
             CALL 'CEE3SRP' USING RECOVERY-POINT FEEDBACK-CODE
             SERVICE LABEL
             IF NUMVAL-ERROR = 'Y'
              MOVE 284 TO MSGNO
              MOVE -1 TO XML-CODE
             END-IF
             IF UNICODE-ERROR = 'Y'
              MOVE 288 TO MSGNO
              MOVE -1 TO XML-CODE
             END-IF
             IF OTHER-ERROR = 'Y'
              MOVE -1 TO XML-CODE
             END-IF
            WHEN 'END-OF-DOCUMENT'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-END-OF-DOCUMENT TO TRUE
             END-IF
             SET N-END-OF-DOCUMENT TO TRUE
             IF XPATH-MAPPINGS-FOUND = 0
              MOVE 282 TO MSGNO
              MOVE -1 TO XML-CODE
              GO TO GENERAL-LOGIC-EXIT
             END-IF
             IF X00000011 < 1
              MOVE 299 TO MSGNO
              MOVE -1 TO XML-CODE
              GO TO GENERAL-LOGIC-EXIT
             END-IF
            WHEN 'EXCEPTION'
             IF NOT P-EMPTY-EVENT
              MOVE N-XML-EVENT TO P-XML-EVENT
             ELSE
              SET P-PARSE-EXCEPTION TO TRUE
             END-IF
             SET N-PARSE-EXCEPTION TO TRUE
             IF XML-CODE >= 50 AND XML-CODE <= 99
              MOVE 0 TO XML-CODE
             ELSE
              MOVE 280 TO MSGNO
              MOVE -1 TO XML-CODE
             END-IF
           END-EVALUATE
           GO TO GENERAL-LOGIC-EXIT
           .
       HASH-XPATH.
           MOVE ALL X'00' TO HASH-TOKEN
           MOVE XPATH(2:XPOS - 2) TO HASH-TOKEN
                (56 - (XPOS - 3):XPOS - 2)
           MOVE 0 TO HASH-VALUE
           SET HASH-DIGIT-NDX TO 1
           PERFORM 14 TIMES
            ADD HASH-DIGIT (HASH-DIGIT-NDX) TO
                HASH-VALUE
            SET HASH-DIGIT-NDX UP BY 1
           END-PERFORM
           DIVIDE HASH-VALUE BY 7 GIVING HASH-DISCARD
            REMAINDER HASH-VALUE
           END-DIVIDE
           IF HASH-VALUE LESS THAN ZERO
            ADD 7 TO HASH-VALUE
           ELSE
            ADD 1 TO HASH-VALUE
           END-IF
           .
       CHECK-CONVERT-ATTRIBUTE.
           IF XML-ATTRIBUTE-MAPPED
            IF P-ATTRIBUTE-CHARACTERS OR
               P-ATTRIBUTE-CHARACTER  OR
               P-ATTRIBUTE-NATIONAL-CHARACTER
             SET REPEAT-CURRENT-XML-EVENT TO TRUE
             GO TO SELECT-CONTENT-MAPPING
            END-IF
           END-IF
           .
       BUFFER-CHARACTER-CONTENT.
           COMPUTE CMP-TMPB = FUNCTION LENGTH (XML-TEXT)
           COMPUTE CMP-TMPA = CHAR-CON-LEN + CMP-TMPB
           IF CMP-TMPA <= 256
            MOVE XML-TEXT
             TO CHAR-CON (CHAR-CON-NDX:CMP-TMPB)
            ADD CMP-TMPB TO CHAR-CON-LEN CHAR-CON-NDX
           ELSE
            MOVE CMP-TMPA TO CHAR-CON-LEN
            GO TO CHARACTER-BUFFER-OVERFLOW
           END-IF
           .
       BUFFER-NATIONAL-CHARACTER.
           COMPUTE CMP-TMPB = FUNCTION LENGTH (XML-NTEXT)
           IF CMP-TMPB > 1
            MOVE SPACE TO XML-NAT-CHAR
           ELSE
            MOVE XML-NTEXT TO XML-NAT-CHAR
           END-IF
           COMPUTE CMP-TMPA = CHAR-CON-LEN + 1
           IF CMP-TMPA <= 256
            COMPUTE CMP-TMPB = FUNCTION LENGTH (
            FUNCTION DISPLAY-OF (XML-NAT-CHAR))
            IF CMP-TMPB > 1
             MOVE SPACE
               TO CHAR-CON (CHAR-CON-NDX:1)
            ELSE
             MOVE FUNCTION DISPLAY-OF (XML-NAT-CHAR)
               TO CHAR-CON (CHAR-CON-NDX:1)
            END-IF
            ADD 1 TO CHAR-CON-LEN CHAR-CON-NDX
           ELSE
            MOVE CMP-TMPA TO CHAR-CON-LEN
            GO TO CHARACTER-BUFFER-OVERFLOW
           END-IF
           .
       PUSH-ELEMENT.
           MOVE 0 TO CHAR-CON-LEN
           MOVE 1 TO CHAR-CON-NDX
           PERFORM PARSE-XML-NAME
           IF XML-ROOT-NOT-FOUND AND
              LOCAL-NAME (1:LOCAL-NAME-LEN) EQUAL
              XML-ROOT-ELEMENT
            SET XML-ROOT-FOUND TO TRUE
           END-IF
           IF XML-ROOT-FOUND
            COMPUTE CMP-TMPA = XPOS + (LOCAL-NAME-LEN + 1)
            IF CMP-TMPA <= 550
             MOVE XPATH-DELIM
               TO XPATH (XPOS:1)
             MOVE LOCAL-NAME (1:LOCAL-NAME-LEN)
               TO XPATH (XPOS + 1:LOCAL-NAME-LEN)
             COMPUTE XPOS = XPOS + (LOCAL-NAME-LEN + 1)
            ELSE
             COMPUTE LOCAL-NAME-LEN
              = FUNCTION LENGTH (XML-TEXT)
             MOVE XML-TEXT TO LOCAL-NAME
             MOVE 291 TO MSGNO
             MOVE -1 TO XML-CODE
            END-IF
            ADD 1 TO XSTACK-DEPTH
            PERFORM STORE-CEECMI-XPATH
           END-IF
           .
       POP-ELEMENT.
           IF XSTACK-DEPTH > 0
            PERFORM PARSE-XML-NAME
            COMPUTE XPOS = XPOS - (LOCAL-NAME-LEN + 1)
            SUBTRACT 1 FROM XSTACK-DEPTH
           END-IF
           .
       PUSH-ATTRIBUTE.
           MOVE 0 TO CHAR-CON-LEN
           MOVE 1 TO CHAR-CON-NDX
           PERFORM PARSE-XML-NAME
           COMPUTE CMP-TMPA = XPOS + (LOCAL-NAME-LEN + 2)
           IF CMP-TMPA <= 550
            MOVE XPATH-DELIM
              TO XPATH (XPOS:1)
            ADD 1 TO XPOS
            MOVE XPATH-DELIM-ATT
              TO XPATH (XPOS:1)
            ADD 1 TO XPOS
            MOVE LOCAL-NAME (1:LOCAL-NAME-LEN)
              TO XPATH (XPOS:LOCAL-NAME-LEN)
            ADD LOCAL-NAME-LEN TO XPOS
           ELSE
            COMPUTE LOCAL-NAME-LEN
             = FUNCTION LENGTH (XML-TEXT)
            MOVE XML-TEXT TO LOCAL-NAME
            MOVE 291 TO MSGNO
            MOVE -1 TO XML-CODE
           END-IF
           PERFORM STORE-CEECMI-XPATH
           ADD 1 TO XSTACK-DEPTH
           .
       POP-ATTRIBUTE.
           IF XSTACK-DEPTH > 0
            COMPUTE XPOS = XPOS - (LOCAL-NAME-LEN + 2)
            SUBTRACT 1 FROM XSTACK-DEPTH
           END-IF
           .
       STORE-CEECMI-XPATH.
           INITIALIZE CMP-TMPA
           IF XPOS > 1
            COMPUTE CMP-TMPA = XPOS - 1
           END-IF
           IF CMP-TMPA > 0
            IF CMP-TMPA <= 254
             MOVE CMP-TMPA TO CEECMI-XPATH-LEN
             MOVE XPATH (1:CMP-TMPA)
               TO CEECMI-XPATH
            ELSE
             MOVE 254 TO CEECMI-XPATH-LEN
             COMPUTE CMP-TMPB = (CMP-TMPA - 254) + 1
             MOVE XPATH (CMP-TMPB:254)
               TO CEECMI-XPATH
            MOVE '...' TO CEECMI-XPATH (1:3)
            END-IF
           ELSE
            MOVE 0 TO CEECMI-XPATH-LEN
           END-IF
           .
       PARSE-XML-NAME.
           COMPUTE LOCAL-NAME-LEN
            = FUNCTION LENGTH (XML-TEXT)
           MOVE 1 TO CMP-TMPB
           PERFORM VARYING CMP-TMPA FROM 1 BY 1
            UNTIL CMP-TMPA > LOCAL-NAME-LEN
            IF XML-TEXT (CMP-TMPA:1) = QNAME-DELIM
             COMPUTE CMP-TMPB = CMP-TMPA + 1
             MOVE LOCAL-NAME-LEN TO CMP-TMPA
            END-IF
           END-PERFORM
           IF CMP-TMPB > 1
            COMPUTE LOCAL-NAME-LEN
             = (LOCAL-NAME-LEN - CMP-TMPB) + 1
            IF LOCAL-NAME-LEN <= 14
             MOVE XML-TEXT (CMP-TMPB:LOCAL-NAME-LEN)
               TO LOCAL-NAME (1:LOCAL-NAME-LEN)
            ELSE
             MOVE 0 TO LOCAL-NAME-LEN
            END-IF
           ELSE
            IF LOCAL-NAME-LEN <= 14
             MOVE XML-TEXT (1:LOCAL-NAME-LEN)
               TO LOCAL-NAME (1:LOCAL-NAME-LEN)
            ELSE
             MOVE 0 TO LOCAL-NAME-LEN
            END-IF
           END-IF
           IF LOCAL-NAME-LEN = 0
            MOVE 1 TO LOCAL-NAME-LEN
            MOVE '0' TO LOCAL-NAME (1:LOCAL-NAME-LEN)
           END-IF
           .
       SELECT-CONTENT-MAPPING.
           GO TO
            X00000033
            X00000034
            X00000035
            X00000036
            X00000037
           DEPENDING ON XPATH-CON-MAP-NDX (HASH-VALUE)
           GO TO GENERAL-LOGIC-EXIT
           .
       CONTENT-PROCESSING SECTION.
       X00000033.
           ADD 1 TO X00000011
           IF X00000011 > 1
            MOVE 300 TO MSGNO
            MOVE -1 TO XML-CODE
            GO TO GENERAL-LOGIC-EXIT
           END-IF
           SET ADDRESS OF DFHCOMMAREA
            TO XML2LS-LANG-BUFFER-POINTER
           MOVE 'DFHCOMMAREA'
             TO LANG-STRUCT-NAME
           MOVE 11
             TO LANG-STRUCT-NAME-LENGTH
           MOVE HASH-VALUE
             TO LANG-STRUCT-HASH-VALUE
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000034.
           IF CHAR-CON-LEN = 0
            MOVE SPACES TO
             USER-REQUEST
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           IF CHAR-CON-LEN <= 1
            MOVE CHAR-CON (1:CHAR-CON-LEN) TO
             USER-REQUEST
              OF LINK-COMM
              OF FILLER-LINK
              OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           MOVE 1 TO CHAR-CON-LIMIT
           GO TO CHARACTER-CONTENT-OVERFLOW
           .
       X00000035.
           IF CHAR-CON-LEN = 0
            MOVE SPACES TO
             SERVER-REQUEST
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           IF CHAR-CON-LEN <= 1
            MOVE CHAR-CON (1:CHAR-CON-LEN) TO
             SERVER-REQUEST
              OF LINK-COMM
              OF FILLER-LINK
              OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           MOVE 1 TO CHAR-CON-LIMIT
           GO TO CHARACTER-CONTENT-OVERFLOW
           .
       X00000036.
           IF CHAR-CON-LEN = 0
            MOVE ZEROS TO
             KEYNUM
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           SET ADDRESS OF X00000012 TO ADDRESS OF
            KEYNUM
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
           GO TO
            X00000038
            X00000039
            X0000003A
            X0000003B
            X0000003C
            X0000003D
           DEPENDING ON CHAR-CON-LEN
           GO TO NUMERIC-RESCUE
           .
       X00000037.
           IF CHAR-CON-LEN = 0
            MOVE SPACES TO
             TEAMID
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           IF CHAR-CON-LEN <= 2
            MOVE CHAR-CON (1:CHAR-CON-LEN) TO
             TEAMID
              OF LINK-COMM
              OF FILLER-LINK
              OF DFHCOMMAREA
            GO TO CONTENT-CONVERTED-EXIT
           END-IF
           MOVE 2 TO CHAR-CON-LIMIT
           GO TO CHARACTER-CONTENT-OVERFLOW
           .
       CHARACTER-CONTENT-OVERFLOW.
           MOVE 283 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       CHARACTER-BUFFER-OVERFLOW.
           MOVE 286 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       COMPUTE-LOOKUP-SOURCE SECTION.
       X00000038.
           MOVE CHAR-CON (1:1) TO X0000002D X00000027
           INSPECT X00000027 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X00000027 TO X00000026
           GO TO LOOKUP-SOURCE
           .
       X00000039.
           MOVE CHAR-CON (1:2) TO X0000002E X00000028
           INSPECT X00000028 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X00000028 TO X00000026
           GO TO LOOKUP-SOURCE
           .
       X0000003A.
           MOVE CHAR-CON (1:3) TO X0000002F X00000029
           INSPECT X00000029 REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X00000029 TO X00000026
           GO TO LOOKUP-SOURCE
           .
       X0000003B.
           MOVE CHAR-CON (1:4) TO X00000030 X0000002A
           INSPECT X0000002A REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X0000002A TO X00000026
           GO TO LOOKUP-SOURCE
           .
       X0000003C.
           MOVE CHAR-CON (1:5) TO X00000031 X0000002B
           INSPECT X0000002B REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X0000002B TO X00000026
           GO TO LOOKUP-SOURCE
           .
       X0000003D.
           MOVE CHAR-CON (1:6) TO X00000032 X0000002C
           INSPECT X0000002C REPLACING ALL
            '0' BY '9' '1' BY '9' '2' BY '9' '3' BY '9' '4' BY '9'
            '5' BY '9' '6' BY '9' '7' BY '9' '8' BY '9' ' ' BY '?'
           MOVE X0000002C TO X00000026
           GO TO LOOKUP-SOURCE
           .
       LOOKUP-SOURCE.
           SEARCH ALL NPSA
            AT END
             GO TO NUMERIC-RESCUE
            WHEN NPSA (NPSA-NDX) = X00000026
             SET NPSAN TO NPSA-NDX
           END-SEARCH
           GO TO
            X0000003E
            X0000003F
            X00000040
            X00000041
            X00000042
            X00000043
           DEPENDING ON NPSAN
           GO TO NUMERIC-RESCUE
           .
       SET-NUMERIC-SOURCE SECTION.
       X0000003E.
           SET ADDRESS OF X00000013
            TO ADDRESS OF X0000002D
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X0000003F.
           SET ADDRESS OF X00000014
            TO ADDRESS OF X0000002E
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000040.
           SET ADDRESS OF X00000015
            TO ADDRESS OF X0000002F
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000041.
           SET ADDRESS OF X00000016
            TO ADDRESS OF X00000030
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000042.
           SET ADDRESS OF X00000017
            TO ADDRESS OF X00000031
           GO TO COMPLETE-NUMERIC-MOVE
           .
       X00000043.
           SET ADDRESS OF X00000018
            TO ADDRESS OF X00000032
           GO TO COMPLETE-NUMERIC-MOVE
           .
       MOVE-NUMERIC SECTION.
       COMPLETE-NUMERIC-MOVE.
           MOVE XPATH-NUM-TRG-NDX (HASH-VALUE) TO CMP-TMPA
           GO TO
            X00000044
            X00000045
            X00000046
            X00000047
            X00000048
            X00000049
           DEPENDING ON NMAR-ENTRY (NPSAN, CMP-TMPA)
           GO TO NUMERIC-RESCUE
           .
       X00000044.
           MOVE X00000013 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000045.
           MOVE X00000014 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000046.
           MOVE X00000015 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000047.
           MOVE X00000016 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000048.
           MOVE X00000017 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       X00000049.
           MOVE X00000018 TO X00000012
           GO TO CONTENT-CONVERTED-EXIT
           .
       NUMERIC-RESCUE-PROCESSING SECTION.
       NUMERIC-RESCUE.
           GO TO
            X0000004A
           DEPENDING ON XPATH-NUM-RES-NDX (HASH-VALUE)
           .
       X0000004A.
           COMPUTE KEYNUM
             OF LINK-COMM
             OF FILLER-LINK
             OF DFHCOMMAREA
             = FUNCTION NUMVAL-C(CHAR-CON (1:CHAR-CON-LEN))
            ON SIZE ERROR
             GO TO NUMERIC-RESCUE-FAILED
            NOT ON SIZE ERROR
             GO TO CONTENT-CONVERTED-EXIT
           END-COMPUTE
           .
       NUMERIC-RESCUE-FAILED.
           MOVE 284 TO MSGNO
           MOVE -1 TO XML-CODE
           GO TO GENERAL-LOGIC-EXIT
           .
       CONTENT-CONVERTED-EXIT.
           MOVE 0 TO CHAR-CON-LEN
           MOVE 1 TO CHAR-CON-NDX
           .
       GENERAL-LOGIC-EXIT.
           IF REPEAT-CURRENT-XML-EVENT
            SET PROCESS-NEXT-XML-EVENT TO TRUE
            GO TO XML2LS-XML-PARSE-HANDLER
           END-IF
           .
       CONDITION-SIGNALER SECTION.
       SIGNAL-CONDITION.
           IF OTHER-ERROR = 'N'
            MOVE 3 TO SEV SEV2
            MOVE 1 TO CASE CNTRL
            MOVE 0 TO ISINFO
            MOVE 0 TO INSERTNO
            MOVE 'IRZ' TO FACID
            CALL 'CEENCOD' USING
              SEV MSGNO CASE SEV2
              CNTRL FACID ISINFO
              NEW-CONDITION FEEDBACK-CODE
            PERFORM CHECK-LE-SERVICE-FC
            MOVE 8 TO CEECMI-DATA-LEN
            MOVE 'BKP92S1I' TO CEECMI-DATA (1:8)
            PERFORM INSERT-CEECMI-STRING
            EVALUATE MSGNO
             WHEN 280
      * PARSER_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0280S
             WHEN 281
      * SUBSCRIPT_RANGE_EXCEEDED_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0281S
             WHEN 282
      * NO_KNOWN_ELEMENTS_FOUND_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0282S
             WHEN 283
      * CHARACTER_CONTENT_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0283S
             WHEN 284
      * NUMERIC_TX_FAILURE_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0284S
             WHEN 285
      * MESSAGE_TOO_LARGE_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0285S
             WHEN 286
      * CHARACTER_CONTENT_BUFFER_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0286S
             WHEN 288
      * UNICODE_RUNTIME_ERROR
              PERFORM CEECMI-IRZ0288S
             WHEN 291
      * XPATH_OVERFLOW_ERROR_MSG_NO
              PERFORM CEECMI-IRZ0291S
             WHEN 294
      * INVALID_PARAMETERS_MSG_NO
              PERFORM CEECMI-IRZ0294S
             WHEN 299
      * XML2LS_LANGUAGE_STRUCTURE_MIN_COUNT_NOT_MET
              PERFORM CEECMI-IRZ0299S
             WHEN 300
      * XML2LS_LANGUAGE_STRUCTURE_MAX_COUNT_EXCEEDED
              PERFORM CEECMI-IRZ0300S
            END-EVALUATE
           ELSE
            MOVE SAVED-CONDITION TO NEW-CONDITION
            MOVE MSG-NO OF NEW-CONDITION TO ERROR-CODE
           END-IF
           MOVE 0 TO QDATA
           IF ADDRESS OF OPTIONAL-FEEDBACK-CODE = NULL
            CALL 'CEESGL' USING NEW-CONDITION QDATA OMITTED
           ELSE
            MOVE NEW-CONDITION TO OPTIONAL-FEEDBACK-CODE
           END-IF
           IF MSGNO NOT EQUAL 294
            MOVE ERROR-CODE TO CONVERTER-RETURN-CODE
           END-IF
           .
       CEECMI-IRZ0280S.
           MOVE XML-CODE TO ERROR-CODE
           MOVE ERROR-CODE TO EEC
           PERFORM INSERT-NUMBER
           PERFORM INSERT-XML-PATH
           PERFORM INSERT-CHAR-CON
           .
       CEECMI-IRZ0281S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-XML-PATH
           .
       CEECMI-IRZ0282S.
           MOVE MSGNO TO ERROR-CODE
           .
       CEECMI-IRZ0283S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-XML-PATH
           MOVE CHAR-CON-LIMIT TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IRZ0284S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-XML-PATH
           PERFORM INSERT-CHAR-CON
           .
       CEECMI-IRZ0285S.
           MOVE MSGNO TO ERROR-CODE
           MOVE XML2LS-XML-BUFFER-LENGTH TO EEC
           PERFORM INSERT-NUMBER
           MOVE 33554436 TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IRZ0286S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-XML-PATH
           MOVE CHAR-CON-LEN TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IRZ0288S.
           MOVE MSGNO TO ERROR-CODE
           MOVE 1200 TO EEC
           PERFORM INSERT-NUMBER
           MOVE 1140 TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IRZ0291S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-XML-PATH
           PERFORM INSERT-CHAR-CON
           .
       CEECMI-IRZ0294S.
           MOVE MSGNO TO ERROR-CODE
           .
       CEECMI-IRZ0299S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-STRUCT-NAME
           .
       CEECMI-IRZ0300S.
           MOVE MSGNO TO ERROR-CODE
           PERFORM INSERT-STRUCT-NAME
           .
       INSERT-CHAR-CON.
           IF CHAR-CON-LEN > 254
            MOVE 254 TO CHAR-CON-LEN
            MOVE '...' TO CHAR-CON (252:3)
           END-IF
           IF CHAR-CON-LEN <= 0
            MOVE 1 TO CHAR-CON-LEN
            MOVE '?' TO CHAR-CON
           END-IF
           MOVE CHAR-CON-LEN TO CEECMI-DATA-LEN
           MOVE CHAR-CON
             TO CEECMI-DATA (1:254)
           PERFORM INSERT-CEECMI-STRING
           .
       INSERT-XML-PATH.
           IF CEECMI-XPATH-LEN > 0
            MOVE CEECMI-XPATH
              TO CEECMI-DATA
            MOVE CEECMI-XPATH-LEN
              TO CEECMI-DATA-LEN
           ELSE
            MOVE 1 TO CEECMI-DATA-LEN
            MOVE '?' TO CEECMI-DATA
           END-IF
           PERFORM INSERT-CEECMI-STRING
           .
       INSERT-NUMBER.
           MOVE ZERO TO CMP-TMPA
           INSPECT EEC TALLYING CMP-TMPA FOR LEADING '0'
           COMPUTE CMP-TMPB = 9 - CMP-TMPA
           MOVE CMP-TMPB TO CEECMI-DATA-LEN
           MOVE EEC (CMP-TMPA + 1:CMP-TMPB) TO CEECMI-DATA
           PERFORM INSERT-CEECMI-STRING
           .
       INSERT-STRUCT-NAME.
           MOVE LANG-STRUCT-NAME-LENGTH TO CEECMI-DATA-LEN
           MOVE LANG-STRUCT-NAME TO CEECMI-DATA(1:254)
           PERFORM INSERT-CEECMI-STRING
           .
       INSERT-CEECMI-STRING.
           ADD 1 TO INSERTNO
           CALL 'CEECMI' USING
            NEW-CONDITION INSERTNO
            CEECMI-STRING FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-7)
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-8) ' '
             FACILITY OF FEEDBACK-CODE
             MSG-NO OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-9)
            STOP RUN
           END-IF
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE TO ENTRY 'BKP92S1A'
           SET TOKEN TO ADDRESS OF CEESRP-DATA
           CALL 'CEEHDLR' USING ROUTINE TOKEN FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-3)
            STOP RUN
           END-IF
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING ROUTINE FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-4)
            STOP RUN
           END-IF
           .
       END PROGRAM 'BKP92S1I'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Required Length in Bytes of the XML to Language Structure Output
      *  Buffer
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1J'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 XML2LS-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER-LENGTH
           XML2LS-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF XML2LS-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 176
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF XML2LS-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO XML2LS-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'BKP92S1J'.
      ******************************************************************
      * PRODUCT: IBM Rational Developer for System z V7.6
      * COMPONENT: Enterprise Service Tools
      * PROGRAM: XML to Language Structure Exception Handler
      * RUNTIME: Web Services for CICS
      * REQUIRED COMPILER: IBM Enterprise COBOL 4.1
      * XMLPARSE OPTION: COMPAT
      * LANGUAGE STRUCTURE CCSID: 1140
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1A'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-6 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F00200072006500730075006D'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500200065007800650063007500740069006F006E0020'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006100660074006500720020006300610074006300680069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006E0067002000610020004C0061006E0067007500610067'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500200045006E007600690072006F006E006D0065006E'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'007400200065007800630065007000740069006F006E002E'.
       LOCAL-STORAGE SECTION.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 TOKEN  POINTER.
       1 RESULT PIC S9(9) COMP.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
           SET ADDRESS OF CEESRP-DATA TO TOKEN
           CALL 'CEEMRCE' USING RECOVERY-POINT FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-6)
           END-IF
           SET RESUME TO TRUE
           IF FACILITY OF CURRENT-CONDITION = 'IGZ'
            EVALUATE MSG-NO OF CURRENT-CONDITION
             WHEN 97
             WHEN 151
             WHEN 152
             WHEN 155
             WHEN 196
              MOVE 'Y' TO NUMVAL-ERROR
             WHEN 272
              MOVE 'Y' TO UNICODE-ERROR
             WHEN OTHER
              MOVE 'Y' TO OTHER-ERROR
            END-EVALUATE
           ELSE
            MOVE 'Y' TO OTHER-ERROR
           END-IF
           MOVE CURRENT-CONDITION TO SAVED-CONDITION
           GOBACK
           .
       END PROGRAM 'BKP92S1A'.
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,NOCICS
      ******************************************************************
      * PRODUCT: IBM Rational Developer for System z V7.6
      * COMPONENT: Enterprise Service Tools
      * PROGRAM: Language Structure to XML Converter
      * RUNTIME: Web Services for CICS
      * REQUIRED COMPILER: IBM Enterprise COBOL 4.1
      * XMLPARSE OPTION: COMPAT
      * LANGUAGE STRUCTURE CCSID: 1140
      * LS2XML XML CCSID: 1140
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1O'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 ERROR-MESSAGES.
       2 CONVERTER-ERROR-3 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F002000720065006700690073'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'007400650072002000610020004C0061006E006700750061'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0067006500200045006E007600690072006F006E006D0065'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006E007400200065007800630065007000740069006F006E'.
       3 PIC N(9) USAGE NATIONAL
           VALUE NX'002000680061006E0064006C00650072002E'.
       2 CONVERTER-ERROR-4 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F00200075006E007200650067'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690073007400650072002000610020004C0061006E0067'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'007500610067006500200045006E007600690072006F006E'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'006D0065006E007400200065007800630065007000740069'.
       3 PIC N(11) USAGE NATIONAL
           VALUE NX'006F006E002000680061006E0064006C00650072002E'.
       2 CONVERTER-ERROR-7 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020005300650072'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0076006900630065002000430061006C006C002000460061'.
       3 PIC N(4) USAGE NATIONAL
           VALUE NX'0069006C00650064'.
       2 CONVERTER-ERROR-8 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'004C0061006E0067007500610067006500200045006E0076'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00690072006F006E006D0065006E00740020004D00650073'.
       3 PIC N(11) USAGE NATIONAL
           VALUE NX'00730061006700650020004E0075006D006200650072'.
       2 CONVERTER-ERROR-9 GROUP-USAGE NATIONAL.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'0058004D004C00200043006F006E00760065007200740065'.
       3 PIC N(12) USAGE NATIONAL
           VALUE NX'00720020004900730020005400650072006D0069006E0061'.
       3 PIC N(7) USAGE NATIONAL
           VALUE NX'00740069006E0067002E002E002E'.
       1 X0000000E.
       2 PIC 9(4) COMP VALUE 4.
       2 PIC X(4) USAGE DISPLAY
           VALUE 'name'.
       1 X00000002.
       2 PIC 9(4) COMP VALUE 69.
       2 PIC X(48) USAGE DISPLAY
           VALUE 'DFHCOMMAREA xmlns="http://www.BKP92S1O.com/schem'.
       2 PIC X(21) USAGE DISPLAY
           VALUE 'as/BKP92S1OInterface"'.
       1 X00000008.
       2 PIC 9(4) COMP VALUE 7.
       2 PIC X(7) USAGE DISPLAY
           VALUE 'filerec'.
       1 X00000001.
       2 PIC 9(4) COMP VALUE 16.
       2 PIC X(16) USAGE DISPLAY
           VALUE '</SOAP-ENV:Body>'.
       1 X00000010.
       2 PIC 9(4) COMP VALUE 5.
       2 PIC X(5) USAGE DISPLAY
           VALUE 'addrx'.
       1 X0000000A.
       2 PIC 9(4) COMP VALUE 4.
       2 PIC X(4) USAGE DISPLAY
           VALUE 'stat'.
       1 X00000016.
       2 PIC 9(4) COMP VALUE 6.
       2 PIC X(6) USAGE DISPLAY
           VALUE 'amount'.
       1 X00000018.
       2 PIC 9(4) COMP VALUE 7.
       2 PIC X(7) USAGE DISPLAY
           VALUE 'comment'.
       1 X0000000C.
       2 PIC 9(4) COMP VALUE 4.
       2 PIC X(4) USAGE DISPLAY
           VALUE 'numb'.
       1 X00000012.
       2 PIC 9(4) COMP VALUE 5.
       2 PIC X(5) USAGE DISPLAY
           VALUE 'phone'.
       1 X00000014.
       2 PIC 9(4) COMP VALUE 5.
       2 PIC X(5) USAGE DISPLAY
           VALUE 'datex'.
       1 X00000006.
       2 PIC 9(4) COMP VALUE 5.
       2 PIC X(5) USAGE DISPLAY
           VALUE 'filea'.
       1 X00000000.
       2 PIC 9(4) COMP VALUE 15.
       2 PIC X(15) USAGE DISPLAY
           VALUE '<SOAP-ENV:Body>'.
       1 X00000003.
       2 PIC 9(4) COMP VALUE 11.
       2 PIC X(11) USAGE DISPLAY
           VALUE 'DFHCOMMAREA'.
       LOCAL-STORAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 ROUTINE PROCEDURE-POINTER.
       1 TOKEN POINTER.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 CEECMI-STRING.
       2 CEECMI-DATA-LEN PIC S9(4) COMP.
       2 CEECMI-DATA PIC X(254).
       1 SEV PIC S9(4) COMP.
       1 MSGNO PIC S9(4) COMP.
       1 CASE PIC S9(4) COMP.
       1 SEV2 PIC S9(4) COMP.
       1 CNTRL PIC S9(4) COMP.
       1 FACID PIC X(3) DISPLAY.
       1 ISINFO PIC S9(9) COMP.
       1 QDATA PIC S9(9) COMP.
       1 INSERTNO PIC S9(9) COMP.
       1 EEC PIC 9(9) DISPLAY.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       1 ERROR-CODE PIC S9(9) COMP.
       1 MSG-VAR-PART-LEN PIC 9(9) COMP.
       1 MSGBLD-RETURN-CODE PIC S9(9) COMP.
       1 LAST-LANG-MEM-INST-NDX PIC 9(9) COMP.
       1 LS2XML-LANG-BUFFER-POINTER POINTER.
       1 LS2XML-LANG-BUFFER-ADDRESS
           REDEFINES LS2XML-LANG-BUFFER-POINTER PIC 9(9) COMP-5.
       1 LANG-STRUCT-NAME PIC X(30).
       1 LANG-STRUCT-NAME-LENGTH PIC 9(4) COMP.
       1 LS2XML-XML-TEMPLATE-BUFFER PIC X(80).
       1 ARRAY-SUBSCRIPTS.
       2 X00000019 PIC 9(9) COMP VALUE 0.
       1 INSTRUCTIONS.
       2 INSTRUCT OCCURS 117 TIMES
            INDEXED BY INSTRUCT-NDX.
       3 MBOPCODE PIC X VALUE X'FF'.
       88 MB-END-INSTRUCT VALUE X'FF'.
       88 MB-LEADING-MARKUP VALUE X'FE'.
       88 MB-START-ELEMENT VALUE X'FD'.
       88 MB-ATT-VAL-CHAR VALUE X'FC'.
       88 MB-ATT-VAL-NUMERIC VALUE X'FB'.
       88 MB-ATT-VAL-FLOAT VALUE X'FA'.
       88 MB-ELE-CHAR-CON VALUE X'F9'.
       88 MB-ELE-NUMERIC-CON VALUE X'F8'.
       88 MB-ELE-FLOAT-CON VALUE X'F7'.
       88 MB-END-ELEMENT VALUE X'F6'.
       88 MB-TRAILING-MARKUP VALUE X'F5'.
       3 MBWSPOPT PIC X.
       88 MB-WSP-COLLAPSE VALUE X'FF'.
       88 MB-WSP-REPLACE VALUE X'FE'.
       88 MB-WSP-PRESERVE VALUE X'FD'.
       88 MB-WSP-COMPAT VALUE X'FC'.
       3 MBDNMPTR POINTER.
       3 MBDATPTR POINTER.
       3 MBDATLEN PIC 9(9) COMP.
       3 MBDATYPE PIC X.
       88 TYPE-NUMERIC VALUE 'N'.
       88 TYPE-ALPHANUMERIC VALUE 'X'.
       88 TYPE-PURE-DBCS VALUE 'D'.
       88 TYPE-UNICODE VALUE 'U'.
       88 TYPE-FLOAT VALUE 'F'.
       88 TYPE-DOUBLE VALUE 'B'.
       3 MBSTGPTR POINTER.
       3 MBETGPTR POINTER.
       LINKAGE SECTION.
       01 DFHCOMMAREA
           .
       05 FILLER-LINK
           .
       10 LINK-COMM
           .
       20 USER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-OPEN
           VALUE
           'O'
           .
       88 ACCOUNT-VIEW
           VALUE
           'V'
           .
       88 ACCOUNT-CHANGE
           VALUE
           'U'
           .
       88 ACCOUNT-CLOSE
           VALUE
           'C'
           .
       20 SERVER-REQUEST
           PICTURE X
           USAGE DISPLAY
           .
       88 ACCOUNT-CREATE
           VALUE
           'C'
           .
       88 ACCOUNT-READ
           VALUE
           'R'
           .
       88 ACCOUNT-UPDATE
           VALUE
           'U'
           .
       88 ACCOUNT-DELETE
           VALUE
           'D'
           .
       88 ACCOUNT-BROWSE
           VALUE
           'B'
           .
       20 KEYNUM
           PICTURE 9(6)
           USAGE DISPLAY
           .
       20 TEAMID
           PICTURE X(2)
           USAGE DISPLAY
           .
       20 RCODE
           PICTURE 9(9)
           USAGE COMP
           .
       20 ERR-RESP
           PICTURE 9(2)
           USAGE DISPLAY
           .
       05 FILEA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       05 COMM-AREA
           .
       10 FILEREC
           .
       20 STAT
           PICTURE X
           USAGE DISPLAY
           .
       20 NUMB
           PICTURE X(6)
           USAGE DISPLAY
           .
       20 NAME
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 ADDRX
           PICTURE X(20)
           USAGE DISPLAY
           .
       20 PHONE
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 DATEX
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 AMOUNT
           PICTURE X(8)
           USAGE DISPLAY
           .
       20 COMMENT
           PICTURE X(9)
           USAGE DISPLAY
           .
       1 X0000001A.
       5 X00000005.
       10 X00000007.
       20 X00000009 PIC X(1).
       20 X0000000B PIC X(6).
       20 X0000000D PIC X(20).
       20 X0000000F PIC X(20).
       20 X00000011 PIC X(8).
       20 X00000013 PIC X(8).
       20 X00000015 PIC X(8).
       20 X00000017 PIC X(9).
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-XML-BUFFER PIC X(834).
       1 LS2XML-LANG-BUFFER PIC X(176).
       1 OPTIONAL-FEEDBACK-CODE PIC X(12).
       1 CONVERTER-RETURN-CODE PIC S9(9) COMP.
       PROCEDURE DIVISION USING
           LS2XML-LANG-BUFFER
           LS2XML-XML-BUFFER-LENGTH
           LS2XML-XML-BUFFER
           OPTIONAL-FEEDBACK-CODE
           RETURNING
           CONVERTER-RETURN-CODE.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
       MAINLINE SECTION.
           MOVE 'N' TO NUMVAL-ERROR UNICODE-ERROR OTHER-ERROR
           PERFORM CHECK-PARAMETERS
           PERFORM REGISTER-EXCEPTION-HANDLER
           CALL 'CEE3SRP' USING RECOVERY-POINT FEEDBACK-CODE
           SERVICE LABEL
           IF UNICODE-ERROR = 'Y'
            MOVE 288 TO MSGNO
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           IF OTHER-ERROR = 'Y'
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           SET LS2XML-LANG-BUFFER-POINTER
            TO ADDRESS OF LS2XML-LANG-BUFFER
           INITIALIZE LS2XML-XML-BUFFER-LENGTH
           SET INSTRUCT-NDX TO 1
           SET MB-LEADING-MARKUP(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000000
           SET INSTRUCT-NDX UP BY 1
           PERFORM INVOKE-MESSAGE-BUILDER
           MOVE 'DFHCOMMAREA'
             TO LANG-STRUCT-NAME
           MOVE 11
             TO LANG-STRUCT-NAME-LENGTH
           SET ADDRESS OF DFHCOMMAREA
            TO LS2XML-LANG-BUFFER-POINTER
           SET ADDRESS OF X0000001A
            TO ADDRESS OF LS2XML-XML-TEMPLATE-BUFFER
           INITIALIZE X0000001A
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000002
           SET INSTRUCT-NDX UP BY 1
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000006
           SET INSTRUCT-NDX UP BY 1
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000008
           SET INSTRUCT-NDX UP BY 1
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000A
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 1 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000009
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000A
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000A
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000A
           SET INSTRUCT-NDX UP BY 1
           MOVE STAT
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X00000009
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000C
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 6 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000B
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000C
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000C
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000C
           SET INSTRUCT-NDX UP BY 1
           MOVE NUMB
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X0000000B
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000E
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 20 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000D
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000E
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000E
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000E
           SET INSTRUCT-NDX UP BY 1
           MOVE NAME
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X0000000D
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000010
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 20 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X0000000F
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000010
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000010
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000010
           SET INSTRUCT-NDX UP BY 1
           MOVE ADDRX
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X0000000F
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000012
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 8 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000011
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000012
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000012
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000012
           SET INSTRUCT-NDX UP BY 1
           MOVE PHONE
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X00000011
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000014
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 8 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000013
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000014
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000014
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000014
           SET INSTRUCT-NDX UP BY 1
           MOVE DATEX
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X00000013
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000016
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 8 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000015
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000016
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000016
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000016
           SET INSTRUCT-NDX UP BY 1
           MOVE AMOUNT
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X00000015
           SET MB-START-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000018
           SET INSTRUCT-NDX UP BY 1
           SET MB-ELE-CHAR-CON(INSTRUCT-NDX) TO TRUE
           SET MB-WSP-COLLAPSE(INSTRUCT-NDX) TO TRUE
           MOVE 9 TO MBDATLEN(INSTRUCT-NDX)
           SET MBDATPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000017
           SET TYPE-ALPHANUMERIC(INSTRUCT-NDX) TO TRUE
           SET MBSTGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000018
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000018
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000018
           SET INSTRUCT-NDX UP BY 1
           MOVE COMMENT
             OF FILEREC
             OF FILEA
             OF DFHCOMMAREA
            TO X00000017
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000008
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000006
           SET INSTRUCT-NDX UP BY 1
           SET MB-END-ELEMENT(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000003
           SET INSTRUCT-NDX UP BY 1
           PERFORM INVOKE-MESSAGE-BUILDER
           SET MB-TRAILING-MARKUP(INSTRUCT-NDX) TO TRUE
           SET MBETGPTR(INSTRUCT-NDX)
            TO ADDRESS OF X00000001
           SET INSTRUCT-NDX UP BY 1
           PERFORM INVOKE-MESSAGE-BUILDER
           PERFORM UNREGISTER-EXCEPTION-HANDLER
           GOBACK
           .
       CHECK-PARAMETERS.
           IF ADDRESS OF LS2XML-LANG-BUFFER EQUAL NULL AND
              ADDRESS OF LS2XML-XML-BUFFER-LENGTH NOT EQUAL NULL
            CALL 'BKP92S1L'
                 USING LS2XML-XML-BUFFER-LENGTH OMITTED
            GOBACK
           ELSE
            IF ADDRESS OF LS2XML-XML-BUFFER EQUAL NULL AND
               ADDRESS OF LS2XML-XML-BUFFER-LENGTH NOT EQUAL NULL
             CALL 'BKP92S1K'
                  USING LS2XML-XML-BUFFER-LENGTH OMITTED
             GOBACK
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER EQUAL NULL OR
              ADDRESS OF LS2XML-XML-BUFFER-LENGTH EQUAL NULL OR
              ADDRESS OF LS2XML-XML-BUFFER EQUAL NULL
            MOVE 294 TO MSGNO
            PERFORM SIGNAL-CONDITION
            GOBACK
           END-IF
           .
       INVOKE-MESSAGE-BUILDER.
           CALL 'BKP92S1C' USING
            INSTRUCTIONS LS2XML-XML-BUFFER-LENGTH
            LS2XML-XML-BUFFER LAST-LANG-MEM-INST-NDX
            RETURNING
             MSGBLD-RETURN-CODE
           IF MSGBLD-RETURN-CODE NOT EQUAL ZERO
            MOVE MSGBLD-RETURN-CODE
             TO MSGNO CONVERTER-RETURN-CODE
            PERFORM UNREGISTER-EXCEPTION-HANDLER
            PERFORM SIGNAL-CONDITION
            GOBACK
           ELSE
            MOVE ZERO TO CONVERTER-RETURN-CODE
           END-IF
           SET INSTRUCT-NDX TO 1
           MOVE ALL X'FF' TO INSTRUCTIONS
           .
       CONDITION-SIGNALER SECTION.
       SIGNAL-CONDITION.
           IF OTHER-ERROR = 'N'
            MOVE 3 TO SEV SEV2
            MOVE 1 TO CASE CNTRL
            MOVE 0 TO ISINFO
            MOVE 0 TO INSERTNO
            MOVE 'IRZ' TO FACID
            CALL 'CEENCOD' USING
             SEV MSGNO CASE SEV2
             CNTRL FACID ISINFO
             NEW-CONDITION FEEDBACK-CODE
            PERFORM CHECK-LE-SERVICE-FC
            MOVE 8 TO CEECMI-DATA-LEN
            MOVE 'BKP92S1O'
             TO CEECMI-DATA (1:8)
            PERFORM INSERT-CEECMI-STRING
            EVALUATE MSGNO
             WHEN 287
      * OUTBOUND_MESSAGE_BUFFER_OVERFLOW
              PERFORM CEECMI-IRZ0287S
             WHEN 288
      * UNICODE_RUNTIME_ERROR
              PERFORM CEECMI-IRZ0288S
            END-EVALUATE
           ELSE
            MOVE SAVED-CONDITION TO NEW-CONDITION
            MOVE MSG-NO OF NEW-CONDITION TO ERROR-CODE
           END-IF
           MOVE 0 TO QDATA
           IF ADDRESS OF OPTIONAL-FEEDBACK-CODE = NULL
            CALL 'CEESGL' USING
             NEW-CONDITION QDATA OMITTED
           ELSE
            MOVE NEW-CONDITION TO OPTIONAL-FEEDBACK-CODE
           END-IF
           IF MSGNO NOT EQUAL 294
            MOVE ERROR-CODE TO CONVERTER-RETURN-CODE
           END-IF
           .
       CEECMI-IRZ0287S.
           MOVE MSGNO TO ERROR-CODE
           MOVE 834 TO EEC
           PERFORM INSERT-NUMBER
           .
       CEECMI-IRZ0288S.
           MOVE MSGNO TO ERROR-CODE
           MOVE 1140 TO EEC
           PERFORM INSERT-NUMBER
           MOVE 1140 TO EEC
           PERFORM INSERT-NUMBER
           .
       INSERT-NUMBER.
           MOVE ZERO TO CMP-TMPA
           INSPECT EEC
            TALLYING CMP-TMPA FOR LEADING ZEROS
           COMPUTE CMP-TMPB = 9 - CMP-TMPA
           MOVE CMP-TMPB TO CEECMI-DATA-LEN
           MOVE EEC (CMP-TMPA + 1:CMP-TMPB)
            TO CEECMI-DATA
           PERFORM INSERT-CEECMI-STRING
           .
       INSERT-CEECMI-STRING.
           ADD 1 TO INSERTNO
           CALL 'CEECMI' USING
            NEW-CONDITION INSERTNO
            CEECMI-STRING FEEDBACK-CODE
           PERFORM CHECK-LE-SERVICE-FC
           .
       CHECK-LE-SERVICE-FC.
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-7)
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-8) ' '
             FACILITY OF FEEDBACK-CODE
             MSG-NO OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-9)
            STOP RUN
           END-IF
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE
            TO ENTRY 'BKP92S1E'
           SET TOKEN TO ADDRESS OF CEESRP-DATA
           CALL 'CEEHDLR' USING
            ROUTINE TOKEN FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-3)
            STOP RUN
           END-IF
           .
       UNREGISTER-EXCEPTION-HANDLER.
           CALL 'CEEHDLU' USING
            ROUTINE FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-4)
            STOP RUN
           END-IF
           .
       END PROGRAM 'BKP92S1O'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Language Structure to XML Markup Generator
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1C'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       1 VALID-TEXT-FLAG PIC X VALUE 'Y'.
       1 P-INSTRUCTION-TYPE PIC X VALUE X'00'.
       88 P-XML-ELEMENT VALUE X'FF'.
       88 P-XML-ATTRIBUTE VALUE X'FE'.
       1 N-INSTRUCTION-TYPE PIC X VALUE X'00'.
       88 N-XML-ELEMENT VALUE X'FF'.
       88 N-XML-ATTRIBUTE VALUE X'FE'.
       1 NEXT-CHAR PIC X.
       LINKAGE SECTION.
       1 INSTRUCTIONS.
       2 INSTRUCT OCCURS 117 TIMES
            INDEXED BY INSTRUCT-NDX.
       3 MBOPCODE PIC X.
       88 MB-END-INSTRUCT VALUE X'FF'.
       88 MB-LEADING-MARKUP VALUE X'FE'.
       88 MB-START-ELEMENT VALUE X'FD'.
       88 MB-ATT-VAL-CHAR VALUE X'FC'.
       88 MB-ATT-VAL-NUMERIC VALUE X'FB'.
       88 MB-ATT-VAL-FLOAT VALUE X'FA'.
       88 MB-ELE-CHAR-CON VALUE X'F9'.
       88 MB-ELE-NUMERIC-CON VALUE X'F8'.
       88 MB-ELE-FLOAT-CON VALUE X'F7'.
       88 MB-END-ELEMENT VALUE X'F6'.
       88 MB-TRAILING-MARKUP VALUE X'F5'.
       3 MBWSPOPT PIC X.
       88 MB-WSP-COLLAPSE VALUE X'FF'.
       88 MB-WSP-REPLACE VALUE X'FE'.
       88 MB-WSP-PRESERVE VALUE X'FD'.
       88 MB-WSP-COMPAT VALUE X'FC'.
       3 MBDNMPTR POINTER.
       3 MBDATPTR POINTER.
       3 MBDATLEN PIC 9(9) COMP.
       3 MBDATYPE PIC X.
       88 TYPE-NUMERIC VALUE 'N'.
       88 TYPE-ALPHANUMERIC VALUE 'X'.
       88 TYPE-PURE-DBCS VALUE 'D'.
       88 TYPE-UNICODE VALUE 'U'.
       88 TYPE-FLOAT VALUE 'F'.
       88 TYPE-DOUBLE VALUE 'B'.
       3 MBSTGPTR POINTER.
       3 MBETGPTR POINTER.
       1 XML-BUFFER-OFFSET PIC 9(9) COMP.
       1 XML-BUFFER PIC X(834).
       1 MSGBLD-RETURN-CODE PIC S9(9) COMP.
       1 XML-TAG-DESCRIPTOR.
       2 XML-TAG-LEN PIC 9(4) COMP.
       2 XML-TAG PIC X(69).
       1 CONTENT-TXT PIC X(256).
       1 LAST-LANG-MEM-INST-NDX PIC 9(9) COMP.
       PROCEDURE DIVISION USING
            INSTRUCTIONS XML-BUFFER-OFFSET
            XML-BUFFER LAST-LANG-MEM-INST-NDX
           RETURNING
            MSGBLD-RETURN-CODE.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
           SET INSTRUCT-NDX TO 1
           INITIALIZE LAST-LANG-MEM-INST-NDX
           PERFORM UNTIL MB-END-INSTRUCT(INSTRUCT-NDX)
            EVALUATE TRUE
             WHEN MB-LEADING-MARKUP(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM LEADING-XML-MARKUP
             WHEN MB-START-ELEMENT(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM START-XML-ELEMENT
             WHEN MB-ELE-CHAR-CON(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM CONVERT-ALPHANUMERIC
             WHEN MB-ELE-NUMERIC-CON(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM CONVERT-NUMERIC
             WHEN MB-ELE-FLOAT-CON(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM CONVERT-FLOAT
             WHEN MB-ATT-VAL-CHAR(INSTRUCT-NDX)
              SET N-XML-ATTRIBUTE TO TRUE
              PERFORM START-XML-ATTRIBUTE
              PERFORM CONVERT-ALPHANUMERIC
              PERFORM END-XML-ATTRIBUTE
             WHEN MB-ATT-VAL-NUMERIC(INSTRUCT-NDX)
              SET N-XML-ATTRIBUTE TO TRUE
              PERFORM START-XML-ATTRIBUTE
              PERFORM CONVERT-NUMERIC
              PERFORM END-XML-ATTRIBUTE
             WHEN MB-ATT-VAL-FLOAT(INSTRUCT-NDX)
              SET N-XML-ATTRIBUTE TO TRUE
              PERFORM START-XML-ATTRIBUTE
              PERFORM CONVERT-FLOAT
              PERFORM END-XML-ATTRIBUTE
             WHEN MB-END-ELEMENT(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM END-XML-ELEMENT
             WHEN MB-TRAILING-MARKUP(INSTRUCT-NDX)
              SET N-XML-ELEMENT TO TRUE
              PERFORM TRAILING-XML-MARKUP
            END-EVALUATE
            MOVE N-INSTRUCTION-TYPE
              TO P-INSTRUCTION-TYPE
            SET INSTRUCT-NDX UP BY 1
           END-PERFORM
           GOBACK
           .
       LEADING-XML-MARKUP.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBSTGPTR(INSTRUCT-NDX)
           MOVE XML-TAG(1:XML-TAG-LEN)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           .
       START-XML-ELEMENT.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBSTGPTR(INSTRUCT-NDX)
           ADD 1 TO XML-BUFFER-OFFSET
           MOVE '<'
             TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           MOVE XML-TAG(1:XML-TAG-LEN)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           IF NOT MB-ATT-VAL-CHAR(INSTRUCT-NDX + 1) AND
              NOT MB-ATT-VAL-NUMERIC(INSTRUCT-NDX + 1) AND
              NOT MB-ATT-VAL-FLOAT(INSTRUCT-NDX + 1)
            ADD 1 TO XML-BUFFER-OFFSET
            MOVE '>'
              TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           END-IF
           .
       START-XML-ATTRIBUTE.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBSTGPTR(INSTRUCT-NDX)
           ADD 1 TO XML-BUFFER-OFFSET
           MOVE SPACE
             TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           MOVE XML-TAG(1:XML-TAG-LEN)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           MOVE '="'
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:2)
           ADD 2 TO XML-BUFFER-OFFSET
           .
       END-XML-ATTRIBUTE.
           ADD 1 TO XML-BUFFER-OFFSET
           MOVE '"'
             TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           IF NOT MB-ATT-VAL-CHAR(INSTRUCT-NDX + 1) AND
              NOT MB-ATT-VAL-NUMERIC(INSTRUCT-NDX + 1) AND
              NOT MB-ATT-VAL-FLOAT(INSTRUCT-NDX + 1)
            ADD 1 TO XML-BUFFER-OFFSET
            MOVE '>'
              TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           END-IF
           .
       END-XML-ELEMENT.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBETGPTR(INSTRUCT-NDX)
           MOVE '</'
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:2)
           ADD 2 TO XML-BUFFER-OFFSET
           MOVE XML-TAG(1:XML-TAG-LEN)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           ADD 1 TO XML-BUFFER-OFFSET
           MOVE '>'
             TO XML-BUFFER(XML-BUFFER-OFFSET:1)
           .
       TRAILING-XML-MARKUP.
           SET ADDRESS OF XML-TAG-DESCRIPTOR
            TO MBETGPTR(INSTRUCT-NDX)
           MOVE XML-TAG(1:XML-TAG-LEN)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:XML-TAG-LEN)
           ADD XML-TAG-LEN TO XML-BUFFER-OFFSET
           .
       CONVERT-ALPHANUMERIC.
           CALL 'XCHRFLTR' USING
            BY VALUE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE VALID-TEXT-FLAG
           IF VALID-TEXT-FLAG EQUAL 'Y'
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
            SET ADDRESS OF CONTENT-TXT
             TO MBDATPTR(INSTRUCT-NDX)
            PERFORM VARYING CMP-TMPA FROM 1 BY 1
             UNTIL CMP-TMPA > MBDATLEN(INSTRUCT-NDX)
             MOVE CONTENT-TXT(CMP-TMPA:1) TO NEXT-CHAR
             EVALUATE NEXT-CHAR
              WHEN '&'
               MOVE '&amp;'
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:5)
               ADD 5 TO XML-BUFFER-OFFSET
              WHEN '<'
               MOVE '&lt;'
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:4)
               ADD 4 TO XML-BUFFER-OFFSET
              WHEN '>'
               MOVE '&gt;'
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:4)
               ADD 4 TO XML-BUFFER-OFFSET
              WHEN ''''
               MOVE '&apos;'
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:6)
               ADD 6 TO XML-BUFFER-OFFSET
              WHEN '"'
               MOVE '&quot;'
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:6)
               ADD 6 TO XML-BUFFER-OFFSET
             WHEN OTHER
               MOVE NEXT-CHAR
                 TO XML-BUFFER(XML-BUFFER-OFFSET + 1:1)
               ADD 1 TO XML-BUFFER-OFFSET
             END-EVALUATE
            END-PERFORM
           END-IF
           .
       CONVERT-NUMERIC.
           SET ADDRESS OF CONTENT-TXT
            TO MBDATPTR(INSTRUCT-NDX)
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
           MOVE MBDATLEN(INSTRUCT-NDX)
             TO CMP-TMPA
           IF CMP-TMPA > 0
            MOVE CONTENT-TXT(1:CMP-TMPA)
              TO XML-BUFFER(XML-BUFFER-OFFSET + 1:CMP-TMPA)
            ADD CMP-TMPA TO XML-BUFFER-OFFSET
           ELSE
            MOVE '0'
              TO XML-BUFFER(XML-BUFFER-OFFSET + 1:1)
            ADD 1 TO XML-BUFFER-OFFSET
           END-IF
           .
       CONVERT-FLOAT.
           SET ADDRESS OF CONTENT-TXT
            TO MBDATPTR(INSTRUCT-NDX)
           CALL 'XWSPFLTR' USING
            BY VALUE MBWSPOPT(INSTRUCT-NDX)
            BY VALUE MBDATPTR(INSTRUCT-NDX)
            BY REFERENCE MBDATLEN(INSTRUCT-NDX)
            BY VALUE MBDATYPE(INSTRUCT-NDX)
           MOVE MBDATLEN(INSTRUCT-NDX)
             TO CMP-TMPA
           MOVE CONTENT-TXT(1:CMP-TMPA)
             TO XML-BUFFER(XML-BUFFER-OFFSET + 1:CMP-TMPA)
           ADD CMP-TMPA TO XML-BUFFER-OFFSET
           .
      * --------------------------------------------------------------
      * Language Structure to XML Character Filter
      * --------------------------------------------------------------
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'XCHRFLTR'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 X0000001B.
       2 PIC X(24) USAGE DISPLAY
           VALUE X'0001020304060708090A0B0C0E0F1011121314161718191A'.
       2 PIC X(24) USAGE DISPLAY
           VALUE X'1B1C1D1E1F2021222324262728292A2B2C2D2E2F30313233'.
       2 PIC X(13) USAGE DISPLAY
           VALUE X'3435363738393A3B3C3D3E3FFF'.
       1 ILLEGAL-XML-CHARS REDEFINES X0000001B
           PIC X(61).
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       1 CONTENT-LEN PIC 9(9) COMP.
       1 CONTENT-PTR POINTER.
       1 CONTENT-TXT PIC X(256).
       1 VALID-TEXT-FLAG PIC X.
       PROCEDURE DIVISION USING BY VALUE CONTENT-LEN CONTENT-PTR
           BY REFERENCE VALID-TEXT-FLAG.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
           SET ADDRESS OF CONTENT-TXT TO CONTENT-PTR
           INSPECT CONTENT-TXT(1:CONTENT-LEN) CONVERTING
            ILLEGAL-XML-CHARS TO SPACES
           MOVE 'Y' TO VALID-TEXT-FLAG
           GOBACK
           .
       END PROGRAM 'XCHRFLTR'.
      * --------------------------------------------------------------
      * Language Structure to XML WhiteSpace Filter
      * --------------------------------------------------------------
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'XWSPFLTR'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       1 CONTENT-BUF PIC X(256).
       1 CONTENT-BUF-NDX PIC 9(9) COMP.
       1 CONTENT-TXT-NDX PIC 9(9) COMP.
       1 CMP-TMPA PIC 9(9) COMP.
       1 CMP-TMPB PIC 9(9) COMP.
       LINKAGE SECTION.
       1 CONTENT-WSP PIC X.
       88 MB-WSP-COLLAPSE VALUE X'FF'.
       88 MB-WSP-REPLACE VALUE X'FE'.
       88 MB-WSP-PRESERVE VALUE X'FD'.
       88 MB-WSP-COMPAT VALUE X'FC'.
       1 XPATH-CON-TYPE PIC X.
       88 TYPE-NUMERIC VALUE 'N'.
       88 TYPE-ALPHANUMERIC VALUE 'X'.
       88 TYPE-PURE-DBCS VALUE 'D'.
       88 TYPE-UNICODE VALUE 'U'.
       88 TYPE-FLOAT VALUE 'F'.
       88 TYPE-DOUBLE VALUE 'B'.
       1 CONTENT-TXT PIC X(256).
       1 CONTENT-PTR POINTER.
       1 CONTENT-LEN PIC 9(9) COMP.
       PROCEDURE DIVISION USING
           BY VALUE CONTENT-WSP
           BY VALUE CONTENT-PTR
           BY REFERENCE CONTENT-LEN
           BY VALUE XPATH-CON-TYPE
           .
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
           SET ADDRESS OF CONTENT-TXT
            TO CONTENT-PTR
           IF TYPE-ALPHANUMERIC OR
              TYPE-PURE-DBCS OR
              TYPE-UNICODE
            EVALUATE TRUE
             WHEN MB-WSP-COLLAPSE
              PERFORM REPLACE-CTRL-CHARS
              PERFORM TRIM-LEADING-SPACES
              PERFORM TRIM-TRAILING-SPACES
              PERFORM COLLAPSE-SPACES
             WHEN MB-WSP-REPLACE
              PERFORM REPLACE-CTRL-CHARS
             WHEN MB-WSP-COMPAT
              PERFORM TRIM-TRAILING-SPACES-COMPAT
            END-EVALUATE
           ELSE
            IF TYPE-NUMERIC OR
               TYPE-FLOAT OR
               TYPE-DOUBLE
              PERFORM TRIM-LEADING-SPACES
              PERFORM TRIM-TRAILING-SPACES
            END-IF
            IF TYPE-NUMERIC
             PERFORM TRIM-LEADING-ZEROS
            END-IF
           END-IF
           GOBACK
           .
       TRIM-LEADING-SPACES.
           MOVE 1 TO CONTENT-TXT-NDX
           PERFORM TEST BEFORE
            UNTIL CONTENT-TXT-NDX >= CONTENT-LEN OR
            CONTENT-TXT(CONTENT-TXT-NDX:1) NOT = SPACE
            ADD 1 TO CONTENT-TXT-NDX
           END-PERFORM
           IF CONTENT-TXT-NDX > 1
            COMPUTE CONTENT-LEN
             = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
            MOVE CONTENT-TXT(CONTENT-TXT-NDX:CONTENT-LEN)
              TO CONTENT-BUF(1:CONTENT-LEN)
            MOVE CONTENT-BUF(1:CONTENT-LEN)
              TO CONTENT-TXT(1:CONTENT-LEN)
           END-IF
           .
       TRIM-TRAILING-SPACES.
           PERFORM TEST BEFORE
            VARYING CONTENT-LEN FROM CONTENT-LEN BY -1
            UNTIL CONTENT-LEN = 0
            OR CONTENT-TXT(CONTENT-LEN:1) NOT = SPACE
           END-PERFORM
           .
       TRIM-TRAILING-SPACES-COMPAT.
           PERFORM TEST BEFORE
            VARYING CONTENT-LEN FROM CONTENT-LEN BY -1
            UNTIL CONTENT-LEN = 1
            OR CONTENT-TXT(CONTENT-LEN:1) NOT = SPACE
           END-PERFORM
           .
       REPLACE-CTRL-CHARS.
           INSPECT CONTENT-TXT(1:CONTENT-LEN) REPLACING ALL
            X'05' BY SPACE X'0B' BY SPACE
            X'0D' BY SPACE X'25' BY SPACE
           .
       COLLAPSE-SPACES.
           MOVE 1 TO CONTENT-TXT-NDX
           MOVE 1 TO CONTENT-BUF-NDX
           PERFORM TEST BEFORE
            UNTIL CONTENT-TXT-NDX > CONTENT-LEN
            IF CONTENT-TXT(CONTENT-TXT-NDX:1) = SPACE
             MOVE CONTENT-TXT(CONTENT-TXT-NDX:1)
               TO CONTENT-BUF(CONTENT-BUF-NDX:1)
             ADD 1 TO CONTENT-TXT-NDX
             ADD 1 TO CONTENT-BUF-NDX
             PERFORM TEST BEFORE
              UNTIL CONTENT-TXT-NDX > CONTENT-LEN OR
               CONTENT-TXT(CONTENT-TXT-NDX:1) NOT = SPACE
              ADD 1 TO CONTENT-TXT-NDX
             END-PERFORM
            ELSE
             MOVE CONTENT-TXT(CONTENT-TXT-NDX:1)
               TO CONTENT-BUF(CONTENT-BUF-NDX:1)
             ADD 1 TO CONTENT-TXT-NDX
             ADD 1 TO CONTENT-BUF-NDX
            END-IF
           END-PERFORM
           COMPUTE CONTENT-LEN = CONTENT-BUF-NDX - 1
           IF CONTENT-LEN > 0
            MOVE CONTENT-BUF(1:CONTENT-LEN)
              TO CONTENT-TXT(1:CONTENT-LEN)
           END-IF
           .
       TRIM-LEADING-ZEROS.
           MOVE 1 TO CONTENT-TXT-NDX
           MOVE 1 TO CONTENT-BUF-NDX
           IF CONTENT-LEN > 0
              AND CONTENT-TXT(1:1) = '-'
            MOVE CONTENT-TXT(1:1)
              TO CONTENT-BUF(1:1)
            ADD 1 TO CONTENT-TXT-NDX
            ADD 1 TO CONTENT-BUF-NDX
           END-IF
           COMPUTE CMP-TMPA
            = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
           IF CMP-TMPA > 0 AND
              CONTENT-TXT(CONTENT-TXT-NDX:1) = '0'
            INITIALIZE CMP-TMPB
            INSPECT CONTENT-TXT(CONTENT-TXT-NDX:CMP-TMPA)
             TALLYING CMP-TMPB FOR LEADING '0'
            IF CMP-TMPB > 0
             COMPUTE CMP-TMPA
              = CONTENT-TXT-NDX + CMP-TMPB
             IF CONTENT-TXT(CMP-TMPA:1) = '.'
              SUBTRACT 1 FROM CMP-TMPB
             END-IF
             ADD CMP-TMPB TO CONTENT-TXT-NDX
            END-IF
           END-IF
           COMPUTE CMP-TMPA
            = CONTENT-LEN - (CONTENT-TXT-NDX - 1)
           IF CMP-TMPA > 0
            MOVE CONTENT-TXT(CONTENT-TXT-NDX:CMP-TMPA)
              TO CONTENT-BUF(CONTENT-BUF-NDX:CMP-TMPA)
            ADD  CMP-TMPA TO CONTENT-BUF-NDX
           END-IF
           COMPUTE CONTENT-LEN = CONTENT-BUF-NDX - 1
           IF CONTENT-LEN > 0
            MOVE CONTENT-BUF(1:CONTENT-LEN)
              TO CONTENT-TXT(1:CONTENT-LEN)
           END-IF
           .
       END PROGRAM 'XWSPFLTR'.
       END PROGRAM 'BKP92S1C'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Required Length in Bytes of the Language Structure to XML Output
      *  Buffer
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1K'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           LS2XML-XML-BUFFER-LENGTH
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 834
              TO LS2XML-XML-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'BKP92S1K'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Maximum Length in Bytes of the Language Structure to XML Input B
      * uffer
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1L'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 LS2XML-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           LS2XML-LANG-BUFFER-LENGTH
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 176
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'BKP92S1L'.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * Language Structure to XML Exception Handler
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'BKP92S1E'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.300.V20100501_0728.
        DATE-WRITTEN. 7/12/10 6:08 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-6 GROUP-USAGE NATIONAL.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'00490052005A003900390039003900530020004600610069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006C0065006400200074006F00200072006500730075006D'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500200065007800650063007500740069006F006E0020'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006100660074006500720020006300610074006300680069'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006E0067002000610020004C0061006E0067007500610067'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'006500200045006E007600690072006F006E006D0065006E'.
       2 PIC N(12) USAGE NATIONAL
           VALUE NX'007400200065007800630065007000740069006F006E002E'.
       LOCAL-STORAGE SECTION.
       1 FEEDBACK-CODE.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       LINKAGE SECTION.
       1 CEESRP-DATA.
       2 RECOVERY-POINT PIC S9(9) COMP.
       2 NUMVAL-ERROR PIC X.
       2 UNICODE-ERROR PIC X.
       2 OTHER-ERROR PIC X.
       2 SAVED-CONDITION PIC X(12).
       1 TOKEN  POINTER.
       1 RESULT PIC S9(9) COMP.
       88 RESUME VALUE 10.
       1 CURRENT-CONDITION.
       2 CONDITION-TOKEN-VALUE.
       88 CEE000 VALUE X'0000000000000000'.
       88 CEE0E7 VALUE X'000101C749C3C5C5'.
       3 SEVERITY PIC S9(4) BINARY.
       3 MSG-NO PIC S9(4) BINARY.
       3 CASE-SEV-CTL PIC X.
       3 FACILITY PIC XXX.
       2 I-S-INFO PIC S9(9) BINARY.
       1 NEW-CONDITION PIC X(12).
       PROCEDURE DIVISION USING CURRENT-CONDITION TOKEN
           RESULT NEW-CONDITION.
       MAINLINE SECTION.
      * --------------------------------------------------------------
      * Please Do Not Modify This Program
      * --------------------------------------------------------------
           SET ADDRESS OF CEESRP-DATA TO TOKEN
           SET RESUME TO TRUE
           CALL 'CEEMRCE' USING RECOVERY-POINT FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE
            DISPLAY FUNCTION DISPLAY-OF(CONVERTER-ERROR-6)
           END-IF
           IF FACILITY OF CURRENT-CONDITION = 'IGZ'
            EVALUATE MSG-NO OF CURRENT-CONDITION
             WHEN 272
              MOVE 'Y' TO UNICODE-ERROR
             WHEN OTHER
              MOVE 'Y' TO OTHER-ERROR
            END-EVALUATE
           ELSE
            MOVE 'Y' TO OTHER-ERROR
           END-IF
           MOVE CURRENT-CONDITION TO SAVED-CONDITION
           GOBACK
           .
       END PROGRAM 'BKP92S1E'.