       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,CICS

       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'STOCK2Y'.
        AUTHOR. MAZO.
        INSTALLATION. LIXSE76-02-II.
        DATE-WRITTEN. 29 April 2009.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Fake Stock data to return in the business logic
       01 EXH-QOT-DATA.
      * IBM = C9.C2.D4 = 25F
          05 PIC X(4)             VALUE 'IBM'.
          05 PIC 9(6)V9(3) COMP-3 VALUE 104.61.
          05 PIC X(32) VALUE 'INTL BUSINESS MACH'.
      * DIS = C4.C9.E2 = 26F
          05 PIC X(4)             VALUE 'DIS'.
          05 PIC 9(6)V9(3) COMP-3 VALUE 28.70.
          05 PIC X(32) VALUE 'WALT DISNEY-DISNEY C'.
      * HPQ = C8.D7.D8 = 277
          05 PIC X(4)             VALUE 'HPQ'.
          05 PIC 9(6)V9(3) COMP-3 VALUE 31.22.
          05 PIC X(32) VALUE 'HEWLETT PACKARD CO'.
      * INTC = C9.D5.E3.C3 = 344
          05 PIC X(4)             VALUE 'INTC'.
          05 PIC 9(6)V9(3) COMP-3 VALUE 17.72.
          05 PIC X(32) VALUE 'INTEL CP'.
      * MSFT = D4.E2.C6.E3 = 35F
          05 PIC X(4)             VALUE 'MSFT'.
          05 PIC 9(6)V9(3) COMP-3 VALUE 22.26.
          05 PIC X(32) VALUE 'MICROSOFT CP'.
      *
       01 EXH-QOT-DAT-RECS REDEFINES EXH-QOT-DATA.
          05 EXH-QOT-DAT-REC OCCURS 5 TIMES
             ASCENDING KEY IS EXH-QOT-DAT-SYM
             INDEXED BY EXH-QOT-REC-NDX.
             10 EXH-QOT-DAT-SYM PIC X(4).
             10 EXH-QOT-DAT-SHR-PRC PIC 9(6)V9(3) COMP-3.
             10 EXH-QOT-DAT-SYM-DESC PIC X(32).
      *End Working-Storage Section

       1 CONVERTER-ERROR-7.
       2 PIC X(40) USAGE DISPLAY
           VALUE 'Language Environment Service Call Failed'.
       1 CONVERTER-ERROR-8.
       2 PIC X(35) USAGE DISPLAY
           VALUE 'Language Environment Message Number'.
       1 CONVERTER-ERROR-9.
       2 PIC X(31) USAGE DISPLAY
           VALUE 'XML Converter Is Terminating...'.
      * *************************************************************
      *             Vendor Program Container Definitions
      * *************************************************************
       1 DFH-BODY-CONTAINER PIC X(16) VALUE 'DFHWS-BODY'.
       1 DFH-DATA-CONTAINER PIC X(16) VALUE 'DFH-DATA'.
       LOCAL-STORAGE SECTION.
       01 CUR-REQ-SYM PIC X(4).
       01 CUR-REQ-SHR-QTY PIC 9(4) COMP.
      * *************************************************************
      *             Storage Items For LE Error Handling
      * *************************************************************
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
      * *************************************************************
      *                 Converter Metadata Variables
      * *************************************************************
       1 XML2LS-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC S9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC S9(9) COMP.
       1 XML2LS-XML-CCSID PIC S9(9) COMP.
       1 HOST-LANG-CCSID PIC S9(9) COMP.
       1 LS2XML-XML-CCSID PIC S9(9) COMP.
       1 RESULT-LENGTH PIC S9(9) COMP.
      * *************************************************************
      *                 SOAP Pipeline Work Variables
      * *************************************************************
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
       2 WORK-AREA-PTR2 POINTER.
       2 WORK-AREA-LEN PIC 9(9) BINARY.
       1 WORK-AREA-VAL PIC X VALUE X'00'.
      *XMLTRANSFORM Resource variables
       77 xmltransformF              pic x(32) value 'STK2ZF'.
       77 xmltransformT              pic x(32) value 'STK2ZT'.
       77 xml-cont                   pic x(16) value 'xml-cont'.        00216000
       77 data-cont                  pic x(16) value 'data-cont'.       00217000
       77 outbound-channel pic x(16) value 'OUTBOUND-CHANNEL'.
       1 data-cont-ptr pointer.
       1 Scratch-len PIC 9(9) BINARY.
       LINKAGE SECTION.
       1 DFH-BODY PIC X(100).
       1 SOAP-BODY-ST.
         2 ENV-TAG pic x(15).
         2 SOAP-PAYLOAD PIC X(642).

       1 DFH-DATA PIC X.
      * *************************************************************
      *              Business Program Binary Interfaces
      * *************************************************************
       01 STOCK-QUOTE-QUERY
           .
       05 REQ-SYMBOL
           PICTURE X(4)
           USAGE DISPLAY
           .
       05 REQ-SHARE-QTY
           PICTURE 9(4)
           USAGE COMP
           .
       01 COMPANY-STOCK-INFO
           .
       05 RPY-SYMBOL
           PICTURE X(4)
           USAGE DISPLAY
           .
       05 RPY-SYMBOL-DESC
           PICTURE X(32)
           USAGE DISPLAY
           .
       05 RPY-SHARE-PRICE
           PICTURE 9(6)V9(3)
           USAGE COMP-3
           .
       05 RPY-TOTAL-PRICE
           PICTURE 9(12)V9(3)
           USAGE COMP-3
           .
       05 RPY-TIME
           PICTURE X(21)
           USAGE DISPLAY
           .
       PROCEDURE DIVISION.
       MAINLINE SECTION.
           DISPLAY 'Got into STOCK2Y'
      * -------------------------------------------------------------
      *            Initialize Storage and Browse Channel
      * -------------------------------------------------------------
           PERFORM REGISTER-EXCEPTION-HANDLER
           DISPLAY 'STOCK2Y: Performing   INITIALIZE PIP WRK vars'
           INITIALIZE SOAP-PIPELINE-WORK-VARIABLES
           DISPLAY 'STOCK2Y: PERFORM GET-CONVERTER-METADATA'
           PERFORM GET-CONVERTER-METADATA
           DISPLAY 'STOCK2Y: PERFORM BROWSE-VENDOR-CHANNEL'
      *    PERFORM BROWSE-VENDOR-CHANNEL
      * -------------------------------------------------------------
      *           Branch To Processing Logic For Container
      * -------------------------------------------------------------

            DISPLAY 'STOCK2Y: PERFORM PROCESS-DFH-BODY'
            PERFORM PROCESS-DFH-BODY
            DISPLAY 'STOCK2Y: PERFORM FREE-WORKAREA'
            PERFORM FREE-WORK-AREA
            IF ERROR-OCCURRED = 'N'
              DISPLAY 'STOCK2Y: PERFORM PROCESS-DFH-DATA'
              PERFORM PROCESS-DFH-DATA
              PERFORM FREE-WORK-AREA
              PERFORM FREE-WORK-AREA2
            END-IF
      * -------------------------------------------------------------
      *                           Finished
      * -------------------------------------------------------------
           PERFORM UNREGISTER-EXCEPTION-HANDLER
           EXEC CICS RETURN
           END-EXEC
           .
      *not used
       BROWSE-VENDOR-CHANNEL.
           EXEC CICS STARTBROWSE CONTAINER
             BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
           END-EXEC
           PERFORM TEST AFTER UNTIL
             NEXT-CONTAINER EQUAL DFH-BODY-CONTAINER OR
             COMMAND-RESP2 NOT EQUAL ZERO
             EXEC CICS GETNEXT CONTAINER (NEXT-CONTAINER)
               BROWSETOKEN (CONTAINER-BROWSE-TOKEN)
               RESP(COMMAND-RESP)
               RESP2(COMMAND-RESP2)
             END-EXEC
           END-PERFORM
           .
       PROCESS-DFH-BODY.
           DISPLAY 'STOCK2Y: PERFORM RECEIVE-DFH-BODY'
           PERFORM RECEIVE-DFH-BODY
           DISPLAY 'STOCK2Y: PERFORM ALLOCATE WORK AREA'
           PERFORM ALLOCATE-DFH-DATA-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM INVOKE-XML2LS-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM ALLOCATE-BUS-LOGIC-WORK-AREA
             PERFORM BUSINESS-LOGIC
           END-IF
           .
       PROCESS-DFH-DATA.
      *    PERFORM RECEIVE-DFH-DATA
           PERFORM ALLOCATE-DFH-BODY-WORK-AREA
           MOVE 'N' TO ERROR-OCCURRED
           PERFORM INVOKE-LS2XML-CONVERSION
           IF ERROR-OCCURRED = 'Y'
             PERFORM SEND-SOAP-FAULT
           ELSE
             PERFORM SEND-DFH-BODY
           END-IF
           .
       BUSINESS-LOGIC.
           MOVE REQ-SYMBOL TO CUR-REQ-SYM
           MOVE REQ-SHARE-QTY TO CUR-REQ-SHR-QTY
           INITIALIZE COMPANY-STOCK-INFO
           MOVE CUR-REQ-SYM TO RPY-SYMBOL
           MOVE FUNCTION CURRENT-DATE TO RPY-TIME
      * ....
           SEARCH EXH-QOT-DAT-REC VARYING EXH-QOT-REC-NDX
             AT END
               MOVE 'SYMBOL NOT FOUND' TO RPY-SYMBOL-DESC
             WHEN EXH-QOT-DAT-SYM (EXH-QOT-REC-NDX) = RPY-SYMBOL
               MOVE EXH-QOT-DAT-SYM-DESC (EXH-QOT-REC-NDX)
                 TO RPY-SYMBOL-DESC
               MOVE EXH-QOT-DAT-SHR-PRC (EXH-QOT-REC-NDX)
                 TO RPY-SHARE-PRICE
               COMPUTE RPY-TOTAL-PRICE =
                  CUR-REQ-SHR-QTY * RPY-SHARE-PRICE
               END-COMPUTE
           END-SEARCH.

       RECEIVE-DFH-BODY.
           MOVE 'DFHWS-BODY' TO DFH-BODY-CONTAINER
           EXEC CICS GET CONTAINER(DFH-BODY-CONTAINER)
             SET(DFH-BODY-PTR)
             FLENGTH(DFH-BODY-LEN)
             INTOCCSID(1140)
           END-EXEC
           SET ADDRESS OF DFH-BODY
             TO DFH-BODY-PTR
           Compute Scratch-len = DFH-BODY-LEN - 31 End-compute
           display 'DFH BODY Incoming:'
      *    display DFH-BODY(15:Scratch-len)
           display DFH-BODY
           .
       SEND-DFH-BODY.
           MOVE '<SOAP-ENV:Body>' to ENV-TAG
           MOVE DFH-BODY(1:DFH-BODY-LEN) to SOAP-PAYLOAD
           MOVE '</SOAP-ENV:Body>'
               to SOAP-PAYLOAD(DFH-BODY-LEN + 1:16)
           Compute Scratch-len = DFH-BODY-LEN + 31 End-compute
           display 'DFH BODY Outgoing WITH TAGS:'
           display SOAP-BODY-ST
           MOVE 'DFHWS-BODY' TO DFH-BODY-CONTAINER
           EXEC CICS PUT CONTAINER(DFH-BODY-CONTAINER)
             FROM(SOAP-BODY-ST)
             FLENGTH(Scratch-len)
             FROMCCSID(1140)
           END-EXEC
           .
      *Not used
       RECEIVE-DFH-DATA.
           EXEC CICS GET CONTAINER(DFH-DATA-CONTAINER)
             SET(DFH-DATA-PTR)
             FLENGTH(DFH-DATA-LEN)
           END-EXEC
           SET ADDRESS OF COMPANY-STOCK-INFO
             TO DFH-DATA-PTR
           .
      *Not used
       SEND-DFH-DATA.
           COMPUTE DFH-DATA-LEN =
             LENGTH OF STOCK-QUOTE-QUERY
           EXEC CICS PUT CONTAINER(DFH-DATA-CONTAINER)
             FROM(STOCK-QUOTE-QUERY)
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
           SET ADDRESS OF SOAP-BODY-ST
             TO WORK-AREA-PTR
           .
       ALLOCATE-BUS-LOGIC-WORK-AREA.
           COMPUTE RESULT-LENGTH = FUNCTION LENGTH (COMPANY-STOCK-INFO)
           MOVE RESULT-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR2)
             FLENGTH(WORK-AREA-LEN)
             INITIMG(WORK-AREA-VAL)
           END-EXEC
           SET ADDRESS OF COMPANY-STOCK-INFO
             TO WORK-AREA-PTR2
           .

       ALLOCATE-DFH-DATA-WORK-AREA.
           MOVE XML2LS-LANG-BUFFER-LENGTH
             TO WORK-AREA-LEN
           EXEC CICS GETMAIN
             SET(WORK-AREA-PTR)
             FLENGTH(WORK-AREA-LEN)
             INITIMG(WORK-AREA-VAL)
           END-EXEC
           SET ADDRESS OF STOCK-QUOTE-QUERY
             TO WORK-AREA-PTR
           .
       FREE-WORK-AREA.
           IF WORK-AREA-PTR NOT EQUAL NULL
             EXEC CICS FREEMAIN
               DATAPOINTER(WORK-AREA-PTR)
             END-EXEC
           END-IF
           .
       FREE-WORK-AREA2.
           IF WORK-AREA-PTR2 NOT EQUAL NULL
             EXEC CICS FREEMAIN
               DATAPOINTER(WORK-AREA-PTR2)
             END-EXEC
           END-IF
           .
       GET-CONVERTER-METADATA.
           CALL 'STOCK2YX' USING
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
           DISPLAY 'STOCK2Y: Populating XML container'
           exec cics put container(xml-cont)
                         channel(outbound-channel)                      02242000
                         from(DFH-BODY(15:Scratch-len))                 02242107
                         flength(Scratch-len)                           02242300
                         resp(COMMAND-RESP)
                         resp2(COMMAND-RESP2)
           end-exec
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           DISPLAY 'STOCK2Y: Invoking XMLTODATA!'
           exec cics transform xmltodata
                         xmltransform(xmltransformF)                    03272004
                         channel(outbound-channel)                      03273004
                         xmlcontainer(xml-cont)                         03274004
                         datcontainer(data-cont)                        03275004
                         resp(COMMAND-RESP)                             03279504
                         resp2(COMMAND-RESP2)                           03279604
           end-exec
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           DISPLAY 'STOCK2Y: Getting DATA container'
           EXEC CICS GET CONTAINER(data-cont)
                         channel(outbound-channel)
                         SET(data-cont-ptr)
                         FLENGTH(Scratch-len)
                         resp(COMMAND-RESP)
                         resp2(COMMAND-RESP2)
           END-EXEC
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           Set address of STOCK-QUOTE-QUERY to data-cont-ptr
           DISPLAY 'STOCK2Y: Data from XMLTRANSFORM:'
           DISPLAY 'Scratch-len:'
           Display Scratch-len
           DISPLAY 'REQ-SYMBOL:'
           DISPLAY REQ-SYMBOL
           DISPLAY 'REQ-SHARE-QTY:'
           DISPLAY REQ-SHARE-QTY
      *     CALL 'STOCK2YI'
      *       USING
      *         STOCK-QUOTE-QUERY
      *         DFH-BODY-LEN
      *         DFH-BODY
      *         OMITTED
      *   OPTIONAL-FEEDBACK-CODE
      *       RETURNING
      *         CONVERTER-RETURN-CODE
           .
       INVOKE-LS2XML-CONVERSION.
           DISPLAY 'STOCK2Y: In LS2XML Conversion..'
           DISPLAY 'STOCK2Y: Deleting XML container'
           exec cics delete container(xml-cont)                         04278200
                         channel(outbound-channel)                      04278300
                         resp(command-resp)                             04278600
                         resp2(command-resp2)                           04278700
           end-exec.
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           DISPLAY 'STOCK2Y: Populating DATA container'
           exec cics put container(data-cont)
                         channel(outbound-channel)                      02242000
                         from(COMPANY-STOCK-INFO)                       02242107
                         flength(RESULT-LENGTH)                         02242300
                         resp(COMMAND-RESP)
                         resp2(COMMAND-RESP2)
           end-exec
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           DISPLAY 'STOCK2Y: Invoking DATATOXML!'
           exec cics transform datatoxml
                         xmltransform(xmltransformT)                    03272004
                         channel(outbound-channel)                      03273004
                         xmlcontainer(xml-cont)                         03274004
                         datcontainer(data-cont)                        03275004
                         resp(COMMAND-RESP)                             03279504
                         resp2(COMMAND-RESP2)                           03279604
           end-exec
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           DISPLAY 'STOCK2Y: Getting XML container'
           EXEC CICS GET CONTAINER(xml-cont)
                         channel(outbound-channel)
                         SET(DFH-BODY-PTR)
                         FLENGTH(DFH-BODY-LEN)
                         resp(COMMAND-RESP)
                         resp2(COMMAND-RESP2)
           END-EXEC
           Display 'STOCK2Y: Resp'
           Display COMMAND-RESP
           Display 'STOCK2Y: Resp 2'
           Display COMMAND-RESP2
           SET ADDRESS OF DFH-BODY
             TO DFH-BODY-PTR
           display 'DFH BODY Outgoing:'
           display DFH-BODY
      *    CALL 'STOCK2YO'
      *      USING
      *        COMPANY-STOCK-INFO
      *        DFH-BODY-LEN
      *        DFH-BODY
      *        OMITTED
      *   OPTIONAL-FEEDBACK-CODE
      *      RETURNING
      *        CONVERTER-RETURN-CODE
           .
       REGISTER-EXCEPTION-HANDLER.
           SET ROUTINE TO ENTRY 'STOCK2YF'
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
             DISPLAY CONVERTER-ERROR-7
             DISPLAY CONVERTER-ERROR-8 ' '
               FACILITY OF FEEDBACK-CODE
               MSG-NO OF FEEDBACK-CODE
             DISPLAY CONVERTER-ERROR-9
             STOP RUN
           END-IF
           .
       END PROGRAM 'STOCK2Y'.
      *          *********************************************
      *    *********************************************************
      *  *************************************************************
      *                       Exception Handler
      *  *************************************************************
      *    *********************************************************
      *          *********************************************
       PROCESS NODYNAM,CODEPAGE(1140),NSYMBOL(NATIONAL)
       PROCESS ARITH(EXTEND),NOOPT,NOCICS
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'STOCK2YF'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.100.V20090421_0706.
        DATE-WRITTEN. 4/29/09 4:14 PM
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CONVERTER-ERROR-5.
       2 PIC X(31) USAGE DISPLAY
           VALUE 'Failed To Get Exception Message'.
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
      * -------------------------------------------------------------
      *             Storage For Saving Exception Details
      * -------------------------------------------------------------
           SET ADDRESS OF ERROR-RESPONSE TO TOKEN
      * -------------------------------------------------------------
      *                    Get Exception Message
      * -------------------------------------------------------------
           PERFORM FILL-DESCRIPTION-BUFFER
      * -------------------------------------------------------------
      *                  Display Exception Message
      * -------------------------------------------------------------
           PERFORM DISPLAY-MESSAGE-TEXT
      * -------------------------------------------------------------
      *        Recover From Exception To Produce XML Response
      * -------------------------------------------------------------
           MOVE 'Y' TO ERROR-OCCURRED
           SET RESUME TO TRUE
      * -------------------------------------------------------------
      *                           Finished
      * -------------------------------------------------------------
           GOBACK
           .
       FILL-DESCRIPTION-BUFFER.
           MOVE 0 TO MSG-PTR
           MOVE 512 TO ERROR-REASON-LENGTH
           MOVE SPACES TO MSG-PART ERROR-REASON
           CALL 'CEEMGET' USING
             CURRENT-CONDITION MSG-PART
             MSG-PTR FEEDBACK-CODE
           IF NOT CEE000 OF FEEDBACK-CODE AND
              NOT CEE0E7 OF FEEDBACK-CODE
            DISPLAY CONVERTER-ERROR-5
           END-IF
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
              DISPLAY CONVERTER-ERROR-5
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
       END PROGRAM 'STOCK2YF'.
      *          *********************************************
      *    *********************************************************
      *  *************************************************************
      *             Compiled XML Conversion Properties API
      *  *************************************************************
      *    *********************************************************
      *          *********************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. 'STOCK2YX'.
        AUTHOR. WD4Z.
        INSTALLATION. 9.3.100.V20090421_0706.
        DATE-WRITTEN. 4/29/09 4:14 PM.
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
            MOVE 6
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 70
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 657
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
       END PROGRAM 'STOCK2YX'.