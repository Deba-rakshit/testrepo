      * PROCESS CICS('COBOL3,APOST,SP')
      * PROCESS APOST TRUNC(BIN) NOSSRANGE  SIZE(4000K) TEST
      *****************************************************************
      *                                                               *
      * MODULE NAME = DFH0CALL                                        *
      *                                                               *
      * Licensed Materials - Property of IBM                          *
      *                                                               *
      * 5655-M15              EXECDLI                                 *
      *                                                               *
      * (C) Copyright IBM Corp. 1990, 1992                            *
      *                                                               *
      * CICS                                                          *
      *---------------------------------------------------------------*
      *                                                               *
      * CHANGE ACTIVITY :                                             *
      * $SEG(DFH0CALL),COMP(SAMPLES),PROD(CICS    ):                  *
      *                                                               *
      *     PN= REASON REL YYMMDD HDXIII : REMARKS                    *
      *    $P0= .      320 900320        : Created.                   *
      *    $P1= M90474 330 910807 HDBWSH : Prologue fixed.            *
      *    $P2= M91869 330 920204 HDCTRC : INSPECTs removed.          *
      *                                                               *
      * DFH0CALL DIVIDED INTO CLIENT AND SERVER FOR BANKING DEMO      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXECDLIP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SEGKEYA                PIC X(4).
       77  SEGKEYB                PIC X(4).
       77  SEGKEYC                PIC X(4).
       77  SEGKEY1                PIC X(4).
       77  SEGKEY2                PIC X(4).
       77  SEGKEY3                PIC X(4).
       77  SEGKEY4                PIC X(4).
       77  CONKEYB                PIC X(8).
       77  SEGNAME                PIC X(8).
       77  SEGLEN                 COMP PIC S9(4).
       77  PCBNUM                 COMP PIC S9(4).
       01  AREAA                  PIC X(80).
      *    DEFINE SEGMENT I/O AREA
       01  AREAB                  PIC X(80).
       01  AREAC                  PIC X(80).
       01  AREAG                  PIC X(250).
       01  AREASTAT               PIC X(360).
      *    COPY MAPSET.
       PROCEDURE DIVISION.
      *
      * ***************************************************************
      *  INITIALIZATION
      *  HANDLE ERROR CONDITIONS IN ERROR ROUTINE
      *  HANDLE ABENDS (DLI ERROR STATUS CODES) IN ABEND ROUTINE
      *  RECEIVE INPUT MESSAGE
      * ***************************************************************
      *
           EXEC CICS HANDLE CONDITION ERROR(ERRORS) END-EXEC.
      *
           EXEC CICS HANDLE ABEND LABEL(ABENDS) END-EXEC.
      *
           EXEC CICS RECEIVE MAP ('SAMPMAP') MAPSET('MAPSET') END-EXEC.
      *    ANALYZE INPUT MESSAGE AND PERFORM NON-DLI PROCESSING
      *
      * ***************************************************************
      *  SCHEDULE PSB NAMED 'SAMPLE1'
      * ***************************************************************
      *
           EXEC DLI SCHD PSB(SAMPLE1) END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  RETRIEVE ROOT SEGMENT AND ALL ITS DEPENDENTS
      * ***************************************************************
      *
           MOVE 'A300' TO SEGKEYA.
           EXEC DLI GU USING PCB(1) SEGMENT(SEGA) INTO(AREAA)
                SEGLENGTH(80) WHERE(KEYA=SEGKEYA)
                FIELDLENGTH(4)
           END-EXEC.


           PERFORM TEST-DIB THRU OK.
       GNPLOOP.
           EXEC DLI GNP USING PCB(1) INTO(AREAG) SEGLENGTH(250)
           END-EXEC.
           IF DIBSTAT EQUAL TO 'GE' THEN GO TO LOOPDONE.
           PERFORM TEST-DIB THRU OK.
           GO TO GNPLOOP.
       LOOPDONE.
      *
      * ***************************************************************
      *  INSERT NEW ROOT SEGMENT
      * ***************************************************************
      *
           MOVE 'DATA FOR NEW SEGMENT INCLUDING KEY' TO AREAA.
           EXEC DLI ISRT USING PCB(1) SEGMENT(SEGA) FROM(AREAA)
                SEGLENGTH(80) END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      * RETRIEVE 3 SEGMENTS IN PATH AND REPLACE THEM
      * ***************************************************************
      *
           MOVE 'A200' TO SEGKEYA.
           MOVE 'B240' TO SEGKEYB.
           MOVE 'C241' TO SEGKEYC.
           EXEC DLI GU USING PCB(1)
             SEGMENT(SEGA) WHERE(KEYA=SEGKEYA) FIELDLENGTH(4)
                INTO(AREAA)
                SEGLENGTH(80)
             SEGMENT(SEGB) WHERE(KEYB=SEGKEYB) FIELDLENGTH(4)
                INTO(AREAB)
                SEGLENGTH(80)
             SEGMENT(SEGC) WHERE(KEYC=SEGKEYC) FIELDLENGTH(4)
                INTO(AREAC)
                SEGLENGTH(80)
           END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *    UPDATE FIELDS IN THE 3 SEGMENTS
           EXEC DLI REPL USING PCB(1)
             SEGMENT(SEGA) FROM(AREAA) SEGLENGTH(80)
             SEGMENT(SEGB) FROM(AREAB) SEGLENGTH(80)
             SEGMENT(SEGC) FROM(AREAC) SEGLENGTH(80)
           END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  INSERT NEW SEGMENT USING CONCATENATED KEY TO QUALIFY PARENT
      * ***************************************************************
      *
           MOVE 'DATA FOR NEW SEGMENT INCLUDING KEY' TO AREAC.
           MOVE 'A200B240' TO CONKEYB.
           EXEC DLI ISRT USING PCB(1)
             SEGMENT(SEGB) KEYS(CONKEYB) KEYLENGTH(8)
             SEGMENT(SEGC) FROM(AREAC) SEGLENGTH(80)
           END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  RETRIEVE SEGMENT DIRECTLY USING CONCATENATED KEY
      *  AND THEN DELETE IT AND ITS DEPENDENTS
      * ***************************************************************
      *
           MOVE 'A200B230' TO CONKEYB.
           EXEC DLI GU USING PCB(1)
             SEGMENT(SEGB)
                KEYS(CONKEYB) KEYLENGTH(8)
                INTO(AREAB) SEGLENGTH(80)
           END-EXEC.
           PERFORM TEST-DIB THRU OK.
           EXEC DLI DLET USING PCB(1)
             SEGMENT(SEGB) SEGLENGTH(80) FROM(AREAB) END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  RETRIEVE SEGMENT BY QUALIFYING PARENT WITH CONCATENATED KEY,
      *  OBJECT SEGMENT WITH WHERE OPTION,
      *  AND THEN SET PARENTAGE
      *
      *  USE VARIABLES FOR PCB INDEX, SEGMENT NAME, AND SEGMENT LENGTH
      * ***************************************************************
      *
           MOVE 'A200B230' TO CONKEYB.
           MOVE 'C520' TO SEGKEYC.
           MOVE 'SEGA' TO SEGNAME.
           MOVE 80 TO SEGLEN.
           MOVE 1 TO PCBNUM.
           EXEC DLI GU USING PCB(PCBNUM)
             SEGMENT((SEGNAME))
                KEYS(CONKEYB) KEYLENGTH(8) SETPARENT
             SEGMENT(SEGC) INTO(AREAC) SEGLENGTH(SEGLEN)
                WHERE(KEYC=SEGKEYC) FIELDLENGTH(4) END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  RETRIEVE DATABASE STATISTICS
      * ***************************************************************
      *
           EXEC DLI STAT USING PCB(1) INTO(AREASTAT)
                VSAM FORMATTED LENGTH(360) END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  RETRIEVE ROOT SEGMENT USING BOOLEAN OPERATORS
      * ***************************************************************
      *
           MOVE 'A050' TO SEGKEY1.
           MOVE 'A150' TO SEGKEY2.
           MOVE 'A275' TO SEGKEY3.
           MOVE 'A350' TO SEGKEY4.
           EXEC DLI GU USING PCB(1) SEGMENT(SEGA) INTO(AREAA)
                SEGLENGTH(80) FIELDLENGTH(4,4,4,4)
                WHERE(KEYA > SEGKEY1 AND KEYA < SEGKEY2 OR
                      KEYA > SEGKEY3 AND KEYA < SEGKEY4)
           END-EXEC.
           PERFORM TEST-DIB THRU OK.
      *
      * ***************************************************************
      *  TERMINATE PSB WHEN DLI PROCESSING IS COMPLETED
      * ***************************************************************
      *
             EXEC DLI TERM END-EXEC.
      *
      * ***************************************************************
      * ***************************************************************
      *  SEND OUTPUT MESSAGE
      * ***************************************************************
      *
           EXEC CICS SEND MAP('SAMPMAP') MAPSET('MAPSET') END-EXEC.
           EXEC CICS WAIT TERMINAL END-EXEC.
      *
      * ***************************************************************
      *  COMPLETE TRANSACTION AND RETURN TO CICS
      * ***************************************************************
      *
           EXEC CICS RETURN END-EXEC.
      *
      * ***************************************************************
      *  CHECK STATUS IN DIB
      * ***************************************************************
      *
       TEST-DIB.
           IF DIBSTAT EQUAL TO '  ' THEN GO TO OK.
       OK.
       ERRORS.
      *    HANDLE ERROR CONDITIONS
       ABENDS.
      *    HANDLE ABENDS INCLUDING DLI ERROR STATUS CODES