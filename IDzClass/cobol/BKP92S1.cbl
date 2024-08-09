      * PROCESS CICS('COBOL3,APOST,SP')                                 00000100
      * PROCESS APOST TRUNC(BIN) NOSSRANGE  SIZE(4000K) TEST            00000200
      ***************************************************************** 00003400
       IDENTIFICATION DIVISION.                                         00003500
       PROGRAM-ID. BKP92S1.                                             00003600
       ENVIRONMENT DIVISION.                                            00003700
       DATA DIVISION.                                                   00003800
       WORKING-STORAGE SECTION.                                         00003900
       77  RESPONSE     PIC 9(9)  COMP.                                 00004600
      *                                                                 00005100
      *    NEXT FIELD TO VERIFY AMOUNTI                                 00005200
      *                                                                 00005300
       01  AMOUNTN.                                                     00005400
           03  AMOUNTN1  PIC X.                                         00005500
           03  AMOUNTN25 PIC X(4).                                      00005600
           03  AMOUNTN6  PIC X.                                         00005700
           03  AMOUNTN78 PIC X(2).                                      00005800
       01  FILEA-TEMP.       COPY BKCACCT.                              00006011
       01  LOGA.        COPY BKCLOG.                                    00006211
       LINKAGE SECTION.                                                 00006400
       01  DFHCOMMAREA.                                                 00006500
           05  FILLER-LINK.      COPY BKCLINK.                          00006602
           05  FILEA.       COPY BKCACCT.                               00006711
           05  COMM-AREA.    COPY BKCACCT.                              00006800
                                                                        00007000
       PROCEDURE DIVISION.                                              00007100
       MAINLINE.                                                        00007200
           MOVE ZERO TO RCODE.                                          00007300
           EVALUATE TRUE                                                00007400
              WHEN ACCOUNT-READ PERFORM READ-FILEA                      00007502
              WHEN ACCOUNT-UPDATE PERFORM UPDATE-FILEA                  00007602
                 THRU UPDATE-FILEA-EXIT                                 00007704
              WHEN ACCOUNT-CREATE PERFORM ADD-FILEA                     00007802
              WHEN OTHER MOVE 99 TO RCODE.                              00008004
           EXEC CICS RETURN END-EXEC.                                   00008100
      *                                                                 00008200
       READ-FILEA.                                                      00008300
           EXEC CICS READ FILE('FILEA') INTO(FILEA)                     00008511
                   RIDFLD(KEYNUM) RESP(RESPONSE) END-EXEC.              00008601
           MOVE RESPONSE TO RCODE.                                      00008711
      *                                                                 00008901
      *    FOR AN UPDATE, THE FILE RECORD IS SAVED IN COMM-AREA         00009001
      *    TO COMPARE LATER                                             00009101
      *                                                                 00009201
           IF ACCOUNT-CHANGE THEN                                       00009301
              MOVE FILEREC IN FILEA TO FILEREC IN COMM-AREA.            00009401
                                                                        00009501
                                                                        00009604
       UPDATE-FILEA.                                                    00009704
      *                                                                 00009800
      *       IF THIS IS AN UPDATE REQUEST A FILE CONTROL "READ UPDATE" 00009900
      *       READS THE EXISTING RECORD USING THE NUMBER STORED IN      00010000
      *       "COMMAREA" BY THE LAST INVOCATION OF THIS PROGRAM.        00010100
      *                                                                 00010200
           EXEC CICS READ UPDATE FILE('FILEA') INTO(FILEA-TEMP)         00010311
                   RIDFLD(NUMB IN COMM-AREA)                            00010400
                   RESP(RESPONSE)  END-EXEC.                            00010500
              IF RESPONSE NOT = DFHRESP(NORMAL)                         00010600
                  THEN MOVE RESPONSE TO RCODE                           00010704
                  GO TO UPDATE-FILEA-EXIT.                              00010811
      *                                                                 00010900
      *       IF THE CURRENT FILE RECORD IS NOT THE SAME AS THE ONE     00011000
      *       SAVED IN THE "COMMAREA" THEN ANOTHER USER HAS UPDATED THE 00011100
      *       RECORD. A WARNING MESSAGE IS DISPLAYED, WITH FIELDS FROM  00011200
      *       THE RECORD READ FROM "FILEA", FOR REENTRY OF THE UPDATES. 00011300
      *                                                                 00011400
              IF FILEREC IN FILEA-TEMP NOT = FILEREC IN COMM-AREA THEN  00011500
                 MOVE 98 TO RCODE                                       00011600
                MOVE FILEREC IN FILEA TO FILEREC IN COMM-AREA           00011701
                 GO TO UPDATE-FILEA-EXIT.                               00011800
      *                                                                 00011900
       REWRITE-FILEA.                                                   00012000
      *       FOR AN UPDATE REQUEST THE UPDATED ACCOUNT RECORD IS       00012100
      *       REWRITTEN TO "FILEA".                                     00012200
      *                                                                 00012300
              EXEC CICS REWRITE FILE('FILEA') FROM(FILEA)               00012400
                        RESP(RESPONSE) END-EXEC                         00012500
              IF RESPONSE NOT = DFHRESP(NORMAL)                         00012604
                  THEN MOVE RESPONSE TO RCODE                           00012704
                       GO TO UPDATE-FILEA-EXIT                          00012804
                 ELSE PERFORM LOG-WRITE.                                00012904
      *                                                                 00013800
       UPDATE-FILEA-EXIT.                                               00013900
           EXIT.                                                        00014000
      *                                                                 00014100
                                                                        00014200
       LOG-WRITE.                                                       00014304
      *                                                                 00014400
      *    THE RECORD FIELDS, THE DATE, THE TIME, AND THE TERMINAL      00014500
      *    IDENTIFICATION ARE MOVED TO UPDATE THE LOG RECORD AREA.      00014600
      *                                                                 00014700
           MOVE EIBDATE TO LDAY.                                        00014800
           MOVE FILEREC IN FILEA TO LOGREC.                             00014900
           MOVE EIBTIME TO LTIME.                                       00015000
           MOVE EIBTRMID TO LTERML.                                     00015100
      *                                                                 00015200
      *    THE RECORD IS WRITTEN TO THE UPDATE LOG WHICH IS A TRANSIENT 00015300
      *    DATA QUEUE.                                                  00015400
      *                                                                 00015500
           EXEC CICS WRITEQ TD QUEUE('LOGA') FROM(LOGA) LENGTH(92)      00015600
                     END-EXEC.                                          00015700
      *                                                                 00015800
      *                                                                 00015900
       ADD-FILEA.                                                       00016004
      *       TRANSACTION IS 'ADDS'. FOR AN ADD REQUEST THE NEW ACCOUNT 00016100
      *       RECORD IS WRITTEN TO "FILEA".                             00016200
      *                                                                 00016300
              EXEC CICS WRITE FILE('FILEA') FROM(FILEA)                 00016400
                   RIDFLD(NUMB IN FILEA) RESP(RESPONSE) END-EXEC.       00016501
              IF RESPONSE NOT = DFHRESP(NORMAL)                         00016604
                  THEN MOVE RESPONSE TO RCODE.                          00017404
      *                                                                 00017500
           GOBACK.                                                      00017600