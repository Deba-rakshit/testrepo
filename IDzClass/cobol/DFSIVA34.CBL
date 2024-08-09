 CBL  APOST                                                             00010000
       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID.  DFSIVA34.                                           00030000
      *                                                                 00040000
      ********************************************************@SCPYRT** 00050000
      *                                                               * 00051000
      *  Licensed Materials - Property of IBM                         * 00052000
      *                                                               * 00053000
      *  5655-J38                                                     * 00054000
      *                                                               * 00055000
      *  (C) Copyright IBM Corp. 1991,1998 All Rights Reserved        * 00056000
      *                                                               * 00057000
      *  US Government Users Restricted Rights - Use, duplication or  * 00080000
      *  disclosure restricted by GSA ADP Schedule contract with      * 00100000
      *  IBM Corp.                                                    * 00110000
      *                                                               * 00120000
      ********************************************************@ECPYRT** 00140000
      *                                                                 00160000
      *   APPLICATION  :  CONVERSATIONAL PROGRAM                        00170000
      *   TRANSACTION  :  IVTCB                                         00180000
      *   PSB          :  DFSIVP34                                      00190000
      *   DATABASE     :  DFSIVD2                                       00200000
      *   INPUT:                                                        00210000
      *         TELEPHONE DIRECTORY SYSTEM                              00220000
      *         PROCESS CODE : CCCCCCCC                                 00230000
      *         LAST NAME    : XXXXXXXXXX                               00240000
      *         FIRST NAME   : XXXXXXXXXX                               00250000
      *         EXTENSION#   : N-NNN-NNNN                               00260000
      *         INTERNAL ZIP : XXX/XXX                                  00270000
      *   CCCCCCCC = COMMAND                                            00280000
      *         ADD = INSERT ENTRY IN DB                                00290000
      *         DELETE = DELETE ENTRY FROM DB                           00300000
      *         UPDATE = UPDATE ENTRY FROM DB                           00310000
      *         DISPLAY = DISPLAY ENTRY                                 00320000
      *         TADD = SAME AS ADD, BUT ALSO WRITE TO OPERATOR          00330000
      *         END = TERMINATE CONVERSATION                            00340000
      *                                                                 00350000
      *       CHANGES:  THIS MODULE IS NEW IN IMS/ESA 3.2               00360000
      *  APAR...  ID  PREREQ.  DATE....  DESCRIPTION................... 00370000
      *  KNQ0115  01           11/17/91  ADD COBOL LANG VERSION         00370010
      *                                                                 00380000
                                                                        00390000
       ENVIRONMENT DIVISION.                                            00400000
       CONFIGURATION SECTION.                                           00410000
       SOURCE-COMPUTER. IBM-370.                                        00420000
       OBJECT-COMPUTER. IBM-370.                                        00430000
      *                                                                 00440000
       DATA DIVISION.                                                   00450000
       WORKING-STORAGE SECTION.                                         00460000
                                                                        00470000
      * DL/I FUNCTION CODES                                             00480000
                                                                        00490000
       77  GET-UNIQUE       PICTURE X(4)  VALUE 'GU  '.                 00500000
       77  GET-HOLD-UNIQUE  PICTURE X(4)  VALUE 'GHU '.                 00510000
       77  GET-NEXT         PICTURE X(4)  VALUE 'GN  '.                 00520000
       77  GET-HOLD-NEXT    PICTURE X(4)  VALUE 'GHN '.                 00530000
       77  DLET             PICTURE X(4)  VALUE 'DLET'.                 00540000
       77  ISRT             PICTURE X(4)  VALUE 'ISRT'.                 00550000
       77  REPL             PICTURE X(4)  VALUE 'REPL'.                 00560000
                                                                        00570000
      * DL/I CALL STATUS CODES                                          00580000
                                                                        00590000
       77  MESSAGE-EXIST    PIC X(2) VALUE 'CF'.                        00600000
       77  NO-MORE-SEGMENT  PIC X(2) VALUE 'QD'.                        00610000
       77  NO-MORE-MESSAGE  PIC X(2) VALUE 'QC'.                        00620000
                                                                        00630000
      * MESSAGES                                                        00640000
                                                                        00650000
       77  MDEL    PICTURE X(40) VALUE                                  00660000
                     'ENTRY WAS DELETED                       '.        00670000
       77  MADD    PICTURE X(40) VALUE                                  00680000
                     'ENTRY WAS ADDED                         '.        00690000
       77  MDIS    PICTURE X(40) VALUE                                  00700000
                     'ENTRY WAS DISPLAYED                     '.        00710000
       77  MUPD    PICTURE X(40) VALUE                                  00720000
                     'ENTRY WAS UPDATED                       '.        00730000
       77  MEND    PICTURE X(40) VALUE                                  00740000
                     'CONVERSATION HAS ENDED                  '.        00750000
       77  MMORE   PICTURE X(40) VALUE                                  00760000
                     'DATA IS NOT ENOUGH.  PLEASE KEY IN MORE '.        00770000
       77  MINV    PICTURE X(40) VALUE                                  00780000
                     'PROCESS CODE IS NOT VALID               '.        00790000
       77  MNODATA PICTURE X(40) VALUE                                  00800000
                     'NO DATA WAS INPUT.  PLEASE KEY IN MORE  '.        00810000
       77  MNONAME PICTURE X(40) VALUE                                  00820000
                     'LAST NAME WAS NOT SPECIFIED             '.        00830000
       77  MNOENT  PICTURE X(40) VALUE                                  00840000
                     'SPECIFIED PERSON WAS NOT FOUND          '.        00850000
       77  MISRTE  PICTURE X(40) VALUE                                  00860000
                     'ADDITION OF ENTRY HAS FAILED            '.        00870000
       77  MDLETE  PICTURE X(40) VALUE                                  00880000
                     'DELETION OF ENTRY HAS FAILED            '.        00890000
       77  MREPLE  PICTURE X(40) VALUE                                  00900000
                     'UPDATE OF ENTRY HAS FAILED              '.        00910000
                                                                        00920000
      * VARIABLES                                                       00930000
                                                                        00940000
       77  SSA1    PICTURE X(9) VALUE 'A1111111 '.                      00950000
       77  MODNAME PICTURE X(8) VALUE SPACES.                           00960000
       77  TRAN-CODE  PICTURE X(8) VALUE 'IVTCB'.                       00970000
       77  REPLY      PICTURE X(16).                                    00980000
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.                        00990000
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.                        01000000
                                                                        01010000
      * DATA AREA FOR TERMINAL INPUT                                    01020000
                                                                        01030000
       01  INPUT-MSG.                                                   01040000
           02  IN-LL          PICTURE S9(3) COMP.                       01050000
           02  IN-ZZ          PICTURE S9(3) COMP.                       01060000
           02  IN-FILL        PICTURE X(4).                             01070000
           02  IN-COMMAND     PICTURE X(8).                             01080000
           02  TEMP-COMMAND REDEFINES IN-COMMAND.                       01090000
               04  TEMP-IOCMD    PIC X(3).                              01100000
               04  TEMP-FILLER   PIC X(5).                              01110000
           02  IN-LAST-NAME   PICTURE X(10).                            01120000
           02  IN-FIRST-NAME  PICTURE X(10).                            01130000
           02  IN-EXTENSION   PICTURE X(10).                            01140000
           02  IN-ZIP-CODE    PICTURE X(7).                             01150000
                                                                        01160000
      * DATA AREA OUTPUT                                                01170000
                                                                        01180000
       01  OUTPUT-AREA.                                                 01190000
           02  OUT-LL       PICTURE S9(3) COMP VALUE +95.               01200000
           02  OUT-ZZ       PICTURE S9(3) COMP VALUE +0.                01210000
           02  OUTPUT-LINE  PICTURE X(85) VALUE SPACES.                 01220000
           02  OUTPUT-DATA REDEFINES OUTPUT-LINE.                       01230000
               04  OUT-MESSAGE   PIC X(40).                             01240000
               04  OUT-COMMAND   PIC X(8).                              01250000
               04  OUT-DATA-TYPE.                                       01260000
                   06  OUT-LAST-NAME   PIC X(10).                       01270000
                   06  OUT-FIRST-NAME  PIC X(10).                       01280000
                   06  OUT-EXTENSION   PIC X(10).                       01290000
                   06  OUT-ZIP-CODE    PIC X(7).                        01300000
           02  OUT-SEGMENT-NO  PICTURE X(4) VALUE '0001'.               01310000
                                                                        01320000
      * I/O AREA FOR DATA BASE HANDLING                                 01330000
                                                                        01340000
       01  IOAREA.                                                      01350000
           02  IO-LINE PICTURE X(37) VALUE SPACES.                      01360000
           02  IO-DATA REDEFINES IO-LINE.                               01370000
               04  IO-LAST-NAME    PIC X(10).                           01380000
               04  IO-FIRST-NAME   PIC X(10).                           01390000
               04  IO-EXTENSION    PIC X(10).                           01400000
               04  IO-ZIP-CODE     PIC X(7).                            01410000
           02  IO-FILLER       PIC X(3) VALUE SPACES.                   01420000
           02  IO-COMMAND      PIC X(8) VALUE SPACES.                   01430000
                                                                        01440000
      * SCRATCH PAD AREA                                                01450000
                                                                        01460000
       01  SPA.                                                         01470000
           02  SPA-LL        PICTURE X(2).                              01480000
           02  SPA-ZZ        PICTURE X(4).                              01490000
           02  SPA-TRANCODE  PICTURE X(8).                              01500000
           02  SPA-CALL      PICTURE X(2).                              01510000
           02  SPA-COMMAND   PICTURE X(8).                              01520000
           02  SPA-DATA.                                                01530000
               04  SPA-LAST-NAME    PIC X(10).                          01540000
               04  SPA-FIRST-NAME   PIC X(10).                          01550000
               04  SPA-EXTENSION    PIC X(10).                          01560000
               04  SPA-ZIP-CODE     PIC X(7).                           01570000
           02  FILLER        PICTURE X(19).                             01580000
                                                                        01590000
      * DC TEXT FOR ERROR CALL                                          01600000
                                                                        01610000
       01 DC-TEXT.                                                      01620000
          02  TEXT1         PIC  X(7) VALUE 'STATUS '.                  01630000
          02  ERROR-STATUS  PIC  X(2).                                  01640000
          02  TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.            01650000
          02  ERROR-CALL    PIC  X(4).                                  01660000
                                                                        01670000
      * SEGMENT SEARCH ARGUMENT                                         01680000
                                                                        01690000
       01 SSA.                                                          01700000
          02  SEGMENT-NAME  PIC X(8)  VALUE 'A1111111'.                 01710000
          02  SEG-KEY-NAME  PIC X(11) VALUE '(A1111111 ='.              01720000
          02  SSA-KEY       PIC X(10).                                  01730000
          02  FILLER        PIC X VALUE ')'.                            01740000
                                                                        01750000
      * FLAGS                                                           01760000
                                                                        01770000
       01 FLAGS.                                                        01780000
          02  SET-DATA-FLAG  PIC X VALUE '0'.                           01790000
             88  NO-SET-DATA       VALUE '1'.                           01800000
          02  TADD-FLAG      PIC X VALUE '0'.                           01810000
             88  PROCESS-TADD      VALUE '1'.                           01820000
                                                                        01830000
      * COUNTERS                                                        01840000
                                                                        01850000
       01 COUNTERS.                                                     01860000
          02  SPA-CALL-NO    PIC   9(2) COMP VALUE 0.                   01870000
          02  L-SPACE-CTR    PIC   9(2) COMP VALUE 0.                   01880000
                                                                        01890000
       LINKAGE SECTION.                                                 01900000
                                                                        01910000
       01  IOPCB.                                                       01920000
           02  LTERM-NAME   PICTURE X(8).                               01930000
           02  FILLER       PICTURE X(2).                               01940000
           02  TPSTATUS     PICTURE XX.                                 01950000
           02  FILLER       PICTURE X(20).                              01960000
       01  DBPCB.                                                       01970000
           02  DBNAME       PICTURE X(8).                               01980000
           02  SEG-LEVEL-NO PICTURE X(2).                               01990000
           02  DBSTATUS     PICTURE XX.                                 02000000
           02  FILLER       PICTURE X(20).                              02010000
                                                                        02020000
       PROCEDURE DIVISION USING IOPCB, DBPCB.                           02030000
                                                                        02040000
      * ON ENTRY IMS PASSES ADDRESSES FOR IOPCB AND DBPCB               02050000
                                                                        02060000
       MAIN-RTN.                                                        02070000
           MOVE GET-UNIQUE TO ERROR-CALL.                               02080000
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, SPA.                 02090000
           IF TPSTATUS  = '  ' OR MESSAGE-EXIST                         02100000
           THEN                                                         02110000
             CALL 'CBLTDLI' USING GET-NEXT, IOPCB, INPUT-MSG            02120000
             IF TPSTATUS = SPACES                                       02130000
               THEN PERFORM PROCESS-INPUT THRU PROCESS-INPUT-END        02140000
             ELSE IF TPSTATUS = NO-MORE-SEGMENT                         02150000
                  THEN GOBACK                                           02160000
                  ELSE                                                  02170000
                    MOVE GET-NEXT TO ERROR-CALL                         02180000
                    PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END        02190000
           ELSE IF TPSTATUS = NO-MORE-MESSAGE                           02200000
                THEN GOBACK                                             02210000
                ELSE PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.      02220000
           GOBACK.                                                      02230000
                                                                        02240000
      * PROCEDURE PROCESS-INPUT                                         02250000
                                                                        02260000
       PROCESS-INPUT.                                                   02270000
           IF IN-LL < 5                                                 02280000
             MOVE MNODATA TO OUT-MESSAGE                                02290000
             PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                02300000
                                                                        02310000
      *    CHECK THE LEADING SPACE IN INPUT COMMAND AND TRIM IT OFF     02320000
                                                                        02330000
           INSPECT IN-COMMAND TALLYING L-SPACE-CTR FOR LEADING SPACE    02340000
             REPLACING LEADING SPACE BY '*'.                            02350000
           IF L-SPACE-CTR > 0                                           02360000
             UNSTRING IN-COMMAND DELIMITED BY ALL '*' INTO TEMP-ONE     02370000
               TEMP-TWO                                                 02380000
             MOVE TEMP-TWO TO IN-COMMAND                                02390000
             MOVE 0 TO L-SPACE-CTR                                      02400000
             MOVE SPACES TO TEMP-TWO.                                   02410000
                                                                        02420000
      *    CHECK THE LEADING SPACE IN INPUT LAST NAME AND TRIM IT OFF   02430000
                                                                        02440000
           INSPECT IN-LAST-NAME TALLYING L-SPACE-CTR FOR LEADING        02450000
             SPACE REPLACING LEADING SPACE BY '*'.                      02460000
           IF L-SPACE-CTR > 0                                           02470000
             UNSTRING IN-LAST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE   02480000
               TEMP-TWO                                                 02490000
             MOVE TEMP-TWO TO IN-LAST-NAME                              02500000
             MOVE 0 TO L-SPACE-CTR                                      02510000
             MOVE SPACES TO TEMP-TWO.                                   02520000
                                                                        02530000
      *    CHECK THE LEADING SPACE IN INPUT FIRST NAME AND TRIM IT OFF  02540000
                                                                        02550000
           INSPECT IN-FIRST-NAME TALLYING L-SPACE-CTR FOR LEADING       02560000
             SPACE REPLACING LEADING SPACE BY '*'.                      02570000
           IF L-SPACE-CTR > 0                                           02580000
             UNSTRING IN-FIRST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE  02590000
               TEMP-TWO                                                 02600000
             MOVE TEMP-TWO TO IN-FIRST-NAME                             02610000
             MOVE 0 TO L-SPACE-CTR                                      02620000
             MOVE SPACES TO TEMP-TWO.                                   02630000
                                                                        02640000
      *    CHECK THE LEADING SPACE IN INPUT EXTENSION AND TRIM IT OFF   02650000
                                                                        02660000
           INSPECT IN-EXTENSION TALLYING L-SPACE-CTR FOR LEADING        02670000
             SPACE REPLACING LEADING SPACE BY '*'.                      02680000
           IF L-SPACE-CTR > 0                                           02690000
             UNSTRING IN-EXTENSION DELIMITED BY ALL '*' INTO TEMP-ONE   02700000
               TEMP-TWO                                                 02710000
             MOVE TEMP-TWO TO IN-EXTENSION                              02720000
             MOVE 0 TO L-SPACE-CTR                                      02730000
             MOVE SPACES TO TEMP-TWO.                                   02740000
                                                                        02750000
      *    CHECK THE LEADING SPACE IN INPUT ZIP CODE AND TRIM IT OFF    02760000
                                                                        02770000
           INSPECT IN-ZIP-CODE TALLYING L-SPACE-CTR FOR LEADING SPACE   02780000
             REPLACING LEADING SPACE BY '*'.                            02790000
           IF L-SPACE-CTR > 0                                           02800000
             UNSTRING IN-ZIP-CODE DELIMITED BY ALL '*' INTO TEMP-ONE    02810000
               TEMP-TWO                                                 02820000
             MOVE TEMP-TWO TO IN-ZIP-CODE                               02830000
             MOVE 0 TO L-SPACE-CTR                                      02840000
             MOVE SPACES TO TEMP-TWO.                                   02850000
      *                                                                 02860000
           MOVE IN-LAST-NAME TO IO-LAST-NAME.                           02870000
           MOVE IN-COMMAND TO IO-COMMAND.                               02880000
           IF SPA-CALL-NO = 0                                           02890000
             MOVE IN-LAST-NAME TO IO-LAST-NAME                          02900000
             MOVE IN-COMMAND TO IO-COMMAND                              02910000
           ELSE IF IN-LAST-NAME EQUAL SPACES                            02920000
                THEN MOVE SPA-LAST-NAME TO IO-LAST-NAME                 02930000
                ELSE MOVE IN-LAST-NAME TO IO-LAST-NAME.                 02940000
                                                                        02950000
           IF IN-COMMAND EQUAL SPACES                                   02960000
             MOVE SPA-COMMAND TO IO-COMMAND                             02970000
           ELSE                                                         02980000
             MOVE IN-COMMAND TO IO-COMMAND.                             02990000
                                                                        03000000
           IF IO-COMMAND EQUAL SPACES                                   03010000
           THEN MOVE MINV TO OUT-MESSAGE                                03020000
                PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END              03030000
           ELSE IF IO-LAST-NAME EQUAL SPACES AND TEMP-IOCMD NOT = 'END' 03040000
                THEN MOVE MNONAME TO OUT-MESSAGE                        03050000
                    PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END          03060000
           ELSE IF TEMP-IOCMD EQUAL 'ADD'                               03070000
                THEN PERFORM TO-ADD THRU TO-ADD-END                     03080000
           ELSE IF TEMP-IOCMD EQUAL 'TAD'                               03090000
                THEN MOVE 1 TO TADD-FLAG                                03100000
                    PERFORM TO-ADD THRU TO-ADD-END                      03110000
           ELSE IF TEMP-IOCMD EQUAL 'UPD'                               03120000
                THEN PERFORM TO-UPD THRU TO-UPD-END                     03130000
           ELSE IF TEMP-IOCMD EQUAL 'DEL'                               03140000
                THEN PERFORM TO-DEL THRU TO-DEL-END                     03150000
           ELSE IF TEMP-IOCMD EQUAL 'DIS'                               03160000
                THEN PERFORM TO-DIS THRU TO-DIS-END                     03170000
           ELSE IF TEMP-IOCMD EQUAL 'END'                               03180000
                THEN PERFORM TO-END THRU TO-END-END                     03190000
           ELSE                                                         03200000
               MOVE MINV TO OUT-MESSAGE                                 03210000
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.              03220000
       PROCESS-INPUT-END.                                               03230000
           EXIT.                                                        03240000
                                                                        03250000
      * PROCEDURE TO-ADD : ADDITION REQUEST HANDLER                     03260000
                                                                        03270000
       TO-ADD.                                                          03280000
           IF IO-LAST-NAME EQUAL SPA-LAST-NAME                          03290000
           THEN MOVE SPA-DATA TO IO-DATA.                               03300000
           IF IN-FIRST-NAME EQUAL SPACES OR                             03310000
              IN-EXTENSION EQUAL SPACES OR                              03320000
              IN-ZIP-CODE EQUAL SPACES                                  03330000
           THEN                                                         03340000
              MOVE MMORE TO OUT-MESSAGE                                 03350000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                03360000
           ELSE                                                         03370000
              MOVE IN-FIRST-NAME TO IO-FIRST-NAME                       03380000
              MOVE IN-EXTENSION  TO IO-EXTENSION                        03390000
              MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                         03400000
              MOVE IO-DATA       TO SPA-DATA                            03410000
              MOVE IO-DATA       TO OUT-DATA-TYPE                       03420000
              MOVE IO-COMMAND    TO OUT-COMMAND                         03430000
              PERFORM ISRT-DB THRU ISRT-DB-END.                         03440000
       TO-ADD-END.                                                      03450000
           EXIT.                                                        03460000
                                                                        03470000
      * PROCEDURE TO-UPD : UPDATE REQUEST HANDLER                       03480000
                                                                        03490000
       TO-UPD.                                                          03500000
           MOVE 0 TO SET-DATA-FLAG.                                     03510000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03520000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      03530000
           IF DBSTATUS = SPACES                                         03540000
           THEN                                                         03550000
             IF IN-FIRST-NAME NOT = SPACES                              03560000
               MOVE 1 TO SET-DATA-FLAG                                  03570000
               MOVE IN-FIRST-NAME TO IO-FIRST-NAME                      03580000
             END-IF                                                     03590000
             IF IN-EXTENSION  NOT = SPACES                              03600000
               MOVE 1 TO SET-DATA-FLAG                                  03610000
               MOVE IN-EXTENSION  TO IO-EXTENSION                       03620000
             END-IF                                                     03630000
             IF IN-ZIP-CODE   NOT = SPACES                              03640000
               MOVE 1 TO SET-DATA-FLAG                                  03650000
               MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                        03660000
             END-IF                                                     03670000
             MOVE IO-DATA TO OUT-DATA-TYPE.                             03680000
             MOVE IO-COMMAND TO OUT-COMMAND.                            03690000
             IF NO-SET-DATA                                             03700000
             THEN                                                       03710000
               PERFORM REPL-DB THRU REPL-DB-END                         03720000
             ELSE                                                       03730000
               MOVE MNODATA TO OUT-MESSAGE                              03740000
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.              03750000
       TO-UPD-END.                                                      03760000
           EXIT.                                                        03770000
                                                                        03780000
      * PROCEDURE TO-DEL : DELETE REQUEST HANDLER                       03790000
                                                                        03800000
       TO-DEL.                                                          03810000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03820000
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.      03830000
           IF DBSTATUS = SPACES                                         03840000
           THEN                                                         03850000
              MOVE IO-DATA TO OUT-DATA-TYPE                             03860000
              MOVE IO-COMMAND TO OUT-COMMAND                            03870000
              PERFORM DLET-DB THRU DLET-DB-END.                         03880000
       TO-DEL-END.                                                      03890000
           EXIT.                                                        03900000
                                                                        03910000
      * PROCEDURE TO-DIS : DISPLAY REQUEST HANDLER                      03920000
                                                                        03930000
       TO-DIS.                                                          03940000
           MOVE IO-LAST-NAME TO SSA-KEY.                                03950000
           PERFORM GET-UNIQUE-DB THRU GET-UNIQUE-DB-END.                03960000
           IF DBSTATUS = SPACES                                         03970000
           THEN                                                         03980000
              MOVE IO-DATA TO OUT-DATA-TYPE                             03990000
              MOVE IO-COMMAND TO OUT-COMMAND                            04000000
              MOVE MDIS TO OUT-MESSAGE                                  04010000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04020000
       TO-DIS-END.                                                      04030000
           EXIT.                                                        04040000
                                                                        04050000
      * PROCEDURE TO-END : END REQUEST HANDLER                          04060000
                                                                        04070000
       TO-END.                                                          04080000
           MOVE SPACES TO SPA-TRANCODE.                                 04090000
           MOVE MEND TO OUT-MESSAGE.                                    04100000
           PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                  04110000
       TO-END-END.                                                      04120000
           EXIT.                                                        04130000
                                                                        04140000
      * PROCEDURE ISRT-DB : DATA BASE SEGMENT INSERT REQUEST HANDLER    04150000
                                                                        04160000
       ISRT-DB.                                                         04170000
           MOVE ISRT TO ERROR-CALL.                                     04180000
           CALL 'CBLTDLI' USING ISRT, DBPCB, IOAREA, SSA1.              04190000
           IF DBSTATUS  = SPACES                                        04200000
           THEN                                                         04210000
              IF PROCESS-TADD                                           04220000
                 DISPLAY 'INSERT IS DONE, REPLY' UPON CONSOLE           04230000
      *           ACCEPT REPLY FROM CONSOLE                             04240000
                 MOVE 0 TO TADD-FLAG                                    04250000
              END-IF                                                    04260000
              MOVE MADD TO OUT-MESSAGE                                  04270000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04280000
           ELSE                                                         04290000
              MOVE MISRTE TO OUT-MESSAGE                                04300000
              MOVE DBSTATUS TO ERROR-STATUS                             04310000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04320000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04330000
       ISRT-DB-END.                                                     04340000
           EXIT.                                                        04350000
                                                                        04360000
      * PROCEDURE GET-UNIQUE-DB                                         04370000
      *    DATA BASE SEGMENT GET-UNIQUE-DB REQUEST HANDLER              04380000
                                                                        04390000
       GET-UNIQUE-DB.                                                   04400000
           MOVE GET-UNIQUE TO ERROR-CALL.                               04410000
           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB, IOAREA, SSA.         04420000
           IF DBSTATUS NOT = SPACES                                     04430000
           THEN                                                         04440000
              MOVE MNOENT TO OUT-MESSAGE                                04450000
              MOVE DBSTATUS TO ERROR-STATUS                             04460000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04470000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04480000
       GET-UNIQUE-DB-END.                                               04490000
           EXIT.                                                        04500000
                                                                        04510000
      * PROCEDURE GET-HOLD-UNIQUE-DB                                    04520000
      *    DATA BASE SEGMENT GET-HOLD-UNIQUE-DB REQUEST HANDLER         04530000
                                                                        04540000
       GET-HOLD-UNIQUE-DB.                                              04550000
           MOVE GET-HOLD-UNIQUE TO ERROR-CALL.                          04560000
           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB, IOAREA, SSA.    04570000
           IF DBSTATUS NOT = SPACES                                     04580000
           THEN                                                         04590000
              MOVE MNOENT TO OUT-MESSAGE                                04600000
              MOVE DBSTATUS TO ERROR-STATUS                             04610000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04620000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04630000
       GET-HOLD-UNIQUE-DB-END.                                          04640000
           EXIT.                                                        04650000
                                                                        04660000
      * PROCEDURE REPL-DB : DATA BASE SEGMENT REPLACE REQUEST HANDLER   04670000
                                                                        04680000
       REPL-DB.                                                         04690000
           MOVE REPL TO ERROR-CALL.                                     04700000
           CALL 'CBLTDLI' USING REPL, DBPCB, IOAREA.                    04710000
           IF DBSTATUS = SPACES                                         04720000
           THEN                                                         04730000
              MOVE MUPD TO OUT-MESSAGE                                  04740000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04750000
           ELSE                                                         04760000
              MOVE MREPLE TO OUT-MESSAGE                                04770000
              MOVE DBSTATUS TO ERROR-STATUS                             04780000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04790000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04800000
       REPL-DB-END.                                                     04810000
           EXIT.                                                        04820000
                                                                        04830000
      * PROCEDURE DLET-DB : DATA BASE SEGMENT DELETE REQUEST HANDLER    04840000
                                                                        04850000
       DLET-DB.                                                         04860000
           MOVE DLET TO ERROR-CALL.                                     04870000
           CALL 'CBLTDLI' USING DLET, DBPCB, IOAREA.                    04880000
           IF DBSTATUS = SPACES                                         04890000
           THEN                                                         04900000
              MOVE MDEL TO OUT-MESSAGE                                  04910000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                04920000
           ELSE                                                         04930000
              MOVE MDLETE TO OUT-MESSAGE                                04940000
              MOVE DBSTATUS TO ERROR-STATUS                             04950000
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END              04960000
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.               04970000
       DLET-DB-END.                                                     04980000
           EXIT.                                                        04990000
                                                                        05000000
      * PROCEDURE TERM-ROUTINE : TERMINAL ROUTINE                       05010000
                                                                        05020000
       TERM-ROUTINE.                                                    05030000
           MOVE SPACES TO MODNAME.                                      05040000
           PERFORM INSERT-SPA THRU INSERT-SPA-END.                      05050000
           IF IN-COMMAND = 'END'                                        05060000
             MOVE TRAN-CODE TO MODNAME.                                 05070000
           PERFORM INSERT-IO THRU INSERT-IO-END.                        05080000
       TERM-ROUTINE-END.                                                05090000
           EXIT.                                                        05100000
                                                                        05110000
      * PROCEDURE INSERT-SPA : SPA INSERT FOR IOPCB REQUEST HANDLER     05120000
                                                                        05130000
       INSERT-SPA.                                                      05140000
           MOVE ISRT TO ERROR-CALL.                                     05150000
           MOVE IO-DATA TO SPA-DATA.                                    05160000
           MOVE IO-COMMAND TO SPA-COMMAND.                              05170000
           ADD 1 TO SPA-CALL-NO.                                        05180000
           MOVE SPA-CALL-NO TO SPA-CALL.                                05190000
           CALL 'CBLTDLI' USING ISRT, IOPCB, SPA.                       05200000
           IF TPSTATUS NOT = SPACES                                     05210000
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.         05220000
       INSERT-SPA-END.                                                  05230000
           EXIT.                                                        05240000
                                                                        05250000
      * PROCEDURE INSERT-IO : INSERT FOR IOPCB REQUEST HANDLER          05260000
                                                                        05270000
       INSERT-IO.                                                       05280000
           MOVE ISRT TO ERROR-CALL.                                     05290000
           IF MODNAME EQUAL SPACES                                      05300000
           THEN                                                         05310000
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA             05320000
           ELSE                                                         05330000
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA, MODNAME    05340000
              MOVE SPACES TO MODNAME.                                   05350000
           IF TPSTATUS NOT = SPACES                                     05360000
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.         05370000
       INSERT-IO-END.                                                   05380000
           EXIT.                                                        05390000
                                                                        05400000
      * PROCEDURE WRITE-DC-TEXT : WRITE ERROR STATUS CODE               05410000
                                                                        05420000
       WRITE-DC-TEXT.                                                   05430000
           MOVE TPSTATUS TO ERROR-STATUS.                               05440000
           DISPLAY DC-TEXT UPON CONSOLE.                                05450000
       WRITE-DC-TEXT-END.                                               05460000
           EXIT.                                                        05470000
                                                                        05480000
                                                                        05490000