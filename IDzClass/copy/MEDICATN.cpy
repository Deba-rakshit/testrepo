      ******************************************************************
      * DCLGEN TABLE(DDS0001.MEDICATION)                               *
      *        LIBRARY(DDS0001.TEST.COPYLIB(MEDICATN))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.MEDICATION TABLE
           ( MEDICATION_ID                  CHAR(8),
             MED_NAME                       CHAR(40),
             SHORT_DESCRIPTION              CHAR(100),
             COST                           DECIMAL(7, 2),
             PHARMACY_COST                  DECIMAL(5, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.MEDICATION                 *
      ******************************************************************
       01  DCLMEDICATION.
           10 MEDICATION-ID        PIC X(8).
           10 MED-NAME             PIC X(40).
           10 SHORT-DESCRIPTION    PIC X(100).
           10 COST                 PIC S9(5)V9(2) USAGE COMP-3.
           10 PHARMACY-COST        PIC S9(3)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************