      ******************************************************************
      * DCLGEN TABLE(DDS0001.PROVIDER)                                 *
      *        LIBRARY(DDS0001.TEST.COPYLIB(PROVIDER))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.PROVIDER TABLE
           ( PROVIDER_ID                    CHAR(8) NOT NULL,
             NETWORK_FLAG                   CHAR(1) NOT NULL,
             COST_OVERRIDE_PCT              SMALLINT NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.PROVIDER                   *
      ******************************************************************
       01  DCLPROVIDER.
           10 PROVIDER-ID          PIC X(8).
           10 NETWORK-FLAG         PIC X(1).
           10 COST-OVERRIDE-PCT    PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************