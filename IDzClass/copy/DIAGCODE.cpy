      ******************************************************************
      * DCLGEN TABLE(DDS0001.DIAG_CODES)                               *
      *        LIBRARY(DDS0001.TEST.COPYLIB(DIAGCODE))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.DIAG_CODES TABLE
           ( DIAG_CODE                      CHAR(5) NOT NULL,
             INS_TYPE                       CHAR(3) NOT NULL,
             COPAY                          SMALLINT NOT NULL,
             DEDUCTIBLE                     SMALLINT NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.DIAG_CODES                 *
      ******************************************************************
       01  DCLDIAG-CODES.
           10 DIAG-CODE            PIC X(5).
           10 INS-TYPE             PIC X(3).
           10 COPAY                PIC S9(4) USAGE COMP.
           10 DEDUCTIBLE           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************