      ******************************************************************
      * DCLGEN TABLE(DDS0001.HOSP_BED)                                 *
      *        LIBRARY(DDS0001.TEST.COPYLIB(HOSPBED))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.HOSP_BED TABLE
           ( BED_ID                         CHAR(4),
             ROOM_ID                        CHAR(4),
             WARD_ID                        CHAR(4),
             SPECIAL_CHARGES                DECIMAL(7, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.HOSP_BED                   *
      ******************************************************************
       01  DCLHOSP-BED.
           10 BED-ID               PIC X(4).
           10 ROOM-ID              PIC X(4).
           10 WARD-ID              PIC X(4).
           10 SPECIAL-CHARGES      PIC S9(5)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************