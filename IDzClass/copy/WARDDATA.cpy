      ******************************************************************
      * DCLGEN TABLE(DDS0001.WARD_DATA)                                *
      *        LIBRARY(DDS0001.TEST.COPYLIB(WARDDATA))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.WARD_DATA TABLE
           ( WARD_ID                        CHAR(4),
             PRIMARY_PHYSICIAN_ID           CHAR(8),
             SUPERVISE_NURSE_ID             CHAR(8),
             LOCATION                       CHAR(8),
             NUMBER_OF_BEDS                 SMALLINT,
             BASE_ROOM_CHARGE               DECIMAL(7, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.WARD_DATA                  *
      ******************************************************************
       01  DCLWARD-DATA.
           10 WARD-ID              PIC X(4).
           10 PRIMARY-PHYSICIAN-ID
              PIC X(8).
           10 SUPERVISE-NURSE-ID   PIC X(8).
           10 LOCATION             PIC X(8).
           10 NUMBER-OF-BEDS       PIC S9(4) USAGE COMP.
           10 BASE-ROOM-CHARGE     PIC S9(5)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************