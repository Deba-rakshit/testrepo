      ******************************************************************
      * DCLGEN TABLE(DDS0001.ROOM_DATA)                                *
      *        LIBRARY(DDS0001.TEST.COPYLIB(ROOMDATA))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.ROOM_DATA TABLE
           ( WARD_ID                        CHAR(4),
             ROOM_ID                        CHAR(4),
             PRIVATE                        SMALLINT,
             SEMI_PRIVATE                   SMALLINT,
             NUMBER_OF_BEDS                 SMALLINT,
             SPECIAL_EQUIPMENT              CHAR(255)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.ROOM_DATA                  *
      ******************************************************************
       01  DCLROOM-DATA.
           10 WARD-ID              PIC X(4).
           10 ROOM-ID              PIC X(4).
           10 PRIVATE              PIC S9(4) USAGE COMP.
           10 SEMI-PRIVATE         PIC S9(4) USAGE COMP.
           10 NUMBER-OF-BEDS       PIC S9(4) USAGE COMP.
           10 SPECIAL-EQUIPMENT    PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************