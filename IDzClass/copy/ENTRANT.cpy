      ******************************************************************
      * DCLGEN TABLE(DDS0001.ENTRANTS)                                 *
      *        LIBRARY(DDS0001.TEST.COPYLIB(ENTRANT))                  *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(ENTRANTS)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.ENTRANTS TABLE
           ( NAME                           CHAR(20) NOT NULL,
             ENTRANT_AGE                    SMALLINT NOT NULL,
             SEX                            CHAR(1) NOT NULL,
             ADDR                           CHAR(20) NOT NULL,
             BRACKET                        CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.ENTRANTS                   *
      ******************************************************************
       01  ENTRANTS.
      *    *************************************************************
           10 NAME                 PIC X(20).
      *    *************************************************************
           10 ENTRANT-AGE          PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 SEX                  PIC X(1).
      *    *************************************************************
           10 ADDR                 PIC X(20).
      *    *************************************************************
           10 BRACKET              PIC X(1).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  IENTRANTS.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 5 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************