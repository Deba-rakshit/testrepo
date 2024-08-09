      ******************************************************************
      * DCLGEN TABLE(DDS0001.HEALTH_PLAN)                              *
      *        LIBRARY(DDS0001.TEST.COPYLIB(HLTHPLAN))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.HEALTH_PLAN TABLE
           ( PLAN_ID                        CHAR(20) NOT NULL,
             GROUP_ID                       CHAR(10) NOT NULL,
             PROVIDER                       CHAR(8) NOT NULL,
             DEDUCTIBLE                     DECIMAL(5, 2) NOT NULL,
             COPAYMENT                      DECIMAL(5, 2) NOT NULL,
             CO_INSURANCE                   SMALLINT NOT NULL,
             COVERAGE_LIMITS                DECIMAL(7, 2) NOT NULL,
             OOP_MAX                        DECIMAL(5, 2) NOT NULL,
             IN_NETWORK_REQ                 CHAR(1) NOT NULL,
             PRIOR_AUTHORIZATION            CHAR(1) NOT NULL,
             EXCLUSIONS                     CHAR(254) NOT NULL,
             PLAN_COMMENTS                  CHAR(100) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.HEALTH_PLAN                *
      ******************************************************************
       01  DCLHEALTH-PLAN.
           10 PLAN-ID              PIC X(20).
           10 GROUP-ID             PIC X(10).
           10 PROVIDER             PIC X(8).
           10 DEDUCTIBLE           PIC S9(3)V9(2) USAGE COMP-3.
           10 COPAYMENT            PIC S9(3)V9(2) USAGE COMP-3.
           10 CO-INSURANCE         PIC S9(4) USAGE COMP.
           10 COVERAGE-LIMITS      PIC S9(5)V9(2) USAGE COMP-3.
           10 OOP-MAX              PIC S9(3)V9(2) USAGE COMP-3.
           10 IN-NETWORK-REQ       PIC X(1).
           10 PRIOR-AUTHORIZATION  PIC X(1).
           10 EXCLUSIONS           PIC X(254).
           10 PLAN-COMMENTS        PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************