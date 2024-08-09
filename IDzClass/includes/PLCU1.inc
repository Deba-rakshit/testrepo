 /*   ---------------------------------------------------   */
 /*   SAMPLE PLI COPYBOOK FOR IBM PD TOOLS WORKSHOPS        */
 /*   DESCRIBES FILE <USERID>.ADLAB.CUST1                   */
 /*   ---------------------------------------------------   */
 DCL 1 CUST_REC,
       2 CUSTOMER_KEY,
         3 CUST_ID         CHAR(5),
       2 NAME              CHAR(17),
       2 ACCT_BALANCE      FIXED DEC (9,2) UNALIGNED,
       2 ORDERS_YTD        FIXED BINARY(15,0) SIGNED UNALIGNED,
       2 ADDR              CHAR(20),
       2 CITY              CHAR(14),
       2 STATE             CHAR(2),
       2 COUNTRY           CHAR(11),
       2 MONTH             FIXED DEC(9,2) UNALIGNED DIMENSION(12),
       2 OCCUPATION        CHAR(30),
       2 NOTES             CHAR(120),
       2 LAB_DATA1         CHAR(5),
       2 LAB_DATA2         CHAR(40);