 /*   ---------------------------------------------------   */
 /*   SAMPLE PLI COPYBOOK FOR IBM PD TOOLS WORKSHOPS        */
 /*   DESCRIBES FILE <USERID>.ADLAB.FILES(CUST2)            */
 /*   ---------------------------------------------------   */
 DCL 1 CUSTOMER_RECORD,
       2 CUSTOMER_KEY,
         3 CUST_ID         CHAR(5),
         3 RECORD_TYPE     CHAR(1),
         3 *               CHAR(7),
       2 NAME              CHAR(17),
       2 ACCT_BALANCE      FIXED DEC (9,2) UNALIGNED,
       2 ORDERS_YTD        FIXED BINARY(15,0) SIGNED UNALIGNED,
       2 CITY              CHAR(15),
       2 OCCUPATION        CHAR(28);
 DCL 1 PRODUCT_RECORD,
       2 PRODUCT_KEY,
         3 CUST_ID         CHAR(5),
         3 RECORD_TYPE     CHAR(1),
         3 PRODUCT_ID      CHAR(7),
       2 PRODUCT_NAME      CHAR(25),
       2 DATE_PURCHASED    CHAR(10),
       2 SERVICE_CALLS     FIXED BINARY(15,0) SIGNED UNALIGNED,
       2 LAST_SERVICE_CALL CHAR(10),
       2 *                 CHAR(20);