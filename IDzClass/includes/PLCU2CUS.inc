 /* ---------------------------------------------------        */
 /* SAMPLE PLI COPYBOOK FOR IBM PD TOOLS WORKSHOPS             */
 /* Describes Customer Records in <userid>.ADLAB.FILES(CUST2)  */
 /* USE THIS COPYBOOK IN CONJUNCTION WITH PLCU2CUS             */
 /* ---------------------------------------------------        */
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