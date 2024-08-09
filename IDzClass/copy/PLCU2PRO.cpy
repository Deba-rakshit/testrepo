 /* ---------------------------------------------------        */
 /* SAMPLE PLI COPYBOOK FOR IBM PD TOOLS WORKSHOPS             */
 /* Describes Product  Records in <userid>.ADLAB.FILES(CUST2)  */
 /* USE THIS COPYBOOK IN CONJUNCTION WITH PLCU2CUS             */
 /* ---------------------------------------------------        */
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