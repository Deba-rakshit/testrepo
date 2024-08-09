 /* ---------------------------------------------------        */
 /* SAMPLE PLI COPYBOOK FOR IBM PD TOOLS WORKSHOPS             */
 /* Describes SYSTEM  Records in <userid>.ADLAB.FILES(CUST2)  */
 /* USE THIS COPYBOOK IN CONJUNCTION WITH PLCU2CUS             */
 /* ---------------------------------------------------        */
 DCL 1 SYSTEM_RECORD,
       2 SYSTEM_KEY,
         3 CUST_ID1         CHAR(5),
         3 RECORD_TYPE1     CHAR(1),
         3 SYSTEM_ID      CHAR(7),
       2 SYSTEM_NAME      CHAR(25),
       2 DATE_PURCHASE    CHAR(10),
       2 SERVICE_CALL     FIXED BINARY(15,0) SIGNED UNALIGNED,
       2 LAST_SERVICE_CALL CHAR(10),
       2 *                 CHAR(20);