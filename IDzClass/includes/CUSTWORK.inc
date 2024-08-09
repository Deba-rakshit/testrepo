   /*   CUSTOMER WORK RECORD LAYOUTS */
   DCL 01  WS_CUST_REC_STRING PIC '(334)X' BASED(ADDR(WS_CUST_REC));
   DCL 01  WS_CUST_REC,
           05  WS_CUST_KEY,
               10  WS_CUST_ID           PIC '(5)X',
               10  WS_CUST_REC_TYPE     PIC 'X',
           05  WS_CUST_NAME             PIC '(17)X',
           05  WS_CUST_ACCT_BALANCE     FIXED DEC(9,2),
           05  WS_CUST_ORDERS_YTD       SIGNED FIXED BINARY (31),
           05  WS_CUST_ADDR             PIC '(20)X',
           05  WS_CUST_CITY             PIC '(14)X',
           05  WS_CUST_STATE            PIC '(02)X',
           05  WS_CUST_COUNTRY          PIC '(11)X',
           05  WS_CUST_MONTH(12)        FIXED DEC(9,2),
           05  WS_CUST_OCCUPATION       PIC '(30)X',
           05  WS_CUST_NOTES            PIC '(120)X',
           05  WS_CUST_DATA_1           PIC '(05)X',
           05  WS_CUST_DATA_2           PIC '(40)X';

   DCL 01  WS_CUST_CONTACT_REC_STRING PIC '(65)X'
                                 BASED(ADDR(WS_CUST_CONTACT_REC));
   DCL 01  WS_CUST_CONTACT_REC,
           05  WS_CUST_CONTACT_KEY,
               10  WS_CUST_CONTACT_ID      PIC '(5)X',
               10  WS_CUST_CONTACT_REC_TYPE PIC 'X',
           05  WS_CUST_CONTACT_NAME     PIC '(17)X',
           05  WS_CUST_DESCRIPTION      PIC '(10)X',
           05  WS_CUST_CONTACT_INFO     PIC '(20)X',
     /*    05  WS_CUST_DATA_3           PIC '(05)X',  */
           05  WS_CUST_DATA_3           FIXED BIN(31),
     /*    05  WS_CUST_DATA_4           PIC '(05)X',  */
           05  WS_CUST_DATA_4           FIXED DEC(5,0),
           05  WS_CUST_DATA_5           PIC '(05)X';