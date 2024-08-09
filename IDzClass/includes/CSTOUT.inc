   /*   CUSTOMER OUTPUT RECORD LAYOUTS */
   DCL 01  CSTOUT_REC_STRING PIC '(334)X' BASED(ADDR(CSTOUT_REC));
   DCL 01  CSTOUT_REC,
           05  CSTOUT_KEY,
               10  CSTOUT_ID            PIC '(5)X',
               10  CSTOUT_REC_TYPE      PIC 'X',
           05  CSTOUT_NAME              PIC '(17)X',
           05  CSTOUT_ACCT_BALANCE      FIXED DEC(9,2),
           05  CSTOUT_ORDERS_YTD        SIGNED FIXED BINARY (31),
           05  CSTOUT_ADDR              PIC '(20)X',
           05  CSTOUT_CITY              PIC '(14)X',
           05  CSTOUT_STATE             PIC '(02)X',
           05  CSTOUT_COUNTRY           PIC '(11)X',
           05  CSTOUT_MONTH(12)         FIXED DEC(9,2),
           05  CSTOUT_OCCUPATION        PIC '(30)X',
           05  CSTOUT_NOTES             PIC '(120)X',
           05  CSTOUT_DATA_1            PIC '(05)X',
           05  CSTOUT_DATA_2            PIC '(40)X';

   DCL 01  CSTOUT_CONTACT_REC_STRING  PIC '(65)X'
                                 BASED(ADDR(CSTOUT_CONTACT_REC));
   DCL 01  CSTOUT_CONTACT_REC,
           05  CSTOUT_CONTACT_KEY,
               10  CSTOUT_CONTACT_ID       PIC '(5)X',
               10  CSTOUT_CONTACT_REC_TYPE PIC 'X',
           05  CSTOUT_CONTACT_NAME      PIC '(17)X',
           05  CSTOUT_DESCRIPTION       PIC '(10)X',
           05  CSTOUT_CONTACT_INFO      PIC '(20)X',
     /*    05  CSTOUT_DATA_3            PIC '(05)X',  */
           05  CSTOUT_DATA_3            FIXED BIN(31),
     /*    05  CSTOUT_DATA_4            PIC '(05)X',  */
           05  CSTOUT_DATA_4            FIXED DEC(5,0),
           05  CSTOUT_DATA_5            PIC '(05)X';