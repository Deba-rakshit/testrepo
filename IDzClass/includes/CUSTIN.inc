   /*   CUSTOMER INPUT RECORD LAYOUTS */
   DCL 01  CUSTIN_REC_STRING PIC '(334)X' BASED(ADDR(CUSTIN_REC));
   DCL 01  CUSTIN_REC,
           05  CUSTIN_KEY,
               10  CUSTIN_ID            PIC '(5)X',
               10  CUSTIN_REC_TYPE      PIC 'X',
           05  CUSTIN_NAME              PIC '(17)X',
           05  CUSTIN_ACCT_BALANCE      FIXED DEC(9,2),
           05  CUSTIN_ORDERS_YTD        SIGNED FIXED BINARY (31),
           05  CUSTIN_ADDR              PIC '(20)X',
           05  CUSTIN_CITY              PIC '(14)X',
           05  CUSTIN_STATE             PIC '(02)X',
           05  CUSTIN_COUNTRY           PIC '(11)X',
           05  CUSTIN_MONTH(12)         FIXED DEC(9,2),
           05  CUSTIN_OCCUPATION        PIC '(30)X',
           05  CUSTIN_NOTES             PIC '(120)X',
           05  CUSTIN_DATA_1            PIC '(05)X',
           05  CUSTIN_DATA_2            PIC '(40)X';

   DCL 01  CUSTIN_CONTACT_REC_STRING  PIC '(65)X'
                                 BASED(ADDR(CUSTIN_CONTACT_REC));
   DCL 01  CUSTIN_CONTACT_REC,
           05  CUSTIN_CONTACT_KEY,
               10  CUSTIN_CONTACT_ID       PIC '(5)X',
               10  CUSTIN_CONTACT_REC_TYPE PIC 'X',
           05  CUSTIN_CONTACT_NAME      PIC '(17)X',
           05  CUSTIN_DESCRIPTION       PIC '(10)X',
           05  CUSTIN_CONTACT_INFO      PIC '(20)X',
     /*    05  CUSTIN_DATA_3            PIC '(05)X',  */
           05  CUSTIN_DATA_3            FIXED BIN(31),
     /*    05  CUSTIN_DATA_4            PIC '(05)X',  */
           05  CUSTIN_DATA_4            FIXED DEC(5,0),
           05  CUSTIN_DATA_5            PIC '(05)X';