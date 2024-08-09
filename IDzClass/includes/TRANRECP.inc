  DCL  TRANSACTION_RECORD_DATA   PIC '(80)X'
                                      BASED(ADDR(TRANSACTION_RECORD));

  DCL  01  TRANSACTION_RECORD,
           05  TRAN_CODE                  PIC '(6)X',
           05  *                          PIC 'X',
           05  TRAN_PARMS,
               10  TRAN_KEY               PIC '(10)X',
               10  *                      PIC 'X',
               10  TRAN_ACTION            PIC '(8)X',
               10  *                      PIC 'X',
               10  TRAN_FIELD_NAME        PIC '(10)X',
               10  *                      PIC 'X',
               10  TRAN_FIELD_SS          PIC '(2)9',
               10  *                      PIC 'X',
               10  TRAN_UPDATE_DATA       PIC '(39)X';

  DCL  01 TRAN_CODE_PARTS BASED(ADDR(TRAN_CODE)),
               10  TRAN_COMMENT       PIC 'X',
               10  *                  PIC '(5)X';

  DCL  01  TRAN_UPDATE_DATA_1 BASED(ADDR(TRAN_UPDATE_DATA)),
               15  TRAN_UPDATE_NUM        FIXED DEC(9,2),
               15  *                      PIC '(30)X';

  DCL  01  TRAN_UPDATE_DATA_2 BASED(ADDR(TRAN_UPDATE_DATA)),
               15  TRAN_UPDATE_NUMX,
                   20  TRAN_UPDATE_NUMX1      PIC 'X',
                   20  TRAN_UPDATE_NUMX2_7    PIC '(6)X',
               15  TRAN_UPDATE_NUM_HH     PIC '(2)9',
               15  *                      PIC '(30)X';

  DCL  01  CRUNCH_PARMS   BASED(ADDR(TRAN_PARMS)),
           10  CRUNCH_IO_LOOPS        PIC '(5)9',
           10  *                      PIC 'X',
           10  CRUNCH_CPU_LOOPS       PIC '(9)9',
           10  *                      PIC '(58)X';