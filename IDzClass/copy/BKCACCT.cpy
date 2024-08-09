      ***************************************************************** 08800000
      *                                                               * 09600000
      * MODULE NAME = DFH0CFIL                                        * 10400000
      *                                                               * 11200000
      * DESCRIPTIVE NAME = File Layout for Sample Application         * 12000000
      *                                                               * 12800002
      ***************************************************************** 23100000
         10  FILEREC.                                                   24000001
           20  STAT           PIC X.                                    32000001
           20  NUMB           PIC X(6).                                 40000001
           20  NAME           PIC X(20).                                48000001
           20  ADDRX          PIC X(20).                                56000001
           20  PHONE          PIC X(8).                                 64000001
           20  DATEX          PIC X(8).                                 73000001
           20  AMOUNT         PIC X(8).                                 82000001
           20  COMMENT        PIC X(9).                                 91000001