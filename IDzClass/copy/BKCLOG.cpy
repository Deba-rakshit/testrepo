      ***************************************************************** 06600000
      *                                                               * 07200000
      * MODULE NAME = DFH0CLOG                                        * 07800000
      *                                                               * 08400000
      * DESCRIPTIVE NAME = Log Layout for Sample Application          * 09000000
      *                                                               * 09600000
      ***************************************************************** 09700000
         02  LOGHDR.                                                    18000000
           03  LDAY           PIC S9(7) COMP-3.                         24000000
           03  LTIME          PIC S9(7) COMP-3.                         30000000
           03  LTERML         PIC X(4).                                 36000000
         02  LOGREC.                                                    42000000
           03  LSTAT          PIC X.                                    48000000
           03  LNUMB          PIC X(6).                                 54000000
           03  LNAME          PIC X(20).                                60000000
           03  LADDR          PIC X(20).                                66000000
           03  LPHONE         PIC X(8).                                 72000000
           03  LDATE          PIC X(8).                                 79000000
           03  LAMOUNT        PIC X(8).                                 86000000
           03  LCOMMENT       PIC X(9).                                 93000000