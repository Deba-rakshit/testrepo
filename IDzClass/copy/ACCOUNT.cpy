000640****       ACCOUNT CURRENT ENTRIES                                00006200
000650****       SALES-AVAIL-ADJ FIELDS                                 00006210
000660****       INTEREST FIELDS                                        00006220
000670                                                                  00006300
000680 01  PLATFRM-ACCOUNT-CURRENT-REC            REDEFINES             00006400
000690     BILL-LADING-REC.                                             00006500
000700                                                                  00006600
000710     05  IDz987-TRAN-CODE                   PIC  9(03).           00006700
000720     05  IDz987-PRICE-NUMBER                PIC  9(03).           00006800
000730     05      FILLER                         PIC  X(07).           00006900
000740     05  IDz987-SUPPLR-NUMBER               PIC  X(05).           00007000
000750     05  IDz987-ACCT-NUMBER                 PIC  9(15) COMP-3.    00007100
000760     05  IDz987-ENTRY-NUMBER                PIC  9(03).           00007200
000770     05  FILLER                             REDEFINES             00007300
000780         IDz987-ENTRY-NUMBER.                                     00007400
000790         10  IDz987-BANK-NUMBER             PIC 9(03).            00007500
000800     05  IDz987-ENTRY-DATE                  PIC  9(06).           00007600
000810     05  FILLER                             REDEFINES             00007700
000820         IDz987-ENTRY-DATE.                                       00007800
000830         10  IDz987-PRICE-DATE              PIC 9(06).            00007900
000840     05      FILLER                         PIC  X(05).           00008000
000850     05  IDz987-APPR-AMOUNT                 PIC S9(07)V99.        00008100
000860     05      FILLER                         REDEFINES             00008200
000870         IDz987-APPR-AMOUNT.                                      00008300
000880                                                                  00008400
000890         10  IDz987-APPR-AMOUNT-PACKED      PIC S9(09)V99 COMP-3. 00008500
000900         10      FILLER                     PIC  X(03).           00008600
000910                                                                  00008700
000920     05  IDz987-INTEREST                    PIC  9(05)V99.        00008800
000930     05  IDz987-SALES-AVAIL-ADJ             PIC  9(06)V99.        00008900
000940     05  IDz987-DESCRIPTION                 PIC  X(20).
000950     05      FILLER                         PIC  X(06).           00009000
000960                                                                  00009100