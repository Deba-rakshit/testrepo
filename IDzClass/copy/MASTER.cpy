      *******************************************************************
000010***                                                               00000100
000020**** LOOKUP SUPPLR MASTER                                         00000200
000070**** 06/94 REM RE-LO-SEPAC-SUPPLR-ID AND REPLACE THOSE 4 POSITIONS00000500
000070****       WITH 2 FIELDS: RE-LO-PRIMARY-PRCE1-MGR, PIC 9(3) AND   00000500
000070****       RE-LO-IMP-EXP-IND, PIC X. REM RE-LO-SIGN-OFF-LEVELS AND00000500
000070****       REPLACE IT WITH RE-LO-CAN-MEX-IND, PIC X.              00000500
000030**** 09/94 EXPANDED.                                              00000300
000030**** 05/95 REPLACE CAN-MEX-IND WITH EDILOOKUP-IND                 00000300
000030****       ADD IMP-EXP-TYPE, MULTICURRENCY-IND, FCI-CODE          00000300
000030****                                                              00000300
000080 01  SUPPLR-MASTER-REC.                                           00000600
000090                                                                  00000700
000100     05  RE-LO-COMPANY-CODE                 PIC  X(01).           00000800
000110     05  RE-LO-SUPPLR-NO                    PIC  X(05).           00000900
000120     05  RE-LO-COMM-CHG-CODE                PIC  9(01).           00001000
000130     05  RE-LO-SUPPLR-NAME                  PIC  X(30).           00001100
000130     05  RE-LO-SUPPLR-NAME2                 PIC  X(30).           00001100
000140     05  RE-LO-ADDRESS1                     PIC  X(30).           00001200
000150     05  RE-LO-ADDRESS2                     PIC  X(30).           00001300
000160     05  RE-LO-CITY                         PIC  X(20).           00001400
000170     05  RE-LO-STATE                        PIC  X(05).           00001500
000180     05  RE-LO-ZIP-CODE                     PIC  X(09).           00001600
           05  FILLER                             REDEFINES
               RE-LO-ZIP-CODE.
               10  RE-LO-ZIP                      PIC  9(05).
               10  RE-LO-EXPANDED-ZIP             PIC  X(04).
000190     05  RE-LO-MIN-MNTHLY-COMM              PIC  9(05)    COMP-3. 00001700
000200     05  RE-LO-INDUSTRY-CODE                PIC S9(03)    COMP-3. 00001800
           05  RE-LO-COUNTRY-CODE                 PIC  X(02).
           05  RE-LO-CURRENCY-CODE                PIC  X(03).
           05  RE-LO-COLLATERAL-CODE              PIC  X(03).
           05  RE-LO-SIC                          PIC  9(04)    COMP-3.
000210     05  RE-LO-PRINT-YY                     PIC  9(01).           00001900
               88  PRINT-YY-DEFAULT               VALUE 0.
               88  PRINT-YY-PRINT-AVG-DUE-DATE    VALUE 1.
               88  PRINT-YY-SUPPRESS-NO-SALESPRC  VALUE 2.
000250     05  RE-LO-PRINT-AGEING                 PIC  9(01).           00002300
               88  PRINT-AGEING-1-LINE-PER-CUST         VALUE 0.
               88  PRINT-AGEING-DETAIL-AGEING           VALUE 1.
               88  PRINT-AGEING-PAST-DUE-1-LINE         VALUE 2.
               88  PRINT-AGEING-ALT-FOR-LOU-LEVY        VALUE 7.
               88  PRINT-AGEING-NO-PRINT                VALUE 8.
               88  PRINT-AGEING-MERCHANT-FACTORS        VALUE 9.
000320     05  RE-LO-PRINT-SALES-SUMMARY          PIC  9(01).           00002900
               88  PRINT-SALES-SUMM-DEFAULT             VALUE 0.
               88  PRINT-SALES-SUMM-QUARTERLY           VALUE 1.
               88  PRINT-SALES-SUMM-MONTHLY             VALUE 2.
               88  PRINT-SALES-SUMM-MTHLY-TRDSTYL       VALUE 3.
               88  PRINT-SALES-SUMM-QTRLY-TRDSTYL       VALUE 4.
000380     05  RE-LO-CHARGES-RATE                 PIC S9(2)V999 COMP-3. 00003500
000390     05  RE-LO-PRCE1-RATE                  PIC S9(2)V999 COMP-3.  00003600
000400     05  RE-LO-BASE-DAYS                    PIC S9(03)    COMP-3. 00003700
000410     05  RE-LO-UNIT-DAYS                    PIC S9(03)    COMP-3. 00003800
000420     05  RE-LO-NUMBER-UNITS                 PIC S9(01)    COMP-3. 00003900
000430     05  RE-LO-COMMISION-RATE               PIC      V999 COMP-3. 00004000
000440     05  RE-LO-INT-RATE-DIFFER              PIC S9(2)V999 COMP-3. 00004100
000450     05  RE-LO-CATALOG001-DAYS              PIC S9(03)    COMP-3. 00004200
000460     05  RE-LO-MINIMUM-INV-AMT              PIC S9(01)V99 COMP-3. 00004300
000470     05  RE-LO-CONTRACT-CODE                PIC  9(01).           00004400
000480     05  RE-LO-SPECIAL-AUDIT-AMT            PIC S9(05)    COMP-3. 00004500
000490     05  RE-LO-ACCT-CURR-AGE-CODE           PIC  9(01).           00004600
000500     05  RE-LO-ASSIGNMENT1                  PIC S9(03)    COMP-3. 00004700
000510     05  RE-LO-ASSIGNMENT2                  PIC S9(03)    COMP-3. 00004800
000520     05  RE-LO-ASSIGNMENT3                  PIC S9(03)    COMP-3. 00004900
000530     05  RE-LO-ASSIGNMENT4                  PIC S9(03)    COMP-3. 00005000
000540     05  RE-LO-ASSIGNMENT5                  PIC S9(03)    COMP-3. 00005100
000550     05  RE-LO-LADING-ACCT-NO               PIC  9(07)    COMP-3. 00005200
000560     05  RE-LO-TERMS-1                      PIC  X(22).           00005300
000570     05  FILLER      REDEFINES      RE-LO-TERMS-1.                00005400
000600         10  RE-LO-TERM-CODE-1              PIC  9(03).           00005700
000610         10  RE-LO-TERM-CODE-2              PIC  9(03).           00005800
000620         10  RE-LO-TERM-CODE-3              PIC  9(03).           00005900
000630         10  RE-LO-TERM-CODE-4              PIC  9(03).           00006000
000640         10  RE-LO-TERM-CODE-5              PIC  9(03).           00006100
000650         10  RE-LO-TERM-CODE-6              PIC  9(03).           00006200
000660         10  RE-LO-TERM-CODE-7              PIC  9(03).           00006300
000670         10  FILLER                         PIC  X(01).           00006400
000690     05  RE-LO-TERMS-2                      PIC  X(22).           00006600
000700     05  FILLER      REDEFINES      RE-LO-TERMS-2.                00006700
000730         10  RE-LO-TERM-CODE-8              PIC  9(03).           00007000
000740         10  RE-LO-TERM-CODE-9              PIC  9(03).           00007100
000750         10  RE-LO-TERM-CODE-10             PIC  9(03).           00007200
000760         10  RE-LO-TERM-CODE-11             PIC  9(03).           00007300
000770         10  RE-LO-TERM-CODE-12             PIC  9(03).           00007400
000780         10  RE-LO-TERM-CODE-13             PIC  9(03).           00007500
000790         10  RE-LO-TERM-CODE-14             PIC  9(03).           00007600
000800         10  FILLER                         PIC  X(01).           00007700
000820     05  RE-LO-UNPROCESSED-SALES            PIC S9(11)V99 COMP-3. 00007900
003850     05  RE-LO-MATURING-11-30               PIC S9(09)    COMP-3. 00037900
000830     05  RE-LO-MATURING-31-60               PIC S9(09)    COMP-3. 00008000
000840     05  RE-LO-MATURING-61-90               PIC S9(09)    COMP-3. 00008100
000850     05  RE-LO-MATURING-91-180              PIC S9(09)    COMP-3. 00008200
           05  RE-LO-MATURING-151-180             PIC S9(09)    COMP-3.
000860     05  RE-LO-MATURING-181                 PIC S9(09)    COMP-3. 00008300
000870     05  RE-LO-MERCHANDISE-DISPUTE          PIC S9(07)    COMP-3. 00008400
000880     05  RE-LO-OSD-CUST-DEDUCT              PIC S9(07)    COMP-3. 00008500
000890     05  RE-LO-SUPPLR-LOC1                  PIC S9(07)    COMP-3. 00008600
000900     05  RE-LO-EFFECTIVE-IR-DATE            PIC  9(07)    COMP-3. 00008700
000910     05  RE-LO-ASSIGN-AMT1                  PIC S9(07)V99 COMP-3. 00008800
000920     05  RE-LO-ASSIGN-AMT2                  PIC S9(07)V99 COMP-3. 00008900
000930     05  RE-LO-ASSIGN-AMT3                  PIC S9(07)V99 COMP-3. 00009000
000940     05  RE-LO-ASSIGN-AMT4                  PIC S9(07)V99 COMP-3. 00009100
000950     05  RE-LO-ASSIGN-AMT5                  PIC S9(07)V99 COMP-3. 00009200
000960     05  RE-LO-TRADE-STYLES                 PIC  X(03).           00009300
000970     05  RE-LO-GROUP-CODE                   PIC  9(03)    COMP-3. 00009400
000980     05  RE-LO-GROUP-CODE-A REDEFINES RE-LO-GROUP-CODE PIC XX.    00009500
000990     05  RE-LO-STOP-ADVANCE-CODE            PIC  9(01).           00009600
001000     05  RE-LO-ADVANCE-PRIORITY             PIC  9(01).           00009700
001010     05  RE-LO-SALES-PRCE1-CODE            PIC  9(01).            00009800
001020     05  RE-LO-TERMINATING-SUPPLR           PIC  9(01).           00009900
001030     05  RE-LO-TERM-MMYY                    PIC  9(04).           00010000
001040     05  RE-LO-SUPPLR-EXPIRE-MMYY           PIC  9(04).           00010100
001050     05  RE-LO-COA-CODE                     PIC  X(01).           00010200
001060     05  RE-LO-CHARGE-BACK-PRINT            PIC  9(01).           00010300
001070     05  RE-LO-TRANSFER-CODE                PIC  X(01).           00010400
               88  TXFER-DETAILS-TO-VALID-SUPPLR       VALUE '*'.
               88  PAY-COLLECTED-INSRN-FROM-FC024      VALUE 'C'.
               88  USE-CALC-AMT-ADD-MTD-INT-COMM       VALUE 'A'.
               88  PAY-COLLECTED-INSRN-FROM-FC017      VALUE 'R'.
               88  TRANSFER-CALC-AMT-TO-SUPPLR         VALUE 'T'.
               88  HOLD-TRANSFER                       VALUE 'H'.
               88  PAY-INTEREST                        VALUE 'I'.
001170     05  RE-LO-TRANSFER-SUPPLR              PIC  X(05).           00011400
001180     05  RE-LO-AQR-RATING                   PIC  9(02).           00011500
001190     05  RE-LO-AQR-OADV-STATUS-CD           PIC  X(01).                 00
001200     05  RE-LO-AQR-OADV-STRATEGY-CD         PIC  X(01).                 00
001210     05  RE-LO-REVIEW-DATE                  PIC  9(05)    COMP-3. 00011600
001220     05  RE-LO-FISCAL-DATE                  PIC  9(02).           00011700
001230     05  RE-LO-FIN-STMT-CODE                PIC  X(01).           00011800
001240     05  RE-LO-LAST-FIN-STMT-DATE           PIC  9(05)    COMP-3. 00011900
001250     05  RE-LO-WIRE-TRANSFER-FEE            PIC  999V99   COMP-3. 00012000
001260     05  RE-LO-UNPROC-SALES-X876-PCT        PIC S9V99     COMP-3. 00012100
001270     05  RE-LO-FIXED-ADJ-AMT                PIC S9(09)    COMP-3. 00012200
001280     05  RE-LO-SPECIAL-MAX-RESERVE          PIC S9(07)    COMP-3. 00012300
001290     05  RE-LO-RECEIVABLE-RESV-PCT          PIC     S9V99 COMP-3. 00012400
001300     05  RE-LO-LIABILITY-RESV-PCT           PIC     S9V99 COMP-3. 00012500
001310     05  RE-LO-NEW-SALES-AVAIL-PCT          PIC     S9V99 COMP-3. 00012600
001320     05  RE-LO-NEW-SALES-RESV-PCT           PIC     S9V99 COMP-3. 00012700
001330     05  RE-LO-CTLS-EXPIRE-DATE             PIC  9(07)    COMP-3. 00012800
001340     05  RE-LO-OVER-ADV-DIFFER              PIC S9(2)V999 COMP-3. 00012900
001350     05  RE-LO-MAT-INSRN-DIFFER             PIC S9(2)V999 COMP-3. 00013000
001360     05  RE-LO-MAX-INT-RATE                 PIC S9(2)V999 COMP-3. 00013100
001370     05  RE-LO-MIN-INT-RATE                 PIC S9(2)V999 COMP-3. 00013200
001380     05  RE-LO-NEW-SALES-AVAIL              PIC S9(07)V99 COMP-3. 00013300
001390     05  RE-LO-NET-CHGS-THIS-MO             PIC S9(07)V99 COMP-3. 00013400
001400     05  RE-LO-SPECIAL-RESERVE              PIC S9(07)V99 COMP-3. 00013500
001410     05  RE-LO-SUPPLR-RESERVE               PIC S9(09)V99 COMP-3. 00013600
001420     05  RE-LO-SALES-THIS-MO                PIC S9(13)V99 COMP-3. 00013700
001430     05  RE-LO-RETURNS-THIS-MO              PIC S9(07)V99 COMP-3. 00013800
001440     05  RE-LO-LAST-ADV-DATE                PIC  9(07)    COMP-3. 00013900
001450     05  RE-LO-LAST-ADV-AMT                 PIC S9(07)V99 COMP-3. 00014000
001460     05  RE-LO-ADVANCES-THIS-MO             PIC S9(09)V99 COMP-3. 00014100
001470     05  RE-LO-NEXT-MAT-DATE                PIC  9(07)    COMP-3. 00014200
001480     05  RE-LO-NEXT-MAT-AMT                 PIC S9(09)V99 COMP-3. 00014300
001490     05  RE-LO-ADVANCES-OUTSTAND            PIC S9(11)V99 COMP-3. 00014400
001500     05  RE-LO-ADVANCES-TODAY               PIC S9(07)V99 COMP-3. 00014500
001510     05  RE-LO-TOT-OS-SUPPLIERLOC           PIC S9(09)V99 COMP-3. 00014600
001520     05  RE-LO-SALES-YTD                    PIC S9(09)    COMP-3. 00014700
001530     05  RE-LO-RETURNS-YTD                  PIC S9(09)    COMP-3. 00014800
001540     05  RE-LO-PREV-RESV-MO1                PIC S9(07)V99 COMP-3. 00014900
001550     05  RE-LO-PREV-RESV-MO2                PIC S9(07)V99 COMP-3. 00015000
001560     05  RE-LO-PREV-RESV-MO3                PIC S9(07)V99 COMP-3. 00015100
001570     05  RE-LO-CURR-RESV-MO1                PIC S9(07)V99 COMP-3. 00015200
001580     05  RE-LO-CURR-RESV-MO2                PIC S9(07)V99 COMP-3. 00015300
001590     05  RE-LO-CURR-RESV-MO3                PIC S9(07)V99 COMP-3. 00015400
001600     05  RE-LO-FIXED-ADJ-CODE               PIC  9(01).           00015500
001610     05  RE-LO-SPLIT-MAT-INSRN              PIC  9(01).           00015600
001620     05  RE-LO-NEW-SALES-PCT-SW             PIC  9(01).           00015700
001630     05  RE-LO-FE-PERCENT                   PIC  X(01).           00015800
001670     05  RE-LO-TRANSMISSION-CODE            PIC  X(01).           00016200
               88  RECEIVE-CHARGEBACKS-FROM-FC018      VALUE '1'.
001710     05  RE-LO-ADVANCE-CONTACT              PIC  X(03).           00016600
001720     05  RE-LO-ACCOUNT-OFFICER              PIC  X(03).           00016700
001730     05  RE-LO-ACCOUNT-SPECIALIST           PIC  X(03).           00016800
001740     05  RE-LO-CUST-DED-PERCENT             PIC     S9V99 COMP-3. 00016900
               88  DEFAULT-PERCENT                VALUE +1.00.
001800**** GROUP CODE ALTERNATE INDEX KEY POS 488                       00017500
001780     05  RE-LO-ALT1-KEY.                                          00017300
001820         10  RE-LO-ALT1-GROUP-CODE          PIC  9(03)    COMP-3. 00017700
001830         10  RE-LO-ALT1-SUPPLR              PIC  X(05).           00017800
001850     05  RE-LO-LAST-IDz-DATE                PIC  9(07)    COMP-3. 00018000
001860     05  RE-LO-PRCE1-UNIT-1                PIC  9(03).            00018100
001870     05  RE-LO-PRCE1-UNIT-2                PIC  9(03).            00018200
001880     05  RE-LO-PRCE1-UNIT-3                PIC  9(03).            00018300
003220     05  RE-LO-PRCE1-UNIT-4                PIC  9(03).            00031700
003230     05  RE-LO-PRCE1-UNIT-5                PIC  9(03).            00031800
003240     05  RE-LO-PRCE1-UNIT-6                PIC  9(03).            00031900
003250     05  RE-LO-PRCE1-UNIT-7                PIC  9(03).            00032000
003260     05  RE-LO-PRCE1-UNIT-8                PIC  9(03).            00032100
003270     05  RE-LO-PRCE1-UNIT-9                PIC  9(03).            00032200
003280     05  RE-LO-PRCE1-UNIT-10               PIC  9(03).            00032300
003290     05  RE-LO-PRCE1-UNIT-11               PIC  9(03).            00032400
003300     05  RE-LO-PRCE1-UNIT-12               PIC  9(03).            00032500
           05  RE-LO-PRCE1-UNIT-IND-AT           PIC  9(03).
           05  RE-LO-PRCE1-UNIT-IND-LA           PIC  9(03).
           05  RE-LO-PRCE1-UNIT-IND-NY           PIC  9(03).
001890     05  RE-LO-FE-DAYS-CODE                 PIC  X(01).           00018400
               88  BUSINESS-DAYS                       VALUE 'B'.
               88  CALENDAR-DAYS                       VALUE 'C'.
001910     05  RE-LO-FE-LATE-APPLICATION          PIC  X(01).           00018600
001920     05  RE-LO-SALES-STORUNIT               PIC  9(03).           00018700
001930     05  RE-LO-DATE-OPENED                  PIC  9(07)    COMP-3. 00018800
001940     05  RE-LO-WREHOUSE-BILLING-CODE        PIC  X(01).           00018900
001950     05  RE-LO-INVENTORY-CTL-CODE           PIC  X(01).           00019000
001960     05  RE-LO-COMMISSION-GROUP             PIC  X(01).           00019100
001970     05  RE-LO-INTEREST-PAID                PIC S9(09)V99 COMP-3. 00019200
001980     05  RE-LO-INTEREST-CHARGED             PIC S9(09)V99 COMP-3. 00019300
001990     05  RE-LO-REGULAR-COMMISSION           PIC S9(09)V99 COMP-3. 00019400
002000     05  RE-LO-ACTUAL-COMM-YTD              PIC S9(09)V99 COMP-3. 00019500
002010     05  RE-LO-MINIMUM-ANNUAL-COMM          PIC S9(09)V99 COMP-3. 00019600
002020     05  RE-LO-COMM-DIFFERENCE-YTD          PIC S9(09)V99 COMP-3. 00019700
002060     05  RE-LO-MTD-CHARGES                  PIC S9(09)V99 COMP-3. 00020100
002070     05  RE-LO-UNPROCESSED-LOC1             PIC S9(07)    COMP-3. 00020200
002080     05  RE-LO-SHIPING-METHOD               PIC  X(05).           00020300
002110**** GROUP CODE ALTERNATE INDEX KEY POS 571                       00020600
002090     05  RE-LO-ALT3-KEY.                                          00020400
002130         10  RE-LO-ALT3-MAIL-TO-CODE        PIC  X(03).           00020800
002140         10  RE-LO-ALT3-SUPPLR              PIC  X(05).           00020900
002180**** GROUP CODE ALTERNATE INDEX KEY POS 579                       00021300
002160     05  RE-LO-ALT4-KEY.                                          00021100
002200         10  RE-LO-ALT4-RELATED-SUPPLR      PIC  X(03).           00021500
002210         10  RE-LO-ALT4-SUPPLR              PIC  X(05).           00021600
002230     05  RE-LO-INTEREST-BASIS-CODE          PIC  X(01).           00021800
               88  INTEREST-BASIS-MONTHLY              VALUE 'M'.
               88  INTEREST-BASIS-DAILY                VALUE 'D'.
002250     05  RE-LO-INTEREST-PAID-RATE-CODE      PIC  X(01).           00022000
               88  INTEREST-PAID-RATE-PRIME            VALUE 'P'.
               88  INTEREST-PAID-RATE-COMM-MIA         VALUE 'M'.
               88  INTEREST-PAID-RATE-MIA-PREM         VALUE 'S'.
002270     05  RE-LO-MIN-ANNUAL-ASSESSMENT        PIC  X(01).           00022200
               88  MIN-ANNUAL-ASSESS-QTRLY             VALUE 'Q'.
               88  MIN-ANNUAL-ASSESS-MTHLY             VALUE 'M'.
               88  MIN-ANNUAL-ASSESS-ANN               VALUE 'A'.
               88  MIN-ANNUAL-ASSESS-NONE              VALUE 'N'.
002290     05  RE-LO-COMM-YEAR-BASIS-CODE         PIC  X(01).           00022400
               88  COMM-YEAR-BASIS-CONTRACT            VALUE '1'.
               88  COMM-YEAR-BASIS-CALENDAR            VALUE '0'.
002310     05  RE-LO-NEW-BUSINESS-CODE            PIC  X(03).           00022600
002320     05  RE-LO-LPI-CODE                     PIC  X(01).           00022700
               88  LPI-WHOSL-INTEREST                  VALUE 'C'.
               88  LPI-INTEREST                        VALUE 'I'.
               88  LPI-ADJUST-COLLECT-DAYS             VALUE 'A'.
               88  LPI-NOT-APPLICABLE                  VALUE 'N'.
               88  LPI-RETAIL                          VALUE 'R'.
               88  LPI-WHOLESALE                       VALUE 'W'.
               88  LPI-EXTEND                          VALUE 'X'.
002370     05  RE-LO-LPI-PRINT-CODE               PIC  X(01).           00023200
               88  LPI-PRINT-PAPER                     VALUE 'Y'.
002390     05  RE-LO-CBI-CODE                     PIC  X(01).           00023400
               88  CBI-DOLLAR-DAYS                     VALUE 'D'.
               88  CBI-INTEREST                        VALUE 'I'.
               88  CBI-EXTEND                          VALUE 'X'.
002410     05  RE-LO-PHONE-COUNTRY                PIC  X(03).           00023600
002410     05  RE-LO-PHONE-AREA-CITY              PIC  X(03).           00023600
002410     05  RE-LO-PHONE-NUMBER                 PIC  X(09).           00023600
002420     05  RE-LO-CHARGE-BACK-DAYS             PIC S9(03)    COMP-3. 00023700
002430     05  RE-LO-ACCT-OFFICER-SECOND          PIC  X(03).           00023800
002440     05  RE-LO-NEW-INT-RATE                 PIC S99V999   COMP-3. 00023900
002450     05  RE-LO-NEW-MIA-RATE                 PIC S99V999   COMP-3. 00024000
002460     05  RE-LO-NEW-OVER-ADV-RATE            PIC S99V999   COMP-3. 00024100
002470     05  RE-LO-DR-COMMISSION-RATE           PIC S99V999   COMP-3. 00024200
002480     05  RE-LO-SPEC-CUST-COMM-RATE-A        PIC S99V999   COMP-3. 00024300
002490     05  RE-LO-SPEC-CUST-COMM-RATE-B        PIC S99V999   COMP-3. 00024400
002500     05  RE-LO-SPEC-CUST-COMM-RATE-C        PIC S99V999   COMP-3. 00024500
002510     05  RE-LO-SPEC-CUST-COMM-RATE-D        PIC S99V999   COMP-3. 00024600
002520     05  RE-LO-SPEC-CUST-COMM-RATE-E        PIC S99V999   COMP-3. 00024700
002530     05  RE-LO-SPEC-CUST-COMM-RATE-F        PIC S99V999   COMP-3. 00024800
002540     05  RE-LO-SPEC-CUST-COMM-RATE-G        PIC S99V999   COMP-3. 00024900
002550     05  RE-LO-SPEC-CUST-COMM-RATE-H        PIC S99V999   COMP-3. 00025000
002560     05  RE-LO-SPEC-CUST-COMM-RATE-I        PIC S99V999   COMP-3. 00025100
002570     05  RE-LO-SPEC-CUST-COMM-RATE-J        PIC S99V999   COMP-3. 00025200
002580     05  RE-LO-COMM-HANDLING-CHARGE         PIC S999V99   COMP-3. 00025300
002590     05  RE-LO-TERMS-3                      PIC  X(22).           00025400
002600     05  FILLER      REDEFINES      RE-LO-TERMS-3.                00025500
002630         10  RE-LO-TERM-CODE-15             PIC  9(03).           00025800
002640         10  RE-LO-TERM-CODE-16             PIC  9(03).           00025900
002650         10  RE-LO-TERM-CODE-17             PIC  9(03).           00026000
002660         10  RE-LO-TERM-CODE-18             PIC  9(03).           00026100
002670         10  RE-LO-TERM-CODE-19             PIC  9(03).           00026200
002680         10  RE-LO-TERM-CODE-20             PIC  9(03).           00026300
002690         10  RE-LO-TERM-CODE-21             PIC  9(03).           00026400
002700         10  FILLER                         PIC  X(01).           00026500
002720     05  RE-LO-TERMS-4                      PIC  X(22).           00026700
002730     05  FILLER      REDEFINES      RE-LO-TERMS-4.                00026800
002760         10  RE-LO-TERM-CODE-22             PIC  9(03).           00027100
002770         10  RE-LO-TERM-CODE-23             PIC  9(03).           00027200
002780         10  RE-LO-TERM-CODE-24             PIC  9(03).           00027300
002790         10  RE-LO-TERM-CODE-25             PIC  9(03).           00027400
002800         10  RE-LO-TERM-CODE-26             PIC  9(03).           00027500
002810         10  RE-LO-TERM-CODE-27             PIC  9(03).           00027600
002820         10  RE-LO-TERM-CODE-28             PIC  9(03).           00027700
002830         10  FILLER                         PIC  X(01).           00027800
002850     05  RE-LO-PRIMARY-PRCE1-MGR           PIC  9(03).            00028000
002850     05  RE-LO-IMP-EXP-IND                  PIC  X(01).           00028000
002860     05  RE-LO-AMT0-NUMBER                  PIC  9(09)    COMP-3. 00028100
002870     05  RE-LO-REBILL-LIMIT                 PIC  9(03).           00028200
002880     05  RE-LO-TERMINAL-ACCESS-FEE          PIC  9(03).           00028300
002940     05  RE-LO-TRANSMISSION-SUPPLR          PIC  9(01).           00028900
               88  TRANSMIT-NO                         VALUE 0.
               88  TRANSMIT-YES                        VALUE 1.
002950     05  RE-LO-TRANSMISSION-ORDERS          PIC  9(01).           00029000
002960     05  RE-LO-TRANSMISSION-SALES           PIC  9(01).           00029100
002970     05  RE-LO-COMMISSION-RATE-1            PIC  9(2)V999 COMP-3. 00029200
002980     05  RE-LO-COMMISSION-DOLLAR-1          PIC S9(07)    COMP-3. 00029300
002990     05  RE-LO-COMMISSION-RATE-2            PIC  9(2)V999 COMP-3. 00029400
003000     05  RE-LO-COMMISSION-DOLLAR-2          PIC S9(07)    COMP-3. 00029500
003010     05  RE-LO-COMMISSION-RATE-3            PIC  9(2)V999 COMP-3. 00029600
003020     05  RE-LO-COMMISSION-DOLLAR-3          PIC S9(07)    COMP-3. 00029700
003030     05  RE-LO-COMMISSION-RATE-4            PIC  9(2)V999 COMP-3. 00029800
003040     05  RE-LO-COMMISSION-DOLLAR-4          PIC S9(07)    COMP-3. 00029900
003050     05  RE-LO-LC-INVENTORY                 PIC S9(09)V99 COMP-3. 00030000
003060     05  RE-LO-LC-PIECE-GOODS               PIC S9(09)V99 COMP-3. 00030100
003070     05  RE-LO-LC-FINISHED-GOODS            PIC S9(09)V99 COMP-3. 00030200
003080     05  RE-LO-LC-LIMIT                     PIC S9(11)    COMP-3. 00030300
003090     05  RE-LO-SUPPLR-LINE                  PIC S9(11)    COMP-3. 00030400
003100     05  RE-LO-LC-ACCEPTANCES-OPEN          PIC S9(09)V99 COMP-3. 00030500
003110     05  RE-LO-LC-DOCUMENTS-CST9ING         PIC S9(09)V99 COMP-3. 00030600
003120     05  RE-LO-LC-OTHER-COLLATERAL          PIC S9(11)    COMP-3. 00030700
003130     05  RE-LO-LINE-EXPIRE-DATE             PIC  9(07)    COMP-3. 00030800
003140     05  RE-LO-INVENTORY-RESERVE            PIC      9V99 COMP-3. 00030900
003150     05  RE-LO-LC-PG-PERCENT                PIC      9V99 COMP-3. 00031000
003160     05  RE-LO-LC-FG-PERCENT                PIC      9V99 COMP-3. 00031100
003170     05  RE-LO-LIQUID-COLL-RESERVE          PIC      9V99 COMP-3. 00031200
003180     05  RE-LO-LC-GROSS-MARGIN              PIC      9V99 COMP-3. 00031300
003190     05  RE-LO-LC-DUTY-FREIGHT              PIC      9V99 COMP-3. 00031400
003200     05  RE-LO-LADING-COLR-RESERVE          PIC      9V99 COMP-3. 00031500
003210     05  RE-LO-SUPPLR-LOC1-RESERVE          PIC      9V99 COMP-3. 00031600
003310     05  RE-LO-APPROVING-AE                 PIC  X(03).           00032600
003320     05  RE-LO-LIQUID-COLLATERAL            PIC  S9(09) COMP-3.   00032700
003330     05  RE-LO-CHARGESS-PD-MTD            PIC  S9(09)V99 COMP-3.  00032800
003340     05  RE-LO-LAST-AUDIT-DATE              PIC  9(04).           00032900
003350     05  RE-LO-MULTIPLE-FACTOR-CODE         PIC  X(01).           00033000
           05  RE-LO-FACTOR-CODE                  PIC  X(07).
003360     05  RE-LO-MTD-LC-SHIPINGS            PIC  S9(09)V99 COMP-3.  00033100
003370     05  RE-LO-MTD-ACCEPT-PAID            PIC  S9(09)V99 COMP-3.  00033200
003380     05  RE-LO-MTD-INT-COMM               PIC  S9(09)V99 COMP-3.  00033300
003390     05  RE-LO-INVENTORY-DATE               PIC  9(4).            00033400
003400     05  RE-LO-YY-EXTRA-COPY                PIC  X.               00033500
003410     05  RE-LO-AGE-EXTRA-COPY               PIC  X.               00033600
003420     05  RE-LO-LIABILITY-EXTRA-COPY         PIC  X.               00033700
003430     05  RE-LO-STATEMENT-EXTRA-COPY         PIC  X.               00033800
003440     05  RE-LO-MANUFACTURING-LOC1           PIC  X.               00033900
003450     05  RE-LO-FAX-COUNTRY                  PIC  X(03).           00034000
003450     05  RE-LO-FAX-AREA-CITY                PIC  X(03).           00034000
003450     05  RE-LO-FAX-NUMBER                   PIC  X(09).           00034000
003460     05  RE-LO-RELATIONSHIPS                PIC  9(7) COMP-3.     00034100
003470     05  RE-LO-PREV-NEW-SALES-AVAIL-PCT     PIC S9V99 COMP-3.     00034200
003480     05  RE-LO-TERMS-5                      PIC  X(22).           00034300
003490     05  FILLER      REDEFINES      RE-LO-TERMS-5.                00034400
003520         10  RE-LO-TERM-CODE-29             PIC  9(03).           00034700
003530         10  RE-LO-TERM-CODE-30             PIC  9(03).           00034800
003540         10  RE-LO-TERM-CODE-31             PIC  9(03).           00034900
003550         10  RE-LO-TERM-CODE-32             PIC  9(03).           00035000
003560         10  RE-LO-TERM-CODE-33             PIC  9(03).           00035100
003570         10  RE-LO-TERM-CODE-34             PIC  9(03).           00035200
003580         10  RE-LO-TERM-CODE-35             PIC  9(03).           00035300
 03590         10  FILLER                         PIC  X(01).           00035400
003610     05  RE-LO-TERMS-6                      PIC  X(22).           00035600
003620     05  FILLER      REDEFINES      RE-LO-TERMS-6.                00035700
003650         10  RE-LO-TERM-CODE-36             PIC  9(03).           00036000
003660         10  RE-LO-TERM-CODE-37             PIC  9(03).           00036100
003670         10  RE-LO-TERM-CODE-38             PIC  9(03).           00036200
003680         10  RE-LO-TERM-CODE-39             PIC  9(03).           00036300
003690         10  RE-LO-TERM-CODE-40             PIC  9(03).           00036400
003700         10  RE-LO-TERM-CODE-41             PIC  9(03).           00036500
003710         10  RE-LO-TERM-CODE-42             PIC  9(03).           00036600
003720         10  FILLER                         PIC  X(01).           00036700
003740     05  RE-LO-COMMISSION-SALES-MTD         PIC S9(9)V99  COMP-3. 00036900
003750     05  RE-LO-COMMISSION-SALES-YTD         PIC S9(9)V99  COMP-3. 00037000
003760     05  RE-LO-EDILOOKUP-IND             PIC  X.                  00037100
003770     05  RE-LO-BILL-N-HOLD-SALES            PIC  X.               00037200
003780     05  RE-LO-ALT5-KEY.                                          000373
003790         10  RE-LO-ALT5-ADVANCE-GROUP-CODE  PIC X(3).             00037400
003800         10  RE-LO-ALT5-SUPPLR              PIC X(5).             00037400
003810     05  RE-LO-ORDER-AUTO-APPROVAL          PIC S9(5)  COMP-3.    00037500
003820     05  RE-LO-CHARGE-OFF-DEDUCTABLE        PIC S9(5)  COMP-3.    00037600
003830     05  RE-LO-MATURED-NET-SALES            PIC S9(9)V99  COMP-3. 00037700
003840     05  RE-LO-TERM-ACCESS-FEE              PIC  9(04).           00037800
003860     05  RE-LO-INVENTORY-LOAN-LIMIT         PIC S9(11)    COMP-3. 00038000
003870     05  RE-LO-MTD-DF-CF                    PIC S9(9)V99  COMP-3. 00038100
003880     05  RE-LO-CANADIAN-ACCORD-IND          PIC X(01).            00038200
003890     05  RE-LO-INT-ON-COMM-IND              PIC X(01).            00038201
003900     05  RE-LO-TRANSMISSION-IND             PIC X(01).            00038202
003910     05  RE-LO-CS-NUMBER-IND                PIC X(01).            00038203
003920     05  RE-LO-NON-NOTIF-CR-IND             PIC X(01).            00038204
003930     05  RE-LO-NON-NOTIF-AR-IND             PIC X(01).            00038205
003940     05  RE-LO-LOCATION-CODE                PIC 9(01).            00038210
           05  RE-LO-ORDER-RATE                   PIC S9V9(06)  COMP-3.
           05  RE-LO-TAX-ID                       PIC 9(10)     COMP-3.
           05  RE-LO-DO-NOT-POST                  PIC X(01).
           05  RE-LO-SPEC-SRCHGE-MTD              PIC S9(07)V99 COMP-3.
           05  RE-LO-SPEC-SRCHGE-YTD              PIC S9(09)V99 COMP-3.
           05  RE-LO-DO-NOT-OWN-ASSET             PIC X(01).
           05  RE-LO-SALES-TAX-IND                PIC X(01).
           05  RE-LO-EFF-START-DATE               PIC 9(08)     COMP-3.
           05  RE-LO-EFF-COMMISSION-DATE          PIC 9(08)     COMP-3.
           05  RE-LO-AV-LAST-VERIFY-DT            PIC 9(08)     COMP-3.
           05  RE-LO-AV-FORCE-OR-EXCL-VERIFY      PIC X(01).
           05  RE-LO-CUST-STAT-PRINT-CODE         PIC X(01).
           05  RE-LO-AV-ADV-GP-STAT-CHG-DT        PIC X(08).
           05  RE-LO-AV-ADV-GP-STAT-CHG-BY        PIC X(08).
           05  RE-LO-IMP-EXP-TYPE                 PIC 9(01).
           05  RE-LO-MULTI-CURRENCY-IND           PIC X(01).
           05  RE-LO-FCI-CODE                     PIC X(07).
           05  RE-LO-FILLER                       PIC X(41).
003950                                                                  00038300
003960 01  RE-LO-REC-LENGTH                       PIC S9(4)     COMP    00038400
003970                                            VALUE +1200.          00038500