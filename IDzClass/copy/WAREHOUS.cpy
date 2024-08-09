000980**** MASTER FILE CHANGE - WREHOUSE                                00009300
000990                                                                  00009400
001000 01  PLATFRM-IDz-WREHOUSE-REC               REDEFINES             00009500
001010     BILL-LADING-REC.                                             00009700
001020                                                                  00009800
001030     05  IDz001-CUST-TRAN-CODE              PIC  9(03).           00009900
001040     05  IDz001-WREHOUSE-NUMBER             PIC  9(07).           00010000
001050     05  IDz001-SUPPLR-NUMBER               PIC  X(05).           00010400
001060     05  IDz001-DATA.                                             00010500
001070                                                                  00010600
001080         10  IDz001-FIELD-NO                PIC  9(03).           00010700
001090         10  IDz001-BUYR-CREDIT            PIC  X(07).            00010800
001100         10      FILLER                     PIC  X(65).           00010900
001110                                                                  00011000
001120     05  IDz001-038                         REDEFINES             00011100
001130         IDz001-DATA.                                             00011200
001140                                                                  00011300
001150         10  IDz001-TERRITORY               PIC  9(02).           00011400
001160         10      FILLER                     PIC  X(03).           00011500
001170         10  IDz001-PRCE1-LINE             PIC  9(07).            00011600
001180         10  IDz001-PRCE1-LINE-CODE        PIC  9(01).            00011700
001190         10  IDz001-ORDER-MAX               PIC  9(06).           00011800
001200         10      FILLER                     PIC  X(06).           00011900
001210         10  IDz001-MAX-TERM-DAYS           PIC  9(03).           00012000
001220         10  IDz001-COMMON-ACCT             PIC  9(07).           00012100
001230         10      FILLER                     PIC  X(40).           00012200
001240                                                                  00012300
001250     05  IDz001-023                         REDEFINES             00012400
001260         IDz001-DATA.                                             00012500
001270                                                                  00012600
001280         10  IDz001-23-FIELD-NO             PIC  9(03).           00012700
001290         10  IDz001-23-BUYR-CREDIT         PIC  X(20).            00012800
001300         10      FILLER                     PIC  X(34).           00012900
001310         10  IDz001-OFFICER                 PIC  X(03).           00013000
001320         10  IDz001-TERM-ID                 PIC  X(04).           00013100
001330         10      FILLER                     PIC  X(11).           00013200
001340                                                                  00013300
001350     05  IDz001-011                         REDEFINES             00013400
001360         IDz001-DATA.                                             00013500
001370                                                                  00013600
001380         10  IDz001-NAME1                   PIC  X(30).           00013700
001390         10  IDz001-NAME2                   PIC  X(30).           00013800
001400         10      FILLER                     PIC  X(15).           00013900
001410                                                                  00014000
001420     05  IDz001-012                         REDEFINES             00014100
001430         IDz001-DATA.                                             00014200
001440                                                                  00014300
001450         10  IDz001-ADDR                    PIC  X(30).           00014400
001460         10  IDz001-CITY                    PIC  X(20).           00014500
001470         10  IDz001-STATE                   PIC  X(05).           00014600
001480         10  IDz001-ZIP                     PIC  9(05).           00014700
001490         10      FILLER                     PIC  X(15).           00014800
001500                                                                  00014900
001510     05  IDz001-013                         REDEFINES             00015000
001520         IDz001-DATA.                                             00015100
001530                                                                  00015200
001540         10  IDz001-STATE-CODE              PIC  9(02).           00015300
001550         10  IDz001-13-TERRITORY            PIC  9(01).           00015400
001560         10  IDz001-INDUSTRY-CODE           PIC  9(03).           00015500
001570         10  IDz001-DANDB-NO                PIC  9(09).           00015600
001580         10  IDz001-DANDB-RATE              PIC  X(03).           00015700
001590         10  IDz001-DANDB-DATE              PIC  9(06).           00015800
001600         10  IDz001-COLR-CODE               PIC  X(01).           00015900
001610         10  IDz001-LOC1-CODE               PIC  X(01).           00016000
001620         10  IDz001-TYPE-CODE               PIC  X(01).           00016100
001630         10  IDz001-FILE-CODE               PIC  X(01).           00016200
001640         10  IDz001-LIST-NO                 PIC  9(07).           00016300
001650         10  IDz001-CUST-PRCE1-LIMIT       PIC  9(08).            00016400
001660         10  IDz001-PRCE1-UNIT             PIC  9(03).            00016500
001670         10  IDz001-COLLECTOR-CODE          PIC  9(02).           00016600
001680         10      FILLER                     PIC  X(27).           00016700
001690                                                                  00016800
001700**** IKD, KIU MATERIAL ENTRY TO RESERVE                           00016901