       01  ACCTMNUI.
           02  FILLER PIC X(12).
           02  SNAMEML    COMP  PIC  S9(4).
           02  SNAMEMF    PICTURE X.
           02  FILLER REDEFINES SNAMEMF.
             03 SNAMEMA    PICTURE X.
           02  SNAMEMI  PIC X(18).
           02  FNAMEML    COMP  PIC  S9(4).
           02  FNAMEMF    PICTURE X.
           02  FILLER REDEFINES FNAMEMF.
             03 FNAMEMA    PICTURE X.
           02  FNAMEMI  PIC X(12).
           02  REQML    COMP  PIC  S9(4).
           02  REQMF    PICTURE X.
           02  FILLER REDEFINES REQMF.
             03 REQMA    PICTURE X.
           02  REQMI  PIC X(1).
           02  ACCTML    COMP  PIC  S9(4).
           02  ACCTMF    PICTURE X.
           02  FILLER REDEFINES ACCTMF.
             03 ACCTMA    PICTURE X.
           02  ACCTMI  PIC X(5).
           02  PRTRML    COMP  PIC  S9(4).
           02  PRTRMF    PICTURE X.
           02  FILLER REDEFINES PRTRMF.
             03 PRTRMA    PICTURE X.
           02  PRTRMI  PIC X(4).
           02  SUMTTLML    COMP  PIC  S9(4).
           02  SUMTTLMF    PICTURE X.
           02  FILLER REDEFINES SUMTTLMF.
             03 SUMTTLMA    PICTURE X.
           02  SUMTTLMI  PIC X(79).
           02  SUMLNMD OCCURS 6 TIMES.
             03  SUMLNML    COMP  PIC  S9(4).
             03  SUMLNMF    PICTURE X.
             03  SUMLNMI  PIC X(79).
           02  MSGINVML    COMP  PIC  S9(4).
           02  MSGINVMF    PICTURE X.
           02  FILLER REDEFINES MSGINVMF.
             03 MSGINVMA    PICTURE X.
           02  MSGINVMI  PIC X(65).
           02  MSGML    COMP  PIC  S9(4).
           02  MSGMF    PICTURE X.
           02  FILLER REDEFINES MSGMF.
             03 MSGMA    PICTURE X.
           02  MSGMI  PIC X(60).
       01  ACCTMNUO REDEFINES ACCTMNUI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SNAMEMO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  FNAMEMO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  REQMO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ACCTMO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  PRTRMO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  SUMTTLMO  PIC X(79).
           02  DFHMS1 OCCURS 6 TIMES.
             03  FILLER PICTURE X(2).
             03  SUMLNMA    PICTURE X.
             03  SUMLNMO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  MSGINVMO  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MSGMO  PIC X(60).
       01  ACCTDTLI.
           02  FILLER PIC X(12).
           02  TITLEDL    COMP  PIC  S9(4).
           02  TITLEDF    PICTURE X.
           02  FILLER REDEFINES TITLEDF.
             03 TITLEDA    PICTURE X.
           02  TITLEDI  PIC X(10).
           02  ACCTDL    COMP  PIC  S9(4).
           02  ACCTDF    PICTURE X.
           02  FILLER REDEFINES ACCTDF.
             03 ACCTDA    PICTURE X.
           02  ACCTDI  PIC X(5).
           02  SNAMEDL    COMP  PIC  S9(4).
           02  SNAMEDF    PICTURE X.
           02  FILLER REDEFINES SNAMEDF.
             03 SNAMEDA    PICTURE X.
           02  SNAMEDI  PIC X(18).
           02  TTLDL    COMP  PIC  S9(4).
           02  TTLDF    PICTURE X.
           02  FILLER REDEFINES TTLDF.
             03 TTLDA    PICTURE X.
           02  TTLDI  PIC X(4).
           02  FNAMEDL    COMP  PIC  S9(4).
           02  FNAMEDF    PICTURE X.
           02  FILLER REDEFINES FNAMEDF.
             03 FNAMEDA    PICTURE X.
           02  FNAMEDI  PIC X(12).
           02  MIDL    COMP  PIC  S9(4).
           02  MIDF    PICTURE X.
           02  FILLER REDEFINES MIDF.
             03 MIDA    PICTURE X.
           02  MIDI  PIC X(1).
           02  TELDL    COMP  PIC  S9(4).
           02  TELDF    PICTURE X.
           02  FILLER REDEFINES TELDF.
             03 TELDA    PICTURE X.
           02  TELDI  PIC X(10).
           02  ADDR1DL    COMP  PIC  S9(4).
           02  ADDR1DF    PICTURE X.
           02  FILLER REDEFINES ADDR1DF.
             03 ADDR1DA    PICTURE X.
           02  ADDR1DI  PIC X(24).
           02  ADDR2DL    COMP  PIC  S9(4).
           02  ADDR2DF    PICTURE X.
           02  FILLER REDEFINES ADDR2DF.
             03 ADDR2DA    PICTURE X.
           02  ADDR2DI  PIC X(24).
           02  ADDR3DL    COMP  PIC  S9(4).
           02  ADDR3DF    PICTURE X.
           02  FILLER REDEFINES ADDR3DF.
             03 ADDR3DA    PICTURE X.
           02  ADDR3DI  PIC X(24).
           02  CARDSDL    COMP  PIC  S9(4).
           02  CARDSDF    PICTURE X.
           02  FILLER REDEFINES CARDSDF.
             03 CARDSDA    PICTURE X.
           02  CARDSDI  PIC X(1).
           02  CCODEDL    COMP  PIC  S9(4).
           02  CCODEDF    PICTURE X.
           02  FILLER REDEFINES CCODEDF.
             03 CCODEDA    PICTURE X.
           02  CCODEDI  PIC X(1).
           02  IMODL    COMP  PIC  S9(4).
           02  IMODF    PICTURE X.
           02  FILLER REDEFINES IMODF.
             03 IMODA    PICTURE X.
           02  IMODI  PIC X(2).
           02  IDAYDL    COMP  PIC  S9(4).
           02  IDAYDF    PICTURE X.
           02  FILLER REDEFINES IDAYDF.
             03 IDAYDA    PICTURE X.
           02  IDAYDI  PIC X(2).
           02  IYRDL    COMP  PIC  S9(4).
           02  IYRDF    PICTURE X.
           02  FILLER REDEFINES IYRDF.
             03 IYRDA    PICTURE X.
           02  IYRDI  PIC X(2).
           02  RSNDL    COMP  PIC  S9(4).
           02  RSNDF    PICTURE X.
           02  FILLER REDEFINES RSNDF.
             03 RSNDA    PICTURE X.
           02  RSNDI  PIC X(1).
           02  APPRDL    COMP  PIC  S9(4).
           02  APPRDF    PICTURE X.
           02  FILLER REDEFINES APPRDF.
             03 APPRDA    PICTURE X.
           02  APPRDI  PIC X(3).
           02  AUTH1DL    COMP  PIC  S9(4).
           02  AUTH1DF    PICTURE X.
           02  FILLER REDEFINES AUTH1DF.
             03 AUTH1DA    PICTURE X.
           02  AUTH1DI  PIC X(32).
           02  AUTH2DL    COMP  PIC  S9(4).
           02  AUTH2DF    PICTURE X.
           02  FILLER REDEFINES AUTH2DF.
             03 AUTH2DA    PICTURE X.
           02  AUTH2DI  PIC X(32).
           02  AUTH3DL    COMP  PIC  S9(4).
           02  AUTH3DF    PICTURE X.
           02  FILLER REDEFINES AUTH3DF.
             03 AUTH3DA    PICTURE X.
           02  AUTH3DI  PIC X(32).
           02  AUTH4DL    COMP  PIC  S9(4).
           02  AUTH4DF    PICTURE X.
           02  FILLER REDEFINES AUTH4DF.
             03 AUTH4DA    PICTURE X.
           02  AUTH4DI  PIC X(32).
           02  SCODE1DL    COMP  PIC  S9(4).
           02  SCODE1DF    PICTURE X.
           02  FILLER REDEFINES SCODE1DF.
             03 SCODE1DA    PICTURE X.
           02  SCODE1DI  PIC X(1).
           02  SCODE2DL    COMP  PIC  S9(4).
           02  SCODE2DF    PICTURE X.
           02  FILLER REDEFINES SCODE2DF.
             03 SCODE2DA    PICTURE X.
           02  SCODE2DI  PIC X(1).
           02  SCODE3DL    COMP  PIC  S9(4).
           02  SCODE3DF    PICTURE X.
           02  FILLER REDEFINES SCODE3DF.
             03 SCODE3DA    PICTURE X.
           02  SCODE3DI  PIC X(1).
           02  HISTTLDL    COMP  PIC  S9(4).
           02  HISTTLDF    PICTURE X.
           02  FILLER REDEFINES HISTTLDF.
             03 HISTTLDA    PICTURE X.
           02  HISTTLDI  PIC X(33).
           02  LIMTTLDL    COMP  PIC  S9(4).
           02  LIMTTLDF    PICTURE X.
           02  FILLER REDEFINES LIMTTLDF.
             03 LIMTTLDA    PICTURE X.
           02  LIMTTLDI  PIC X(12).
           02  LIMITDL    COMP  PIC  S9(4).
           02  LIMITDF    PICTURE X.
           02  FILLER REDEFINES LIMITDF.
             03 LIMITDA    PICTURE X.
           02  LIMITDI  PIC X(8).
           02  STATTLDL    COMP  PIC  S9(4).
           02  STATTLDF    PICTURE X.
           02  FILLER REDEFINES STATTLDF.
             03 STATTLDA    PICTURE X.
           02  STATTLDI  PIC X(6).
           02  STATDL    COMP  PIC  S9(4).
           02  STATDF    PICTURE X.
           02  FILLER REDEFINES STATDF.
             03 STATDA    PICTURE X.
           02  STATDI  PIC X(2).
           02  VFYTLDL    COMP  PIC  S9(4).
           02  VFYTLDF    PICTURE X.
           02  FILLER REDEFINES VFYTLDF.
             03 VFYTLDA    PICTURE X.
           02  VFYTLDI  PIC X(26).
           02  VFYDL    COMP  PIC  S9(4).
           02  VFYDF    PICTURE X.
           02  FILLER REDEFINES VFYDF.
             03 VFYDA    PICTURE X.
           02  VFYDI  PIC X(1).
           02  MSGDL    COMP  PIC  S9(4).
           02  MSGDF    PICTURE X.
           02  FILLER REDEFINES MSGDF.
             03 MSGDA    PICTURE X.
           02  MSGDI  PIC X(60).
       01  ACCTDTLO REDEFINES ACCTDTLI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TITLEDO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ACCTDO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  SNAMEDO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  TTLDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  FNAMEDO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  MIDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  TELDO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ADDR1DO  PIC X(24).
           02  FILLER PICTURE X(3).
           02  ADDR2DO  PIC X(24).
           02  FILLER PICTURE X(3).
           02  ADDR3DO  PIC X(24).
           02  FILLER PICTURE X(3).
           02  CARDSDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  CCODEDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  IMODO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  IDAYDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  IYRDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  RSNDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  APPRDO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  AUTH1DO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  AUTH2DO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  AUTH3DO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  AUTH4DO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  SCODE1DO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SCODE2DO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SCODE3DO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  HISTTLDO  PIC X(33).
           02  FILLER PICTURE X(3).
           02  LIMTTLDO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  LIMITDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  STATTLDO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  STATDO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  VFYTLDO  PIC X(26).
           02  FILLER PICTURE X(3).
           02  VFYDO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MSGDO  PIC X(60).
       01  ACCTERRI.
           02  FILLER PIC X(12).
           02  DATEEL    COMP  PIC  S9(4).
           02  DATEEF    PICTURE X.
           02  FILLER REDEFINES DATEEF.
             03 DATEEA    PICTURE X.
           02  DATEEI  PIC X(10).
           02  TIMEEL    COMP  PIC  S9(4).
           02  TIMEEF    PICTURE X.
           02  FILLER REDEFINES TIMEEF.
             03 TIMEEA    PICTURE X.
           02  TIMEEI  PIC X(8).
           02  MSGED OCCURS 3 TIMES.
             03  MSGEL    COMP  PIC  S9(4).
             03  MSGEF    PICTURE X.
             03  MSGEI  PIC X(79).
       01  ACCTERRO REDEFINES ACCTERRI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  DATEEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  TIMEEO  PIC X(8).
           02  DFHMS2 OCCURS 3 TIMES.
             03  FILLER PICTURE X(2).
             03  MSGEA    PICTURE X.
             03  MSGEO  PIC X(79).
