       01 W-CDAT3-LINKAGE-AREA.
           10 W-CDAT3-DATE-IN.
             15 W-CDAT3-RET-YYYY              PIC X(4).
             15 W-CDAT3-RET-MM                PIC X(2).
             15 W-CDAT3-RET-DD                PIC X(2).
           10 W-CDAT3-RETIRE-DATE              PIC X(80).
           10 W-CDAT3-PROGRAM-RETCODE          PIC 9(4) VALUE 0.
              88 W-CDAT3-REQUEST-SUCCESS          VALUE 0.
           10 W-CDAT3-RETIRE-ERRMSG            PIC X(30). 