       01  ADMENUI.
           02  FILLER PIC X(12).
           02  MAPDATL    COMP  PIC  S9(4).
           02  MAPDATF    PICTURE X.
           02  FILLER REDEFINES MAPDATF.

             03 MAPDATA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPDATI  PIC X(8).
           02  MAPREQL    COMP  PIC  S9(4).
           02  MAPREQF    PICTURE X.
           02  FILLER REDEFINES MAPREQF.

             03 MAPREQA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPREQI  PIC X(1).
           02  REQMSG1L    COMP  PIC  S9(4).
           02  REQMSG1F    PICTURE X.
           02  FILLER REDEFINES REQMSG1F.

             03 REQMSG1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REQMSG1I  PIC X(45).
           02  REQMSG2L    COMP  PIC  S9(4).
           02  REQMSG2F    PICTURE X.
           02  FILLER REDEFINES REQMSG2F.

             03 REQMSG2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REQMSG2I  PIC X(25).
           02  REQMSG3L    COMP  PIC  S9(4).
           02  REQMSG3F    PICTURE X.
           02  FILLER REDEFINES REQMSG3F.

             03 REQMSG3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REQMSG3I  PIC X(25).
           02  REQMSG4L    COMP  PIC  S9(4).
           02  REQMSG4F    PICTURE X.
           02  FILLER REDEFINES REQMSG4F.

             03 REQMSG4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REQMSG4I  PIC X(45).
           02  REQMSG5L    COMP  PIC  S9(4).
           02  REQMSG5F    PICTURE X.
           02  FILLER REDEFINES REQMSG5F.

             03 REQMSG5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  REQMSG5I  PIC X(45).
           02  MAPMSG1L    COMP  PIC  S9(4).
           02  MAPMSG1F    PICTURE X.
           02  FILLER REDEFINES MAPMSG1F.

             03 MAPMSG1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPMSG1I  PIC X(65).
           02  MAPMSG2L    COMP  PIC  S9(4).
           02  MAPMSG2F    PICTURE X.
           02  FILLER REDEFINES MAPMSG2F.

             03 MAPMSG2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPMSG2I  PIC X(65).
           02  MAPMSG3L    COMP  PIC  S9(4).
           02  MAPMSG3F    PICTURE X.
           02  FILLER REDEFINES MAPMSG3F.

             03 MAPMSG3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPMSG3I  PIC X(65).
           02  MAPMSG4L    COMP  PIC  S9(4).
           02  MAPMSG4F    PICTURE X.
           02  FILLER REDEFINES MAPMSG4F.

             03 MAPMSG4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPMSG4I  PIC X(65).
           02  MAPMSG5L    COMP  PIC  S9(4).
           02  MAPMSG5F    PICTURE X.
           02  FILLER REDEFINES MAPMSG5F.

             03 MAPMSG5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MAPMSG5I  PIC X(65).
           02  MSGOUTL    COMP  PIC  S9(4).
           02  MSGOUTF    PICTURE X.
           02  FILLER REDEFINES MSGOUTF.

             03 MSGOUTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGOUTI  PIC X(65).
       01  ADMENUO REDEFINES ADMENUI.

           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  MAPDATC    PICTURE X.
           02  MAPDATH    PICTURE X.
           02  MAPDATO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MAPREQC    PICTURE X.
           02  MAPREQH    PICTURE X.
           02  MAPREQO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  REQMSG1C    PICTURE X.
           02  REQMSG1H    PICTURE X.
           02  REQMSG1O  PIC X(45).
           02  FILLER PICTURE X(3).
           02  REQMSG2C    PICTURE X.
           02  REQMSG2H    PICTURE X.
           02  REQMSG2O  PIC X(25).
           02  FILLER PICTURE X(3).
           02  REQMSG3C    PICTURE X.
           02  REQMSG3H    PICTURE X.
           02  REQMSG3O  PIC X(25).
           02  FILLER PICTURE X(3).
           02  REQMSG4C    PICTURE X.
           02  REQMSG4H    PICTURE X.
           02  REQMSG4O  PIC X(45).
           02  FILLER PICTURE X(3).
           02  REQMSG5C    PICTURE X.
           02  REQMSG5H    PICTURE X.
           02  REQMSG5O  PIC X(45).
           02  FILLER PICTURE X(3).
           02  MAPMSG1C    PICTURE X.
           02  MAPMSG1H    PICTURE X.
           02  MAPMSG1O  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MAPMSG2C    PICTURE X.
           02  MAPMSG2H    PICTURE X.
           02  MAPMSG2O  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MAPMSG3C    PICTURE X.
           02  MAPMSG3H    PICTURE X.
           02  MAPMSG3O  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MAPMSG4C    PICTURE X.
           02  MAPMSG4H    PICTURE X.
           02  MAPMSG4O  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MAPMSG5C    PICTURE X.
           02  MAPMSG5H    PICTURE X.
           02  MAPMSG5O  PIC X(65).
           02  FILLER PICTURE X(3).
           02  MSGOUTC    PICTURE X.
           02  MSGOUTH    PICTURE X.
           02  MSGOUTO  PIC X(65). 