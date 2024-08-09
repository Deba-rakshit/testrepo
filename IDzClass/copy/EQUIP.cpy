       01  WS-EQUIPMENT-REC.
           05  EQUIP-TABLE-REC OCCURS 100 TIMES INDEXED BY ROW-IDX.
             10  EQUIPMENT-ID         PIC X(08).
             10  EQUIP-CATEGORY       PIC X(04).
                   88 HEATING-PAD   VALUE "HEAT".
                   88 AUTOCLAVE     VALUE "AUTO".
                   88 SCOPE         VALUE "SCOP".
                   88 DRIP          VALUE "DRIP".
                   88 MONITOR       VALUE "MON ".
                   88 SHUNT         VALUE "SHNT".
                   88 MISCELLANEOUS VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "HEAT", "AUTO",
                      "SCOP", "DRIP", "MON ", "SHNT", "MISC".
      ****** FOR PERFORMANCE, MOVE H1N1 TO THE TOP OF THE LIST
             10  EQUIP-SHORT-DESC         PIC X(25).
             10  EQUIP-COST               PIC 9(5)V99.
             10  PRESCRIBING-PHYS         PIC X(08).
             10  DIAG-CDE                 PIC X(05).
             10  EQUIP-LONG-DESCRIPTION   PIC X(39).