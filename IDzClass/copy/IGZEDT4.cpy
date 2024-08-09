      *** 05/26/99 DLA FIX THE CENTURY DATE FIX.
      **** Current System Date from IGZEDT4
       01  CURR-DATE-FOR-Y2K.
           05  CURR-DATE                     PIC X(08).
           05  CURR-DATE-X REDEFINES CURR-DATE.
               10 CURR-CC                    PIC 99.
               10 CURR-YY                    PIC 99.
               10 CURR-MM                    PIC 99.
               10 CURR-DD                    PIC 99.
      ***********************************************************
      **  ACCEPT SYSTEM DATE INCLUDING CENTURY.
      ***********************************************************
      *    CALL 'IGZEDT4' USING BY REFERENCE CURR-DATE.