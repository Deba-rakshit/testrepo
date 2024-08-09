1 dcl 1 sqlca,
   2 sqlcaid char(8),                 /* Literal      'SQLCA '     */
   2 sqlcabc fixed bin(31),           /* SQLCA size in bytes = 136 */
   2 sqlcode fixed bin(31),      /* SQL return code           */
   2 sqlerrm char(70) varying,   /* Error message tokens      */
   2 sqlerrp char (8),           /* Diagnostic information    */
   2 sqlerrd(1:6) fixed bin(31), /* Diagnostic information    */
   2 sqlwarn,                    /* Warning flags             */
     3 sqlwarn0 char(1),
     3 sqlwarnl char(1),
     3 sqlwarn2 char(1),
     3 sqlwarn3 char(1),
     3 sqlwarn5 char(1),
     3 sqlwarn6 char(1),
     3 sqlwarn7 char(1),
   2 sqltext,
     3 sqlwarn8 char(1),
     3 sqlwarn9 char(1),
     3 sqlwarna char(1),
     3 sqlstate char(5);
