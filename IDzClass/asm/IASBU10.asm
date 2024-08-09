IASBU10  CEEENTRY PPA=MAINPPA                                           IAS00010
*                                                                       IAS00020
         CALL  CEEMOUT,(STRING,DEST,0),VL                               IAS00030
*                                                                       IAS00040
FTCH     CEEFETCH  NAME=IASBU10A,TOKEN=TOKE                             IAS00050
         CALL      IASBU10A                                             IAS00060
RLS      CEERELES  TOKEN=TOKE                                           IAS00070
*                                                                       IAS00080
         CALL  CEEMOUT,(STRING,DEST,0),VL                               IAS00090
*                                                                       IAS00100
         CEETERM  RC=0,MODIFIER=0                                       IAS00110
**************************************************************          IAS00120
*        Constants and Workareas                             *          IAS00130
**************************************************************          IAS00140
*                                                                       IAS00150
TOKE     DC    F'0'                                                     IAS00160
DEST     DC    F'2'                                                     IAS00170
STRING   DC    Y(STRLEN)                                                IAS00180
STRBEGIN DC    C'In the main routine'                                   IAS00190
STRLEN   EQU   *-STRBEGIN                                               IAS00200
MAINPPA  CEEPPA  ,                Constants describing the code block   IAS00210
         CEEDSA  ,                Mapping of the dynamic save area      IAS00220
         CEECAA  ,                Mapping of the common anchor area     IAS00230
         END   IASBU10            Nominate as the entry point           IAS00240
