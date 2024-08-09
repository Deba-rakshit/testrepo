*-----------------------------------------------------------------
*   SAMPLE ASSEMBLER DSECT FOR IBM PD TOOLS WORKSHOPS
*   DESCRIBES FILE <USERID>.ADLAB.CUST1
*-----------------------------------------------------------------
CUST_REC DSECT
CUSTKEY  DS    0CL5
CUST_ID  DS    CL5
NAME     DS    CL17
ACCT_BAL DS    PL5
ORDS_BIN DS    0BL2
ORDS_YTD DS    H
ADDR     DS    CL20
CITY     DS    CL14
STATE    DS    CL2
COUNTRY  DS    CL11
MONTH    DS    12PL5
OCCUPTN  DS    CL30
NOTES    DS    CL120
LABDATA1 DS    CL5
LABDATA2 DS    CL40
         ORG   LABDATA2
LABRDF1  DS    CL5
LABRDF2  DS    CL5
LABRDF3  DS    CL30
         END