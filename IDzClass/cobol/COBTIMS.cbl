       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTIMS.
      *****************************************************************
      *    PROCESSING
      *        IMS STUB PROGRAM TO INITIATE LE PRIOR TO INVOKING
      *        IMS. THIS ALLOWS LE RUNTIME PARMS TO BE INVOKED WHILE
      *        RUNNING A IMS PROGRAM.
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFSRRC00                      PIC X(8) VALUE 'DFSRRC00'.
       LINKAGE SECTION.
       01  IMS-PARM.
           05  IMS-PARM-LENGTH           PIC S9(4) COMP.
           05  IMS-PARM-DATA.
               10  IMS-PARM-CHAR         PIC X
                   OCCURS 0 TO 255 TIMES DEPENDING ON IMS-PARM-LENGTH.
       PROCEDURE DIVISION USING IMS-PARM.
           CALL DFSRRC00 USING IMS-PARM.
           GOBACK.