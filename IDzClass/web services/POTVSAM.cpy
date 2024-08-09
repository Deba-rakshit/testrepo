      *****************************************************************         
      ***   DSN     = DNET045.WDZV7.POT.VSAM                                    
      ***   FCT     = POTVSAM                DSORG   = VSAM KSDS   ****         
      *****************************************************************         
       01  POTVSAM-RECORD-REC.                                                  
           03 CUST-NO            PIC 999.                                       
           03 CUST-LN            PIC X(25).                                     
           03 CUST-FN            PIC X(15).                                     
           03 CUST-ADDR1         PIC X(20).                                     
           03 CUST-CITY          PIC X(20).                                     
           03 CUST-ST            PIC X(5).                                      
           03 CUST-CTRY          PIC X(15).                                     