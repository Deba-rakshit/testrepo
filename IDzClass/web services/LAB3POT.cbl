       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    LAB3POT.                                                  
       AUTHOR.        Reginaldo Barosa.                                         
       INSTALLATION.  IBM Dallas.                                               
       DATE-WRITTEN.  01/21/07                                                  
      *****************************************************************         
      *  DESCRIPTION : CICS SUBROUTINE TO CUSTOMER DATA               *         
      *  Receive customer #, read VSAM,  move data  to COMMAREA       *         
      *  ENVIRONMENT : CICS, COBOL II                                 *         
      *  CICS TRANSACTION NAME : NONE                                 *         
      *  SUBROUTINE: NONE                                             *         
      *  VSAM FILES:  DNET045.WDZV7.POT                               *         
      *****************************************************************         
      ***   MODIFICATIONS                                            **         
      *** WHO   DATE    CHANGE                                       **         
      *** --- -------- ----------------------------------------------**         
      *** KMV 10/05/06  ORIGINAL                                     **         
      *** RB  01/21/07  ADAPTED TO WDZ V7 POT                        **         
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       EJECT                                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
       01  WS-PROGRAM                 PIC X(08)  VALUE 'LAB3POT'.               
       01  WS-LITERAL-WS              PIC X(48)  VALUE                          
            '        WORKING STORAGE STARTS HERE'.                              
       01  WORK-VARIABLES.                                                      
           05  WS-RESP                PIC S9(8)    VALUE 0  COMP.               
      * Below is the VSAM record area                                           
       COPY POTVSAM.                                                            
       01  WS-LITERAL                  PIC X(48)  VALUE                         
            '        LINKAGE SECTION STARTS HERE'.                              
      * ------------------------------------------------------------- *         
       LINKAGE SECTION.                                                         
      * ------------------------------------------------------------- *         
       01  DFHCOMMAREA.                                                         
            COPY COMAREA.                                                       
       PROCEDURE DIVISION.                                                      
      ******************************************************************        
       0100-MAIN-ROUTINE.                                                       
           IF  CustNo > 10 OR  CustNo < 1 THEN                                  
               MOVE CustNo  TO CUST-NO                                          
               MOVE  SPACES TO DFHCOMMAREA                                      
               MOVE CUST-NO TO CustNo                                           
               MOVE 'Should be > 0 and < 11' TO LastName                        
               MOVE  -1 to RetCode                                              
               EXEC CICS RETURN                                                 
               END-EXEC.                                                        
      ******************************************************************        
      * READ VSAM FILE - FCT is POTVSAM                                *        
      ******************************************************************        
       0300-POTVSAM-READ.                                                       
            MOVE CustNo  TO CUST-NO .                                           
            EXEC CICS READ                                                      
                     FILE   ('POTVSAM')                                         
                     INTO   ( POTVSAM-RECORD-REC )                              
                     LENGTH (LENGTH OF POTVSAM-RECORD-REC )                     
                     RIDFLD ( CUST-NO )                                         
                     EQUAL                                                      
                     RESP   (WS-RESP)                                           
           END-EXEC.                                                            
       0400-CHECK-IFOK.                                                         
           IF  WS-RESP NOT = DFHRESP(NORMAL)                                    
                MOVE SPACES                    TO DFHCOMMAREA                   
                MOVE 'INVALID READ POTVSAM ' TO LastName                        
               EXEC CICS RETURN                                                 
               END-EXEC.                                                        
           IF WS-RESP = DFHRESP(NOTFND)                                         
               MOVE SPACES                    TO DFHCOMMAREA                    
               MOVE 'POTVSAM  NOT FOUND ' TO LastName                           
               EXEC CICS RETURN                                                 
               END-EXEC.                                                        
       0500-DATA-RETURNED-OK.                                                   
           MOVE  CUST-LN    TO LastName.                                        
           MOVE  CUST-FN    TO FirstName.                                       
           MOVE  CUST-ADDR1 TO Address1 .                                       
           MOVE  CUST-CITY  TO City .                                           
           MOVE  CUST-ST    TO State .                                          
           MOVE  CUST-CTRY  TO Country .                                        
           MOVE ZEROES TO RetCode .                                             
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                