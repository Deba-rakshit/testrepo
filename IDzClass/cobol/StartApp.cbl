      ******************************************************************
      *                                                                *
      * MODULE NAME    STARTAPP.CBL                                    *
      *                                                                *
      * STATEMENT      IBM WebSphere Developer for System z            *
      *                5724-L44                                        *
      *                (c) Copyright IBM Corp. 2006                    *
      *                                                                *
      * DISCLAIMER OF WARRANTIES                                       *
      * You may copy, modify, and distribute these samples, or their   *
      * modifications, in any form, internally or as part of your      *
      * application or related documentation. These samples have not   *
      * been tested under all conditions and are provided to you by    *
      * IBM without obligation of support of any kind. IBM PROVIDES    *
      * THESE SAMPLES "AS IS" SUBJECT TO ANY STATUTORY WARRANTIES THAT *
      * CANNOT BE EXCLUDED. IBM MAKES NO WARRANTIES OR CONDITIONS,     *
      * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO, THE   *
      * IMPLIED WARRANTIES OR CONDITIONS OF MERCHANTABILITY, FITNESS   *
      * FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT REGARDING THESE *
      * SAMPLES OR TECHNICAL SUPPORT, IF ANY.                          *
      * You will indemnify IBM or third parties that provide IBM       *
      * products ("Third Parties") from and against any third party    *
      * claim arising out of the use, modification or distribution of  *
      * these samples with your application. You may not use the same  *
      * path name as the original files/modules. You must not alter or *
      * delete any copyright information in the Samples.               *
      *                                                                *
      ******************************************************************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Startapp.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Program-Pass-Fields.
          05 Temp-Name         PIC X(30).

       01 Program-Other-Fields.
          05 Input-Name         PIC X(30).
          05 Char-Count         PIC 99 VALUE ZEROS.

       01 Program-Flags.
          05 Loop-Flag          PIC 9(01).
             88 Loop-Done       VALUE 1.

       PROCEDURE DIVISION.

           INITIALIZE Program-Pass-Fields
           Program-Other-Fields
           Program-Flags.

           PERFORM UNTIL Loop-Done
                   DISPLAY " "
                   DISPLAY "Enter a name or Q to quit:"
                   MOVE SPACES TO Input-Name
                   ACCEPT Input-Name
                   IF Input-Name = SPACES
                     MOVE "Q" TO Input-Name
                   END-IF

                   MOVE 1 TO Char-Count
               INSPECT Input-Name TALLYING Char-Count FOR LEADING SPACES
                  
                   IF FUNCTION UPPER-CASE (Temp-Name) = "Q"
                   OR Temp-Name = SPACES
                     SET Loop-Done TO TRUE
                   ELSE
                     CALL 'PrintApp' USING Program-Pass-Fields
                   END-IF
           END-PERFORM.
           GOBACK.
           MOVE 1 TO Char-Count.

           GOBACK.






