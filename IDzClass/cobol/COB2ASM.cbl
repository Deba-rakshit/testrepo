  CBL LIB,QUOTE,NODYNAM,TEST(SEP)                                       00010003
      **************************************************************    00010200
      *                                                            *    00010300
      *   COB2ASM  Staticaliy call a HLASM routine passing an int  *    00010400
      *                                                            *    00010704
      **************************************************************    00013300
       IDENTIFICATION DIVISION.                                         00013400
       PROGRAM-ID.    COB2ASM.                                          00013500
       DATA DIVISION.                                                   00013600
       WORKING-STORAGE SECTION.                                         00013700
                                                                        00013800
       01   TheParm    PIC S9(9) Usage is binary.                       00016200
                                                                        00016300
       PROCEDURE DIVISION.                                              00016400
       0001-BEGIN-PROCESSING.                                           00016500
           Move 127 to TheParm.                                         00016600
                                                                        00016700
           Display "COB2ASM - calling Assembler with 127".              00018100
                                                                        00018204
           CALL "ASMSUB" USING TheParm.                                 00018300
                                                                        00018400
           Display "COB2ASM - Return from Assembler with " TheParm.     00018600
                                                                        00018700
           GOBACK.                                                      00020500
       END PROGRAM COB2ASM.                                             00020600