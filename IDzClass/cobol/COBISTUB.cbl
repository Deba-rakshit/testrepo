 CBL DATA(24),RMODE(24),NODYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBISTUB.
      *****************************************************************
      * PROGRAM:  COBISTUB
      *
      * AUTHOR :  DOUG STOUT
      *
      * PROCESSING :
      *        IMS STUB PROGRAM USED TO AUTOMATICALLY INITIATE
      *        LANGUAGE ENVIRONMENT PRIOR TO CALLING AN OS/VS COBOL
      *        OR NON-LE ASSEMBLER PROGRAM.
      *        THIS ENABLES THE USE OF DEBUG TOOL FOR OS/VS COBOL
      *        OR NON-LE ASSEMBLER PROGRAMS FOR APPLICATIONS WHERE
      *        THE EQANMDBG UTILITY PROGRAM CANNOT BE USED
      *        (ONLINE IMS, FOR EXAMPLE).
      * STEPS FOR USAGE :
      *        1) MODIFY THE NAME OF THE MODULE CALLED IN EACH CALL
      *           STATEMENT TO BE THE NAME OF THE ENTRY POINT IN THE
      *           OS/VS COBOL OR ASSEMBLER PROGRAM TO BE DEBUGGED.
      *           FOR OS/VS COBOL IMS PROGRAMS, THIS WILL TYPICALLY
      *           BE "DLITCBL".
      *        2) COMPILE THIS PROGRAM AND LINK IT INTO ANY LOAD
      *           LIBRARY.
      *        3) RE-LINK THE APPLICATION LOAD MODULE, AND "INCLUDE"
      *           THIS MODULE.  MAKE THIS MODULE (COBISTUB) THE ENTRY
      *           POINT.
      *        4) EXECUTE THE PROGRAM.  DEBUG TOOL CAN BE INVOKED
      *           USING THE SAMPLE CEEBXITA LE EXIT.  SEE THE SAMPLE
      *           JCL IN _.ADLAB.INSTALL($PREPDTX) TO PREPARE THE
      *           SAMPLE LE EXITS.  ALTERNATIVELY, CEEUOPT CAN BE USED
      *           TO INVOKE DEBUG TOOL.
      *        5) WHEN DEBUG TOOL STARTS, IT WILL BE IN THIS PROGRAM.
      *           FOLLOW THESE STEPS TO STEP INTO AN OS/VS COBOL
      *           PROGRAM AND CONTINUE DEBUGGING THE APPLICATION:
      *              ISSUE COMMANDS:
      *                 AT CALL *;
      *                 GO;    (THIS GETS TO THE CALL)
      *                 SET DISASSEMBLY ON;
      *                 STEP;  (GETS INTO THE APP PGM)
      *                 SET DISASSEMBLY OFF;
      *              ISSUE THE "LDD..." COMMAND TO GET PROGRAM SOURCE
      *              (YOU MAY HAVE TO USE THE "SET DEFAULT LIST..."
      *               COMMAND BEFORE USING THE "LDD..." COMMAND)
      *
      *              OPTIONAL COMMAND SEQUENCE FOR OS/VS COBOL SUB:
      *                 AT CALL *;
      *                 GO;    (THIS GETS TO THE CALL)
      *                 LDD PROGRAM-NAME ;   (THE OS/VS COBOL PROGRAM)
      *                 STEP;  (GETS INTO THE APP PGM)
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME       PIC X(16) VALUE 'PROGRAM COBISTUB'.
       LINKAGE SECTION.
       01  PARM1.
           05  FILLER                    PIC X(100).
       01  PARM2.
           05  FILLER                    PIC X(100).
       01  PARM3.
           05  FILLER                    PIC X(100).
       01  PARM4.
           05  FILLER                    PIC X(100).
       01  PARM5.
           05  FILLER                    PIC X(100).
       01  PARM6.
           05  FILLER                    PIC X(100).
       01  PARM7.
           05  FILLER                    PIC X(100).
       01  PARM8.
           05  FILLER                    PIC X(100).
       01  PARM9.
           05  FILLER                    PIC X(100).
       PROCEDURE DIVISION USING PARM1 PARM2 PARM3
                                PARM4 PARM5 PARM6
                                PARM7 PARM8 PARM9 .
           EVALUATE NULL
             WHEN ADDRESS OF PARM1
                 PERFORM CALL-0-PARMS
             WHEN ADDRESS OF PARM2
                 PERFORM CALL-1-PARMS
             WHEN ADDRESS OF PARM3
                 PERFORM CALL-2-PARMS
             WHEN ADDRESS OF PARM4
                 PERFORM CALL-3-PARMS
             WHEN ADDRESS OF PARM5
                 PERFORM CALL-4-PARMS
             WHEN ADDRESS OF PARM6
                 PERFORM CALL-5-PARMS
             WHEN ADDRESS OF PARM7
                 PERFORM CALL-6-PARMS
             WHEN ADDRESS OF PARM8
                 PERFORM CALL-7-PARMS
             WHEN ADDRESS OF PARM9
                 PERFORM CALL-8-PARMS
             WHEN OTHER
                 PERFORM CALL-9-PARMS
           END-EVALUATE.
           GOBACK.

       CALL-0-PARMS.
           CALL 'DLITCBL' .

       CALL-1-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1 .

       CALL-2-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2 .

       CALL-3-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3 .

       CALL-4-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4 .

       CALL-5-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4
                                 BY REFERENCE PARM5 .

       CALL-6-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4
                                 BY REFERENCE PARM5
                                 BY REFERENCE PARM6 .

       CALL-7-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4
                                 BY REFERENCE PARM5
                                 BY REFERENCE PARM6
                                 BY REFERENCE PARM7 .

       CALL-8-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4
                                 BY REFERENCE PARM5
                                 BY REFERENCE PARM6
                                 BY REFERENCE PARM7
                                 BY REFERENCE PARM8 .

       CALL-9-PARMS.
           CALL 'DLITCBL' USING  BY REFERENCE PARM1
                                 BY REFERENCE PARM2
                                 BY REFERENCE PARM3
                                 BY REFERENCE PARM4
                                 BY REFERENCE PARM5
                                 BY REFERENCE PARM6
                                 BY REFERENCE PARM7
                                 BY REFERENCE PARM8
                                 BY REFERENCE PARM9 .