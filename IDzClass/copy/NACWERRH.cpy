000100*--------------------------------------------------------------*
000110*                                                              *
000120*               @BANNER_START@                                 *
000130*      nacwerrh.cpy                                            *
000131*      (C) Copyright IBM Corp. 2000. All Rights Reserved.      *
000132*                                                              *
000133* Element of Designing and Programming CICS Applications book  *
000194*               @BANNER_END@                                   *
000195*                                                              *
000196*--------------------------------------------------------------*
000197*
000210* The interface to the Error Handler program is described in a
000300* copy book in order to ensure consistency. The values in this
000400* area designed to be in character format to enable ease of
000500* translation when the program is invoked from a remote system
000600* which uses a different encoding scheme (e.g., ASCII) than the
000700* EBCDIC of the mainframe.
000800*
000810* This is the working storage version of the interface to the
000820* Error Handler program.
000830*
000900     05  WS-ERRH-ERROR-COMMAREA.
001000*
001100* This is an "Eyecatcher" and integrity check field.
001200*
001300         10  WS-ERRH-VERSION             PIC XXX VALUE SPACES.
001400             88  WS-ERRH-CORRECT-VERSION VALUE 'V1A'.
001500         10  FILLER                      PIC X   VALUE SPACES.
001600*
001700* The error field is designed to conform to the CICS EIBRESP
001800* characteristics which always contains a numeric value. There
001900* are also architected values to indicate errors detected by the
002000* various programs in the applications suite.
002100*
002200         10  WS-ERRH-ERROR               PIC 9(4) VALUE ZERO.
002300         10  WS-ERRH-ERROR-X REDEFINES WS-ERRH-ERROR
002400                                         PIC X(4).
002500*
002600* The reason field is designed to conform to the CICS EIBRESP2
002700* characteristics which always contains a numeric value. There
002800* are also architected values to indicate errors detected by the
002900* various programs in the applications suite.
003000*
003100         10  WS-ERRH-REASON              PIC 9(4) VALUE ZERO.
003200         10  WS-ERRH-REASON-X REDEFINES WS-ERRH-REASON
003300                                         PIC X(4).
003400*
003500* If the response contains a numeric value, this contains the
003600* character representation of the EIBFN value giving rise to
003700* the exception condition.
003800*
003900         10  WS-ERRH-CICS-FUNCTION       PIC 9(5) VALUE ZERO.
004000         10  WS-ERRH-CICS-FUNCTION-X
004010                 REDEFINES WS-ERRH-CICS-FUNCTION
004100                                         PIC X(5).
004200*
004300* Since the Error Handler can be LINKed or XCTLed to as well as
004400* being entered via CICS ABEND handling, this field allows the
004500* program trapping the error to identify itself.
004600*
004700         10  WS-ERRH-PROGRAM             PIC X(8) VALUE SPACES.
004800*
004900* This is set by the Error Handler to indicate the number of
005000* messages it generated from the error information. This is
005100* intended to allow a program which has LINKed to the Error
005200* Handler to use the information in a manner it deems suitable.
005300*
005400         10  WS-ERRH-NUMBER              PIC 9(4) VALUE ZERO.
005500*
005600* The array of messages generated.
005700*
005800         10  WS-ERRH-MESSAGES.
005900             15  WS-ERRH-MESSAGE         PIC X(120) OCCURS 3.
