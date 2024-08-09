//*********************************************************************
//*     CUSTOMIZE THE JOB CARD FOR THIS UTILITY PROGRAM               *
//*********************************************************************
//*
//LEEBR14 JOB ,
// MSGCLASS=H,TIME=(,4),REGION=28M,COND=(16,LT)
//* *******************************************************************
//* The job will delete the dataset specified in the THEFILE DD
//* *******************************************************************
//ERASE    EXEC PGM=IEFBR14
//THEFILE  DD   DSN=EZXCVJ9.IDzT.I8FES.JCL,DISP=(OLD,DELETE)
//
//* NOTE THE ABOVE DATASET SHOULD NOT BE FOUND