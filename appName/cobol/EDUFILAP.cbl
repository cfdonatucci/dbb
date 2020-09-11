      *----------------------------------------------------------------*
      *  LICENSED MATERIALS - PROPERTY OF IBM                          *
      *  SAMPLE                                                        *
      *  (C) COPYRIGHT IBM CORP. 2018 ALL RIGHTS RESERVED              *
      *  US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION OR   *
      *  DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE CONTRACT WITH       *
      *  IBM CORP.                                                     *
      *----------------------------------------------------------------*
      *-- THIS PROGRAM RECEIVES A KEY VALUE AND RETURNS THE RELATED
      *-- RECORD. COMMAREA MAPPED BY EDUFILAC.
      *-- AUTHOR CARLOS DONATUCCI...
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDUFILAP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FLEN          PIC 9(4)   VALUE 80 COMP.
       77  WTIME-NUM     PIC 9(15)  VALUE ZEROS.
       77  WTIME         PIC S9(15) COMP-3.
       77  WLEN          PIC 9(4)   COMP VALUE 180.
       77  WLEN2         PIC 9(4)   COMP VALUE 180.
       77  WLEN3         PIC 9(4)   COMP VALUE 180.
       01  MENSAJE-LONG  PIC 9(4)   COMP VALUE 100.
       01  LOGAREA.
           02   FILLER PIC X(10) VALUE 'EDUFILAP :'.
           02   LOGD   PIC X(70).
      *
           COPY EDUFILAS.
      */////////////////////  COMMAREA 70 ///////////////////////////
       LINKAGE SECTION.
           COPY EDUFILAC.
      *//////////////////////////////////////////////////////////////
       PROCEDURE DIVISION.
      *//////////////////////////////////////////////////////////////
       PROCESO SECTION.
            IF EIBCALEN = 0
               MOVE 'NO COMMAERA ' TO LOGD
               GO TO WRITEQ.

            IF COM-NUMB = SPACES
               MOVE 'NO COM-NUMB ' TO LOGD
               GO TO WRITEQ.

           MOVE DFHCOMMAREA TO LOGD.
      **
           EXEC CICS READ FILE('FILEA') INTO(FILEA) RIDFLD(COM-NUMB)
                KEYLENGTH(6) LENGTH(FLEN) NOHANDLE
           END-EXEC.
           IF EIBRESP NOT = 0
              MOVE 'ERROR READING ' TO LOGD
              GO TO WRITEQ.
      **
            MOVE NUMB    TO COM-NUMB.
            MOVE NAME    TO COM-NAME.
            MOVE ADDRX   TO COM-ADDRX.
            MOVE PHONE   TO COM-PHONE.
            MOVE AMOUNT  TO COM-AMOUNT.
            MOVE DFHCOMMAREA TO LOGD.
      *
       WRITEQ.
            EXEC CICS ASKTIME    ABSTIME(WTIME) END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(WTIME) TIME(COM-HORA) END-EXEC.
            EXEC CICS WRITEQ TD QUEUE('CSMT') FROM(LOGAREA)
                    LENGTH(MENSAJE-LONG) NOHANDLE
            END-EXEC.
       BYEBYE.
            EXEC CICS RETURN END-EXEC.
      *