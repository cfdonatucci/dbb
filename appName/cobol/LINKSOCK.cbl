       IDENTIFICATION DIVISION.
      */////////////////////////////////////////////////////////////////
      *  LICENSED MATERIALS - PROPERTY OF IBM                          *
      *  (C) COPYRIGHT IBM CORP. 2020 ALL RIGHTS RESERVED              *
      *  US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION OR   *
      *  DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE CONTRACT WITH       *
      *  IBM CORP.                                                     *
      */////////////////////////////////////////////////////////////////
      */// Carlos Donatucci - SET 2020
      */// EZACIC20 Interface
      */// Input parameters: xxxxZYtttt
      */// xxxx transaction code
      */// Z   I Initialization
      *///     T  Immediate termination
      *///     D  Deferred termination
      *///     Q  Quiesce the CICS socket interface by querying the PLT
      */// Y   C CICS socket Interface
      *///     L Listener
      *///       If L is set the listener trn code must be informed
      */////////////////////////////////////////////////////////////////
       PROGRAM-ID. LINKSOCK.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 FLEN          PIC 9(4)   VALUE 10 COMP.
       77 UTIME         PIC S9(8) COMP VALUE +0.
      *
       01  WTOA.
           02 MSGTXT        PIC X(35) VALUE SPACES.
           02 FILLER        PIC X(3) VALUE 'rc='.
           02 MSGRET        PIC 99.
           02 FILLER        PIC X VALUE SPACES.
           02 MSG-FECHA     PIC X(10) VALUE SPACES.
           02 FILLER        PIC X VALUE SPACES.
           02 MSG-HORA      PIC X(8)  VALUE SPACES.
           02 FILLER        PIC X(10) VALUE SPACES.
      *
       01  WAREA.
           02  FILLER    PIC X(4).
           02  WAREAT    PIC X.
           02  WAREAO    PIC X.
           02  WAREAL    PIC X(4).
      *
       01  P20PARMS.
           03  P20TYPE  PIC   X(1).
             88  P20TYPEI   VALUE 'I'.
             88  P20TYPET   VALUE 'T'.
             88  P20TYPED   VALUE 'D'.
             88  P20TYPEQ   VALUE 'Q'.
             88  P20TYPEOK  VALUE 'D' 'I' 'T' 'Q'.
           03  P20OBJ   PIC   X(1).
             88  P20OBJC    VALUE 'C'.
             88  P20OBJL    VALUE 'L'.
             88  P20OBJOK   VALUE 'L' 'C'.
           03  P20LIST  PIC  X(4) VALUE '    '.
           03  P20RET   PIC X.
             88  P20RETOK    VALUE X'00'.
             88  P20RETCI    VALUE X'01'.
             88  P20RETLI    VALUE X'02'.
             88  P20RETCT    VALUE X'04'.
             88  P20RETLT    VALUE X'08'.
             88  P20RETCA    VALUE X'10'.
             88  P20RETSY    VALUE X'20'.
      *
      *//////////////////////////////////////////////////////////////
       LINKAGE SECTION.
      *//////////////////////////////////////////////////////////////
       PROCEDURE DIVISION.
      *//////////////////////////////////////////////////////////////
       PROCESO SECTION.
           EXEC CICS RECEIVE INTO(WAREA) LENGTH(FLEN)  NOHANDLE
           END-EXEC.
           MOVE WAREAT  TO P20TYPE.
           MOVE WAREAO  TO P20OBJ.
           MOVE WAREAL  TO P20LIST.

           IF NOT P20TYPEOK
              MOVE 'LINSO001 Invalid type. Code I/T/D/Q ' TO MSGTXT
              PERFORM OP-MSG
           ELSE
             IF NOT P20OBJOK
                MOVE 'LINSO002 Invalid Object. Code C/L  ' TO MSGTXT
                PERFORM OP-MSG
             ELSE
                PERFORM CALL-20
             END-IF
           END-IF.
      *
       FIN-PROC. EXIT.
      *//////////////////////////////////////////////////////////////
       CALL-20 SECTION.
      *//////////////////////////////////////////////////////////////
            EXEC CICS LINK PROGRAM('EZACIC20')
                      COMMAREA(P20PARMS)
                      LENGTH(7) END-EXEC.

            IF P20RETOK
               MOVE 'LINSO003 Call succesfull           ' TO MSGTXT
               MOVE 0 TO MSGRET
            ELSE
            IF P20RETCI
               MOVE 'LINSO004 CICS Interface init error ' TO MSGTXT
               MOVE 1 TO MSGRET
            ELSE
            IF P20RETLI
               MOVE 'LINSO005 Listener Init error       ' TO MSGTXT
               MOVE 2 TO MSGRET
            ELSE
            IF P20RETCT
               MOVE 'LINSO006 CICS Interface term error ' TO MSGTXT
               MOVE 4 TO MSGRET
            ELSE
            IF P20RETLT
               MOVE 'LINSO007 Listener term error       ' TO MSGTXT
               MOVE 8 TO MSGRET
            ELSE
            IF P20RETCA
               MOVE 'LINSO008 Error in COMMAREA Contents' TO MSGTXT
               MOVE 10 TO MSGRET
            ELSE
            IF P20RETSY
               MOVE 20 TO MSGRET
               MOVE 'LINSO009 Error in CICS/MVS         ' TO MSGTXT.

            PERFORM OP-MSG.

       END-CALL-20.EXIT.
      *//////////////////////////////////////////////////////////////
       OP-MSG SECTION.
      *////////////////////
            EXEC CICS ASKTIME ABSTIME(UTIME) NOHANDLE END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(UTIME)
                 MMDDYYYY(MSG-FECHA) DATESEP('/')
                 TIME(MSG-HORA) TIMESEP NOHANDLE
            END-EXEC.

            EXEC CICS WRITE OPERATOR
                TEXT(WTOA) TEXTLENGTH(60) NOHANDLE END-EXEC.
            EXEC CICS RETURN END-EXEC.

       FIN-MSG. EXIT.
      */////////////////////
