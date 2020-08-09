       IDENTIFICATION DIVISION.
      */////////////////////////////////////////////////////////////////
      */// Carlos Donatucci - DIC 2018
      */// Interface para EZACIC20.
      */// Recibe parametros: xxxxZYllll por receive
      */// xxxx codigo transaccion
      */// Z   I Initialization
      *///     T  Immediate termination
      *///     D  Deferred termination
      *///     Q  Quiesce the CICS socket interface by querying the PLT
      */// Y   C CICS socket Interface
      *///     L Listener
      */////////////////////////////////////////////////////////////////
       PROGRAM-ID. LINKSOCK.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 FLEN          PIC 9(4)   VALUE 10 COMP.
       77 WTIME-NUM     PIC 9(15)  VALUE ZEROS.
       77 WTIME         PIC S9(15) COMP-3.
       77 WLEN          PIC 9(4)   COMP VALUE 180.
       01 RESPCODE      PIC S9(8)  COMP-4 VALUE 0.
       01 RESPCODE2     PIC S9(8)  COMP-4 VALUE 0.
       01 MENSAJE-LONG  PIC 9(4)   COMP VALUE 100.
       01 WTOA.
           02  FILLER PIC X(10) VALUE 'LINKSOCK :'.
           02  LOGD   PIC X(25).
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
           03  P20RET   PIC  X(1).
      *
      *///////////////////// ///////////////////////////
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
              MOVE 'Tipo invalido I/T/D/Q ' TO LOGD
           ELSE
             IF NOT P20OBJOK
                MOVE 'Objeto invalido C/L ' TO LOGD
             ELSE
                EXEC CICS LINK PROGRAM('EZACIC20')
                           COMMAREA(P20PARMS)
                           LENGTH(7) END-EXEC
                IF P20RET > '0'
                   MOVE 'Error en el CALL    ' TO LOGD
                ELSE
                   MOVE 'Call satisfactorio  ' TO LOGD.
      *
            EXEC CICS WRITE OPERATOR
                TEXT(WTOA) TEXTLENGTH(45) NOHANDLE END-EXEC.
            EXEC CICS RETURN END-EXEC.
      *
       FIN-PROC. EXIT.
      *//////////////////////////////////////////////////////////////
