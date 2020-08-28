       CBL CICS("SP")
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITTRAN.
      */////////////////////////////////////////////////////////////////
      *  Carlos Donatucci - FEB 2019
      *  Inicia por PLT el START de trns con n SECS de intervalo
      */////////////////////////////////////////////////////////////////
      */// Lee TDQ INIT que tiene CICSNAME y TRANSACCION
      */// TDQ INIT extrapartition file CICSVI.CICS.INITTRAN
      */// Cierra la TD al final del proceso
      */// Un * en la columna uno es un comentario
      */// Un + en la columna uno resetea el parametro itime
      */// **** en la region significa TODAS.
      */////////////////////////////////////////////////////////////////
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      */////////////////////////////////////////////////////////////////
       WORKING-STORAGE SECTION.
      */////////////////////////////////////////////////////////////////
       77  FILLER                  PIC X(14) VALUE 'INICIO WORKING'.
       77  I                       PIC 9(3).
       77  ITIME                   PIC S9(8) COMP VALUE +5.
       77  UTIME                   PIC S9(8) COMP VALUE +0.
       77  PTIME                   PIC S9(8) COMP VALUE +0.
       77  RESPONSE                PIC S9(8) COMP.
       77  TRAN-ERR                PIC X(4)  VALUE SPACES.
       77  TIPO-ERR                PIC X(6)  VALUE SPACES.
       77  APPLCICS                PIC X(8)  VALUE SPACES.
       77  FLEN                    PIC 9(4)  VALUE 8 COMP.
       77  FLEN80                  PIC 9(4)  VALUE 80 COMP.
       77  FLAGEND                 PIC 9     VALUE 0.
       01  WAREA.
           02  FILLER              PIC X(4).
           02  WFUNC               PIC X(4).
       01  WS-INIT                 PIC X(80) VALUE
              'INIT000I start program INITTRAN '.
       01  WS-STOP                 PIC X(80) VALUE
              'INIT099I Stop  program INITTRAN '.
       01  WS-TRANS.
           05 FILLER               PIC X(12) VALUE 'INIT001I    '.
           05 FILLER               PIC X(23) VALUE
              'Arranco la transaccion '.
           05 WS-TRANSID           PIC X(4) VALUE SPACES.
           05                      PIC X(9) VALUE
                              '  Fecha: '.
           05 WS-FECHA-TRAN        PIC X(10) VALUE SPACES.
           05                      PIC X(8) VALUE
                              '  Hora: '.
           05 WS-HORA-TRAN         PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE SPACES.
       01  WSREADQ.
           05 PROG-READQ           PIC X(12) VALUE 'INIT001E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-READQ           PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error READQ TD:INIT      '.
       01  WSINVREQ.
           05 PROG-INVREQ          PIC X(12) VALUE 'INIT002E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-INVREQ          PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error START  INVREQ      '.
       01  WSNOTAUT.
           05 PROG-NOTAUTH         PIC X(12) VALUE 'INIT003E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-NOTAUTH         PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error START  NOTAUTH     '.
       01  WSTRAERR.
           05 PROG-TRANSIDERR      PIC X(12) VALUE 'INIT004E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-TRANSIDERR      PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error START  TRANSIDERR  '.
       01  WSDESCON.
           05 PROG-DESCON          PIC X(12) VALUE 'INIT005E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-DESCON          PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error START  ??????      '.
       01  WSGETAPP.
           05 PROG-GETAPP          PIC X(12) VALUE 'INIT006E    '.
           05 FILLER               PIC X(16) VALUE 'Transaction ID  '.
           05 TRAN-GETAPP          PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(31) VALUE
              '  Error en ASSIGN APPLID   '.
      *
       01  QUE-REC.
           05 QUE-GRUPO.
              07 QUE-TYPE           PIC X.
              88 COMMENT            VALUE '*'.
              88 RESYNC             VALUE '+'.
              07 FILLER             PIC X(3).
           05 QUE-REGION            PIC X(8).
           05 QUE-TRAN              PIC X(4).
           05 FILLER                PIC X(64).
      *
      */////////////////////////////////////////////////////////////////
       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////
       PRINCIPAL SECTION.
      */////////////////////
           EXEC CICS ASSIGN APPLID(APPLCICS) RESP(RESPONSE) END-EXEC.
           IF RESPONSE IS NOT EQUAL DFHRESP(NORMAL)
              MOVE 'GETAPP' TO TIPO-ERR
              PERFORM START-ERR
              GO TO FINALI
           END-IF.
           EXEC CICS WRITEQ TD QUEUE('CSSL')
                FROM (WS-INIT) END-EXEC.
      *
       LOOPST.
           PERFORM LEER-QUE.
           IF FLAGEND = 0
              IF QUE-REGION = APPLCICS OR
                 QUE-REGION = '********'
                 PERFORM ARRANCA-TR
                 ADD ITIME TO PTIME
              END-IF
              GO TO LOOPST
           END-IF.
       FINALI.
           PERFORM CLOSE-QUE.
           EXEC CICS WRITEQ TD QUEUE('CSSL')
                FROM (WS-STOP) END-EXEC.
           EXEC CICS RETURN END-EXEC.
      *
       END-PRIN. EXIT.
      */////////////////////
       ARRANCA-TR SECTION.
      */////////////////////
           MOVE SPACES     TO TIPO-ERR.
           MOVE QUE-TRAN   TO TRAN-ERR.

           EXEC CICS START TRANSID(QUE-TRAN) AFTER SECONDS(PTIME)
                     RESP(RESPONSE) END-EXEC.

           IF RESPONSE = DFHRESP(NORMAL)
              MOVE QUE-TRAN TO WS-TRANSID
              EXEC CICS ASKTIME ABSTIME(UTIME) END-EXEC
              EXEC CICS FORMATTIME ABSTIME(UTIME)
                   MMDDYYYY(WS-FECHA-TRAN) DATESEP('/')
                   TIME(WS-HORA-TRAN) TIMESEP
              END-EXEC
              EXEC CICS WRITEQ TD QUEUE('CSSL')
                   FROM (WS-TRANS) END-EXEC
           ELSE
           IF RESPONSE = DFHRESP(NOTAUTH)
                 MOVE 'NOTAUT' TO TIPO-ERR
                 PERFORM START-ERR
           ELSE
           IF RESPONSE = DFHRESP(INVREQ)
                 MOVE 'INVREQ' TO TIPO-ERR
                 PERFORM START-ERR
           ELSE
           IF RESPONSE = DFHRESP(TRANSIDERR)
                MOVE 'TRAERR' TO TIPO-ERR
                PERFORM START-ERR
           ELSE
           IF RESPONSE IS NOT EQUAL DFHRESP(NORMAL)
                MOVE '??????' TO TIPO-ERR
                PERFORM START-ERR.

       FIN-ARRANCA-TR. EXIT.
      */////////////////////
       LEER-QUE SECTION.
      */////////////////////
       LEERQ.
           EXEC CICS READQ TD QUEUE ('INIT')
                     INTO(QUE-REC)
                     LENGTH(FLEN80)
                     RESP(RESPONSE) END-EXEC.

           IF RESPONSE = DFHRESP(QZERO)
              MOVE 1 TO FLAGEND
              GO TO FIN-LEERQ
           END-IF.

           IF RESPONSE IS NOT EQUAL DFHRESP(NORMAL)
              MOVE 'ERREAD' TO TIPO-ERR
              PERFORM START-ERR
              MOVE 1 TO FLAGEND
              GO TO FIN-LEERQ
           END-IF.

           IF COMMENT GO TO LEERQ     END-IF.
           IF RESYNC  MOVE 0 TO PTIME END-IF.

       FIN-LEERQ. EXIT.
      */////////////////////
       CLOSE-QUE SECTION.
      */////////////////////
       CLOSEQ.
           EXEC CICS SET TDQUEUE ('INIT')
                     CLOSED NOHANDLE
                     RESP(RESPONSE) END-EXEC.

       FIN-CLOSQ. EXIT.
      */////////////////////
       START-ERR SECTION.
      */////////////////////
           IF TIPO-ERR = 'GETAPP'
             MOVE SPACES   TO TRAN-GETAPP
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSGETAPP) END-EXEC.
           IF TIPO-ERR = 'ERREAD'
             MOVE SPACES   TO TRAN-READQ
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSREADQ) END-EXEC.
           IF TIPO-ERR = 'NOTAUT'
             MOVE TRAN-ERR TO TRAN-NOTAUTH
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSNOTAUT) END-EXEC.
           IF TIPO-ERR = 'INVREQ'
             MOVE TRAN-ERR TO TRAN-INVREQ
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSINVREQ) END-EXEC.
           IF TIPO-ERR = 'TRAERR'
             MOVE TRAN-ERR TO TRAN-TRANSIDERR
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSTRAERR) END-EXEC.
             MOVE SPACES TO TIPO-ERR TRAN-ERR.
           IF TIPO-ERR = '??????'
             MOVE TRAN-ERR TO TRAN-DESCON
             EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(WSDESCON) END-EXEC.
      *
           MOVE SPACES TO TIPO-ERR TRAN-ERR.
      *
       END-START-ERR. EXIT.
      */////////////////////