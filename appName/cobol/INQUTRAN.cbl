       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQUTRAN.
      */////////////////////////////////////////////////////////////////
      *  Carlos Donatucci - MAY 2019
      *  Verifica que esten arrancadas determinadas transacciones.
      */////////////////////////////////////////////////////////////////
      */// Abre la TD al inicio del proceso
      */// Lee TDQ extra INQT que tiene CICSNAME y TRANSACCION
      */// Cierra la TD al final del proceso
      */// El programa se debe arrancar por PLT que asume trn=CPLT
      */// Cuando la EIBTRNID=CPLT el programa se rearranca con
      */// el intervalo de la TDQ para evitar conflictos en el inicio
      */// Logica:
      */// if S up  and E up   -> OK
      */// If QUE-STFLG is '<'
      *///   if E dwn and S down -> start all
      *///   if S up  and E down -> purge S start all
      *///   if E up  and S down -> purge E start all
      */////////////////////////////////////////////////////////////*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  TRAN-PTR    USAGE IS POINTER.
       77  TASK-PTR    USAGE IS POINTER.
       77  CHKTRAN     PIC X(4)  VALUE 'TSOK'.
       77  CHKINT      PIC S9(8) COMP VALUE 5.
       77  RESPON      PIC S9(8) COMP.
       77  RESPON2     PIC S9(8) COMP.
       77  I           PIC 9(3)  COMP.
       77  IDXI        PIC 9(3)  COMP.
       77  IDXO        PIC 9(3)  COMP.
       77  UTIME       PIC S9(8) COMP VALUE +0.
       77  FLEN        PIC 9(4)  VALUE 8 COMP.
       77  FLEN80      PIC 9(4)  VALUE 80 COMP.
       77  LSIZE       PIC S9(8) COMP.
       77  APPLCICS    PIC X(8)  VALUE SPACES.
       77  WTRAN       PIC X(4)  VALUE SPACES.
       77  TRNFLAG     PIC 9.
           88 ACTIVA VALUE 0.
           88 INACTI VALUE 1.
       77  PURFLAG     PIC 9.
           88 PURGE-OK   VALUE 0.
           88 PURGE-ERR  VALUE 1.
           88 PURGE-DEF  VALUE 2.
      *
       01  WMSGIN.
           02 FILLER        PIC X(35) VALUE
              'INQUT001 Control Iniciara en '.
           02 MSGINTER      PIC X(4)  VALUE '05'.
           02 FILLER        PIC X(8)  VALUE  ' minutos'.
           02 FILLER        PIC X(33) VALUE SPACES.
      *
       01  WMSGOP.
           02 MSGTXT        PIC X(35) VALUE SPACES.
           02 MSGTRAN       PIC X(5)  VALUE SPACES.
           02 FILLER        PIC X(10) VALUE  '  Fecha: '.
           02 MSG-FECHA     PIC X(10) VALUE SPACES.
           02 FILLER        PIC X(8)  VALUE  '  Hora: '.
           02 MSG-HORA      PIC X(8)  VALUE SPACES.
           02 FILLER        PIC X(10) VALUE SPACES.
      *
       01  QUE-REC.
           05 QUE-REGION.
              07 QUE-TYPE           PIC X.
                 88 COMMENT         VALUE '*'.
              07 FILLER             PIC X(7).
           05 QUE-TRANI             PIC X(4).
           05 QUE-TRANO             PIC X(4).
           05 QUE-STFLG             PIC X.
              88 PURGEABLE       VALUE '<'.
           05 FILLER                PIC X(63).
       01  QUE-RECC REDEFINES QUE-REC.
           05 QUE-INT               PIC X(2).
           05 FILLER                PIC X(78).
      */////////////////////////////////////////////////////////////*
       LINKAGE SECTION.
       01  TRAN-TSK.
           02  TASKID OCCURS 600 PIC S9(7) COMP-3.
       01  TRAN-LST.
           02  TRANID OCCURS 600 PIC X(4).
      */////////////////////////////////////////////////////////////*
       PROCEDURE DIVISION.
      *//////////////////
       PROCESO SECTION.
           PERFORM OPEN-QUE.
           PERFORM READ-CTL.
           PERFORM PROC-QUE.
           PERFORM CLOS-QUE.
           PERFORM RESTART-CHK.
      *
       FIN-PROCESO. EXIT.
      */////////////////////
       PROC-QUE SECTION.
      */////////////////////
           EXEC CICS HANDLE ABEND LABEL(GETABEND) END-EXEC.
           MOVE 'INQUT008 Inicia ciclo control Trns ' TO MSGTXT.
           PERFORM OP-MSG.
       LEERQ.
           EXEC CICS READQ TD QUEUE ('INQT')
                     INTO(QUE-REC)
                     LENGTH(FLEN80)
                     RESP(RESPON) END-EXEC.
           IF RESPON = DFHRESP(QZERO)
              GO TO FIN-PROC
           END-IF.
           IF RESPON IS NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT004 Error READ QUEUE INQT     ' TO MSGTXT
              PERFORM OP-MSG
              GO TO FIN-PROC
           END-IF.

           IF NOT COMMENT AND APPLCICS = QUE-REGION
              PERFORM CHK-TRAN
           END-IF.

           GO TO LEERQ.

       FIN-PROC. EXIT.
      *//////////////////
       CHK-TRAN SECTION.
      *////////////////////
           EXEC CICS INQUIRE TASK LIST
                LISTSIZE(LSIZE)
                SET (TASK-PTR)
                SETTRANSID(TRAN-PTR) END-EXEC.

           SET ADDRESS OF TRAN-LST TO TRAN-PTR.
           SET ADDRESS OF TRAN-TSK TO TASK-PTR.

           IF TRAN-PTR = NULLS OR LSIZE = 0
              PERFORM RESTART-CHK.

           IF QUE-TRANO = SPACES
              MOVE QUE-TRANI TO WTRAN
              PERFORM LOOK-TRAN
              IF NOT ACTIVA
                 MOVE QUE-TRANI TO WTRAN
                 PERFORM START-TRN
              END-IF
              GO TO FIN-CHK
           END-IF.

           MOVE QUE-TRANO TO WTRAN.
           PERFORM LOOK-TRAN.
           IF ACTIVA
              MOVE I TO IDXO
              MOVE QUE-TRANI TO WTRAN
              PERFORM LOOK-TRAN
              IF ACTIVA
                 NEXT SENTENCE
              ELSE
                 MOVE IDXO      TO I
                 MOVE QUE-TRANO TO WTRAN
                 IF PURGEABLE
                    PERFORM PURGE-TRN
                    IF PURGE-OK
                       PERFORM START-ALL
                    END-IF
                 END-IF
              END-IF
           ELSE
              MOVE QUE-TRANI TO WTRAN
              PERFORM LOOK-TRAN
              IF ACTIVA
                 IF PURGEABLE
                    PERFORM PURGE-TRN
                    IF PURGE-OK
                       PERFORM START-ALL
                    END-IF
                 END-IF
              ELSE
                 IF PURGEABLE
                    PERFORM START-ALL
                 END-IF
              END-IF
           END-IF.
       FIN-CHK. EXIT.
      *//////////////////
       LOOK-TRAN SECTION.
      *////////////////////
           MOVE 1  TO I.
           MOVE 0  TO TRNFLAG.
       LOOPTRAN.
           IF TRANID(I) = WTRAN GO TO FIN-LOOK.
           ADD 1 TO I.
           IF I <= LSIZE GO TO LOOPTRAN.

           MOVE 1                                      TO TRNFLAG.
           MOVE WTRAN                                  TO MSGTRAN.
           MOVE 'INQUT009 Se encuentra caida la Trn '  TO MSGTXT.
           PERFORM OP-MSG.
      *
       FIN-LOOK. EXIT.
      */////////////////////
       START-ALL SECTION.
      */////////////////////
           MOVE QUE-TRANO TO WTRAN.
           PERFORM START-TRN.
           MOVE QUE-TRANI TO WTRAN.
           PERFORM START-TRN.
       FIN-STALL. EXIT.
      */////////////////////
       PURGE-TRN SECTION.
      */////////////////////
           MOVE 0  TO PURFLAG.
           MOVE WTRAN TO MSGTRAN.
           EXEC CICS SET TASK(TASKID(I))    FORCEPURGE
                RESP(RESPON) RESP2(RESPON2) NOHANDLE END-EXEC.
           IF RESPON NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT012 Error PURGE RESP]=0       ' TO MSGTXT
              MOVE 1 TO PURFLAG
           ELSE
              IF RESPON2 EQUAL 0
                 MOVE 'INQUT010 PURGE OK      transaccion ' TO MSGTXT
                 MOVE 0 TO PURFLAG
              ELSE
                 IF RESPON2 EQUAL  13
                    MOVE 'INQUT011 PURGE DEFERRED RSP2=13 ' TO MSGTXT
                    MOVE 2 TO PURFLAG
                 ELSE
                    MOVE 'INQUT013 Error PURGE RESP2]=13  ' TO MSGTXT
                    MOVE 1 TO PURFLAG
                 END-IF
              END-IF
           END-IF.
           PERFORM OP-MSG.
      *
       FIN-PURGE. EXIT.
      *////////////////////
       OPEN-QUE SECTION.
      */////////////////////
       OPENQ.
           EXEC CICS SET TDQUEUE ('INQT')
                OPEN   NOHANDLE
                RESP(RESPON) END-EXEC.
           IF RESPON NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT002 Error OPEN TD Queue INQT  ' TO MSGTXT
              PERFORM OP-MSG
              MOVE 5 TO CHKINT
              PERFORM RESTART-CHK
           END-IF.

           EXEC CICS ASSIGN APPLID(APPLCICS) RESP(RESPON) END-EXEC.
           IF RESPON IS NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT006 Error ASSIGN APPLID       ' TO MSGTXT
              PERFORM OP-MSG
              MOVE 5 TO CHKINT
              PERFORM RESTART-CHK
           END-IF.

       FIN-OPENQ. EXIT.
      *////////////////////
       CLOS-QUE SECTION.
      */////////////////////
       CLOSEQ.
           EXEC CICS SET TDQUEUE ('INQT')
                     CLOSED NOHANDLE
                     RESP(RESPON) END-EXEC.
           IF RESPON NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT005 Error CLOSE TD Queue INQT ' TO MSGTXT
              PERFORM OP-MSG
              PERFORM RESTART-CHK
           END-IF.

       FIN-CLOSQ. EXIT.
      *////////////////////
       READ-CTL SECTION.
      *////////////////////
          READCTL.
           EXEC CICS READQ TD QUEUE ('INQT')
                     INTO(QUE-REC)
                     LENGTH(FLEN80)
                     RESP(RESPON) END-EXEC.
           IF RESPON IS NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT004 Error READ QUEUE INQT CTL ' TO MSGTXT
              PERFORM OP-MSG
              MOVE 5 TO CHKINT
              PERFORM RESTART-CHK
           END-IF.

           MOVE QUE-INT  TO CHKINT.

      * Si tran=CPLT el programa viene de la PLT init
            IF EIBTRNID = 'CPLT'
               MOVE WMSGIN  TO WMSGOP
               PERFORM OP-MSG
               PERFORM CLOS-QUE
               MOVE 5 TO CHKINT
               PERFORM RESTART-CHK
            END-IF.

          FIN-READCTL. EXIT.
      */////////////////////
       RESTART-CHK SECTION.
      */////////////////////
           EXEC CICS START TRANSID(CHKTRAN) AFTER MINUTES(CHKINT)
                RESP(RESPON) NOHANDLE END-EXEC.
           IF RESPON NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT003 Error START tran TSOK '  TO MSGTXT
              PERFORM OP-MSG.

           EXEC CICS RETURN  END-EXEC.
       FIN-STRCHK. EXIT.
      *////////////////////
       GETABEND SECTION.
      */////////////////////
           PERFORM CLOS-QUE.
           MOVE 'INQUT007 ABEND Tran TSOK              ' TO MSGTXT.
           PERFORM OP-MSG.
           EXEC CICS RETURN  END-EXEC.

       FIN-ABEND. EXIT.
      */////////////////////
       OP-MSG SECTION.
      *////////////////////
            EXEC CICS ASKTIME ABSTIME(UTIME) NOHANDLE END-EXEC.
            EXEC CICS FORMATTIME ABSTIME(UTIME)
                 MMDDYYYY(MSG-FECHA) DATESEP('/')
                 TIME(MSG-HORA) TIMESEP NOHANDLE
            END-EXEC.

            EXEC CICS WRITEQ TD QUEUE('CSSL')
                      FROM(WMSGOP)
                      LENGTH(80)
                      NOHANDLE
                      END-EXEC.

       FIN-MSG. EXIT.
      */////////////////////
       START-TRN SECTION.
      */////////////////////
           MOVE WTRAN TO MSGTRAN.
           EXEC CICS START TRANSID(WTRAN)
                RESP(RESPON) END-EXEC.
           IF RESPON NOT EQUAL DFHRESP(NORMAL)
              MOVE 'INQUT015 Error START   transaccion ' TO MSGTXT
           ELSE
              MOVE 'INQUT014 Se arranca la transaccion ' TO MSGTXT
           END-IF.
           PERFORM OP-MSG.
      *
       FIN-START. EXIT.
      */////////////////////