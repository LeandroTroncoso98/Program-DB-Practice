       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDB2003.
       AUTHOR. TRONCOSO LEANDRO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * FICHERO DE ENTRADA DE MOVIMIENTOS
           SELECT FICHERO-ENT ASSIGN TO ENTRADA
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

      * FICHERO DE SALIDA DE INCIDENCIAS
           SELECT FICHERO-SAL ASSIGN TO SALIDA
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.

      * REGISTRO DEL FICHERO DE ENTRADA
       FD FICHERO-ENT RECORDING MODE IS F
                      DATA RECORD IS REG-ENTRADA.
       01 REG-ENTRADA             PIC X(81).

       FD FICHERO-SAL RECORDING MODE IS F
                      DATA RECORD IS REG-SALIDA.
       01 REG-SALIDA              PIC X(80).

       WORKING-STORAGE SECTION.

      * CAMPOS DE ENTRADA
       01 WS-ENTRADA.
          05 WS-E-CONT-ORIGEN     PIC 9(9).
          05 WS-E-CONT-DESTINO    PIC 9(9).                                       05 WS-E-IMPORTE         PIC 9(11)V99.
          05 WS-E-DESCRIPCION     PIC X(50).

      * CAMPOS DE SALIDA
       01 WS-SALIDA.
          05 WS-S-SQLCODE         PIC -999.
          05 WS-S-DESCRIPCION     PIC X(76).

      * FILESTATUS DEL FICHERO ENTRADA
       01 FS-ENTRADA              PIC 99.
          88 FS-ENTRADA-END       VALUE 10.

      * FILESTATUS DEL FICHERO SALIDA
       01 FS-SALIDA               PIC 99.

      * VARIABLES IN PROGRAM.
       77 WS-COMMIT               PIC 99.
       77 WS-NUEVA-OPERACION      PIC 9(9).
       77 IND-NULL                PIC S9(4) COMP-5.
       77 WS-ERROR                PIC X.
          88 WS-ERROR-SI          VALUE 'S'.
          88 WS-ERROR-NO          VALUE 'N'.

      * SQLCA Y DCLGEN DE LA TABLA
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE MOVIMIEN END-EXEC.
           EXEC SQL INCLUDE CONTRATO END-EXEC.

       PROCEDURE DIVISION.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL FS-ENTRADA-END.
           PERFORM 3000-FIN.
      ******************************************************************
      ** PARRAFO DE INICIO.                                           **
      ******************************************************************
       1000-INICIO.
           DISPLAY 'INICIO'
           INITIALIZE WS-COMMIT

       * APERTURA DE FICHEROS DE ENTRADA Y SALIDA
            OPEN INPUT FICHERO-ENT
            OPEN OUTPUT FICHERO-SAL

       * PRIMERA LECTURA DEL FICHERO DE ENTRADA
            READ FICHERO-ENT INTO WS-ENTRADA
       * CALCULAMOS EL NUMERO DEL PRIMER MOVIMIENTO
            PERFORM 1200-CALCULA-NUEVO-MOVIMIENTO.

      *****************************************************************
      * CALCULAMOS EL ID DEL MOVIMIENTO, RECUPERANDO EL MAYOR DE LOS  *
      * ALMACENADOS EN LA TABLA DE MOVIMIENTOS Y SUMANDOLE 1 O        *
      * PONIENDOLO DIRECTAMENTE A 1 SI EN LA TABLA NO HAY NI 1.       *
      *****************************************************************
        1200-CALCULA-NUEVO-MOVIMIENTO.
            DISPLAY 'CALCULA NUEVO MOVIMIENTO'
            EXEC SQL
              SELECT MAX(CLAVE_MOVIMIENTO)
              INTO :CLAVE-MOVIMIENTO :IND-NULL
              FROM IBMUSER.MOVIMIENTOS
            END-EXEC.

       * SI SE PRODUCE ALGUN ERROR, SALIMOS DEL PROGRAMA
            IF SQLCODE NOT = 0 THEN
               MOVE SQLCODE TO WS-S-SQLCODE
               MOVE 'ERROR AL RECUPERAR CLAVE' TO WS-S-DESCRIPCION
               MOVE WS-SALIDA TO REG-SALIDA
               WRITE REG-SALIDA
               PERFORM 9999-ERROR
            END-IF
       * SI IND-NULL ES -1 ES PORQUE NO HABIA MIVIMIENTOS EN LA
       *TABLA, PONEMOS EL NUMERO DE MOVIMIENTO A 1
            IF IND-NULL = -1
               MOVE 1 TO CLAVE-MOVIMIENTO
            ELSE
       * INCREMENTAMOS LA CLAVE DE MIVIMIENTO
             PERFORM 2400-INCREMENTA-CLAVE-MOV
           END-IF.

      *****************************************************************
       2000-PROCESO.
      *****************************************************************
           DISPLAY 'PROCESO'
           ADD 1 TO WS-COMMIT
           MOVE 'N' TO WS-ERROR
      * COMPROBAMOS QUE EL CONTRATO DESTINO ES CORRECTO.
           PERFORM 2200-COMPRUEBA-CONTRATO-DES
      * COMPROBAMOS QUE EL CONTRATO ORIGEN SEA CORRECTO
           IF WS-ERROR-NO THEN
              PERFORM 2300-COMPRUEBA-CONTRATO-ORI
           END-IF
      * ACTUALIZAMOS EL IMPORTE DE LOS CONTRATOS
           IF WS-ERROR-NO
              PERFORM 2500-ACTUALIZA-IMPORTE-CONT
           END-IF
      * INSERTAMOS EL REGISTRO EN LA TABLA DE MOVIMIENTOS
           IF WS-ERROR-NO THEN
              PERFORM 2700-INSERTA-MOVIMIENTO
           END-IF
      * INCREMENTAMOS LA CLAVE DEL MOVIMIENTO
           IF WS-ERROR-NO THEN
              PERFORM 2400-INCREMENTA-CLAVE-MOV
           END-IF
      * SIGUIENTE LECTURA DEL FICHERO DE ENTRADA
           READ FICHERO-ENT INTO WS-ENTRADA
           IF WS-COMMIT = 10 THEN
              EXEC SQL COMMIT END-EXEC
           END-IF.

      *****************************************************************
      * COMPRUEBA QUE EL CONTRATO DESTINO SEA VIGENTE                 *
      *****************************************************************
       2200-COMPRUEBA-CONTRATO-DES.
           DISPLAY 'COMPROBAMOS ESTADO CONTRATO DESTINO'
      * RECUPERAMOS EL IMPORTE DEL CONTRATO ORIGEN DEL MOVIMIENTO
          MOVE WS-E-CONT-DESTINO TO CLAVE-CONTRATO OF DCLCONTRATOS

          EXEC SQL
             SELECT ESTADO
             INTO :DCLCONTRATOS.ESTADO
             FROM IBMUSER.CONTRATOS
             WHERE CLAVE_CONTRATO = :DCLCONTRATOS.CLAVE-CONTRATO
          END-EXEC
     * EN CASO DE ERROR SALIMOS DEL PROGRAMA
          IF SQLCODE NOT = 0
             MOVE SQLCODE TO WS-S-SQLCODE
             MOVE 'ERROR AL RECUPERAR ESTADO DESTINO'
                          TO WS-S-DESCRIPCION
             MOVE WS-SALIDA TO REG-SALIDA
             WRITE REG-SALIDA
             PERFORM 9999-ERROR
          END-IF
          IF ESTADO OF DCLCONTRATOS NOT EQUAL 'V'
             MOVE 0 TO WS-S-SQLCODE
             MOVE 'CONTRATO DESTINO NO OPERATIVO. MOVIMIENTO ERRONEO'
                          TO WS-S-DESCRIPCION
             MOVE WS-SALIDA TO WS-S-DESCRIPCION
             MOVE WS-SALIDA TO REG-SALIDA
             WRITE REG-SALIDA
             MOVE 'S' TO WS-ERROR
          END-IF.
     ******************************************************************
     * COMPRUEBA QUE EL CONTRATO DE ORIGEN PUEDA SOPORTAR EL MOVIMIENTO
     * CONTROLADO QUE EL ESTADO SEA ABIERTO Y QUE EL IMPORTE FINAL
     * ES MAYOR QUE CERO
     ******************************************************************
      2300-COMPRUEBA-CONTRATO-ORI.
          DISPLAY 'RECUPERAMOS ESTADO/IMPORTE CONTRATO ORIGEN'
                  WS-E-CONT-ORIGEN
     * RECUPERAMOS EL IMPORTE DEL CONTRATO ORIGEN DEL MOVIMIENTO
          MOVE WS-E-CONT-ORIGEN TO CLAVE-CONTRATO OF DCLCONTRATOS
          EXEC SQL
               SELECT IMPORTE, ESTADO
                INTO :DCLCONTRATOS.IMPORTE, :DCLCONTRATOS.ESTADO
                FROM IBMUSER.CONTRATOS
                WHERE CLAVE_CONTRATO = :DCLCONTRATOS.CLAVE-CONTRATO
           END-EXEC
           IF SQLCODE NOT = 0 THEN
              MOVE SQLCODE TO WS-S-SQLCODE
              MOVE 'ERROR AL RECUPERAR IMPORTE ORIGEN'
                       TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              PERFORM 9999-ERROR
           END-IF
           DISPLAY 'CALCULAMOS EL NUEVO IMPORTE'
      * CALCULAMOS EL NUEVO IMPORTE DEL CONTRATO, RESTANDOLE AL INICIAL
      * EL IMPORTE DEL MOVIMIENTO
           COMPUTE IMPORTE OF DCLCONTRATOS = IMPORTE OF DCLCONTRATOS -
                   WS-E-IMPORTE
      * EN CASO DE QUE EL IMPORTE CALCULADO SEA MENOR QUE CERO,
      * NO PODEMOS CONTINUAR, PORQUE UN CONTRATO NO PUEDE QUEDAR
      * AL DESCUBIERTO.
           IF ESTADO OF DCLCONTRATOS NOT = 'V'
              MOVE 0 TO WS-S-SQLCODE
              MOVE 'CONTRATO ORIGEN NO OPERATIVO. MOVIMIENTO ERRONEO'
                                     TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              MOVE 'S' TO WS-ERROR
           ELSE
              IF IMPORTE OF DCLCONTRATOS LESS THAN ZERO
                 MOVE 0 TO WS-S-SQLCODE
                 MOVE 'CONTRATO DE ORIGEN AL DESCUBIERTO. ERROR'
                                           TO WS-S-DESCRIPCION
                 MOVE WS-SALIDA TO REG-SALIDA
                 WRITE REG-SALIDA
                 MOVE 'S' TO WS-ERROR
              END-IF
           END-IF.

      * ***************************************************************
      * ACTUALIZAMOS LOS IMPORTES DE LOS CONTRATOS ORIGEN Y DESTINO
      * ***************************************************************
       2500-ACTUALIZA-IMPORTE-CONT.
           DISPLAY 'ACTUALIZAMOS IMPORTES EN CONTRATOS'
      * ACTUALIZAMOS EL IMPORTE DEL CONTRATO CON EL NUEVO VALOR
           EXEC SQL
             UPDATE IBMUSER.CONTRATOS
             SET IMPORTE = :DCLCONTRATOS.IMPORTE
             WHERE CLAVE_CONTRATO = :DCLCONTRATOS.CLAVE-CONTRATO
           END-EXEC
      * EN CASO DE ERROR SALIMOS DEL PROGRAMA
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO WS-S-SQLCODE
              MOVE 'ERROR AL ACTUALIZAR IMPORTE ORIGEN'
                                  TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              PERFORM 9999-ERROR
           END-IF
      * RECUPERAMOS EL IMPORTE DEL CONTRATO DESTINO
           MOVE WS-E-CONT-DESTINO TO CLAVE-CONTRATO OF DCLCONTRATOS
           EXEC SQL
             SELECT IMPORTE
             INTO :DCLCONTRATOS.IMPORTE
             FROM IBMUSER.CONTRATOS
             WHERE CLAVE_CONTRATO = :DCLCONTRATOS.CLAVE-CONTRATO
           END-EXEC.
      * EN CASO DE ERROR SALIMOS DEL PROGRAMA
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO WS-S-SQLCODE
              MOVE 'ERROR AL RECUPERAR IMPORTE DE DESTINO'
                                  TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              PERFORM 9999-ERROR
           END-IF
      * CALCULAMOS EL IMPORTE DEL CONTRATO DESTINO, SUMANDOLE  EL
      * DEL MOVIMIENTO
           COMPUTE IMPORTE OF DCLCONTRATOS = IMPORTE OF DCLCONTRATOS +
                   WS-E-IMPORTE
      * ACTUALIZAMOS EL IMPORTE DEL CONTRATO DESTINO
           EXEC SQL
             UPDATE IBMUSER.CONTRATOS
             SET IMPORTE = :DCLCONTRATOS.IMPORTE
             WHERE CLAVE_CONTRATO = :DCLCONTRATOS.CLAVE-CONTRATO
           END-EXEC
      * EN CASO DE ERROR SALIMOS DEL PROGRAMA
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO WS-S-SQLCODE
              MOVE 'ERROR AL ACTUALIZAR IMPORTE DESTINO'
                                        TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              PERFORM 9999-ERROR.
      * ****************************************************************
      * INCREMENTA EN 1 LA CLAVE DEL MOVIMIENTO ANTES DE INSERTARLA
      * EN LA TABLA DE MOVIMIENTOS
      * ****************************************************************
       2700-INSERTA-MOVIMIENTO.
           DISPLAY 'INSERTAMOS DATOS DE MOVIMIENTO'
      * MOVEMOS LOS CAMPOS DEL REGISTRO LEIDO A LAS VARIABLES HOST
           MOVE WS-E-CONT-ORIGEN TO ORIGEN
           MOVE WS-E-CONT-DESTINO TO DESTINO
           MOVE WS-E-IMPORTE TO IMPORTE OF DCLMOVIMIENTOS
           MOVE WS-E-DESCRIPCION TO DESCRIPCION OF DCLMOVIMIENTOS
      * INSERTAMOS EL REGISTRO EN LA TABLA
           EXEC SQL
             INSERT INTO IBMUSER.MOVIMIENTOS
               (CLAVE_MOVIMIENTO,
                ORIGEN,
                DESTINO,
                DESCRIPCION,
                IMPORTE)
             VALUES(:CLAVE-MOVIMIENTO,
                    :ORIGEN,
                    :DESTINO,
                    :DCLMOVIMIENTOS.DESCRIPCION,
                    :DCLMOVIMIENTOS.IMPORTE)
           END-EXEC
      * EN CASO DE ERROR
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO WS-S-SQLCODE
              MOVE 'ERROR AL INSERTAR MOVIMIENTO' TO WS-S-DESCRIPCION
              MOVE WS-SALIDA TO REG-SALIDA
              WRITE REG-SALIDA
              PERFORM 9999-ERROR
            END-IF.
      ******************************************************************
      * INCREMENTA EN 1 LA CLAVE DEL MOVIMIENTO ANTES DE INSERTARLA
      * EN LA TABLA DE MOVIMIENTOS
      ******************************************************************
       2400-INCREMENTA-CLAVE-MOV.
           DISPLAY 'INCREMENTAMOS CLAVE MOVIMIENTO'
      * CALCULAMOS EL SIGUIENT ID DEL MOVIMIENTO, SUMANDO 1 AL ANTERIOR
           COMPUTE CLAVE-MOVIMIENTO = CLAVE-MOVIMIENTO + 1.
      ******************************************************************
      * PARRAFO DE FIN DEL PROGRAMA
      ******************************************************************
       3000-FIN.
           DISPLAY 'FIN'
      * CERRAMOS FICHEROS Y DEVOLVEMOS EL CONTROL AL SO.
           CLOSE FICHERO-ENT
           CLOSE FICHERO-SAL
           STOP RUN.
      ******************************************************************
      * PARRAFO DE ERROR, REALIZAMOS UN ROLLBACK DE LA BASE DE DATOS   *
      * Y DEVOLVEMOS AL SISTEMA UN RC=8 (ERROR NO CONTROLADO).         *
      ******************************************************************
       9999-ERROR.
           DISPLAY 'ERROR'
           EXEC SQL ROLLBACK END-EXEC
           MOVE 8 TO RETURN-CODE
           GOBACK.
