      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TP2.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT HOR ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS HOR-KEY
           ALTERNATE RECORD KEY IS HOR-ALT-KEY WITH DUPLICATES
           FILE STATUS IS HOR-FS.

           SELECT CON ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CON-COD-CONS
           FILE STATUS IS CON-FS.

           SELECT PER ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PER-FS.
           SELECT PAR ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PAR-FS.

       DATA DIVISION.
       FILE SECTION.
       FD HOR
          VALUE OF FILE-ID IS "HORAS.DAT".
          01 HOR-REG.
            02 HOR-KEY.
               03 HOR-CONS PIC 9(3).
               03 HOR-ALT-KEY.
                   04 HOR-FECHA PIC X(8).
                   04 HOR-CLIENTE PIC 9(4).
           02 HOR-DATOS.
               03 HOR-CANT-HORAS PIC 9(2)V99.
               03 HOR-OBSERV PIC X(30).

       FD CON
       VALUE OF FILE-ID IS "CONSULTORES.TXT".
       01 CON-REG.
           03 CON-COD-CONS PIC 9(3).
           03 CON-FECHA-INGRESO PIC X(8).
           03 CON-DIRECCION PIC X(20).
           03 CON-APEYNOM PIC X(25).
           03 CON-TELEFONO PIC 9(10).
           03 CON-PERFIL PIC X.

       FD PER LABEL RECORD IS STANDARD
       VALUE OF FILE-ID IS "PERFILES.DAT".
       01 PER-REG.
           03 PER-PERFIL PIC X.
           03 PER-DESCRIPCION PIC X(15).
           03 PER-COND PIC X(50).

       FD PAR LABEL RECORD IS STANDARD
       VALUE OF FILE-ID IS "PARAMETROS.DAT".
       01 PAR-REG.
           03 PAR-FECHA-DESDE PIC X(8).
           03 PAR-FECHA-HASTA PIC X(8).
           03 PAR-CLIENTE-DESDE PIC 9(4).
           03 PAR-CLIENTE-HASTA PIC 9(4).

      *-----------------------
       WORKING-STORAGE SECTION.
       01 PAR-ENTRADA.
           03 PARAM-PERFIL PIC X.
           03 PARAM-FVIGENCIA PIC X(10).
       01 PAR-SALIDA.
           03 RESULTADO PIC 9(5).
       01 COD-OPER PIC X.

       01 HOR-FS PIC X(2).
           88 HOR-OK VALUE '00'.
           88 HOR-NO VALUE '23'.
           88 HOR-EOF VALUE '10'.

       01 CON-FS PIC X(2).
           88 CON-OK VALUE '00'.
           88 CON-NO VALUE '23'.
           88 CON-EOF VALUE '10'.

       01 PER-FS PIC X(2).
           88 PER-OK VALUE '00'.
           88 PER-NO VALUE '23'.
           88 PER-EOF VALUE '10'.

       01 PAR-FS PIC X(2).
           88 PAR-OK VALUE '00'.
           88 PAR-NO VALUE '23'.
           88 PAR-EOF VALUE '10'.

       
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM 010-INICIO.
           PERFORM 050-LEER-PAR.
           MOVE '2012-12-12' TO PARAM-FVIGENCIA.
           MOVE 'X' TO PARAM-PERFIL.
           PERFORM 070-IMPRIMIR-ENCAB.
           CALL 'TARIFAS' USING PAR-ENTRADA,PAR-SALIDA,COD-OPER.

           PERFORM 080-RECORRER-HOR.
           PERFORM 020-FIN.
           STOP RUN.

       010-INICIO.
           OPEN INPUT CON.
           OPEN INPUT HOR.
           OPEN INPUT PER.
           OPEN INPUT PAR.
           MOVE 'O' TO COD-OPER.
           CALL 'TARIFAS' USING PAR-ENTRADA,PAR-SALIDA,COD-OPER.
           MOVE 'P' TO COD-OPER.
           IF(NOT CON-OK) AND (NOT CON-EOF)
               DISPLAY "ERROR APERTURA CON"
               STOP RUN.
           IF(NOT HOR-OK) AND (NOT HOR-EOF)
               DISPLAY "ERROR APERTURA HOR"
               STOP RUN.
           IF(NOT PER-OK) AND (NOT PER-EOF)
               DISPLAY "ERROR APERTURA PER"
               STOP RUN.
           IF(NOT PAR-OK) AND (NOT PAR-EOF)
               DISPLAY "ERROR APERTURA PAR"
               STOP RUN.


       020-FIN.
           CLOSE CON.
           CLOSE HOR.
           CLOSE PAR.
           CLOSE PER.
           MOVE 'C' TO COD-OPER.
           CALL 'TARIFAS' USING PAR-ENTRADA,PAR-SALIDA,COD-OPER.

       030-LEER-CON.

       040-LEER-HOR.
           READ HOR NEXT RECORD.
           IF NOT HOR-OK AND NOT HOR-EOF
               DISPLAY 'ERROR DE LECTURA EN SIGUIENTE HOR'
               STOP RUN.

       050-LEER-PAR.
           READ PAR RECORD.
           IF(NOT PAR-OK) AND (NOT PAR-EOF)
               DISPLAY "ERROR LECTURA PAR"
               STOP RUN.

       060-LEER-PER.
           READ PER RECORD.
           IF(NOT PER-OK) AND (NOT PER-EOF)
               DISPLAY "ERROR LECTURA PER"
               STOP RUN.



       070-IMPRIMIR-ENCAB.
           DISPLAY "FALTA 070-IMPRIMIR-ENCAB".

       080-RECORRER-HOR.
           MOVE PAR-FECHA-DESDE TO HOR-FECHA.
           MOVE PAR-CLIENTE-DESDE TO HOR-CLIENTE.
           START HOR KEY IS NOT LESS THAN HOR-ALT-KEY
            INVALID KEY
                DISPLAY 'ERROR AL BUSCAR EN HOR'
                STOP RUN.
           PERFORM 040-LEER-HOR.
           PERFORM 100-PROCESAR-CONS-HOR UNTIL
                                       HOR-EOF OR
                                       (HOR-FECHA > PAR-FECHA-HASTA AND
                                       HOR-CLIENTE > PAR-CLIENTE-HASTA).



       100-PROCESAR-CONS-HOR.
           IF HOR-FECHA <= PAR-FECHA-HASTA AND
           HOR-CLIENTE <= PAR-CLIENTE-HASTA
               DISPLAY '--------'
               DISPLAY HOR-CLIENTE
               DISPLAY HOR-FECHA
               DISPLAY HOR-CANT-HORAS.
      *        leer cons usando el campo de hor, mezclar registros y
      *        mandarlos para SORT
           PERFORM 040-LEER-HOR.
       END PROGRAM TP2.
