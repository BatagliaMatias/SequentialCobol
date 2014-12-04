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
           SELECT ARCHTRABAJO ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
   *****VER ACA. FALTA DEFINIR BIEN
       SD ARCHTRABAJO DATA RECORD IS REG-TRA.
       01 REG-TRA.
           03 REG-TRA-APEYNOM PIC X(25).
           03 REG-TRA-COD-CONS PIC 9(3).
           03 REG-TRA-TELEFONO PIC 9(10).
           03 REG-TRA-DESC-PER PIC X(15).
           03 REG-TRA-CLIENTE PIC 9(4).
           03 REG-TRA-FECHA PIC X(8).
           03 REG-TRA-CANT-HORAS PIC 9(2)V99.
           03 REG-TRA-TARIFA PIC 9(7)V99.

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
       VALUE OF FILE-ID IS "CONSULTORES.DAT".
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
           03 PARAM-TARIFA PIC 9(7)V99.
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

       01 WS-FECHA PIC X(10) VALUE '----------'.

       01 FILLER REDEFINES WS-FECHA.

           03 WS-FEC-AA PIC 9(04).

           03 FILLER      PIC X .

           03 WS-FEC-MM PIC 9(02).

           03 FILLER      PIC X.

           03 WS-FEC-DD PIC 9(02).

       01 WS-FEC-8.

           03 WS-FEC-8-AA    PIC 9(04).

           03 WS-FEC-8-MM PIC 9(02).

           03 WS-FEC-8-DD   PIC 9(02).

       01 WS-LINEA PIC X(79).

       01 WS-LINEA-BLANCO PIC X(79) VALUE ALL " ".

       01 WS-ENC-L1.
           03 FILLER                   PIC X(7) VALUE "Fecha: ".
           03 WS-ENC-L1-AAAA           PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L1-MM             PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L1-DD             PIC 99.
           03 FILLER                   PIC X(51) VALUE ALL " ".
           03 FILLER                   PIC X(9) VALUE "Hoja nro ".
           03 WS-ENC-L1-HOJA           PIC 99 VALUE IS 0.

       01 WS-ENC-L5.
           03 FILLER                   PIC X(13) VALUE "Fecha desde: ".
           03 WS-ENC-L5-AAAA-DESDE     PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L5-MM-DESDE       PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L5-DD-DESDE       PIC 99.
           03 FILLER                   PIC X(8) VALUE " hasta: ".
           03 WS-ENC-L5-AAAA-HASTA     PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L5-MM-HASTA       PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 WS-ENC-L5-DD-HASTA       PIC 99.

       01 WS-ENC-L6.
           03 FILLER                  PIC X(15) VALUE "Cliente desde: ".
           03 WS-ENC-L6-CLI-DESDE     PIC 9999.
           03 FILLER                  PIC X(8) VALUE " hasta: ".
           03 WS-ENC-L6-CLI-HASTA     PIC 9999.

       01 WS-ENC-CONS-L1.
           03 FILLER              PIC X(19) VALUE "Apellido y Nombre: ".
           03 WS-ENC-CONS-L1-NOM  PIC X(25).
           03 FILLER              PIC X(15) VALUE " Codigo Cons.: ".
           03 WS-ENC-CONS-L1-COD  PIC 999.
           03 FILLER              PIC X(6) VALUE " Tel: ".
           03 WS-ENC-CONS-L1-TEL  PIC 9(10).

       01 WS-ENC-CONS-L2.
           03 FILLER              PIC X(8) VALUE "Perfil: ".
           03 WS-ENC-CONS-L2-PER  PIC X(15).

       01 WS-NRO-LINEA PIC 9(2) VALUE IS 1.

       01 WS-AT-EOF         PIC X(02).

       01 WS-CONS           PIC 999.
       01 WS-CLI            PIC 9999.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM 010-INICIO.
           PERFORM 050-LEER-PAR.
           MOVE PAR-FECHA-DESDE TO WS-FEC-8
           MOVE WS-FEC-8-AA   TO WS-FEC-AA.
           MOVE WS-FEC-8-MM TO WS-FEC-MM.
           MOVE WS-FEC-8-DD   TO WS-FEC-DD.
           MOVE WS-FECHA TO PARAM-FVIGENCIA.
           MOVE 'X' TO PARAM-PERFIL.
           PERFORM 070-IMPRIMIR-ENCAB.


      *     PERFORM 080-RECORRER-HOR.
           SORT ARCHTRABAJO
             ON ASCENDING KEY REG-TRA-APEYNOM
             ON ASCENDING KEY REG-TRA-COD-CONS
             ON ASCENDING KEY REG-TRA-CLIENTE
             ON ASCENDING KEY REG-TRA-FECHA
             INPUT PROCEDURE 080-RECORRER-HOR
             OUTPUT PROCEDURE 110-IMPRIMIR-LISTADO
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
           READ CON RECORD.
           IF CON-NO
               DISPLAY 'ERROR DE LECTURA EN CON'
               STOP RUN.

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
           MOVE FUNCTION CURRENT-DATE (1:4) TO WS-ENC-L1-AAAA.
           MOVE FUNCTION CURRENT-DATE (5:2) TO WS-ENC-L1-MM.
           MOVE FUNCTION CURRENT-DATE (7:2) TO WS-ENC-L1-DD.
           ADD 1 TO WS-ENC-L1-HOJA.
           MOVE 1 TO WS-NRO-LINEA.
           MOVE WS-ENC-L1 TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           MOVE "LISTADO DE FACTURACION" TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           MOVE PAR-FECHA-DESDE TO WS-FEC-8.
           MOVE WS-FEC-8-AA TO WS-ENC-L5-AAAA-DESDE.
           MOVE WS-FEC-8-MM TO WS-ENC-L5-MM-DESDE.
           MOVE WS-FEC-8-DD TO WS-ENC-L5-DD-DESDE.
           MOVE PAR-FECHA-HASTA TO WS-FEC-8.
           MOVE WS-FEC-8-AA TO WS-ENC-L5-AAAA-HASTA.
           MOVE WS-FEC-8-MM TO WS-ENC-L5-MM-HASTA.
           MOVE WS-FEC-8-DD TO WS-ENC-L5-DD-HASTA.
           MOVE WS-ENC-L5 TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           MOVE PAR-CLIENTE-DESDE TO WS-ENC-L6-CLI-DESDE.
           MOVE PAR-CLIENTE-HASTA TO WS-ENC-L6-CLI-HASTA.
           MOVE WS-ENC-L6 TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.

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
               MOVE HOR-CONS TO CON-COD-CONS
               PERFORM 030-LEER-CON
               CALL 'TARIFAS' USING PAR-ENTRADA,PAR-SALIDA,COD-OPER
               MOVE CON-APEYNOM TO REG-TRA-APEYNOM
               MOVE CON-COD-CONS TO REG-TRA-COD-CONS
               MOVE CON-TELEFONO TO REG-TRA-TELEFONO
               MOVE CON-PERFIL TO PER-PERFIL
               READ PER RECORD
               MOVE PER-DESCRIPCION TO REG-TRA-DESC-PER
               MOVE HOR-CLIENTE TO REG-TRA-CLIENTE
               MOVE HOR-FECHA TO REG-TRA-FECHA
               MOVE HOR-CANT-HORAS TO REG-TRA-CANT-HORAS
               MOVE PARAM-TARIFA TO REG-TRA-TARIFA
               RELEASE REG-TRA.
           PERFORM 040-LEER-HOR.

       110-IMPRIMIR-LISTADO.
           RETURN ARCHTRABAJO INTO REG-TRA
               AT END
                   SET WS-AT-EOF TO "SI"
               NOT AT END
                   SET WS-AT-EOF TO "NO"
           END-RETURN
           PERFORM 120-IMPRIMIR-CONS UNTIL WS-AT-EOF = "SI".
           DISPLAY "FALTA TOTAL GRAL".

       120-IMPRIMIR-CONS.
           MOVE REG-TRA-COD-CONS TO WS-CONS.
           MOVE REG-TRA-APEYNOM TO WS-ENC-CONS-L1-NOM.
           MOVE REG-TRA-COD-CONS TO WS-ENC-CONS-L1-COD.
           MOVE REG-TRA-TELEFONO TO WS-ENC-CONS-L1-TEL.
           MOVE WS-ENC-CONS-L1 TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           MOVE REG-TRA-DESC-PER TO WS-ENC-CONS-L2-PER.
           MOVE WS-ENC-CONS-L2 TO WS-LINEA.
           PERFORM 130-IMPRIMIR-LINEA.
           PERFORM 140-IMPRIMIR-CLI UNTIL (WS-AT-EOF = "SI"
                                    OR REG-TRA-COD-CONS <> WS-CONS).
           DISPLAY "FALTA TOTAL CONS".

       130-IMPRIMIR-LINEA.
           DISPLAY WS-LINEA.
           MOVE WS-LINEA-BLANCO TO WS-LINEA.
           ADD 1 TO WS-NRO-LINEA.
           IF WS-NRO-LINEA > 60
               PERFORM 070-IMPRIMIR-ENCAB.

       140-IMPRIMIR-CLI.
           DISPLAY "FALTA 140-IMPRIMIR-CLI"
           MOVE REG-TRA-CLIENTE TO WS-CLI.
           PERFORM 140-IMPRIMIR-FECHA UNTIL (WS-AT-EOF = "SI"
                                    OR REG-TRA-COD-CONS <> WS-CONS
                                    OR REG-TRA-CLIENTE <> WS-CLI).
           DISPLAY "FALTA TOTAL CLIENTE".

       140-IMPRIMIR-FECHA.
           DISPLAY "FECHA"
           RETURN ARCHTRABAJO INTO REG-TRA
               AT END
                   SET WS-AT-EOF TO "SI"
               NOT AT END
                   SET WS-AT-EOF TO "NO"
           END-RETURN.

       END PROGRAM TP2.
