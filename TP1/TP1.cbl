       IDENTIFICATION DIVISION.

       PROGRAM-ID. TP-1.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT HS1 ASSIGN TO "HS1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS HS1-ESTADO.

           SELECT HS2 ASSIGN TO "HS2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS HS2-ESTADO.

           SELECT HS3 ASSIGN TO "HS3.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS HS3-ESTADO.

           SELECT VAL ASSIGN TO "VAL-HORA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS VAL-ESTADO.

           SELECT CONS ASSIGN TO "CONSUL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS CONS-ESTADO.

           SELECT LIS-IMP ASSIGN TO "LIS_IMP.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LIS-NOM ASSIGN TO "LIS_NOM.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LIS-EST ASSIGN TO "LIS_EST.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD HS1.
       01 REG-HS1.
           03 HS1-CONS                 PIC 9(3).
           03 HS1-FECHA                PIC X(8).
           03 HS1-CLIENTE              PIC 9(4).
           03 HS1-CANT-HORAS           PIC 9(2)V99.
           03 HS1-OBSERV               PIC X(30).

       FD HS2.
       01 REG-HS2.
           03 HS2-CONS                 PIC 9(3).
           03 HS2-FECHA                PIC X(8).
           03 HS2-CLIENTE              PIC 9(4).
           03 HS2-CANT-HORAS           PIC 9(2)V99.
           03 HS2-OBSERV               PIC X(30).

       FD HS3.
       01 REG-HS3.
           03 HS3-CONS                 PIC 9(3).
           03 HS3-FECHA                PIC X(8).
           03 HS3-CLIENTE              PIC 9(4).
           03 HS3-CANT-HORAS           PIC 9(2)V99.
           03 HS3-OBSERV               PIC X(30).

       FD VAL.
       01 REG-VAL.
           03 VAL-CONS                 PIC 9(3).
           03 VAL-FEC-DESDE            PIC X(8).
           03 VAL-FEC-HASTA            PIC X(8).
           03 VAL-VALOR-HORA           PIC 9(7)V99.

       FD CONS.
       01 REG-CONS.
           03 CONS-CONS                PIC 9(3).
           03 CONS-FECHA-ALTA          PIC X(8).
           03 CONS-NOMBRE              PIC X(25).

       FD LIS-IMP
           LABEL RECORD OMITTED.
       01 LIS-IMP-LINEA               PIC X(80).

       FD LIS-NOM
           LABEL RECORD OMITTED.
       01 LIS-NOM-LINEA               PIC X(80).

       FD LIS-EST
           LABEL RECORD OMITTED.
       01 LIS-EST-LINEA              PIC X(250).

       WORKING-STORAGE SECTION.

       77 HS1-EOF                      PIC XX VALUE "NO".
           88 EOF                             VALUE "SI".
       77 HS1-ESTADO                   PIC XX.

       77 HS2-EOF                      PIC XX VALUE "NO".
           88 EOF                             VALUE "SI".
       77 HS2-ESTADO                   PIC XX.

       77 HS3-EOF                      PIC XX VALUE "NO".
           88 EOF                             VALUE "SI".
       77 HS3-ESTADO                   PIC XX.

       77 VAL-EOF                      PIC XX VALUE "NO".
           88 EOF                             VALUE "SI".
       77 VAL-ESTADO                   PIC XX.

       77 CONS-EOF                     PIC XX VALUE "NO".
           88 EOF                             VALUE "SI".
       77 CONS-ESTADO                  PIC XX.

       01 WS-T-CONS-COUNT              PIC 9(4) VALUE IS 0.

       01 WS-T-CONS.
           03 WS-T-CONS-CAMPO OCCURS 0 TO 1000 TIMES
                              DEPENDING ON WS-T-CONS-COUNT
                              ASCENDING KEY IS WS-T-CONS-CONS
                              INDEXED BY WS-T-CONS-I.
               05 WS-T-CONS-CONS       PIC 9(3).
               05 WS-T-CONS-FECHA-ALTA PIC X(8).
               05 WS-T-CONS-NOMBRE     PIC X(25).

       01 WS-T-STATS.
           03 WS-T-STATS-FILAS OCCURS 1000 TIMES.
               05 WS-T-STATS-MESES OCCURS 13 TIMES.
                   07 WS-T-STATS-HORAS  PIC 999V99 VALUE IS 0.

       01 WS-T-STATS-TOTALES-MES.
           03 WS-T-STATS-TOTAL OCCURS 13 TIMES PIC 999V99 VALUE IS 0.

       01 WS-VALOR-HORA-DISPLAY         PIC ZZZ9.

       01 WS-ACUM                       PIC 999V99 VALUE IS 0.

       01 WS-MES                       PIC 99 VALUE IS 1.

       01 WS-T-CONS-CAMPO-TEMP.
           03 FILLER                   PIC 9(3).
           03 FILLER                   PIC X(8).
           03 FILLER                   PIC X(25).

      * CONTADORES:
       01 WS-G                         PIC 9(4).
       01 WS-I                         PIC 9(4) VALUE IS 1.
       01 WS-J                         PIC 9(4).

       01 WS-TOT-GRAL-VAL              PIC 9(7)V99 VALUE IS ZERO.
       01 WS-TOT-GRAL-HS               PIC 9(7)V99 VALUE IS ZERO.

       01 WS-MENOR-CONS                PIC 9(3).

       01 LI-ENC1.
           03 FILLER                   PIC X(7) VALUE "Fecha: ".
           03 LI-ENC-FECHA-AAAA            PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 LI-ENC-FECHA-MM              PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 LI-ENC-FECHA-DD              PIC 99.
           03 FILLER                   PIC X(51) VALUE ALL " ".
           03 FILLER                   PIC X(9) VALUE "Hoja nro ".
           03 LI-HOJA                  PIC 99 VALUE IS 0.

       01 LI-ENC2.
           03 FILLER                   PIC X(26) VALUE ALL " ".
           03 FILLER      PIC X(27) VALUE "LISTADO DE TOTALES DE HORAS".

       01 LI-LINEA-BL             PIC X(80) VALUE ALL " ".

       01 LI-NRO-LINEA                 PIC 99 VALUE IS 0.

       01 LI-LINEA1.
           03 FILLER              PIC X(20) VALUE IS "COD. CONSULTOR: ".
           03 LI-LINEA1-CONS      PIC Z(3).

       01 LI-LINEA2.
           03 FILLER              PIC X(15) VALUE IS "FECHA: ".
           03 LI-LINEA2-AA        PIC 99.
           03 FILLER              PIC X VALUE "/".
           03 LI-LINEA2-MM        PIC 99.
           03 FILLER              PIC X VALUE "/".
           03 LI-LINEA2-DD        PIC 99.

       01 LI-LINEA3.
           03 FILLER              PIC X(19) VALUE IS "COD. CLIENTE: ".
           03 LI-LINEA3-CLI       PIC Z(4).

       01 LI-LINEA4.
           03 FILLER              PIC X(17) VALUE IS "TOTAL HORAS: ".
           03 LI-LINEA4-HORAS     PIC ZZ9,99.

       01 LI-LINEA5.
           03 FILLER              PIC X(13) VALUE IS "TOTAL VALOR: ".
           03 LI-LINEA5-VALOR     PIC ZZZZZZ9,99.

       01 LN-ENC1.
           03 FILLER                   PIC X(7) VALUE "Fecha: ".
           03 LN-ENC-FECHA-AAAA        PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 LN-ENC-FECHA-MM          PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 LN-ENC-FECHA-DD          PIC 99.
           03 FILLER                   PIC X(51) VALUE ALL " ".
           03 FILLER                   PIC X(9) VALUE "Hoja nro ".
           03 LN-HOJA                  PIC 99 VALUE IS 0.

       01 LN-ENC2.
           03 FILLER PIC X(24) VALUE ALL " ".
           03 FILLER PIC X(32) VALUE "TIMES - LISTADO HORAS TRABAJADAS".

       01 LN-ENC-CLI.
           03 FILLER              PIC X(16) VALUE IS "COD. CONSULTOR: ".
           03 LN-ENC-CLI-CONS     PIC ZZ9.
           03 FILLER              PIC X(10) VALUE IS "  NOMBRE: ".
           03 LN-ENC-CLI-NOMB     PIC X(25).

       01 LN-ENC-CLI-FECHA.
           03 FILLER                     PIC X(7) VALUE IS "FECHA: ".
           03 LN-ENC-CLI-FECHA-AAAA      PIC 9999.
           03 FILLER                     PIC X VALUE "/".
           03 LN-ENC-CLI-FECHA-MM        PIC 99.
           03 FILLER                     PIC X VALUE "/".
           03 LN-ENC-CLI-FECHA-DD        PIC 99.

       01 LN-ENC-TABLA-CLI.
           03 FILLER              PIC X(13) VALUE " Cod Cliente ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC X(16) VALUE " Cantidad horas ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC X(11) VALUE "   Valor   ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC X(13) VALUE " Obervaciones".

       01 LN-LINEA-PUNT             PIC X(80) VALUE ALL "-".

       01 LN-LINEA-BL             PIC X(80) VALUE ALL " ".

       01 LN-NRO-LINEA                 PIC 99 VALUE IS 0.

       01 LN-LINEA-CLI.
           03 FILLER              PIC X(9) VALUE ALL " ".
           03 LN-LINEA-CLI-COD    PIC ZZ9.
           03 FILLER              PIC X VALUE " ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC X(10) VALUE ALL " ".
           03 LN-LINEA-CLI-CANT   PIC Z9,99.
           03 FILLER              PIC X VALUE " ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC XX VALUE "  ".
           03 LN-LINEA-CLI-VAL    PIC Z.ZZ9,99.
           03 FILLER              PIC X VALUE " ".
           03 FILLER              PIC X VALUE "|".
           03 FILLER              PIC X VALUE " ".
           03 LN-LINEA-CLI-OBS    PIC X(30).

       01 LN-LINEA-TOT-CLI.
           03 FILLER                 PIC X(13) VALUE "TOTAL CLIENTE".
           03 FILLER                 PIC X(11) VALUE ALL " ".
           03 LN-LINEA-TOT-CLI-HS    PIC Z9,99.
           03 FILLER                 PIC X(3) VALUE ALL " ".
           03 LN-LINEA-TOT-CLI-VAL   PIC ZZ.ZZ9,99.

       01 LN-LINEA-TOT-FECHA.
           03 FILLER                    PIC X(11) VALUE "TOTAL FECHA".
           03 FILLER                    PIC X(13) VALUE ALL " ".
           03 LN-LINEA-TOT-FECHA-HS     PIC Z9,99.
           03 FILLER                    PIC X(3) VALUE ALL " ".
           03 LN-LINEA-TOT-FECHA-VAL    PIC ZZ.ZZ9,99.

       01 LN-LINEA-TOT-CONS.
           03 FILLER                  PIC X(15) VALUE "TOTAL CONSULTOR".
           03 FILLER                  PIC X(8) VALUE ALL " ".
           03 LN-LINEA-TOT-CONS-HS    PIC ZZ9,99.
           03 FILLER                  PIC X(2) VALUE ALL " ".
           03 LN-LINEA-TOT-CONS-VAL   PIC ZZZ.ZZ9,99.

       01 LN-LINEA-TOT-GRAL.
           03 FILLER                  PIC X(13) VALUE "TOTAL GENERAL".
           03 FILLER                  PIC X(9) VALUE ALL " ".
           03 LN-LINEA-TOT-GRAL-HS    PIC ZZZ9,99.
           03 FILLER                  PIC X(1) VALUE ALL " ".
           03 LN-LINEA-TOT-GRAL-VAL   PIC ZZZZ.ZZ9,99.

       01 LE-ENC.
           03 FILLER PIC X(32) VALUE "ESTADISTICA DE HS TRABAJADAS POR".
           03 FILLER PIC X(31) VALUE " MES Y POR CONSULTOR EN EL ANIO".

       01 LE-THEADER.
           03 FILLER PIC X(26) VALUE ALL " ".
           03 FILLER PIC X(15) VALUE " ENERO       ".
           03 FILLER PIC X(15) VALUE " FEBRERO     ".
           03 FILLER PIC X(15) VALUE " MARZO       ".
           03 FILLER PIC X(15) VALUE " ABRIL       ".
           03 FILLER PIC X(15) VALUE " MAYO        ".
           03 FILLER PIC X(15) VALUE " JUNIO       ".
           03 FILLER PIC X(15) VALUE " JULIO       ".
           03 FILLER PIC X(15) VALUE " AGOSTO      ".
           03 FILLER PIC X(15) VALUE " SEPTIEMBRE  ".
           03 FILLER PIC X(15) VALUE " OCTUBRE     ".
           03 FILLER PIC X(15) VALUE " NOVIEMBRE   ".
           03 FILLER PIC X(15) VALUE " DICIEMBRE   ".
           03 FILLER PIC X(15) VALUE " TOTAL       ".

       01 LE-TROWS.
           03 LE-TR-NOMBRE PIC X(26) VALUE ALL " ".
           03 LE-TR-MESES OCCURS 12 TIMES.
             05 FILLER PIC X VALUE " ".
             05 LE-TR-HS PIC 999,99.
             05 FILLER PIC XXX VALUE " (".
             05 LE-TR-PC PIC 999.
             05 FILLER PIC XX VALUE "%)".
          03 LE-TR-TOTAL.
             05 FILLER PIC X VALUE " ".
             05 LE-TR-TT PIC 999,99.

       01 LE-TFOOTER.
           03 FILLER PIC X(26) VALUE "TOTAL ".
           03 LE-TF-MESES OCCURS 12 TIMES.
             05 FILLER PIC X VALUE " ".
             05 LE-TF-HS PIC 999,99.
             05 FILLER PIC XXX VALUE " (".
             05 LE-TF-PC PIC 999.
             05 FILLER PIC XX VALUE "%)".
          03 LE-TF-TOTAL.
             05 FILLER PIC X VALUE " ".
             05 LE-TF-TT PIC 999,99.

       01 LE-LINEA-BL             PIC X(250) VALUE ALL " ".

       01  WS-FECHA-HOY.
           03  WS-FECHA-HOY-AAAA       PIC  9(4).
           03  WS-FECHA-HOY-MM         PIC  9(2).
           03  WS-FECHA-HOY-DD         PIC  9(2).

       01 WS-MENOR-FECHA.
           03  WS-MENOR-FECHA-AAAA     PIC  9(4).
           03  WS-MENOR-FECHA-MM       PIC  9(2).
           03  WS-MENOR-FECHA-DD       PIC  9(2).

       01 WS-MENOR-CLIENTE             PIC 9(4).

       01 WS-TOT-FECHA-HS              PIC 9(2)V99 VALUE IS ZERO.
       01 WS-TOT-FECHA-VAL             PIC 9(5)V99 VALUE IS ZERO.

       01 WS-TOT-CLI-HS                PIC 9(2)V99.
       01 WS-TOT-CLI-VAL               PIC 9(5)V99.

       01 WS-LIS-HS.
           03 WS-LIS-HS-CONS                 PIC 9(3).
           03 WS-LIS-HS-FECHA.
               05  WS-LIS-HS-FECHA-AAAA     PIC  9(4).
               05  WS-LIS-HS-FECHA-MM       PIC  9(2).
               05  WS-LIS-HS-FECHA-DD       PIC  9(2).
           03 WS-LIS-HS-CLIENTE              PIC 9(4).
           03 WS-LIS-HS-CANT-HORAS           PIC 9(2)V99.
           03 WS-LIS-HS-OBSERV               PIC X(30).

       01 WS-VALOR                           PIC 9(7)V99.
       01 WS-VALOR-HORA                      PIC 9(5)V99.

       01 LI-VALOR-TOTAL-GRAL                PIC 9(7)V99 VALUE IS ZERO.
       01 LI-HORAS-TOTAL-GRAL                PIC 9(5)V99 VALUE IS ZERO.
       01 LI-HORAS-TOTAL-FECHA               PIC 9(2)V99 VALUE IS ZERO.

       01 LN-HORAS-CLI                       PIC 9(2)V99 VALUE IS ZERO.
       01 LN-VALOR-CLI                       PIC 9(5)V99 VALUE IS ZERO.

       01 WS-TOT-CONS-VAL                    PIC 9(6)V99 VALUE IS ZERO.
       01 WS-TOT-CONS-HS                     PIC 9(3)V99 VALUE IS ZERO.

       01 WS-PORC                           PIC 9(12)V9999.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            PERFORM 010-INICIO.
            PERFORM 020-LEER-CONS.
            PERFORM 030-CARGAR-T-CONS UNTIL CONS-EOF = 'SI'.
            PERFORM 040-ORDENAR-T-CONS.
            PERFORM 060-PROCESAR.
            PERFORM 360-IMPRIMIR-STAT.
            PERFORM 050-FIN.
            STOP RUN.

       010-INICIO.
           OPEN INPUT HS1, HS2, HS3.
           IF HS1-ESTADO NOT = ZERO
               DISPLAY "ERROR: No se pudo abrir el archivo HS1.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS1-ESTADO
               STOP RUN.
           IF HS2-ESTADO NOT = ZERO
               DISPLAY "ERROR: No se pudo abrir el archivo HS2.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS2-ESTADO
               STOP RUN.
           IF HS3-ESTADO NOT = ZERO
               DISPLAY "ERROR: No se pudo abrir el archivo HS3.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS3-ESTADO
               STOP RUN.
           OPEN INPUT VAL.
           IF VAL-ESTADO NOT = ZERO
               DISPLAY "ERROR: No se pudo abrir el archivo VAL-HORA.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " VAL-ESTADO
               STOP RUN.
           OPEN INPUT CONS.
           IF CONS-ESTADO NOT = ZERO
               DISPLAY "ERROR: No se pudo abrir el archivo CONSUL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " CONS-ESTADO
               STOP RUN.
           OPEN OUTPUT LIS-IMP, LIS-NOM, LIS-EST.

       020-LEER-CONS.
           READ CONS AT END MOVE "SI" TO CONS-EOF.
           IF CONS-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo CONSUL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " CONS-ESTADO.

       030-CARGAR-T-CONS.
           MOVE REG-CONS TO WS-T-CONS-CAMPO(WS-I).
           ADD 1 TO WS-I.
           ADD 1 TO WS-T-CONS-COUNT.
           PERFORM 020-LEER-CONS.

       040-ORDENAR-T-CONS.
      * BURBUJEO
           SUBTRACT 1 FROM WS-I.
           PERFORM VARYING WS-G FROM 1 BY 1 UNTIL WS-G = WS-I
               PERFORM VARYING WS-J FROM WS-G BY 1 UNTIL WS-J > WS-I
                   IF WS-T-CONS-CONS(WS-J) < WS-T-CONS-CONS(WS-G) THEN
                       MOVE WS-T-CONS-CAMPO(WS-G)
                           TO WS-T-CONS-CAMPO-TEMP
                       MOVE WS-T-CONS-CAMPO(WS-J)
                           TO WS-T-CONS-CAMPO(WS-G)
                       MOVE WS-T-CONS-CAMPO-TEMP
                           TO WS-T-CONS-CAMPO(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       050-FIN.
           CLOSE HS1, HS2, HS3.
           CLOSE VAL.
           CLOSE CONS.
           CLOSE LIS-IMP, LIS-NOM, LIS-EST.

       060-PROCESAR.
           PERFORM 070-LEER-HS1.
           PERFORM 080-LEER-HS2.
           PERFORM 090-LEER-HS3.
           PERFORM 100-LEER-VAL.
           MOVE 0 TO WS-TOT-GRAL-VAL.
           MOVE 0 TO WS-TOT-GRAL-HS.
           PERFORM 130-IMPRIMIR-LI-ENCAB.
           PERFORM 140-IMPRIMIR-LN-ENCAB.
           PERFORM 110-SUB-PROCESAR1 UNTIL HS1-EOF = "SI" AND
                                           HS2-EOF = "SI" AND
                                           HS3-EOF = "SI".
           PERFORM 310-IMPRIMIR-LN-TOTAL-GRAL.

       070-LEER-HS1.
           READ HS1 AT END MOVE "SI" TO HS1-EOF
           IF HS1-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS1.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS1-ESTADO
           END-IF.

       080-LEER-HS2.
           READ HS2 AT END MOVE "SI" TO HS2-EOF
           IF HS2-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS2.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS2-ESTADO
           END-IF.

       090-LEER-HS3.
           READ HS3 AT END MOVE "SI" TO HS3-EOF
           IF HS3-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS3.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS3-ESTADO
           END-IF.

       100-LEER-VAL.
           READ VAL
               AT END MOVE 'SI' TO VAL-EOF.
           IF VAL-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo VAL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " VAL-ESTADO.

       110-SUB-PROCESAR1.
           PERFORM 120-DET-MENOR-CONS.
           SEARCH ALL WS-T-CONS-CAMPO
               AT END DISPLAY "ERROR: CONS NO ENCONTRADO EN WS-T-CONS"
               WHEN WS-T-CONS-CONS(WS-T-CONS-I) = WS-MENOR-CONS
           MOVE 0 TO WS-TOT-CONS-VAL.
           MOVE 0 TO WS-TOT-CONS-HS.
           PERFORM 145-IMPRIMIR-LN-ENC-CLI.
           PERFORM 150-CONS UNTIL (HS1-EOF = "SI" AND
                                   HS2-EOF = "SI" AND
                                   HS3-EOF = "SI") OR
                                  (WS-MENOR-CONS NOT = HS1-CONS AND
                                   WS-MENOR-CONS NOT = HS2-CONS AND
                                   WS-MENOR-CONS NOT = HS3-CONS).
           PERFORM 300-IMPRIMIR-LN-TOTAL-CONS.
           ADD WS-TOT-CONS-VAL TO WS-TOT-GRAL-VAL.
           ADD WS-TOT-CONS-HS TO WS-TOT-GRAL-HS.
      *     PERFORM 270-IMPRIMIR-LI-LINEAS-1-A-5.

       120-DET-MENOR-CONS.
           MOVE 999 TO WS-MENOR-CONS.
           MOVE HS1-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS2-CONS
               MOVE HS2-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS3-CONS
               MOVE HS3-CONS TO WS-MENOR-CONS.

       130-IMPRIMIR-LI-ENCAB.
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-FECHA-HOY.
           ADD 1 TO LI-HOJA.
           MOVE WS-FECHA-HOY-AAAA TO LI-ENC-FECHA-AAAA.
           MOVE WS-FECHA-HOY-MM TO LI-ENC-FECHA-MM.
           MOVE WS-FECHA-HOY-DD TO LI-ENC-FECHA-DD.
           WRITE LIS-IMP-LINEA FROM LI-ENC1.
           WRITE LIS-IMP-LINEA FROM LI-LINEA-BL.
           WRITE LIS-IMP-LINEA FROM LI-ENC2.
           WRITE LIS-IMP-LINEA FROM LI-LINEA-BL.
           MOVE 5 TO LI-NRO-LINEA.

       140-IMPRIMIR-LN-ENCAB.
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-FECHA-HOY.
           ADD 1 TO LN-HOJA.
           MOVE WS-FECHA-HOY-AAAA TO LN-ENC-FECHA-AAAA.
           MOVE WS-FECHA-HOY-MM TO LN-ENC-FECHA-MM.
           MOVE WS-FECHA-HOY-DD TO LN-ENC-FECHA-DD.
           WRITE LIS-NOM-LINEA FROM LN-ENC1.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           WRITE LIS-NOM-LINEA FROM LN-ENC2.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           MOVE 5 TO LN-NRO-LINEA.

       145-IMPRIMIR-LN-ENC-CLI.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           MOVE WS-T-CONS-CONS(WS-T-CONS-I) TO LN-ENC-CLI-CONS.
           MOVE WS-T-CONS-NOMBRE(WS-T-CONS-I) TO LN-ENC-CLI-NOMB.
           WRITE LIS-NOM-LINEA FROM LN-ENC-CLI.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           ADD 3 TO LN-NRO-LINEA.

       150-CONS.
           PERFORM 160-DET-MENOR-FECHA.
           PERFORM 170-VALOR-FECHA.
           MOVE 0 TO WS-TOT-FECHA-HS.
           MOVE 0 TO WS-TOT-FECHA-VAL.
           PERFORM 320-IMPRIMIR-LN-ENC-CLI-FECHA.
           PERFORM 330-IMPRIMIR-LN-ENC-TABLA-CLI.
           PERFORM 180-FECHA UNTIL (HS1-EOF = "SI" AND
                                    HS2-EOF = "SI" AND
                                    HS3-EOF = "SI") OR
                                   (WS-MENOR-FECHA NOT = HS1-FECHA AND
                                    WS-MENOR-FECHA NOT = HS2-FECHA AND
                                    WS-MENOR-FECHA NOT = HS3-FECHA) OR
                                   (WS-MENOR-CONS NOT = HS1-CONS AND
                                    WS-MENOR-CONS NOT = HS2-CONS AND
                                    WS-MENOR-CONS NOT = HS3-CONS).
           PERFORM 290-IMPRIMIR-LN-TOTAL-FECHA.
           ADD WS-TOT-FECHA-VAL TO WS-TOT-CONS-VAL.
           ADD WS-TOT-FECHA-HS TO WS-TOT-CONS-HS.

       160-DET-MENOR-FECHA.
           MOVE "99999999" TO WS-MENOR-FECHA .
           IF WS-MENOR-CONS = HS1-CONS
               MOVE HS1-FECHA TO WS-MENOR-FECHA
           END-IF.
           IF WS-MENOR-FECHA > HS2-FECHA
               AND WS-MENOR-CONS = HS2-CONS
               MOVE HS2-FECHA TO WS-MENOR-FECHA
           END-IF.
           IF WS-MENOR-FECHA > HS3-FECHA
               AND WS-MENOR-CONS = HS3-CONS
               MOVE HS3-FECHA TO WS-MENOR-FECHA
           END-IF.

       170-VALOR-FECHA.
           PERFORM 175-SIG-VALOR UNTIL VAL-EOF = "SI" OR
                                       WS-MENOR-CONS NOT = VAL-CONS OR
                                       WS-MENOR-FECHA > VAL-FEC-HASTA.

       175-SIG-VALOR.
           MOVE VAL-VALOR-HORA TO WS-VALOR-HORA.
           PERFORM 100-LEER-VAL.


       180-FECHA.
           MOVE 0 TO WS-TOT-CLI-HS.
           MOVE 0 TO WS-TOT-CLI-VAL.
           PERFORM 190-DET-MENOR-CLIE.
           PERFORM 210-CLIENTE UNTIL (HS1-EOF = "SI" AND
                                       HS2-EOF = "SI" AND
                                       HS3-EOF = "SI") OR
                               (NOT (WS-MENOR-CONS = HS1-CONS AND
                                WS-MENOR-FECHA = HS1-FECHA AND
                                WS-MENOR-CLIENTE = HS1-CLIENTE)) AND
                               (NOT (WS-MENOR-CONS = HS2-CONS AND
                                WS-MENOR-FECHA = HS2-FECHA AND
                                WS-MENOR-CLIENTE = HS2-CLIENTE)) AND
                               (NOT (WS-MENOR-CONS = HS3-CONS AND
                                WS-MENOR-FECHA = HS3-FECHA AND
                                WS-MENOR-CLIENTE = HS3-CLIENTE)).
           PERFORM 280-IMPRIMIR-LN-TOTAL-CLI.
           PERFORM 270-IMPRIMIR-LI-LINEAS-1-A-5.
           ADD WS-TOT-CLI-HS TO WS-TOT-FECHA-HS.
           ADD WS-TOT-CLI-VAL TO WS-TOT-FECHA-VAL.

       190-DET-MENOR-CLIE.
           MOVE 9999 TO WS-MENOR-CLIENTE.
           IF WS-MENOR-CONS = HS1-CONS AND WS-MENOR-FECHA = HS1-FECHA
               MOVE HS1-CLIENTE TO WS-MENOR-CLIENTE
           END-IF.
           IF WS-MENOR-CLIENTE > HS2-CLIENTE AND
              WS-MENOR-CONS = HS2-CONS AND
              WS-MENOR-FECHA = HS2-FECHA
               MOVE HS2-CLIENTE TO WS-MENOR-CLIENTE
           END-IF.
           IF WS-MENOR-CLIENTE > HS3-CLIENTE AND
              WS-MENOR-CONS = HS3-CONS AND
              WS-MENOR-FECHA = HS3-FECHA
               MOVE HS3-CLIENTE TO WS-MENOR-CLIENTE
           END-IF.

       210-CLIENTE.
           PERFORM 220-HS1-CLIENTE UNTIL HS1-EOF = "SI" OR
                                   WS-MENOR-CLIENTE NOT = HS1-CLIENTE OR
                                   WS-MENOR-FECHA NOT = HS1-FECHA OR
                                   WS-MENOR-CONS NOT = HS1-CONS.
           PERFORM 230-HS2-CLIENTE UNTIL HS2-EOF = "SI" OR
                                   WS-MENOR-CLIENTE NOT = HS2-CLIENTE OR
                                   WS-MENOR-FECHA NOT = HS2-FECHA OR
                                   WS-MENOR-CONS NOT = HS2-CONS.
           PERFORM 240-HS3-CLIENTE UNTIL HS3-EOF = "SI" OR
                                   WS-MENOR-CLIENTE NOT = HS3-CLIENTE OR
                                   WS-MENOR-FECHA NOT = HS3-FECHA OR
                                   WS-MENOR-CONS NOT = HS3-CONS.

       220-HS1-CLIENTE.
           MOVE REG-HS1 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 070-LEER-HS1.
           IF HS1-EOF = "SI"
               MOVE 999 TO HS1-CONS
               MOVE "99999999" TO HS1-FECHA
               MOVE 9999 TO HS1-CLIENTE
           END-IF.

       230-HS2-CLIENTE.
           MOVE REG-HS2 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 080-LEER-HS2.
           IF HS2-EOF = "SI"
               MOVE 999 TO HS2-CONS
               MOVE "99999999" TO HS2-FECHA
               MOVE 9999 TO HS2-CLIENTE
           END-IF.

       240-HS3-CLIENTE.
           MOVE REG-HS3 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 090-LEER-HS3.
           IF HS3-EOF = "SI"
               MOVE 999 TO HS3-CONS
               MOVE "99999999" TO HS3-FECHA
               MOVE 9999 TO HS3-CLIENTE
           END-IF.

       250-PROCESAR-CLI.
           ADD WS-LIS-HS-CANT-HORAS TO WS-TOT-CLI-HS.
           MULTIPLY WS-LIS-HS-CANT-HORAS BY WS-VALOR-HORA
                    GIVING WS-VALOR.
           ADD WS-VALOR TO WS-TOT-CLI-VAL.
           PERFORM 350-ACTUALIZAR-STAT.
           PERFORM 260-IMPRIMIR-LN-FILA-CLI.

       260-IMPRIMIR-LN-FILA-CLI.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           MOVE WS-LIS-HS-CLIENTE TO LN-LINEA-CLI-COD.
           MOVE WS-LIS-HS-CANT-HORAS TO LN-LINEA-CLI-CANT.
           MOVE WS-VALOR TO LN-LINEA-CLI-VAL.
           MOVE WS-LIS-HS-OBSERV TO LN-LINEA-CLI-OBS.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-CLI.
           ADD 1 TO LN-NRO-LINEA.

       270-IMPRIMIR-LI-LINEAS-1-A-5.
           IF LI-NRO-LINEA > 60
               PERFORM 130-IMPRIMIR-LI-ENCAB.
           MOVE WS-LIS-HS-CONS TO LI-LINEA1-CONS.
           WRITE LIS-IMP-LINEA FROM LI-LINEA1.
           MOVE WS-LIS-HS-FECHA-AAAA TO LI-LINEA2-AA.
           MOVE WS-LIS-HS-FECHA-MM TO LI-LINEA2-MM.
           MOVE WS-LIS-HS-FECHA-DD TO LI-LINEA2-DD.
           WRITE LIS-IMP-LINEA FROM LI-LINEA2.
           MOVE WS-LIS-HS-CLIENTE TO LI-LINEA3-CLI.
           WRITE LIS-IMP-LINEA FROM LI-LINEA3.
           MOVE WS-TOT-CLI-HS TO LI-LINEA4-HORAS.
           WRITE LIS-IMP-LINEA FROM LI-LINEA4.
           MOVE WS-TOT-CLI-VAL TO LI-LINEA5-VALOR.
           WRITE LIS-IMP-LINEA FROM LI-LINEA5.
           WRITE LIS-IMP-LINEA FROM LI-LINEA-BL.
           ADD 6 TO LI-NRO-LINEA.

       280-IMPRIMIR-LN-TOTAL-CLI.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           MOVE WS-TOT-CLI-HS TO LN-LINEA-TOT-CLI-HS.
           MOVE WS-TOT-CLI-VAL TO LN-LINEA-TOT-CLI-VAL.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-TOT-CLI.
           ADD 1 TO LN-NRO-LINEA.

       290-IMPRIMIR-LN-TOTAL-FECHA.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           MOVE WS-TOT-FECHA-HS TO LN-LINEA-TOT-FECHA-HS.
           MOVE WS-TOT-FECHA-VAL TO LN-LINEA-TOT-FECHA-VAL.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-TOT-FECHA.
           ADD 2 TO LN-NRO-LINEA.

       300-IMPRIMIR-LN-TOTAL-CONS.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           MOVE WS-TOT-CONS-HS TO LN-LINEA-TOT-CONS-HS.
           MOVE WS-TOT-CONS-VAL TO LN-LINEA-TOT-CONS-VAL.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-TOT-CONS.
           ADD 2 TO LN-NRO-LINEA.

       310-IMPRIMIR-LN-TOTAL-GRAL.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           MOVE WS-TOT-GRAL-HS TO LN-LINEA-TOT-GRAL-HS.
           MOVE WS-TOT-GRAL-VAL TO LN-LINEA-TOT-GRAL-VAL.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-TOT-GRAL.
           ADD 2 TO LN-NRO-LINEA.

       320-IMPRIMIR-LN-ENC-CLI-FECHA.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           MOVE WS-MENOR-FECHA-AAAA TO LN-ENC-CLI-FECHA-AAAA.
           MOVE WS-MENOR-FECHA-MM TO LN-ENC-CLI-FECHA-MM.
           MOVE WS-MENOR-FECHA-DD TO LN-ENC-CLI-FECHA-DD.
           WRITE LIS-NOM-LINEA FROM LN-ENC-CLI-FECHA.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           ADD 2 TO LN-NRO-LINEA.

       330-IMPRIMIR-LN-ENC-TABLA-CLI.
           PERFORM 340-LN-SALTO-DE-PAGINA.
           WRITE LIS-NOM-LINEA FROM LN-ENC-TABLA-CLI.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-PUNT.
           ADD 2 TO LN-NRO-LINEA.

       340-LN-SALTO-DE-PAGINA.
           IF LN-NRO-LINEA > 60
               PERFORM 140-IMPRIMIR-LN-ENCAB.

       350-ACTUALIZAR-STAT.
           ADD WS-VALOR
               TO WS-T-STATS-HORAS (WS-MENOR-CONS, WS-MENOR-FECHA-MM).

       360-IMPRIMIR-STAT.
           MOVE 1 TO WS-I.
           MOVE 1 TO WS-MES.
           PERFORM 370-STAT-SUMAR-FILAS UNTIL WS-I > 999.
           DISPLAY "ESTADISTICA DE HORAS TRABAJADAS POR MES Y POR CONSUL
      -            "TORA EN EL ANIO".
           WRITE LIS-EST-LINEA FROM LE-ENC.
           WRITE LIS-EST-LINEA FROM LE-LINEA-BL.
           DISPLAY " ".
           DISPLAY "                          " WITH NO ADVANCING.
           DISPLAY "ENE " WITH NO ADVANCING.
           DISPLAY "FEB " WITH NO ADVANCING.
           DISPLAY "MAR " WITH NO ADVANCING.
           DISPLAY "ABR " WITH NO ADVANCING.
           DISPLAY "MAY " WITH NO ADVANCING.
           DISPLAY "JUN " WITH NO ADVANCING.
           DISPLAY "JUL " WITH NO ADVANCING.
           DISPLAY "AGO " WITH NO ADVANCING.
           DISPLAY "SEP " WITH NO ADVANCING.
           DISPLAY "OCT " WITH NO ADVANCING.
           DISPLAY "NOV " WITH NO ADVANCING.
           DISPLAY "DIC " WITH NO ADVANCING.
           DISPLAY "TOT ".
           WRITE LIS-EST-LINEA FROM LE-THEADER.
           MOVE 1 TO WS-MES.
           MOVE 1 TO WS-I.
           PERFORM 390-STAT-MOSTRAR-HORAS UNTIL WS-I > 999.
           PERFORM 410-STAT-MOSTRAR-TOTALES.

       370-STAT-SUMAR-FILAS.
           MOVE 1 TO WS-MES.
           MOVE 0 TO WS-ACUM.
           PERFORM 380-STAT-SUMAR-MES UNTIL WS-MES > 12.
           ADD WS-ACUM TO WS-T-STATS-HORAS (WS-I, 13).
           ADD WS-ACUM TO WS-T-STATS-TOTAL (13).
           ADD 1 TO WS-I.

       380-STAT-SUMAR-MES.
           ADD WS-T-STATS-HORAS(WS-I, WS-MES) TO WS-ACUM.
           ADD WS-T-STATS-HORAS(WS-I, WS-MES)
               TO WS-T-STATS-TOTAL (WS-MES).
           ADD 1 TO WS-MES.

       390-STAT-MOSTRAR-HORAS.
           IF WS-T-STATS-HORAS (WS-I, 13) NOT = 0
               MOVE 1 TO WS-MES
               SEARCH ALL WS-T-CONS-CAMPO
                   WHEN WS-T-CONS-CONS (WS-T-CONS-I) = WS-I
               DISPLAY WS-T-CONS-NOMBRE(WS-T-CONS-I)
                   WITH NO ADVANCING
               MOVE WS-T-CONS-NOMBRE(WS-T-CONS-I) TO LE-TR-NOMBRE
               PERFORM 400-STAT-MOSTRAR-MES UNTIL WS-MES > 13
               WRITE LIS-EST-LINEA FROM LE-TROWS
           END-IF.
           ADD 1 TO WS-I.

       400-STAT-MOSTRAR-MES.
           MOVE WS-T-STATS-HORAS (WS-I, WS-MES)
               TO WS-VALOR-HORA-DISPLAY.
           IF (WS-MES NOT = 13)
               DISPLAY WS-VALOR-HORA-DISPLAY WITH NO ADVANCING
               MOVE WS-T-STATS-HORAS (WS-I, WS-MES)
                    TO LE-TR-HS (WS-MES)
               DIVIDE WS-T-STATS-HORAS(WS-I, WS-MES)
                      BY WS-T-STATS-HORAS(WS-I, 13)
                      GIVING WS-PORC
              MULTIPLY WS-PORC BY 100 GIVING LE-TR-PC (WS-MES)
           ELSE
               DISPLAY WS-VALOR-HORA-DISPLAY.
               MOVE WS-T-STATS-HORAS (WS-I, WS-MES) TO LE-TR-TT
           ADD 1 TO WS-MES.

       410-STAT-MOSTRAR-TOTALES.
           DISPLAY "                   TOTAL " WITH NO ADVANCING.
           MOVE 1 TO WS-MES.
           PERFORM 420-STAT-MOSTRAR-TOTAL UNTIL WS-MES > 13.
           WRITE LIS-EST-LINEA FROM LE-TFOOTER.

       420-STAT-MOSTRAR-TOTAL.
           MOVE WS-T-STATS-TOTAL (WS-MES)
               TO WS-VALOR-HORA-DISPLAY.
           IF (WS-MES NOT = 13)
               DISPLAY WS-VALOR-HORA-DISPLAY WITH NO ADVANCING
               MOVE WS-T-STATS-TOTAL (WS-MES)
                    TO LE-TF-HS (WS-MES)
               DIVIDE WS-T-STATS-TOTAL (WS-MES)
                      BY WS-T-STATS-TOTAL(13)
                      GIVING WS-PORC
              MULTIPLY WS-PORC BY 100 GIVING LE-TF-PC (WS-MES)
           ELSE
               DISPLAY WS-VALOR-HORA-DISPLAY
               MOVE WS-T-STATS-TOTAL (WS-MES) TO LE-TF-TT.
           ADD 1 TO WS-MES.


       END PROGRAM TP-1.
