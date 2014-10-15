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

           SELECT LIS-IMP ASSIGN TO "LISTADO_TOTALES.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LIS-NOM ASSIGN TO "LISTADO_HORAS.DAT"
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
           03 FILLER                   PIC X(37) VALUE ALL " ".
           03 FILLER                   PIC X(9) VALUE "Hoja nro ".
           03 LI-HOJA                  PIC 99 VALUE IS 0.

       01 LI-ENC2.
           03 FILLER                   PIC X(7) VALUE ALL " ".
           03 FILLER                   PIC X(73) VALUE "LISTADO DE TOTAL
      -                                                "LES DE HORAS".

       01 LI-LINEA-BL             PIC X(80) VALUE ALL " ".

       01 LI-NRO-LINEA                 PIC 99 VALUE IS 0.

       01 LI-LINEA1.
           03 FILLER              PIC X(16) VALUE IS "COD. CONSULTOR: ".
           03 LI-LINEA1-CONS      PIC X(3).

       01 LI-LINEA2.
           03 FILLER              PIC X(7) VALUE IS "FECHA: ".
           03 LI-LINEA2-AAAA      PIC 9999.
           03 FILLER              PIC X VALUE "/".
           03 LI-LINEA2-MM        PIC 99.
           03 FILLER              PIC X VALUE "/".
           03 LI-LINEA2-DD        PIC 99.

       01 LI-LINEA3.
           03 FILLER              PIC X(14) VALUE IS "COD. CLIENTE: ".
           03 LI-LINEA3-CLI       PIC X(4).

       01 LI-LINEA4.
           03 FILLER              PIC X(13) VALUE IS "TOTAL HORAS: ".
           03 LI-LINEA4-HORAS     PIC 9(5)V99.

       01 LI-LINEA5.
           03 FILLER              PIC X(13) VALUE IS "TOTAL VALOR: ".
           03 LI-LINEA5-VALOR     PIC 9(7)V99.

       01 LN-ENC1.
           03 FILLER                   PIC X(7) VALUE "Fecha: ".
           03 LN-ENC-FECHA-AAAA        PIC 9999.
           03 FILLER                   PIC X VALUE "/".
           03 LN-ENC-FECHA-MM          PIC 99.
           03 FILLER                   PIC X VALUE "/".
           03 LN-ENC-FECHA-DD          PIC 99.
           03 FILLER                   PIC X(37) VALUE ALL " ".
           03 FILLER                   PIC X(9) VALUE "Hoja nro ".
           03 LN-HOJA                  PIC 99 VALUE IS 0.

       01 LN-ENC2.
           03 FILLER PIC X(7) VALUE ALL " ".
           03 FILLER PIC X(73) VALUE "TIMES - LISTADO HORAS TRABAJADAS".

       01 LN-LINEA1.
           03 FILLER              PIC X(16) VALUE IS "COD. CONSULTOR: ".
           03 LN-LINEA1-CONS      PIC X(3).
           03 FILLER              PIC X(10) VALUE IS "  NOMBRE: ".
           03 LN-LINEA1-NOMB      PIC X(25).

       01 LN-LINEA2.
           03 FILLER              PIC X(7) VALUE IS "FECHA: ".
           03 LN-LINEA2-AAAA      PIC 9999.
           03 FILLER              PIC X VALUE "/".
           03 LN-LINEA2-MM        PIC 99.
           03 FILLER              PIC X VALUE "/".
           03 LN-LINEA2-DD        PIC 99.

       01 LN-ENC-CLI1.
           03 FILLER              PIC X(11) VALUE "Cod Cliente".
           03 FILLER              PIC X(14) VALUE "Cantidad horas".
           03 FILLER              PIC X(15) VALUE "Valor          ".
           03 FILLER              PIC X(12) VALUE "Obervaciones".
           03 FILLER              PIC X(18) VALUE ALL " ".

       01 LN-ENC-CLI2             PIC X(80) VALUE ALL "-".

       01 LN-LINEA-BL             PIC X(80) VALUE ALL " ".

       01 LN-NRO-LINEA                 PIC 99 VALUE IS 0.

       01  WS-FECHA-HOY.
           03  WS-FECHA-HOY-AAAA       PIC  9(4).
           03  WS-FECHA-HOY-MM         PIC  9(2).
           03  WS-FECHA-HOY-DD         PIC  9(2).

       01 WS-MENOR-FECHA.
           03  WS-MENOR-FECHA-AAAA     PIC  9(4).
           03  WS-MENOR-FECHA-MM       PIC  9(2).
           03  WS-MENOR-FECHA-DD       PIC  9(2).

       01 WS-MENOR-CLIENTE             PIC 9(4).

       01 WS-TOT-FECHA-HS              PIC 9(7)V99 VALUE IS ZERO.

       01 WS-TOTAL-HORAS-CLI                 PIC 9(5)V99.

       01 WS-LIS-HS.
           03 WS-LIS-HS-CONS                 PIC 9(3).
           03 WS-LIS-HS-FECHA                PIC X(8).
           03 WS-LIS-HS-CLIENTE              PIC 9(4).
           03 WS-LIS-HS-CANT-HORAS           PIC 9(2)V99.
           03 WS-LIS-HS-OBSERV               PIC X(30).

       01 WS-VALOR                           PIC 9(5)V99.

       01 LI-VALOR-TOTAL-GRAL                PIC 9(7)V99 VALUE IS ZERO.
       01 LI-HORAS-TOTAL-GRAL                PIC 9(5)V99 VALUE IS ZERO.
       01 LI-HORAS-TOTAL-FECHA               PIC 9(2)V99 VALUE IS ZERO.

       01 LN-HORAS-CLI                       PIC 9(2)V99 VALUE IS ZERO.
       01 LN-VALOR-CLI                       PIC 9(5)V99 VALUE IS ZERO.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            PERFORM 010-INICIO.
            PERFORM 020-LEER-CONS.
            PERFORM 030-CARGAR-T-CONS UNTIL CONS-EOF = 'SI'.
            PERFORM 040-ORDENAR-T-CONS.
            PERFORM 060-PROCESAR.
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
           OPEN OUTPUT LIS-IMP, LIS-NOM.

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
           DISPLAY "050-FIN".
           CLOSE HS1, HS2, HS3.
           CLOSE VAL.
           CLOSE CONS.
           CLOSE LIS-IMP, LIS-NOM.

       060-PROCESAR.
           DISPLAY "060-PROCESAR".
           PERFORM 070-LEER-HS1.
           PERFORM 080-LEER-HS2.
           PERFORM 090-LEER-HS3.
           PERFORM 100-LEER-VAL.
           PERFORM 110-SUB-PROCESAR1 UNTIL HS1-EOF = "SI" AND
                                           HS2-EOF = "SI" AND
                                           HS3-EOF = "SI".
      *    PERFORM XXX-IMPRIMIR-TOTAL-GRAL.

       070-LEER-HS1.
           DISPLAY "070-LEER-HS1".
           READ HS1 AT END MOVE "SI" TO HS1-EOF
           IF HS1-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS1.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS1-ESTADO
           END-IF.

       080-LEER-HS2.
           DISPLAY "080-LEER-HS2".
           READ HS2 AT END MOVE "SI" TO HS2-EOF
           IF HS2-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS2.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS2-ESTADO
           END-IF.

       090-LEER-HS3.
           DISPLAY "090-LEER-HS3".
           READ HS3 AT END MOVE "SI" TO HS3-EOF
           IF HS3-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS3.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS3-ESTADO
           END-IF.

       100-LEER-VAL.
           DISPLAY "100-LEER-VAL".
           READ VAL
               AT END MOVE 'SI' TO VAL-EOF.
           IF VAL-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo VAL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " VAL-ESTADO.

       110-SUB-PROCESAR1.
           DISPLAY "110-SUB-PROCESAR1".
           PERFORM 120-DET-MENOR-CONS.
           SEARCH ALL WS-T-CONS-CAMPO
               AT END DISPLAY "ERROR: CONS NO ENCONTRADO EN WS-T-CONS"
               WHEN WS-T-CONS-CONS(WS-T-CONS-I) = WS-MENOR-CONS
               DISPLAY " * NOMBRE: " WS-T-CONS-NOMBRE(WS-T-CONS-I).
           PERFORM 140-IMPRIM-ENCAB-LIS-NOM.
           PERFORM 130-IMPRIM-ENCAB-LIS-IMP.
           PERFORM 145-IMPRIM-ENCAB-CLI-LIS-NOM.
           PERFORM 150-CONS UNTIL (HS1-EOF = "SI" AND
                                   HS2-EOF = "SI" AND
                                   HS3-EOF = "SI") OR
                                  (WS-MENOR-CONS NOT = HS1-CONS AND
                                   WS-MENOR-CONS NOT = HS2-CONS AND
                                   WS-MENOR-CONS NOT = HS3-CONS).

       120-DET-MENOR-CONS.
           DISPLAY "120-DET-MENOR-CONS".
           MOVE 999 TO WS-MENOR-CONS.
           MOVE HS1-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS2-CONS
               MOVE HS2-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS3-CONS
               MOVE HS3-CONS TO WS-MENOR-CONS.
           DISPLAY " * MENOR-CONS: " WS-MENOR-CONS.

       130-IMPRIM-ENCAB-LIS-IMP.
           DISPLAY "130-IMPRIM-ENCAB-LIS-IMP".
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

       140-IMPRIM-ENCAB-LIS-NOM.
           DISPLAY "140-IMPRIM-ENCAB-LIS-NOM".
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

       145-IMPRIM-ENCAB-CLI-LIS-NOM.
           DISPLAY "145-IMPRIM-ENCAB-CLI-LIS-NOM".
           MOVE WS-T-CONS-CONS(WS-T-CONS-I) TO LN-LINEA1-CONS.
           MOVE WS-T-CONS-NOMBRE(WS-T-CONS-I) TO LN-LINEA1-NOMB.
           WRITE LIS-NOM-LINEA FROM LN-LINEA1.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.

       150-CONS.
           DISPLAY "150-CONS".
           PERFORM 160-DET-MENOR-FECHA.
           PERFORM 170-VALOR-FECHA.
           DISPLAY " * VALOR-FECHA: " WS-VALOR.
           MOVE WS-MENOR-CONS TO LI-LINEA1-CONS.
           WRITE LIS-IMP-LINEA FROM LI-LINEA1.
           MOVE WS-MENOR-FECHA-AAAA TO LI-LINEA2-AAAA.
           MOVE WS-MENOR-FECHA-MM TO LI-LINEA2-MM.
           MOVE WS-MENOR-FECHA-DD TO LI-LINEA2-DD.
           WRITE LIS-IMP-LINEA FROM LI-LINEA2.
           MOVE WS-MENOR-FECHA-AAAA TO LN-LINEA2-AAAA.
           MOVE WS-MENOR-FECHA-MM TO LN-LINEA2-MM.
           MOVE WS-MENOR-FECHA-DD TO LN-LINEA2-DD.
           WRITE LIS-IMP-LINEA FROM LN-LINEA2.
           WRITE LIS-NOM-LINEA FROM LN-LINEA-BL.
           PERFORM 200-IMPR-ENCAB-CLIENTE.
           PERFORM 180-FECHA UNTIL (HS1-EOF = "SI" AND
                                    HS2-EOF = "SI" AND
                                    HS3-EOF = "SI") OR
                                   (WS-MENOR-FECHA NOT = HS1-FECHA AND
                                    WS-MENOR-FECHA NOT = HS2-FECHA AND
                                    WS-MENOR-FECHA NOT = HS3-FECHA) OR
                                   (WS-MENOR-CONS NOT = HS1-CONS AND
                                    WS-MENOR-CONS NOT = HS2-CONS AND
                                    WS-MENOR-CONS NOT = HS3-CONS).

       160-DET-MENOR-FECHA.
           DISPLAY "160-DET-MENOR-FECHA".
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
           DISPLAY " * MENOR-FECHA: " WS-MENOR-FECHA.

       170-VALOR-FECHA.
           DISPLAY "170-VALOR-FECHA".
           PERFORM 175-SIG-VALOR UNTIL VAL-EOF = "SI" OR
                                       WS-MENOR-CONS NOT = VAL-CONS OR
                                       WS-MENOR-FECHA > VAL-FEC-HASTA.

       175-SIG-VALOR.
           DISPLAY "175-SIG-VALOR".
           MOVE VAL-VALOR-HORA TO WS-VALOR.
           PERFORM 100-LEER-VAL.


       180-FECHA.
           DISPLAY "180-FECHA".
           MOVE 0 TO WS-TOTAL-HORAS-CLI.
           PERFORM 190-DET-MENOR-CLIE.
           PERFORM 210-CLIENTES UNTIL (HS1-EOF = "SI" AND
                                       HS2-EOF = "SI" AND
                                       HS3-EOF = "SI") OR
                              (WS-MENOR-FECHA NOT = HS1-FECHA AND
                               WS-MENOR-FECHA NOT = HS2-FECHA AND
                               WS-MENOR-FECHA NOT = HS3-FECHA) OR
                              (WS-MENOR-CONS NOT = HS1-CONS AND
                               WS-MENOR-CONS NOT = HS2-CONS AND
                               WS-MENOR-CONS NOT = HS3-CONS) OR
                              (WS-MENOR-CLIENTE NOT = HS1-CLIENTE AND
                               WS-MENOR-CLIENTE NOT = HS2-CLIENTE AND
                               WS-MENOR-CLIENTE NOT = HS3-CLIENTE).

       190-DET-MENOR-CLIE.
           DISPLAY "190-DET-MENOR-CLIE".
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
           DISPLAY " * MENOR_CLIE: " WS-MENOR-CLIENTE.

       200-IMPR-ENCAB-CLIENTE.
           DISPLAY "200-IMPR-ENCAB-CLIENTE".
           WRITE LIS-NOM-LINEA FROM LN-ENC1.
           WRITE LIS-NOM-LINEA FROM LN-ENC2.

       210-CLIENTES.
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
           DISPLAY "220-HS1-CLIENTE".
           MOVE REG-HS1 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 070-LEER-HS1.
           IF HS1-EOF = "SI"
               MOVE 999 TO HS1-CONS
               MOVE "99999999" TO HS1-FECHA
               MOVE 9999 TO HS1-CLIENTE
           END-IF.

       230-HS2-CLIENTE.
           DISPLAY "230-HS2-CLIENTE".
           MOVE REG-HS2 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 080-LEER-HS2.
           IF HS2-EOF = "SI"
               MOVE 999 TO HS2-CONS
               MOVE "99999999" TO HS2-FECHA
               MOVE 9999 TO HS2-CLIENTE
           END-IF.

       240-HS3-CLIENTE.
           DISPLAY "240-HS3-CLIENTE".
           MOVE REG-HS3 TO WS-LIS-HS.
           PERFORM 250-PROCESAR-CLI.
           PERFORM 090-LEER-HS3.
           IF HS3-EOF = "SI"
               MOVE 999 TO HS3-CONS
               MOVE "99999999" TO HS3-FECHA
               MOVE 9999 TO HS3-CLIENTE
           END-IF.

       250-PROCESAR-CLI.
           DISPLAY "250-PROCESAR-CLI".
           ADD WS-LIS-HS-CANT-HORAS TO LN-HORAS-CLI.
           MULTIPLY WS-LIS-HS-CANT-HORAS BY VAL-VALOR-HORA
                    GIVING WS-VALOR.
           ADD WS-VALOR TO LN-VALOR-CLI.

       END PROGRAM TP-1.
