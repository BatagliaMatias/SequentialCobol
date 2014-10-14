      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

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

           SELECT LIST-TOT ASSIGN TO "LISTADO_TOTALES.DAT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT LIST-HOR ASSIGN TO "LISTADO_HORAS.DAT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT ESTADIS ASSIGN TO "ESTADISTICA.DAT"
               ORGANIZATION IS SEQUENTIAL.

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

       FD LIST-TOT
           LABEL RECORD OMITTED.
       01 LIST-TOT-LINEA               PIC X(90).

       FD LIST-HOR
           LABEL RECORD OMITTED.
       01 LIST-HOR-LINEA               PIC X(90).

       FD ESTADIS
           LABEL RECORD OMITTED.
       01 ESTADIS-LINEA                PIC X(90).

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

       01 WS-T-CONS.
           03 WS-T-CONS-CAMPO OCCURS 1000
                              INDEXED BY WS-T-CONS-CAMPO-INDEX.
               05 WS-T-CONS-CONS       PIC 9(3).
               05 WS-T-CONS-FECHA-ALTA PIC X(8).
               05 WS-T-CONS-NOMBRE     PIC X(25).

       01 WS-T-CONS-CAMPO-TEMP.
           03 FILLER                   PIC 9(3).
           03 FILLER                   PIC X(8).
           03 FILLER                   PIC X(25).

      * CONTADORES:
       01 WS-G                         PIC 9(4).
       01 WS-H                         PIC 9(4).
       01 WS-I                         PIC 9(4).
       01 WS-J                         PIC 9(4).
       01 WS-K                         PIC 9(4).

       01 WS-TOT-GRAL-VAL              PIC 9(7)V99 VALUE IS ZERO.
       01 WS-TOT-GRAL-HS               PIC 9(7)V99 VALUE IS ZERO.

       01 WS-MENOR-CONS                PIC 9(3).

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
           OPEN OUTPUT LIST-TOT, LIST-HOR, ESTADIS.

       020-LEER-CONS.
           READ CONS
               AT END MOVE 'SI' TO CONS-EOF.
           IF CONS-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo CONSUL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " CONS-ESTADO
               STOP RUN.

       030-CARGAR-T-CONS.
           MOVE REG-CONS TO WS-T-CONS-CAMPO(WS-I).
           ADD 1 TO WS-I.
           PERFORM 020-LEER-CONS.

       040-ORDENAR-T-CONS.
      * BURBUJEO
           SUBTRACT 1 FROM WS-I.
           MOVE WS-I TO WS-H.
           MOVE WS-I TO WS-G.
           SUBTRACT 1 FROM WS-H.
           PERFORM VARYING WS-G FROM 1 BY 1 UNTIL WS-G = WS-I
               PERFORM VARYING WS-J FROM WS-G BY 1 UNTIL WS-J > WS-H
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
           CLOSE LIST-TOT, LIST-HOR, ESTADIS.

       060-PROCESAR.
           PERFORM 070-LEER-HS1.
           PERFORM 080-LEER-HS2.
           PERFORM 090-LEER-HS3.
           PERFORM 100-LEER-VAL.
           PERFORM 110-SUB-PROCESAR-1 UNTIL HS1-EOF = "SI"
               AND HS2-EOF = "SI" AND HS3-EOF = "SI".

       070-LEER-HS1.
           READ HS1
               AT END MOVE 'SI' TO HS1-EOF.
           IF HS1-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS1.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS1-ESTADO
               STOP RUN.

       080-LEER-HS2.
           READ HS2
               AT END MOVE 'SI' TO HS2-EOF.
           IF HS2-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS2.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS3-ESTADO
               STOP RUN.

       090-LEER-HS3.
           READ HS3
               AT END MOVE 'SI' TO HS3-EOF.
           IF HS3-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo HS3.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " HS3-ESTADO
               STOP RUN.

       100-LEER-VAL.
           READ VAL
               AT END MOVE 'SI' TO VAL-EOF.
           IF VAL-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR: No se pudo leer el archivo VAL.DAT"
               DISPLAY "ERROR:   FILE-STATUS: " VAL-ESTADO
               STOP RUN.

       110-SUB-PROCESAR-1.
           PERFORM 120-DET-MENOR-CONS.
           SEARCH ALL WS-T-CONS-CAMPO
               AT END DISPLAY "ERROR: Consultor no encontrado"
               WHEN WS-T-CONS-CONS(WS-I) = WS-MENOR-CONS
               CONTINUE.

       120-DET-MENOR-CONS.
           MOVE HS1-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS2-CONS
               MOVE HS2-CONS TO WS-MENOR-CONS.
           IF WS-MENOR-CONS > HS3-CONS
               MOVE HS3-CONS TO WS-MENOR-CONS.

       END PROGRAM TP-1.
