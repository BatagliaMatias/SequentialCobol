      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TP-1.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT HS1 ASSIGN TO "HS1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT HS2 ASSIGN TO "HS2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT HS3 ASSIGN TO "HS3.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT VAL ASSIGN TO "VAL-HORA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CONS ASSIGN TO "CONSUL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LIST-TOT ASSIGN TO "LISTADO_TOTALES.DAT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT LIST-HOR ASSIGN TO "LISTADO_HORAS.DAT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT ESTADIS ASSIGN TO "ESTADISTICA.DAT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD HS1.
       01 REG-HS1.
           03 HS1-CONS PIC 9(3).
           03 HS1-FECHA PIC X(8).
           03 HS1-CLIENTE PIC 9(4).
           03 HS1-CANT-HORAS PIC 9(2)V99.
           03 HS1-OBSERV PIC X(30).

       FD HS2.
       01 REG-HS2.
           03 HS2-CONS PIC 9(3).
           03 HS2-FECHA PIC X(8).
           03 HS2-CLIENTE PIC 9(4).
           03 HS2-CANT-HORAS PIC 9(2)V99.
           03 HS2-OBSERV PIC X(30).

       FD HS3.
       01 REG-HS3.
           03 HS3-CONS PIC 9(3).
           03 HS3-FECHA PIC X(8).
           03 HS3-CLIENTE PIC 9(4).
           03 HS3-CANT-HORAS PIC 9(2)V99.
           03 HS3-OBSERV PIC X(30).

       FD VAL.
       01 REG-VAL.
           03 VAL-CONS PIC 9(3).
           03 VAL-FEC-DESDE PIC X(8).
           03 VAL-FEC-HASTA PIC X(8).
           03 VAL-VALOR-HORA PIC 9(7)V99.

       FD CONS.
       01 REG-CONS.
           03 CONS-CONS PIC 9(3).
           03 CONS-FECHA-ALTA PIC X(8).
           03 CONS-NOMBRE PIC X(25).

       FD LIST-TOT
           LABEL RECORD OMITTED.
       01 LIST-TOT-LINEA PIC X(90).

       FD LIST-HOR
           LABEL RECORD OMITTED.
       01 LIST-HOR-LINEA PIC X(90).

       FD ESTADIS
           LABEL RECORD OMITTED.
       01 ESTADIS-LINEA PIC X(90).

       WORKING-STORAGE SECTION.
      *-----------------------
       
       77 HS1-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".
       77 HS2-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".
       77 HS3-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".
       77 VAL-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".
       77 CONS-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".

       01 HS1-ESTADO PIC XX.
       77 HS2-ESTADO PIC XX.
       77 HS3-ESTADO PIC XX.
       77 VAL-ESTADO PIC XX.
       77 CONS-ESTADO PIC XX.

       01 CONS-SUB PIC 9(3) VALUE 1.
       01 CONS-SUB-BIS PIC 9(3) VALUE 1.

       01 T-CONS.
           03 T-CONS-FLD OCCURS 1000.
               05 T-CONS-CONS PIC 9(3).
               05 T-CONS-FECHA-ALTA PIC X(8).
               05 T-CONS-NOMBRE PIC X(25).

       01 T-CONS-FLD-TEMP.
           03 T-CONS-TEMP-CONS PIC 9(3).
           03 T-CONS-TEMP-FECHA-ALTA PIC X(8).
           03 T-CONS-TEMP-NOMBRE PIC X(25).

       01 WS-I              PIC 9(4).
       01 WS-J              PIC 9(4).
       01 WS-K              PIC 9(4).

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

            PERFORM INICIO.
            READ CONS RECORD AT END MOVE 'SI' TO CONS-EOF.
            PERFORM CARGAR-T-CONS UNTIL CONS-EOF = 'SI'.
            PERFORM ORDENAR-T-CONS.
            PERFORM FIN.
            STOP RUN.

       INICIO.
           OPEN INPUT HS1, HS2, HS3.
           OPEN INPUT VAL.
           OPEN INPUT CONS.
           OPEN OUTPUT LIST-TOT, LIST-HOR, ESTADIS.

       CARGAR-T-CONS.
           MOVE REG-CONS TO T-CONS-FLD(CONS-SUB).
           ADD 1 TO CONS-SUB.
           READ CONS RECORD AT END MOVE 'SI' TO CONS-EOF.

       ORDENAR-T-CONS.
           SUBTRACT 1 FROM CONS-SUB.
           MOVE CONS-SUB TO CONS-SUB-BIS.
           SUBTRACT 1 FROM CONS-SUB-BIS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I = CONS-SUB
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > CONS-SUB-BIS
                   IF T-CONS-CONS(WS-J) < T-CONS-CONS(WS-I) THEN
                       MOVE T-CONS-FLD(WS-I)  TO T-CONS-FLD-TEMP
                       MOVE T-CONS-FLD(WS-J)  TO T-CONS-FLD(WS-I)
                       MOVE T-CONS-FLD-TEMP TO T-CONS-FLD(WS-J)
                   END-IF
               END-PERFORM                                           
           END-PERFORM. 

       FIN.
           CLOSE HS1, HS2, HS3.
           CLOSE VAL.
           CLOSE CONS.
           CLOSE LIST-TOT, LIST-HOR, ESTADIS.

       END PROGRAM TP-1.
