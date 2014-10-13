      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. HOLA-MUNDO.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ENTRADA-ESTADO.
           SELECT LISTADO ASSIGN TO "/home/matias/SALIDA.DAT"
               ORGANIZATION IS SEQUENTIAL.
      *-----------------------
       DATA DIVISION.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

       FD ENTRADA LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "/home/matias/ENTRADA.DAT".

       01 ENT.
           03 ENT-DATO PIC X(30).


       FD LISTADO
           LABEL RECORD OMITTED.
       01 LINEA PIC X(90).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 FECHA PIC X(30).

       77 ENTRADA-EOF PIC XX VALUE "NO".
           88 EOF    VALUE "SI".
       77 ENTRADA-ESTADO PIC XX.    
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program      **
            PERFORM INICIO.
            PERFORM LEOENTRADA.
            PERFORM CICLO UNTIL ENTRADA-EOF = 'SI'.

            PERFORM FIN.
            STOP RUN.
      ** add other procedures here

       INICIO.
           OPEN I-O ENTRADA.
           OPEN OUTPUT LISTADO.

       LEOENTRADA.
           READ ENTRADA RECORD AT END MOVE 'SI' TO ENTRADA-EOF.
           IF NOT ENTRADA-ESTADO = '00'
               DISPLAY 'FIN LECTURA ' ENTRADA-ESTADO.

       MOSTRAR.
           MOVE ENT-DATO TO LINEA.
           DISPLAY LINEA.

       CICLO.
           PERFORM MOSTRAR.
           PERFORM LEOENTRADA.

       FIN.
           CLOSE ENTRADA.
           CLOSE LISTADO.
       END PROGRAM HOLA-MUNDO.
