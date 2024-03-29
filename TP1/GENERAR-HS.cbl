       IDENTIFICATION DIVISION.

       PROGRAM-ID. GENERAR-HS.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT HS1 ASSIGN TO "HS1.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT HS2 ASSIGN TO "HS2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT HS3 ASSIGN TO "HS3.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

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

       WORKING-STORAGE SECTION.
      

       PROCEDURE DIVISION.
      
       MAIN-PROCEDURE.

           DISPLAY "Generando HS1.DAT".
           OPEN OUTPUT HS1, HS2, HS3.

           MOVE 1 TO HS1-CONS.
           MOVE "20140112" TO HS1-FECHA.
           MOVE 1 TO HS1-CLIENTE.
           MOVE 4 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 1 TO HS1-CONS.
           MOVE "20140112" TO HS1-FECHA.
           MOVE 1 TO HS1-CLIENTE.
           MOVE 3 TO HS1-CANT-HORAS.
           MOVE "Volvio el mismo dia" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 1 TO HS1-CONS.
           MOVE "20140113" TO HS1-FECHA.
           MOVE 1 TO HS1-CLIENTE.
           MOVE 2 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 1 TO HS1-CONS.
           MOVE "20140214" TO HS1-FECHA.
           MOVE 2 TO HS1-CLIENTE.
           MOVE 5 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 1 TO HS1-CONS.
           MOVE "20140314" TO HS1-FECHA.
           MOVE 3 TO HS1-CLIENTE.
           MOVE 5 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.


           MOVE 1 TO HS1-CONS.
           MOVE "20141012" TO HS1-FECHA.
           MOVE 3 TO HS1-CLIENTE.
           MOVE 3 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 1 TO HS1-CONS.
           MOVE "20141013" TO HS1-FECHA.
           MOVE 3 TO HS1-CLIENTE.
           MOVE 2 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 99 TO HS1-CONS.
           MOVE "20140901" TO HS1-FECHA.
           MOVE 1 TO HS1-CLIENTE.
           MOVE 0,5 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 99 TO HS1-CONS.
           MOVE "20141001" TO HS1-FECHA.
           MOVE 1 TO HS1-CLIENTE.
           MOVE 0,5 TO HS1-CANT-HORAS.
           MOVE "NINGUNA" TO HS1-OBSERV.
           WRITE REG-HS1.

           MOVE 49 TO HS2-CONS.
           MOVE "20141001" TO HS2-FECHA.
           MOVE 1 TO HS2-CLIENTE.
           MOVE 1 TO HS2-CANT-HORAS.
           MOVE "NINGUNA" TO HS2-OBSERV.
           WRITE REG-HS2.

           MOVE 49 TO HS2-CONS.
           MOVE "20141101" TO HS2-FECHA.
           MOVE 2 TO HS2-CLIENTE.
           MOVE 1 TO HS2-CANT-HORAS.
           MOVE "NINGUNA" TO HS2-OBSERV.
           WRITE REG-HS2.

           MOVE 1 TO HS3-CONS.
           MOVE "20141012" TO HS3-FECHA.
           MOVE 1 TO HS3-CLIENTE.
           MOVE 0,5 TO HS3-CANT-HORAS.
           MOVE "NINGUNA" TO HS3-OBSERV.
           WRITE REG-HS3.

           MOVE 50 TO HS3-CONS.
           MOVE "20141105" TO HS3-FECHA.
           MOVE 2 TO HS3-CLIENTE.
           MOVE 1,5 TO HS3-CANT-HORAS.
           MOVE "NINGUNA" TO HS3-OBSERV.
           WRITE REG-HS3.

           MOVE 50 TO HS3-CONS.
           MOVE "20141205" TO HS3-FECHA.
           MOVE 2 TO HS3-CLIENTE.
           MOVE 3 TO HS3-CANT-HORAS.
           MOVE "NINGUNA" TO HS3-OBSERV.
           WRITE REG-HS3.

           CLOSE HS1, HS2, HS3.
           STOP RUN.

       END PROGRAM GENERAR-HS.
