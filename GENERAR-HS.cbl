       IDENTIFICATION DIVISION.

       PROGRAM-ID. GENERAR-HS.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

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

         MOVE 099 TO HS1-CONS.
         MOVE "2014/10/01" TO HS1-FECHA.
         MOVE 1 TO HS1-CLIENTE.
         MOVE 0.5 TO HS1-CANT-HORAS.
         MOVE "NINGUNA" TO HS1-OBSERV.
         WRITE REG-HS1.

         MOVE "049" TO HS2-CONS.
         MOVE "2014/10/01" TO HS2-FECHA.
         MOVE 1 TO HS2-CLIENTE.
         MOVE 1 TO HS2-CANT-HORAS.
         MOVE "NINGUNA" TO HS2-OBSERV.
         WRITE REG-HS2.

         MOVE "050" TO HS3-CONS.
         MOVE "2014/10/05" TO HS3-FECHA.
         MOVE 2 TO HS3-CLIENTE.
         MOVE 1.5 TO HS3-CANT-HORAS.
         MOVE "NINGUNA" TO HS3-OBSERV.
         WRITE REG-HS3.

         MOVE "001" TO HS1-CONS.
         MOVE "2014/10/13" TO HS1-FECHA.
         MOVE 3 TO HS1-CLIENTE.
         MOVE 2 TO HS1-CANT-HORAS.
         MOVE "NINGUNA" TO HS1-OBSERV.
         WRITE REG-HS1.

         CLOSE HS1, HS2, HS3.
         STOP RUN.

       END PROGRAM GENERAR-HS.
