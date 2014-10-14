       IDENTIFICATION DIVISION.

       PROGRAM-ID. GENERAR-CONSUL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CONS ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.

       FILE SECTION.
       
       FD CONS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CONSUL.DAT".
       01 REG-CONS.
           03 CONS-CONS PIC 9(3).
           03 CONS-FECHA-ALTA PIC X(8).
           03 CONS-NOMBRE PIC X(25).
       
       WORKING-STORAGE SECTION.
      

       PROCEDURE DIVISION.
      
       MAIN-PROCEDURE.

         DISPLAY "Generando CONSUL.DAT".
         OPEN OUTPUT CONS.

         MOVE 99 TO CONS-CONS.
         MOVE "2014/01/15" TO CONS-FECHA-ALTA.
         MOVE "DERISIVE DAVID" TO CONS-NOMBRE.
         WRITE REG-CONS.

         MOVE 49 TO CONS-CONS.
         MOVE "2012/02/15" TO CONS-FECHA-ALTA.
         MOVE "BORING BOB" TO CONS-NOMBRE.
         WRITE REG-CONS.

         MOVE 50 TO CONS-CONS.
         MOVE "2010/10/15" TO CONS-FECHA-ALTA.
         MOVE "CRAFTY CHARLES" TO CONS-NOMBRE.
         WRITE REG-CONS.

         MOVE 1 TO CONS-CONS.
         MOVE "2013/03/13" TO CONS-FECHA-ALTA.
         MOVE "AMAZING ADAM" TO CONS-NOMBRE.
         WRITE REG-CONS.

         CLOSE CONS.
         STOP RUN.
           
       END PROGRAM GENERAR-CONSUL.
