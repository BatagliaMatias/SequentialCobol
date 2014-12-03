       IDENTIFICATION DIVISION.

       PROGRAM-ID. GENERAR-CONSUL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PERF ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PARAM ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD PERF LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "PERFILES.DAT".
       01 REG-PERF.
           03 PERF-PERFIL PIC X.
           03 PERF-DESCRIPCION PIC X(15).
           03 PERF-COND PIC X(50).

       FD PARAM LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "PARAMETROS.DAT".
       01 REG-PARAM.
           03 PARAM-FECHA-DESDE PIC X(8).
           03 PARAM-FECHA-HASTA PIC X(8).
           03 PARAM-CLIENTE-DESDE PIC 9(4).
           03 PARAM-CLIENTE-HASTA PIC 9(4).

       WORKING-STORAGE SECTION.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

         DISPLAY "Generando PERFILES.DAT y PARAMETROS.DAT".
         OPEN OUTPUT PERF.
         OPEN OUTPUT PARAM.

         MOVE "X" TO PERF-PERFIL.
         MOVE "PERFIL X" TO PERF-DESCRIPCION.
         MOVE "CONDICION X" TO PERF-COND.
         WRITE REG-PERF.

         MOVE "Z" TO PERF-PERFIL.
         MOVE "PERFIL Z" TO PERF-DESCRIPCION.
         MOVE "CONDICION Z" TO PERF-COND.
         WRITE REG-PERF.
         
         MOVE "A" TO PERF-PERFIL.
         MOVE "PERFIL A" TO PERF-DESCRIPCION.
         MOVE "CONDICION A" TO PERF-COND.
         WRITE REG-PERF.

         MOVE "B" TO PERF-PERFIL.
         MOVE "PERFIL B" TO PERF-DESCRIPCION.
         MOVE "CONDICION B" TO PERF-COND.
         WRITE REG-PERF.


         MOVE "20121212" TO PARAM-FECHA-DESDE.
         MOVE "20131225" TO PARAM-FECHA-HASTA.
         MOVE 0000 TO PARAM-CLIENTE-DESDE.
         MOVE 2222 TO PARAM-CLIENTE-HASTA.
         WRITE REG-PARAM.

         CLOSE PARAM
         CLOSE PERF.
         STOP RUN.
           
       END PROGRAM GENERAR-CONSUL.