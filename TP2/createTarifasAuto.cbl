       IDENTIFICATION DIVISION.
         PROGRAM-ID. TARIFASAUTO.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT TAR ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS TAR-KEY.


       DATA DIVISION.
        FILE SECTION.
         FD TAR
          VALUE OF FILE-ID IS "TARIFAS.DAT".
           01 TAR-REG.
            02 TAR-KEY.
                03 PERFIL PIC X.
                03 FVIGENCIA PIC X(10).
            02 TAR-DATA.
                03 TARIFA PIC 9(7)V99.

         WORKING-STORAGE SECTION.
          01 CHOICE PIC 9.
          01 CH1 PIC X VALUE 'Y'.
          01 REXIST PIC X.

       PROCEDURE DIVISION.
        PARA1.
         OPEN OUTPUT TAR.
         MOVE 'X' TO PERFIL.
         MOVE '2014-12-14' TO FVIGENCIA.
         MOVE 125 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'X' TO PERFIL.
         MOVE '2012-12-12' TO FVIGENCIA.
         MOVE 100 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'Z' TO PERFIL.
         MOVE '2014-12-14' TO FVIGENCIA.
         MOVE 200 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'Z' TO PERFIL.
         MOVE '2012-12-12' TO FVIGENCIA.
         MOVE 100 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'A' TO PERFIL.
         MOVE '2014-12-14' TO FVIGENCIA.
         MOVE 200 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'A' TO PERFIL.
         MOVE '2012-12-12' TO FVIGENCIA.
         MOVE 100 TO TARIFA.
         WRITE TAR-REG.


         MOVE 'B' TO PERFIL.
         MOVE '2014-12-14' TO FVIGENCIA.
         MOVE 200 TO TARIFA.
         WRITE TAR-REG.

         MOVE 'B' TO PERFIL.
         MOVE '2012-12-12' TO FVIGENCIA.
         MOVE 100 TO TARIFA.
         WRITE TAR-REG.

         CLOSE TAR.
         STOP RUN.


       END PROGRAM TARIFASAUTO.
