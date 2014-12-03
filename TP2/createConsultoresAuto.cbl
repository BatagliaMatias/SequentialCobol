       IDENTIFICATION DIVISION.
         PROGRAM-ID. CONSULTORES.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT CON ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS COD-CONS.


       DATA DIVISION.
        FILE SECTION.
         FD CON
          VALUE OF FILE-ID IS "CONSULTORES.DAT".
          01 CON-REG.
            03 COD-CONS PIC 9(3).
            03 FECHA-INGRESO PIC X(8).
            03 DIRECCION PIC X(20).
            03 APEYNOM PIC X(25).
            03 TELEFONO PIC 9(10).
            03 PERFIL PIC X.

         WORKING-STORAGE SECTION.
          01 CHOICE PIC 9.
          01 CH1 PIC X VALUE 'Y'.
          01 REXIST PIC X.

       PROCEDURE DIVISION.
        PARA1.
          OPEN OUTPUT CON.
          DISPLAY 'Generando Archivo CON de consultores'.
          MOVE 010 TO COD-CONS.
          MOVE '20121212' TO FECHA-INGRESO.
          MOVE 'CALLE FALSA 123' TO DIRECCION.
          MOVE 'COSME FULANITO' TO APEYNOM.
          MOVE 49831212 TO TELEFONO.
          MOVE 'Z' TO PERFIL.
          DISPLAY '--------------------------------------'.
          DISPLAY COD-CONS.
          DISPLAY FECHA-INGRESO.
          WRITE CON-REG.

          MOVE 001 TO COD-CONS.
          MOVE '20130313' TO FECHA-INGRESO.
          MOVE 'RIVADAVIA 1222' TO DIRECCION.
          MOVE 'CHARLY' TO APEYNOM.
          MOVE 1149831212 TO TELEFONO.
          MOVE 'X' TO PERFIL.
          DISPLAY '--------------------------------------'.
          DISPLAY COD-CONS.
          DISPLAY FECHA-INGRESO.
          WRITE CON-REG.

          MOVE 100 TO COD-CONS.
          MOVE '20121212' TO FECHA-INGRESO.
          MOVE 'RIVADAVIA 1452' TO DIRECCION.
          MOVE 'NOMBRE' TO APEYNOM.
          MOVE 1149831212 TO TELEFONO.
          MOVE 'A' TO PERFIL.
          DISPLAY '--------------------------------------'.
          DISPLAY COD-CONS.
          DISPLAY FECHA-INGRESO.
          WRITE CON-REG.

          MOVE 101 TO COD-CONS.
          MOVE '20001212' TO FECHA-INGRESO.
          MOVE 'RIVADAVIA 1452' TO DIRECCION.
          MOVE 'VIEJO' TO APEYNOM.
          MOVE 1149831212 TO TELEFONO.
          MOVE 'B' TO PERFIL.
          DISPLAY '--------------------------------------'.
          DISPLAY COD-CONS.
          DISPLAY FECHA-INGRESO.
          WRITE CON-REG.

          CLOSE CON.
          STOP RUN.
       END PROGRAM CONSULTORES.
