       IDENTIFICATION DIVISION.
         PROGRAM-ID. HORASAUTO.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT HOR ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS HOR-KEY
           ALTERNATE RECORD KEY IS HOR-ALT-KEY WITH DUPLICATES.
*******Código de Consultor + Fecha +  Código de Cliente.
      *Clave alterna = Fecha + Cod-Cliente


       DATA DIVISION.
        FILE SECTION.
         FD HOR
          VALUE OF FILE-ID IS "HORAS.DAT".
          01 HOR-REG.
            02 HOR-KEY.
               03 HOR-CONS PIC 9(3).
               03 HOR-ALT-KEY.
                   04 HOR-FECHA PIC X(8).
                   04 HOR-CLIENTE PIC 9(4).
           02 HOR-DATOS.
               03 HOR-CANT-HORAS PIC 9(2)V99.
               03 HOR-OBSERV PIC X(30).

         WORKING-STORAGE SECTION.
          01 CHOICE PIC 9.
          01 CH1 PIC X VALUE 'Y'.
          01 REXIST PIC X.

       PROCEDURE DIVISION.
        PARA1.
          OPEN OUTPUT HOR.
          DISPLAY 'GENERANDO HOR'.
          MOVE 001 TO HOR-CONS.
          MOVE "20141212" TO HOR-FECHA.
          MOVE 1111 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          MOVE 001 TO HOR-CONS.
          MOVE "20131212" TO HOR-FECHA.
          MOVE 1111 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          MOVE 010 TO HOR-CONS.
          MOVE "20121212" TO HOR-FECHA.
          MOVE 1111 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          MOVE 001 TO HOR-CONS.
          MOVE "20141214" TO HOR-FECHA.
          MOVE 2222 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          MOVE 001 TO HOR-CONS.
          MOVE "20000101" TO HOR-FECHA.
          MOVE 3333 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          MOVE 001 TO HOR-CONS.
          MOVE "20141212" TO HOR-FECHA.
          MOVE 4444 TO HOR-CLIENTE.
          MOVE 50 TO HOR-CANT-HORAS.
          MOVE "UnaObservacion" TO HOR-OBSERV.
          WRITE HOR-REG.

          CLOSE HOR.
          STOP RUN.
       END PROGRAM HORASAUTO.
