      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TARIFAS.
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
      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PAR-ENTRADA.
           03 PARAM-PERFIL PIC X.
           03 PARAM-FVIGENCIA PIC X(10).
       01 PAR-SALIDA.
           03 RESULTADO PIC 9(5).
       01 COD-OPER PIC X.
       PROCEDURE DIVISION USING PAR-ENTRADA,PAR-SALIDA,COD-OPER.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
        EVALUATE COD-OPER
            WHEN 'O'
            OPEN I-O TAR


            WHEN 'C'
            CLOSE TAR


            WHEN 'P'
            MOVE PARAM-PERFIL TO PERFIL
            MOVE PARAM-FVIGENCIA TO FVIGENCIA
            READ TAR RECORD
            DISPLAY TARIFA.

        
        MOVE 0 TO PAR-SALIDA
        
      ** add other procedures here
       EXIT PROGRAM.
