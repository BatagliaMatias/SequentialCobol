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
           SELECT indfile1 ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PERFIL
           ALTERNATE RECORD KEY IS FVIGENCIA WITH DUPLICATES.


       DATA DIVISION.
        FILE SECTION.
         FD indfile1
          VALUE OF FILE-ID IS "TARIFAS.DAT".
          01 fileind1.
            03 PERFIL PIC X.
            03 FVIGENCIA PIC X(10).
            03 TARIFA PIC 9(7)V99.

       WORKING-STORAGE SECTION.
      *-----------------------
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PAR-ENTRADA.
           03 PRIMER-OPERANDO PIC 9(3).
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
            OPEN I-O indfile1
            DISPLAY "ARCHIVO ABIERTO"

            WHEN 'C'
            CLOSE indfile1
            DISPLAY "ARCHIVO CERRADO"

            WHEN 'P'
            DISPLAY "PROCESAR".
        
        MOVE 0 TO PAR-SALIDA
        
      ** add other procedures here
       EXIT PROGRAM.
