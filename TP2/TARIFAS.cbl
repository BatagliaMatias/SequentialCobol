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
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TAR-KEY
           FILE STATUS IS TAR-FS.


       DATA DIVISION.
        FILE SECTION.
         FD TAR
          VALUE OF FILE-ID IS "TARIFAS.DAT".
          01 TAR-REG.
            02 TAR-KEY.
                03 TAR-PERFIL PIC X.
                03 TAR-FVIGENCIA PIC X(10).
            02 TAR-DATA.
                03 TAR-TARIFA PIC 9(7)V99.


       WORKING-STORAGE SECTION.
       01 TAR-FS PIC X(2).
           88 TAR-OK VALUE '00'.
           88 TAR-NO VALUE '23'.
           88 TAR-EOF VALUE '10'.


       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PAR-ENTRADA.
           03 PARAM-PERFIL PIC X.
           03 PARAM-FVIGENCIA PIC X(10).
       01 PAR-SALIDA.
           03 PAR-TARIFA PIC 9(7)V99.
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
            MOVE PARAM-PERFIL TO TAR-PERFIL
            MOVE PARAM-FVIGENCIA TO TAR-FVIGENCIA
            
            START TAR KEY IS LESS TAR-KEY
            READ TAR NEXT RECORD

            IF NOT TAR-OK AND NOT TAR-EOF
               DISPLAY 'ERROR DE LECTURA TARIFA'
               STOP RUN.

            PERFORM 010-RELEECTURA UNTIL TAR-EOF OR
                TAR-FVIGENCIA > PARAM-FVIGENCIA.





       010-RELEECTURA.
           IF COD-OPER = 'P'
               IF(TAR-PERFIL = PARAM-PERFIL) AND
                   (TAR-FVIGENCIA < PARAM-FVIGENCIA OR
                   TAR-FVIGENCIA = PARAM-FVIGENCIA)
                   MOVE TAR-TARIFA TO PAR-TARIFA
                   READ TAR NEXT RECORD.

       EXIT PROGRAM.
