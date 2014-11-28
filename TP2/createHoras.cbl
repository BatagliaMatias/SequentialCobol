       IDENTIFICATION DIVISION.
         PROGRAM-ID. HORAS.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT indfile1 ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CONS
           ALTERNATE RECORD KEY IS FECHA WITH DUPLICATES
           ALTERNATE RECORD KEY IS CLIENTE WITH DUPLICATES.


       DATA DIVISION.
        FILE SECTION.
         FD indfile1
          VALUE OF FILE-ID IS "HORAS.DAT".
          01 fileind1.
            03 CONS PIC 9(3).
            03 FECHA PIC X(8).
            03 CLIENTE PIC 9(4).
            03 CANT-HORAS PIC 9(2)V99.
            03 OBSERV PIC X(30).

         WORKING-STORAGE SECTION.
          01 CHOICE PIC 9.
          01 CH1 PIC X VALUE 'Y'.
          01 REXIST PIC X.

       PROCEDURE DIVISION.
        PARA1.
          OPEN I-O indfile1.

         PERFORM UNTIL CH1='N'
          DISPLAY "MENU::"
          DISPLAY "    ENTER YOUR CHOICE"
          DISPLAY "    1=ADD RECORD"
          DISPLAY "    2=DELETE RECORD"
          DISPLAY "    3=UPDATE RECORD"
          DISPLAY "    4=READ PERTICULAR RECORD"
          display "    0=EXIT"
          ACCEPT choice

          EVALUATE CHOICE
           WHEN 1 PERFORM ADD1
           WHEN 2 PERFORM DELETE1
           WHEN 3 PERFORM UPDATE1
           WHEN 4 PERFORM READ1
           WHEN OTHER move 'N' TO ch1
          END-EVALUATE
          
         END-PERFORM.
         STOP RUN.

         ADD1.
           DISPLAY " ".
           DISPLAY "ENTER THE  CONS".
           ACCEPT CONS.
           DISPLAY "ENTER THE  FECHA".
           ACCEPT FECHA.
           DISPLAY "ENTER THE  CLIENTE".
           ACCEPT CLIENTE.
           DISPLAY "ENTER THE  CANT-HORAS".
           ACCEPT CANT-HORAS.
           DISPLAY "ENTER THE  OBSERV".
           ACCEPT OBSERV.
           DISPLAY " ".

           WRITE fileind1
            INVALID KEY DISPLAY"  RECORD IS ALREADY EXIST"
           END-WRITE.


         DELETE1.
            DISPLAY " ".
            DISPLAY "ENTER THE RECORD YOU WANT TO DELETE".
            ACCEPT CONS.
            DELETE indfile1
             INVALID KEY DISPLAY "  RECORD NOT EXIST"
            END-DELETE.

         UPDATE1.
            MOVE 'Y' TO REXIST.
            DISPLAY " ".
            DISPLAY "ENTER THE ROLL NO YOU WANT TO UPDATE".
            ACCEPT CONS.
            READ indfile1
             INVALID KEY MOVE 'N' TO REXIST
            END-READ.
            IF REXIST='N'
              DISPLAY "  RECORD NOT EXIST"
            ELSE
              DISPLAY "ENTER THE NAME YOU WANT TO UPDATE"
              ACCEPT CONS
            END-IF.

            REWRITE fileind1
              INVALID KEY DISPLAY "  RECORD NOT READED"
            END-REWRITE.


         READ1.
           MOVE 'Y' TO REXIST.
           DISPLAY " ".
           DISPLAY "ENTER THE ROLL NO YOU WANT TO SEE".
           ACCEPT CONS.
           READ indfile1
              INVALID KEY MOVE 'N' TO REXIST
           END-READ.
            IF REXIST='N'
              DISPLAY "  RECORD NOT EXIST"
            ELSE
              DISPLAY "ROLL NO: " CONS.


       END PROGRAM HORAS.
