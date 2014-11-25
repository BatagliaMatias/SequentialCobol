       IDENTIFICATION DIVISION.
         PROGRAM-ID. index1.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT indfile1 ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS fno.


       DATA DIVISION.
        FILE SECTION.
         FD indfile1
          VALUE OF FILE-ID IS "INDEX1.TXT".
          01 fileind1.
            05 fno PIC 99.
            05 fname PIC X(10).

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
           DISPLAY "ENTER THE ROLL NO".
           ACCEPT fno.
           DISPLAY "ENTER THE ROLL NAME".
           ACCEPT fname.
           DISPLAY " ".

           WRITE fileind1
            INVALID KEY DISPLAY"  RECORD IS ALREADY EXIST"
           END-WRITE.


         DELETE1.
            DISPLAY " ".
            DISPLAY "ENTER THE RECORD YOU WANT TO DELETE".
            ACCEPT fno.
            DELETE indfile1
             INVALID KEY DISPLAY "  RECORD NOT EXIST"
            END-DELETE.

         UPDATE1.
            MOVE 'Y' TO REXIST.
            DISPLAY " ".
            DISPLAY "ENTER THE ROLL NO YOU WANT TO UPDATE".
            ACCEPT fno.
            READ indfile1
             INVALID KEY MOVE 'N' TO REXIST
            END-READ.
            IF REXIST='N'
              DISPLAY "  RECORD NOT EXIST"
            ELSE
              DISPLAY "ENTER THE NAME YOU WANT TO UPDATE"
              ACCEPT fname
            END-IF.

            REWRITE fileind1
              INVALID KEY DISPLAY "  RECORD NOT READED"
            END-REWRITE.


         READ1.
           MOVE 'Y' TO REXIST.
           DISPLAY " ".
           DISPLAY "ENTER THE ROLL NO YOU WANT TO SEE".
           ACCEPT fno.
           READ indfile1
              INVALID KEY MOVE 'N' TO REXIST
           END-READ.
            IF REXIST='N'
              DISPLAY "  RECORD NOT EXIST"
            ELSE
              DISPLAY "ROLL NO: " fno
              DISPLAY "NAME :" fname
            END-IF.

       END PROGRAM index1.
