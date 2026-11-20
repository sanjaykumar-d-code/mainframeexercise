       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMAST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CUST-FILE
               ASSIGN TO "CUSTVSAM.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CUST-FILE.
       01  CUST-REC.
           05 CUST-ID                PIC X(6).
           05 CUST-NAME              PIC X(20).

       WORKING-STORAGE SECTION.

       01  WS-STATUS                 PIC XX.
       01  WS-CUST-WORK              PIC X(26).

       PROCEDURE DIVISION.

       MAIN-PARA.

           OPEN I-O CUST-FILE

           IF WS-STATUS = '00'

               MOVE 'C00100' TO CUST-ID

               READ CUST-FILE
                   INVALID KEY
                       DISPLAY "CUSTOMER NOT FOUND: " WS-STATUS

                   NOT INVALID KEY

                       DISPLAY "CUSTOMER FOUND: " CUST-ID

                       MOVE CUST-REC TO WS-CUST-WORK

                       DELETE CUST-FILE RECORD

                       IF WS-STATUS = '00'

                           DISPLAY "OLD RECORD DELETED"

                           MOVE WS-CUST-WORK TO CUST-REC
                           MOVE 'C00999' TO CUST-ID

                           WRITE CUST-REC

                           IF WS-STATUS = '00'

                               DISPLAY "CUSTOMER ID UPDATED"
                               DISPLAY "NEW CUSTOMER ID: " CUST-ID
                               DISPLAY "CUSTOMER NAME: " CUST-NAME

                           ELSE

                               DISPLAY "WRITE FAILED: " WS-STATUS

                           END-IF

                       ELSE

                           DISPLAY "DELETE FAILED: " WS-STATUS

                       END-IF

               END-READ

               CLOSE CUST-FILE

           ELSE

               DISPLAY "OPEN FAILED: " WS-STATUS

           END-IF

           STOP RUN.
           