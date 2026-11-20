       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INPUT-FILE
               ASSIGN TO "CUSTIN.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CUST-FILE
               ASSIGN TO "CUSTVSAM.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-FILE.
       01  INPUT-REC                 PIC X(100).

       FD  CUST-FILE.
       01  CUST-REC.
           05 CUST-ID                PIC X(6).
           05 CUST-NAME              PIC X(20).

       WORKING-STORAGE SECTION.

       01  WS-STATUS                 PIC XX.
       01  WS-EOF                    PIC X VALUE 'N'.
       01  WS-CUST-ID                PIC X(6).
       01  WS-CUST-NAME              PIC X(20).

       PROCEDURE DIVISION.

       MAIN-PARA.

           OPEN INPUT INPUT-FILE
           OPEN OUTPUT CUST-FILE

           PERFORM UNTIL WS-EOF = 'Y'

               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       UNSTRING INPUT-REC
                           DELIMITED BY '|'
                           INTO WS-CUST-ID
                                WS-CUST-NAME
                       END-UNSTRING

                       MOVE WS-CUST-ID TO CUST-ID
                       MOVE WS-CUST-NAME TO CUST-NAME

                       WRITE CUST-REC
               END-READ

           END-PERFORM

           CLOSE INPUT-FILE
           CLOSE CUST-FILE

           DISPLAY "CUSTOMER INDEXED FILE CREATED"

           STOP RUN.
           