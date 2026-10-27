
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFER-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 source-amount    PIC 9(7)V99.
       01 destination-amount        PIC 9(7)V99.
       01 transfer-amount     PIC 9(7)V99.
       01 daily-total         PIC 9(7)V99.
       01 daily-limit        PIC 9(7)V99.
       01 new-total     PIC 9(7)V99.
       01 Statuso    PIC X(10).
       01 display-source PIC Z(9).99.
       01 display-destination PIC Z(9).99.
       01 display-total PIC Z(9).99 .

       PROCEDURE DIVISION.

       MAIN-PROCESS.
           DISPLAY "ENTER SOURCE BALANCE:"
           ACCEPT source-amount

           DISPLAY "ENTER DESTINATION BALANCE:"
           ACCEPT destination-amount

           DISPLAY "ENTER TRANSFER AMOUNT:"
           ACCEPT transfer-amount

           DISPLAY "ENTER AMOUNT TRANSFERRED TODAY:"
           ACCEPT daily-total

           DISPLAY "ENTER DAILY LIMIT:"
           ACCEPT daily-limit

           MOVE "SUCCESS" TO Statuso

           PERFORM VALIDATE-TRANSFER

           IF Statuso = "SUCCESS"
               PERFORM POST-TRANSFER
           END-IF

           PERFORM DISPLAY-RESULT

           STOP RUN.

       VALIDATE-TRANSFER.
           IF transfer-amount IS NOT NUMERIC
               MOVE "REJECTED" TO Statuso
           ELSE
               IF transfer-amount <= ZERO
                   MOVE "REJECTED" TO Statuso
               ELSE
                   IF transfer-amount >
                       source-amount
                       MOVE "REJECTED" TO Statuso
                   ELSE
                       COMPUTE new-total =
                           daily-total
                           + transfer-amount

                       IF new-total >
                           daily-limit
                           MOVE "REJECTED" TO Statuso
                       END-IF
                   END-IF
               END-IF
           END-IF.

       POST-TRANSFER.
           SUBTRACT transfer-amount
               FROM source-amount

           ADD transfer-amount
               TO destination-amount

           MOVE new-total
               TO daily-total.

       DISPLAY-RESULT.
           MOVE source-amount to display-source
           MOVE destination-amount to display-destination
           MOVE daily-total to display-total

           DISPLAY "TRANSFER STATUS: " Statuso

           DISPLAY "SOURCE BALANCE: "
           DISPLAY FUNCTION TRIM(display-source LEADING)

           DISPLAY "DESTINATION BALANCE: "
           DISPLAY FUNCTION TRIM(display-destination LEADING)

           DISPLAY "DAILY TRANSFER TOTAL: "
           DISPLAY FUNCTION TRIM(display-total LEADING).
