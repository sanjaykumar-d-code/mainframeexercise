       IDENTIFICATION DIVISION.
       PROGRAM-ID. Repayment-Estimate.
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 loan-amount PIC S9(9).
           01 months PIC S9(2).
           01 monthly-pay PIC 9(9)v99.
           01 display-pay PIC zzzzzzz9.99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY " Enter Current Loan Amount : "
           ACCEPT loan-amount
           DISPLAY " Enter Remaining Months : "
           ACCEPT months

           IF loan-amount > 0 AND loan-amount <= 100000000
               IF months >= 0 AND months <= 600
                   COMPUTE monthly-pay = loan-amount / months
                   MOVE monthly-pay TO display-pay
                   DISPLAY " Your Next month pay is : "
                   DISPLAY FUNCTION TRIM(display-pay LEADING)
               ELSE
                   DISPLAY " Invalid Month "

           ELSE
               DISPLAY "Invalid amount "
           END-IF.



           STOP RUN.
