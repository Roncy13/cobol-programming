       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-APP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT USER-INFO ASSIGN TO "../USER-INFO.DAT"
		      ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD USER-INFO.
           001 USER-INFO-FILE.
               02  W_EMPLOYEE_NO  PIC 9(10).
               02  W_FULL_NAME PIC X(50).
               02  W_PASSWORD PIC X(50).
               02  W_SCHEDULE PIC X(50).
               02  W_TIME-SCHED PIC X(50).
               02  W_SALARY-PER-HOUR PIC 9(10).
           WORKING-STORAGE SECTION.
           01  EMPLOYEE-INFO.
               02  EMPLOYEE_NO  PIC 9(10).
               02  FULL_NAME PIC X(50).
               02  PASSWORD PIC X(50).
               02  SCHEDULE PIC X(50).
               02  TIME-SCHED PIC X(50).
               02  SALARY-PER-HOUR PIC 9(10).
           01  TIME-SHEET.
               02  T_EMPLOYEE_NO PIC X(50).
               02  T-IN PIC X(50).
               02  T-OUT PIC X(50).
               02  DATE_REPORT PIC X(50).
           01  QUESTION.
               02  YES-NO PIC X(1).
               02  WHAT-TO-DO PIC 9(2).
               02  WS-EOF PIC A(1).
       PROCEDURE DIVISION.
           PERFORM ASK-WHAT-TO-DO.

       ASK-WHAT-TO-DO.
           DISPLAY "PRESS 1 TO ADD EMPLOYEE".
           DISPLAY "PRESS 2 TO VIEW EMPLOYEE".
           DISPLAY "PRESS 3 TO EXIT PROGRAM".
           PERFORM SPACE-ENTER.
           ACCEPT WHAT-TO-DO.

           EVALUATE TRUE
               WHEN WHAT-TO-DO = 1 PERFORM ASK-QUESTION
               WHEN WHAT-TO-DO = 2 PERFORM DISPLAY-USERS
               WHEN WHAT-TO-DO = 3 STOP RUN
               WHEN OTHER
                   DISPLAY "PLEASE ENTER DIGITS 1 - 3"
                   PERFORM ASK-WHAT-TO-DO
           END-EVALUATE.

       DISPLAY-USERS.
           OPEN INPUT USER-INFO.
               PERFORM UNTIL WS-EOF='Y'
                   READ USER-INFO INTO EMPLOYEE-INFO
                      AT END MOVE 'Y' TO WS-EOF
                      NOT AT END DISPLAY EMPLOYEE-INFO
                   END-READ
               END-PERFORM.
           CLOSE USER-INFO.
           MOVE 'N' TO WS-EOF.
           PERFORM SPACE-ENTER.
           PERFORM ASK-WHAT-TO-DO.

       SPACE-ENTER.
           DISPLAY "----------------------------".

       ASK-QUESTION.
           PERFORM ADD-USER.
           PERFORM ASK-USER.
           PERFORM ASK-AGAIN-TO-WRITE.

       ADD-USER.
           DISPLAY "ENTER EMPLOYEE NO: ".
           ACCEPT EMPLOYEE_NO.
           PERFORM SPACE-ENTER.
           DISPLAY "ENTER FULL NAME: ".
           ACCEPT FULL_NAME.
           PERFORM SPACE-ENTER.
           DISPLAY "ENTER PASSWORD: ".
           ACCEPT PASSWORD .
           PERFORM SPACE-ENTER.
           DISPLAY "ENTER SCHEDULE, (SEPERATED IN -) EX: (M-W-F)".
           ACCEPT SCHEDULE.
           PERFORM SPACE-ENTER.
           DISPLAY "ENTER TIME-SCHED, EX 8:30 AM - 9:30 PM".
           ACCEPT TIME-SCHED.
           PERFORM SPACE-ENTER.
           DISPLAY "ENTER SALARY PER HOUR: "
           ACCEPT SALARY-PER-HOUR.
           PERFORM SPACE-ENTER.

       ASK-USER.
           DISPLAY "ARE DETAILS RIGHT (Y/N)?".
           ACCEPT YES-NO.
           EVALUATE TRUE
               WHEN YES-NO = "Y" OR YES-NO = "y"
                   PERFORM WRITE-USER-ENTERED
               WHEN YES-NO = "N" OR YES-NO = "n"
                   PERFORM ASK-QUESTION
               WHEN OTHER
                   PERFORM ASK-USER
           END-EVALUATE.

       WRITE-USER-ENTERED.
           OPEN EXTEND USER-INFO.
               MOVE EMPLOYEE-INFO TO USER-INFO-FILE.
               WRITE USER-INFO-FILE
               END-WRITE.
           CLOSE USER-INFO.

       ASK-AGAIN-TO-WRITE.
           DISPLAY "WOULD YOU LIKE TO ADD ANOTHER EMPLOYEE ?".
           ACCEPT YES-NO
           EVALUATE TRUE
               WHEN YES-NO = "Y" OR YES-NO = "y"
                   PERFORM ASK-QUESTION
               WHEN YES-NO = "N" OR YES-NO = "n"
                   STOP RUN
               WHEN OTHER
                   PERFORM ASK-AGAIN-TO-WRITE
           END-EVALUATE.
