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
           01  SEARCH-EMPLOYEE-INFO.
               02  S_EMPLOYEE_NO  PIC 9(10).
               02  S_FULL_NAME PIC X(50).
               02  S_PASSWORD PIC X(50).
               02  S_SCHEDULE PIC X(50).
               02  S_TIME-SCHED PIC X(50).
               02  S_SALARY-PER-HOUR PIC 9(10).
           01  TIME-SHEET.
               02  T_EMPLOYEE_NO PIC X(50).
               02  T-IN PIC X(50).
               02  T-OUT PIC X(50).
               02  DATE_REPORT PIC X(50).
           01  UTILITIES.
               02  YES-NO PIC X(1).
               02  WHAT-TO-DO PIC 9(2).
               02  WS-EOF PIC A(1).
               02  ASK_EMPLOYEE_NO PIC X(10).

       PROCEDURE DIVISION.
           PERFORM ASK-WHAT-TO-DO.

       ASK-WHAT-TO-DO.
           DISPLAY "PRESS 1 TO ADD EMPLOYEE".
           DISPLAY "PRESS 2 TO VIEW EMPLOYEE".
           DISPLAY "PRESS 3 TO SEARCH EMPLOYEE BY EMPLOYEE-NO".
           DISPLAY "PRESS 4 TO EXIT PROGRAM".
           PERFORM SPACE-ENTER.
           ACCEPT WHAT-TO-DO.

           EVALUATE TRUE
               WHEN WHAT-TO-DO = 1 PERFORM ASK-QUESTION
               WHEN WHAT-TO-DO = 2 PERFORM DISPLAY-USERS
               WHEN WHAT-TO-DO = 3 PERFORM ASK-EMP-NO
               WHEN WHAT-TO-DO = 4 STOP RUN
               WHEN OTHER
                   DISPLAY "PLEASE ENTER DIGITS 1 - 4"
                   PERFORM ASK-WHAT-TO-DO
           END-EVALUATE.

       ASK-EMP-NO.
           DISPLAY "ENTER EMPLOYEE NO, WRITE C TO GO BACK TO MENU".
           ACCEPT ASK_EMPLOYEE_NO.

           EVALUATE TRUE
               WHEN ASK_EMPLOYEE_NO = "C" OR ASK_EMPLOYEE_NO = "c"
                   PERFORM ASK-WHAT-TO-DO
               WHEN ASK_EMPLOYEE_NO NOT = SPACE
                   PERFORM SEARCH-EMPLOYEE-BY-NO
               WHEN OTHER
                   PERFORM ASK-AGAIN-TO-WRITE
           END-EVALUATE.

       SEARCH-EMPLOYEE-BY-NO.
           OPEN INPUT USER-INFO.
               PERFORM UNTIL WS-EOF='Y' OR WS-EOF = 'S'
                   READ USER-INFO INTO EMPLOYEE-INFO
                      AT END MOVE 'Y' TO WS-EOF
                      NOT AT END PERFORM CHECK-EMPLOYEE-INFO-BY-NO
                   END-READ
               END-PERFORM.
           CLOSE USER-INFO.

           IF WS-EOF NOT = 'S'
               DISPLAY "EMPLOYEE NO DOES NOT EXIST", ASK_EMPLOYEE_NO
           END-IF.

           MOVE 'N' TO WS-EOF.
           PERFORM SPACE-ENTER.
           PERFORM ASK-WHAT-TO-DO.

       CHECK-EMPLOYEE-INFO-BY-NO.
           IF EMPLOYEE_NO = FUNCTION NUMVAL(ASK_EMPLOYEE_NO)
               DISPLAY "USER EXIST...!"
               MOVE EMPLOYEE-INFO TO SEARCH-EMPLOYEE-INFO
               DISPLAY SEARCH-EMPLOYEE-INFO
               MOVE 'S' TO WS-EOF
           END-IF.

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
           DISPLAY " ".

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
           DISPLAY "WOULD YOU LIKE TO ADD ANOTHER EMPLOYEE: (Y/N) ?".
           ACCEPT YES-NO
           EVALUATE TRUE
               WHEN YES-NO = "Y" OR YES-NO = "y"
                   PERFORM ASK-QUESTION
               WHEN YES-NO = "N" OR YES-NO = "n"
                   PERFORM ASK-WHAT-TO-DO
               WHEN OTHER
                   PERFORM ASK-AGAIN-TO-WRITE
           END-EVALUATE.
