       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-APP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  USER-INFO.
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

       PROCEDURE DIVISION.
           PERFORM ASK-QUESTION.

       ASK-QUESTION.
           PERFORM ADD-USER.
           PERFORM USER-INFO-ENTERED.
           PERFORM ASK-USER.

       ADD-USER.
           DISPLAY "ENTER EMPLOYEE NO: ".
           ACCEPT EMPLOYEE_NO.
           DISPLAY "ENTER FULL NAME: ".
           ACCEPT FULL_NAME.
           DISPLAY "ENTER PASSWORD: ".
           ACCEPT PASSWORD .
           DISPLAY "ENTER SCHEDULE, (SEPERATED IN -) EX: (M-W-F)".
           ACCEPT SCHEDULE.
           DISPLAY "ENTER TIME-SCHED, EX 8:30 AM - 9:30 AM".
           ACCEPT TIME-SCHED.
           DISPLAY "ENTER SALARY PER HOUR: "
           ACCEPT SALARY-PER-HOUR.

       USER-INFO-ENTERED.
           DISPLAY "EMPLOYEE NO: ", EMPLOYEE_NO.
           DISPLAY "FULL NAME: ", FULL_NAME.
           DISPLAY "PASSWORD: ", PASSWORD.
           DISPLAY "SCHEDULE, (SEPERATED IN -) EX: (M-W-F)", SCHEDULE.
           DISPLAY "TIME-SCHED, EX 8:30 AM - 9:30 AM", TIME-SCHED.
           DISPLAY "SALARY PER HOUR: ",SALARY-PER-HOUR.

       ASK-USER.
           DISPLAY "ARE DETAILS RIGHT (Y/N)?".
           ACCEPT YES-NO.
           EVALUATE TRUE
               WHEN YES-NO = "Y" OR YES-NO = "y"
                   STOP RUN
               WHEN YES-NO = "N" OR YES-NO = "n"
                   PERFORM ASK-QUESTION
               WHEN OTHER
                   STOP RUN
           END-EVALUATE.
