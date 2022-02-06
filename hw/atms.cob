       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMS.

      * CSCI3180 Principles of Programming Languages
      *
      *--- Declaration ---
      *
      *I declare that the assignment here submitted is original except
      *for source material explicitly acknowledged. I also acknowledge
      *that I am aware of University policy and regulations on honesty
      *in academic work, and of the disciplinary guidelines and
      *procedures applicable to breaches of such policy and regulations,
      *as contained in the website
      *http://www.cuhk.edu.hk/policy/academichonesty/
      *
      *Assignment 1
      *Name : Liu Yunzhi
      *Student ID : 1155141571
      *Email Addr : yzliu0@cse.cuhk.edu.hk

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FILE-ONE ASSIGN TO "trans711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL FILE-THREE ASSIGN TO "trans713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-MASTER  ASSIGN TO "../master.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FILE-ONE.
       01 ATRANS-RECORD.
           02 AACCOUNT     PIC 9(16).
           02 AOPERATION   PIC A.
           02 AAMOUNT      PIC 9(7).
           02 ATIMESTAMP   PIC 9(5).

       FD FILE-THREE.
       01 CTRANS-RECORD.
           02 CACCOUNT     PIC 9(16).
           02 COPERATION   PIC A.
           02 CAMOUNT      PIC 9(7).
           02 CTIMESTAMP   PIC 9(5).

       FD FILE-MASTER.
       01 ACCOUNT-RECORD.
           02 MNAME        PIC A(20).
           02 MACCOUNT     PIC 9(16).
           02 MPASSWORD    PIC 9(6).
           02 M-SIGN       PIC X.
           02 MBALANCE     PIC 9(15).

       WORKING-STORAGE SECTION.
       01 INPUT-ATM        PIC X.
       01 INPUT-OPERATION  PIC X.
       01 INPUT-CONTINUE   PIC X.
       01 INPUT-ACCOUNT    PIC 9(16).
       01 INPUT-TARGET     PIC 9(16).
       01 INPUT-AMOUNT     PIC 9(7).
       01 INPUT-PASSWORD   PIC 9(6).
       01 LOOP-TIMES       PIC 9 VALUE 0.
       01 TIME-STAMP       PIC 9(5) VALUE 00000.
       01 CHECK-ACCOUNT    PIC 9 VALUE 0.

       01 CURRENT-ACCOUNT.
           02 NNAME        PIC A(20)   VALUE SPACE.
           02 NACCOUNT     PIC 9(16)   VALUE 0.
           02 NPASSWORD    PIC 9(6)    VALUE 0.
           02 N-SIGN       PIC X.
           02 NBALANCE     PIC 9(15).




       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           IF LOOP-TIMES = 0 THEN
               OPEN OUTPUT FILE-ONE
               OPEN OUTPUT FILE-THREE
               DISPLAY '##########################################'
               DISPLAY '##       Gringotts Wizarding Bank       ##'
               DISPLAY '##               Welcome                ##'
               DISPLAY '##########################################'
               MOVE 1 TO LOOP-TIMES
           END-IF.
           GO TO CHOOSE-ATM-PARAGRAPH.

           GO TO CHOOSE-OPERATION-PARAGRAPH.

           GO TO CONTINUE-PARAGRAPH.
           STOP RUN.

       CHOOSE-ATM-PARAGRAPH.
           DISPLAY '=> PLEASE CHOOSE THE ATM'.
           DISPLAY '=> PRESS 1 FOR ATM 711'.
           DISPLAY '=> PRESS 2 FOR ATM 713'.
           ACCEPT INPUT-ATM.
           IF INPUT-ATM NOT = 1 THEN
               IF INPUT-ATM NOT = 2 THEN
                   DISPLAY '=> INVALID INPUT'
                   GO TO CHOOSE-ATM-PARAGRAPH
               END-IF
           END-IF.

       INPUT-AP-PARAGRAPH.
           DISPLAY '=> ACCOUNT'.
           ACCEPT INPUT-ACCOUNT.
           DISPLAY '=> PASSWORD'.
           ACCEPT INPUT-PASSWORD.
           OPEN INPUT FILE-MASTER.
           GO TO TEST-AP-PARAGRAPH.

      *TEST FOR RIGHT ACCOUNT
       TEST-AP-PARAGRAPH.
           READ FILE-MASTER NEXT RECORD
               AT END GO TO INCORRECT-AP-PARAGRAPH
               NOT AT END GO TO CHECK-AP-PARAGRAPH
           END-READ.

       INCORRECT-AP-PARAGRAPH.
           DISPLAY '=> INCORRECT ACCOUNT/PASSWORD'.
           CLOSE FILE-MASTER.
           GO TO INPUT-AP-PARAGRAPH.


      *CHECK FOR RIGHT PASSWORD
       CHECK-AP-PARAGRAPH.
           IF INPUT-ACCOUNT = MACCOUNT THEN
               IF INPUT-PASSWORD = MPASSWORD THEN
                   IF M-SIGN = '-' THEN
                       DISPLAY '=> NEGATIVE REMAINS TRANSACTION ABORT'
                       CLOSE FILE-MASTER
                       GO TO  MAIN-PARAGRAPH
                   END-IF
                   MOVE ACCOUNT-RECORD TO CURRENT-ACCOUNT
                   GO TO CHOOSE-OPERATION-PARAGRAPH
               END-IF
               IF INPUT-PASSWORD NOT = MPASSWORD IN ACCOUNT-RECORD THEN
                   GO TO INCORRECT-AP-PARAGRAPH
               END-IF
           END-IF.
           GO TO TEST-AP-PARAGRAPH.


       CHOOSE-OPERATION-PARAGRAPH.
           CLOSE FILE-MASTER.
           DISPLAY '=> PLEASE CHOOSE YOUR SERVICE'.
           DISPLAY '=> PRESS D FOR DEPOSIT'.
           DISPLAY '=> PRESS W FOR WITHDRAWAL'.
           DISPLAY '=> PRESS T FOR TRANSFER'.
           ACCEPT INPUT-OPERATION.

           IF INPUT-OPERATION = 'D' THEN
               GO TO DEPOSIT-PARAGRAPH
           END-IF.
           IF INPUT-OPERATION NOT = 'D' THEN
               IF INPUT-OPERATION = 'W' THEN
                   GO TO WITHDRAWAL-PARAGRAPH
               END-IF
               IF INPUT-OPERATION NOT = 'W' THEN
                   IF INPUT-OPERATION = 'T' THEN
                       GO TO TRANSFER-PARAGRAPH
                   END-IF
                   IF INPUT-OPERATION NOT = 'T' THEN
                       DISPLAY '=> INVALID INPUT'
                       GO TO CHOOSE-OPERATION-PARAGRAPH
                   END-IF
               END-IF
           END-IF.

       DEPOSIT-PARAGRAPH.
           DISPLAY '=> AMOUNT'.
           ACCEPT INPUT-AMOUNT.
           COMPUTE INPUT-AMOUNT = INPUT-AMOUNT * 100.
           IF INPUT-AMOUNT <= 0 THEN
               DISPLAY '=> INVALID INPUT'
               GO TO DEPOSIT-PARAGRAPH
           END-IF.
           IF INPUT-AMOUNT > 0 THEN
               GO TO WRITE-TRANSFER-PARAGRAPH
           END-IF.



       WITHDRAWAL-PARAGRAPH.
           DISPLAY '=> AMOUNT'.
           ACCEPT INPUT-AMOUNT.
           COMPUTE INPUT-AMOUNT = INPUT-AMOUNT * 100.
           IF INPUT-AMOUNT <= 0 THEN
               DISPLAY '=> INVALID INPUT'
               GO TO WITHDRAWAL-PARAGRAPH
           END-IF.
           IF INPUT-AMOUNT > 0 THEN
               IF INPUT-AMOUNT <= NBALANCE THEN
                   GO TO WRITE-TRANSFER-PARAGRAPH
               END-IF
               IF INPUT-AMOUNT > NBALANCE THEN
                   DISPLAY '=> INSUFFICIENT BALANCE'
                   GO TO WITHDRAWAL-PARAGRAPH
               END-IF
           END-IF.

       TRANSFER-PARAGRAPH.
           DISPLAY '=> TARGET ACCOUNT'.
           ACCEPT INPUT-TARGET.
           IF INPUT-TARGET = NACCOUNT THEN
               DISPLAY '=> YOU CANNOT TRANSFER TO YOURSELF'
               GO TO TRANSFER-PARAGRAPH
           END-IF.
           IF INPUT-TARGET NOT = NACCOUNT THEN
               OPEN INPUT FILE-MASTER
               GO TO CHECK-ACCOUNT-PARAGRAPH
           END-IF.

       CHECK-ACCOUNT-PARAGRAPH.
           READ FILE-MASTER  NEXT RECORD
               NOT AT END GO TO SUBCHECK-ACCOUNT-PARAGRAPH
               AT END GO TO TRANSFER-END-PARAGRAPH
           END-READ.


       SUBCHECK-ACCOUNT-PARAGRAPH.
           IF INPUT-TARGET = MACCOUNT THEN
               MOVE 1 TO CHECK-ACCOUNT
               GO TO TRANSFER-END-PARAGRAPH
           END-IF.
           IF INPUT-TARGET NOT = MACCOUNT THEN
               GO TO CHECK-ACCOUNT-PARAGRAPH
           END-IF.

       TRANSFER-END-PARAGRAPH.
           CLOSE FILE-MASTER.
           IF CHECK-ACCOUNT = 0 THEN
               DISPLAY '=> TARGET ACCOUNT DOES NOT EXIST'
               GO TO TRANSFER-PARAGRAPH
           END-IF.
           IF CHECK-ACCOUNT = 1 THEN
               GO TO TRANSFER-AMOUNT-PARAGRAPH
           END-IF.



       TRANSFER-AMOUNT-PARAGRAPH.
           DISPLAY '=> AMOUNT'.
           ACCEPT INPUT-AMOUNT.
           COMPUTE INPUT-AMOUNT = INPUT-AMOUNT * 100.
           DISPLAY INPUT-AMOUNT.
           DISPLAY NBALANCE.
           IF INPUT-AMOUNT <= 0 THEN
               DISPLAY '=> INVALID INPUT'
               GO TO TRANSFER-AMOUNT-PARAGRAPH
           END-IF.
           IF INPUT-AMOUNT > 0 THEN
               IF INPUT-AMOUNT <= NBALANCE THEN
                   GO TO WRITE-TRANSFER-PARAGRAPH
               END-IF
               IF INPUT-AMOUNT > NBALANCE THEN
                   DISPLAY '=> INSUFFICIENT BALANCE'
                   GO TO TRANSFER-AMOUNT-PARAGRAPH
               END-IF
           END-IF.




       WRITE-TRANSFER-PARAGRAPH.
           IF INPUT-OPERATION NOT = 'T'
               IF INPUT-ATM = 1 THEN
                   MOVE NACCOUNT TO AACCOUNT
                   MOVE INPUT-OPERATION TO AOPERATION
                   MOVE INPUT-AMOUNT TO AAMOUNT
                   MOVE TIME-STAMP TO ATIMESTAMP
                   WRITE ATRANS-RECORD
               END-IF
               IF INPUT-ATM = 2 THEN
                   MOVE NACCOUNT TO CACCOUNT
                   MOVE INPUT-OPERATION TO COPERATION
                   MOVE INPUT-AMOUNT TO CAMOUNT
                   MOVE TIME-STAMP TO CTIMESTAMP
                   WRITE CTRANS-RECORD
               END-IF
           END-IF.
           IF INPUT-OPERATION = 'T'
               IF INPUT-ATM = 1 THEN
                   MOVE NACCOUNT TO AACCOUNT
                   MOVE 'W' TO AOPERATION
                   MOVE INPUT-AMOUNT TO AAMOUNT
                   MOVE TIME-STAMP TO ATIMESTAMP
                   WRITE ATRANS-RECORD
                   COMPUTE TIME-STAMP = TIME-STAMP + 1

                   MOVE INPUT-TARGET TO AACCOUNT
                   MOVE 'D' TO AOPERATION
                   MOVE INPUT-AMOUNT TO AAMOUNT
                   MOVE TIME-STAMP TO ATIMESTAMP
                   WRITE ATRANS-RECORD
               END-IF
               IF INPUT-ATM = 2 THEN
                   MOVE NACCOUNT TO CACCOUNT
                   MOVE 'W' TO COPERATION
                   MOVE INPUT-AMOUNT TO CAMOUNT
                   MOVE TIME-STAMP TO CTIMESTAMP
                   WRITE CTRANS-RECORD
                   COMPUTE TIME-STAMP = TIME-STAMP + 1

                   MOVE INPUT-TARGET TO CACCOUNT
                   MOVE 'D' TO COPERATION
                   MOVE INPUT-AMOUNT TO CAMOUNT
                   MOVE TIME-STAMP TO CTIMESTAMP
                   WRITE CTRANS-RECORD
               END-IF.
               COMPUTE TIME-STAMP = TIME-STAMP + 1.

       CONTINUE-PARAGRAPH.
           DISPLAY '=> CONTINUE?'.
           DISPLAY '=> Y FOR YES'.
           DISPLAY '=> N FOR NO'.
           ACCEPT INPUT-CONTINUE.
           IF INPUT-CONTINUE = 'Y' THEN
               GO TO MAIN-PARAGRAPH
           END-IF.
           IF INPUT-CONTINUE NOT = 'Y' THEN
               IF INPUT-CONTINUE NOT = 'N' THEN
                   DISPLAY '=> INVALID INPUT'
                   GO TO CONTINUE-PARAGRAPH
               END-IF
           END-IF.
           CLOSE FILE-ONE.
           CLOSE FILE-THREE.
