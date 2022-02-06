       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTRAL.

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
           SELECT OPTIONAL FILE-OS ASSIGN TO "transSorted711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL FILE-TS ASSIGN TO "transSorted713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL FILE-S ASSIGN TO "transSorted.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-M  ASSIGN TO "master.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL FILE-MU  ASSIGN TO "updatedMaster.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-O ASSIGN TO "trans711.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FILE-T ASSIGN TO "trans713.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL FILE-NR ASSIGN TO "negReport.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL WORK ASSIGN TO WRK.
       DATA DIVISION.
       FILE SECTION.

       FD FILE-OS.
       01 OS-TRANS-RECORD.
           02 OS-ACCOUNT        PIC 9(16).
           02 OS-OPERATION      PIC A.
           02 OS-AMOUNT         PIC 9(7).
           02 OS-TIMESTAMP      PIC 9(5).

       FD FILE-S.
       01 S-TRANS-RECORD.
           02 S-ACCOUNT        PIC 9(16).
           02 S-OPERATION      PIC A.
           02 S-AMOUNT         PIC 9(7).
           02 S-TIMESTAMP      PIC 9(5).

       SD WORK.
       01 WK-TRANS-RECORD.
           02 WK-ACCOUNT        PIC 9(16).
           02 WK-OPERATION      PIC A.
           02 WK-AMOUNT         PIC 9(7).
           02 WK-TIMESTAMP      PIC 9(5).

       FD FILE-TS.
       01 TS-TRANS-RECORD.
           02 TS-ACCOUNT        PIC 9(16).
           02 TS--OPERATION     PIC A.
           02 TS-AMOUNT         PIC 9(7).
           02 TS-TIMESTAMP      PIC 9(5).

       FD FILE-O.
       01 O-TRANS-RECORD.
           02 O-ACCOUNT     PIC 9(16).
           02 O-OPERATION   PIC A.
           02 O-AMOUNT      PIC 9(7).
           02 O-TIMESTAMP   PIC 9(5).

       FD FILE-T.
       01 T-TRANS-RECORD.
           02 T-ACCOUNT     PIC 9(16).
           02 T-OPERATION   PIC A.
           02 T-AMOUNT      PIC 9(7).
           02 T-TIMESTAMP   PIC 9(5).

       FD FILE-MU.
       01 MU-ACCOUNT-RECORD.
           02 MU-NAME        PIC A(20).
           02 MU-ACCOUNT     PIC 9(16).
           02 MU-PASSWORD    PIC 9(6).
           02 MU-SIGN        PIC X.
           02 MU-BALANCE     PIC 9(15).

       FD FILE-M.
       01 M-ACCOUNT-RECORD.
           02 M-NAME        PIC A(20).
           02 M-ACCOUNT     PIC 9(16).
           02 M-PASSWORD    PIC 9(6).
           02 M-SIGN        PIC X.
           02 M-BALANCE     PIC 9(15).

       FD FILE-NR.
       01 NR-RECORD.
           02 NR-PNAME     PIC X(6).
           02 NR-NAME      PIC A(20).
           02 NR-PACCOUNT  PIC X(17).
           02 NR-ACCOUNT    PIC 9(16).
           02 NR-PBALANCE  PIC X(11).
           02 NR-SIGN       PIC X .
           02 NR-BALANCE    PIC 9(15).

       WORKING-STORAGE SECTION.
           01 TS-FLAG   PIC 9 VALUE 0.
           01 OS-FLAG   PIC 9 VALUE 0.
           01 TS-CONS   PIC 9 VALUE 0.
           01 OS-CONS   PIC 9 VALUE 0.
           01 S-FLAG   PIC 9 VALUE 0.
           01 M-FLAG   PIC 9 VALUE 0.
           01 S-CONS   PIC 9 VALUE 0.
           01 M-CONS   PIC 9 VALUE 0.
           01 BALANCE  PIC 9(15).
           01 BUF-ACCOUNT-RECORD.
               02 BUF-NAME        PIC A(20).
               02 BUF-ACCOUNT     PIC 9(16).
               02 BUF-PASSWORD    PIC 9(6).
               02 BUF-SIGN        PIC X.
               02 BUF-BALANCE     PIC 9(15).

       PROCEDURE DIVISION.
       SORT-PARAGRAPH.

           SORT WORK ON ASCENDING KEY TS-ACCOUNT
                     ON ASCENDING KEY TS-TIMESTAMP
           USING FILE-T GIVING FILE-TS.

           SORT WORK ON ASCENDING KEY OS-ACCOUNT
                     ON ASCENDING KEY OS-TIMESTAMP
           USING FILE-O GIVING FILE-OS.

       BEFORE-MERGE-PARAGRAPH.
           OPEN INPUT FILE-TS.
           OPEN INPUT FILE-OS.
           OPEN OUTPUT FILE-S.

       MERGE-PARAGRAPH.
           IF TS-FLAG = 1 THEN
               IF OS-FLAG = 1 THEN
                   CLOSE FILE-TS
                   CLOSE FILE-OS
                   CLOSE FILE-S
                   GO TO BEFORE-UPDATE-PARAGRAPH
               END-IF
           END-IF.
           IF TS-CONS = 0 THEN
               IF TS-FLAG = 0 THEN
                   GO TO TS-READ-PARAGRAPH
               END-IF
           END-IF.
           IF OS-CONS = 0 THEN
               IF OS-FLAG = 0 THEN
                   GO TO OS-READ-PARAGRAPH
               END-IF
           END-IF.

           IF TS-FLAG = 1 THEN
               MOVE OS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO OS-CONS
               GO TO OS-READ-PARAGRAPH
           END-IF.
           IF OS-FLAG = 1 THEN
               MOVE TS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO TS-CONS
               GO TO TS-READ-PARAGRAPH
           END-IF.

           IF OS-ACCOUNT < TS-ACCOUNT THEN
               MOVE OS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO OS-CONS
               GO TO OS-READ-PARAGRAPH
           END-IF.

           IF OS-ACCOUNT > TS-ACCOUNT THEN
               MOVE TS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO TS-CONS
               GO TO TS-READ-PARAGRAPH
           END-IF.

           IF OS-TIMESTAMP < TS-TIMESTAMP THEN
               MOVE OS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO OS-CONS
               GO TO OS-READ-PARAGRAPH
           END-IF.

           IF OS-TIMESTAMP > TS-TIMESTAMP THEN
               MOVE TS-TRANS-RECORD TO S-TRANS-RECORD
               WRITE S-TRANS-RECORD
               MOVE 0 TO TS-CONS
               GO TO TS-READ-PARAGRAPH
           END-IF.

       TS-READ-PARAGRAPH.
           READ FILE-TS
               AT END MOVE 1 TO TS-FLAG
               NOT AT END MOVE 1 TO TS-CONS
           END-READ.
           GO TO MERGE-PARAGRAPH.

       OS-READ-PARAGRAPH.
           READ FILE-OS
               AT END MOVE 1 TO OS-FLAG
               NOT AT END MOVE 1 TO OS-CONS
           END-READ.
           GO TO MERGE-PARAGRAPH.

       BEFORE-UPDATE-PARAGRAPH.
           OPEN INPUT FILE-S.
           OPEN INPUT FILE-M.
           OPEN OUTPUT FILE-MU.
           OPEN OUTPUT FILE-NR.

       UPDATE-PARAGRAPH.
           IF M-FLAG = 1 THEN
               IF S-FLAG = 1 THEN
                   CLOSE FILE-S
                   CLOSE FILE-M
                   CLOSE FILE-MU
                   CLOSE FILE-NR
                   STOP RUN
               END-IF
           END-IF.

           IF S-FLAG = 0 THEN
               IF S-CONS = 0 THEN
                   GO TO S-READ-PARAGRAPH
               END-IF
           END-IF.

           IF M-FLAG = 0 THEN
               IF M-CONS = 0 THEN
                   GO TO M-READ-PARAGRAPH
               END-IF
           END-IF.

           IF S-FLAG = 1 THEN
               MOVE BUF-ACCOUNT-RECORD TO MU-ACCOUNT-RECORD
               WRITE MU-ACCOUNT-RECORD
               MOVE 0 TO M-CONS
               IF BUF-SIGN = '-' THEN
                   GO TO NEGREPORT-PARAGRAPH
               END-IF
               GO TO M-READ-PARAGRAPH
           END-IF.

           IF BUF-ACCOUNT < S-ACCOUNT THEN
               MOVE BUF-ACCOUNT-RECORD TO MU-ACCOUNT-RECORD
               WRITE MU-ACCOUNT-RECORD
               MOVE 0 TO M-CONS
               IF BUF-SIGN = '-' THEN
                   GO TO NEGREPORT-PARAGRAPH
               END-IF
               GO TO M-READ-PARAGRAPH
           END-IF.

           IF BUF-ACCOUNT = S-ACCOUNT THEN
               IF S-OPERATION = 'D' THEN
                   IF BUF-SIGN = '+' THEN
                       COMPUTE BALANCE = BALANCE + S-AMOUNT
                       MOVE 0 TO S-CONS
                       MOVE BALANCE TO BUF-BALANCE
                       GO TO S-READ-PARAGRAPH
                   END-IF
                   IF BUF-SIGN = '-' THEN
                       IF BUF-BALANCE > S-AMOUNT THEN
                           COMPUTE BALANCE = BALANCE - S-AMOUNT
                       END-IF
                       IF BUF-BALANCE <= S-AMOUNT THEN
                           COMPUTE BALANCE = S-AMOUNT - BALANCE
                           MOVE '+' TO BUF-SIGN
                       END-IF
                       MOVE 0 TO S-CONS
                       MOVE BALANCE TO BUF-BALANCE
                       GO TO S-READ-PARAGRAPH
                   END-IF
               END-IF
               IF S-OPERATION = 'W' THEN
                   IF BUF-SIGN = '-' THEN
                       COMPUTE BALANCE = BALANCE + S-AMOUNT
                       MOVE 0 TO S-CONS
                       MOVE BALANCE TO BUF-BALANCE
                       GO TO S-READ-PARAGRAPH
                   END-IF
                   IF BUF-SIGN = '+' THEN
                       IF BUF-BALANCE <= S-AMOUNT THEN
                           COMPUTE BALANCE = S-AMOUNT - BALANCE
                           MOVE '-' TO BUF-SIGN
                           IF BALANCE = 0 THEN
                               MOVE '+' TO BUF-SIGN
                           END-IF
                       END-IF
                       IF BUF-BALANCE > S-AMOUNT THEN
                           COMPUTE BALANCE = BALANCE - S-AMOUNT
                       END-IF
                       MOVE 0 TO S-CONS
                       MOVE BALANCE TO BUF-BALANCE
                       GO TO S-READ-PARAGRAPH
                   END-IF
               END-IF
           END-IF.

       S-READ-PARAGRAPH.
           READ FILE-S
               AT END MOVE 1 TO S-FLAG
               NOT AT END MOVE 1 TO S-CONS
           END-READ.
           GO TO UPDATE-PARAGRAPH.

       M-READ-PARAGRAPH.
           READ FILE-M
               AT END MOVE 1 TO M-FLAG
               NOT AT END GO TO BUF-PARAGRAPH
           END-READ.
           GO TO UPDATE-PARAGRAPH.

       BUF-PARAGRAPH.
            MOVE 1 TO M-CONS.
            MOVE M-ACCOUNT-RECORD TO BUF-ACCOUNT-RECORD.
            MOVE BUF-BALANCE TO BALANCE.
            GO TO UPDATE-PARAGRAPH.

       NEGREPORT-PARAGRAPH.
           MOVE 'Name: ' TO NR-PNAME
           MOVE BUF-NAME TO NR-NAME.
           MOVE ' Account Number: ' TO NR-PACCOUNT
           MOVE BUF-ACCOUNT TO NR-ACCOUNT.
           MOVE ' Balance: ' TO NR-PBALANCE.
           MOVE '-' TO NR-SIGN.
           MOVE BUF-BALANCE TO NR-BALANCE.
           WRITE NR-RECORD.
           GO TO M-READ-PARAGRAPH.
