      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABSUCH.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KARTEIN  ASSIGN TO RANDOM "GABKART.DAT"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS KA-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT OLKART   ASSIGN TO RANDOM "GABKART.ADI"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER "SUCHEN.LST"
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  OLKART                      LABEL RECORD STANDARD.
       01  OK-SATZ.
           03  OK-X                    PIC X.
      *********************************************** Karteibewegungen *
       FD  KARTEIN     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 286 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  KA-SATZ.
           03  KA-KEY.
               05 KA-ADNUM             PIC 9(6)     COMP.
               05 KA-DATUM             PIC 9(6)     COMP.
           03  KA-TXT                  PIC X(280).
      ******************************** P3800 = PRINTER **** PC = LPT1 *
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(4).
           03  DRA-TX.
               05 DRA-ARNUM            PIC ZZZ9-.
               05 DRA-REC              PIC 99.99.
               05 DRA-MENGEA           PIC ZZZZ-.
               05 DRA-MENGEB           PIC ZZZZ-.
               05 DRA-MENGEC           PIC ZZZZ-.
               05 DRA-KTONR            PIC ZZZZZZ-.
               03 DRA-DATUM            PIC 99.99.99.
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY "WHCREG.CPY".
       01  WX-R.
           03  IX                      PIC 999        COMP.
           03  WS                      PIC 999        COMP.
           03  WH-KART                 PIC 9(5).
           03  WD-KZ                   PIC 9.
       01  WA-SATZ.
           03  WA-KEY.
               05 WA-SYS               PIC XX       COMP-X.
               05 WA-ADNUM             PIC 9(6)     COMP.
               05 WA-DATUM             PIC 9(6)     COMP.
           03  WA-TXT                  PIC X(300).
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACES UPON CRT.
           CALL "CAUP" USING "9901012480000" WH-CREG.
           OPEN INPUT OLKART REVERSED.
           OPEN I-O KARTEIN.
       B.  MOVE 300 TO IX.
           MOVE LOW-VALUES TO WA-SATZ.
       C.  READ OLKART AT END GO Q.
           MOVE OK-X TO WA-TXT(IX:1).
           ADD -1 TO IX.
           IF IX < 2 PERFORM LIESANF GO G.
           IF OK-X not = "@" GO C.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 300
               or WA-SATZ(WX:1) = "@" CONTINUE.
           MOVE WX TO WS.
           IF WX < 14 GO B.
           MOVE WA-SATZ(WS:) TO WA-SATZ(1:).
       G.  DISPLAY WA-ADNUM AT 2010 " " WA-DATUM.
           DISPLAY WA-TXT(1:69) AT 2110.
           MOVE WA-DATUM TO KA-DATUM WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           IF WZ-DATUM = 0 GO B.
           MOVE WA-ADNUM TO KA-ADNUM WD-KZ.
           IF WD-KZ not = 2 GO B.
           MOVE WA-TXT TO KA-TXT.
           SUBTRACT WX FROM 300 GIVING WH-KART.
           ADD 7 TO WH-KART.
           WRITE KA-SATZ.
           GO B.
       Q.  CLOSE KARTEIN OLKART.
       Z.  STOP RUN.
      ******************************************************************
       LIESANF SECTION.
       A.  READ OLKART AT END GO Z.
           IF OK-X not = "@" GO A.
       Z.  EXIT.
