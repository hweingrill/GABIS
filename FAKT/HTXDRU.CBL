      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      HTXDRU.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  P3800.
       OBJECT-COMPUTER.  P3800.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HILFPOS    ASSIGN TO "HTX.TX"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ****************************************** Hilfsdatei f. Reorg. *
       FD  HILFPOS                     LABEL RECORD STANDARD.
       01  HI-SATZ.
           03  HI-TX                   PIC X(96).
      ******************************** P3800 = PRINTER **** PC = LPT1 *
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-NO                  PIC XX.
           03  DRA-TEXT.
               05 DRA-KTONR            PIC ZZ.ZZ9,9.
               05 DRA-ARNUM            PIC ZZZZ.ZZ9-.
               05 DRA-ART              PIC ZZZ9-.
               05 DRA-MON              PIC ZZZ9/99-.
               05 FILLER               PIC X(210).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-VL               PIC 99.
               05  WL-VP               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-AZ               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY WHCREG.CPY.
       01  WH-TXKEY       external     PIC X(20).
       01  WK-TXKEY       external     PIC 9(5).
       01  WH-REG.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88 AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON HILFPOS.
       A.  CALL "CADECL" USING "GABIHILF.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  OPEN INPUT HILFPOS.
           MOVE X"0000" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
       C.  READ HILFPOS AT END GO Y.
           DISPLAY HI-SATZ AT 1602.
           IF HI-SATZ(40:21) = "Firma   K R I S Z A N"
               MOVE "Firma  Walter  H A A S" TO HI-SATZ(40:22).
           IF HI-SATZ(40:11) = "4400  Steyr"
               MOVE "8430  Leibnitz    Tel. 03452 / 2047"
                  TO HI-SATZ(40:).
           MOVE HI-SATZ TO DRA-SATZ.
           PERFORM DRUCK.
           GO C.
       Y.  PERFORM END-DRU.
           CLOSE HILFPOS.
           STOP RUN.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************* loeschen Zeile 23/24 *
       LOE34 SECTION.
       A.  CALL "CAUP" using "1323012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           PERFORM LOE34.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              PERFORM WEITER GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
