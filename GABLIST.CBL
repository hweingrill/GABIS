      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABLIST.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEDEB.CPY.
           COPY GABSEART.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSESTD.CPY.
           COPY GABSEKON.CPY.
           COPY GABSEDRU.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
       COPY GABARTIK.CPY.
       COPY GABKONST.CPY.
       COPY GABANBOT.CPY.
       COPY GABSTDFD.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(132).
       01  DRB-SATZ.
           03  FILLER                  PIC XX.
           03  DRB-NUM                 PIC ZZZ.ZZ9-.
           03  DRB-DATUM               PIC X(9).
           03  DRB-BEZ                 PIC X(73).
       01  DRS-SATZ.
           03  FILLER                  PIC X(6).
           03  DRS-STR.
               05 DRS-AUFNUM           PIC ZZZ.ZZ9-.
               05 DRS-RENR             PIC ZZZ.ZZ9-.
               05 DRS-DATUM            PIC X(8).
               05 FILLER               PIC XX.
               05 DRS-BEZ              PIC X(50).
               05 DRS-STD              PIC ZZZZ.ZZ9,99-.
       01  DRV-SATZ.
           03  FILLER                  PIC X(15).
           03  DRV-GRP                 PIC Z9.
           03  FILLER                  PIC X(5).
           03  DRV-BET                 PIC ZZ.ZZZ.ZZ9,99-.
           03  DRV-PROZ                PIC ZZZZ9,9-.
       01  DRW-SATZ.
           03  DRW-ARNUM               PIC Z9.9999.
           03  DRW-FX                  PIC X.
           03  DRW-BEZA                PIC X(30).
           03  DRW-BEZB                PIC X(25).
           03  DRW-PREIS               PIC ZZZ.ZZ9,99-.
           03  DRW-RAB                 PIC Z9-.
           03  DRW-ZRAB                PIC Z9-.
           03  DRW-AUF                 PIC ZZ9-.
           03  DRW-VKP                 PIC ZZZ.ZZ9,99.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WA-TAGE                 PIC 9(8)      COMP.
           03  WH-P                    PIC 99        COMP.
           03  WH-PX                   PIC XX        OCCURS 2.
           03  WK-SEITE                PIC ZZ9-.
           03  WM-OPEN                 PIC 9        COMP  VALUE ZERO.
           03  WZ-SEITE                PIC 99       COMP  VALUE ZERO.
           03  WZ-SCHALT               PIC 99       COMP  VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP  VALUE ZERO.
      *--------------------------> fuer Datumsaufbereitung im DATUM <--
           03  WH-MOD                  PIC 99       COMP.
           03  WH-VAR                  PIC 99       COMP.
           03  WH-RAB                  PIC S99V9    COMP.
           03  WK-RAB                  PIC S99V9    COMP.
           03  WV-NUM                  PIC 9(3)     COMP.
           03  WV-ARNUM                PIC 9(6)     COMP.
           03  WH-ARNUM                PIC 9(5)     COMP.
           03  WM-ARNUM                PIC 9(5)     COMP.
           03  WD-RAB                  PIC Z9-.
           03  WD-UST                  PIC Z9,99-.
           03  WD-ART                  PIC ZZ.9999.
           03  WH-PREIS                PIC 9(5)V999 COMP.
           03  WS-BET                  PIC 9(9)V99  COMP.
           03  WS-STD                  PIC 9(5)V99  COMP.
           03  WH-KB                   PIC 99       COMP.
           03  WX-PRNO                 PIC 99       COMP-X.
           03  WX-PRSTAT               PIC 99       COMP-X.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-J SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 10 PERFORM PR-WART GO X.
           IF WL-CA = 20 PERFORM LI-WAHL GO X.
           IF WL-CA = 30 PERFORM VK-LIST GO W.
           IF WL-CA = 80 PERFORM GAB-LIS GO W.
           GO Z.
       W.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY "weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
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
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) not = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              PERFORM WEITER GO A.
       Z.  EXIT.
      ******************************************************************
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ AFTER PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           PERFORM DRU-OK.
           IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER
           else OPEN EXTEND DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           IF WM-DRU = 1 GO X.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      *****************************************************************
       KB-STAT SECTION.
       A.  CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 0 GO Z.
       C.  DISPLAY "Programm unterbrochen!" WITH HIGHLIGHT AT 2401
              "<esc>= Abbruch, <ret>= Fortsetzung " AT 2424.
           CALL "CAUP" USING "0024591000" WH-CREG.
           PERFORM UNTER.
           IF NOT RET AND NOT ESC GO C.
       Z.  EXIT.
      ****************************************** Text 'Unterbrechnung' *
       UNTER SECTION.
       A.  DISPLAY "Unterbrechung mîglich" AT 2301.
       Z.  EXIT.
      ******************************************************************
       LI-WAHL SECTION.
       A.  CALL "CAUP" USING "0706201045000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY  " Auswahl " with highlight AT VDU-LP.
       C.  ADD 306 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Artikellistung" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "2 - Stundenlistung" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "3 - Stundenlîschung" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "4 - Auftragslîschung" AT VDU-LP.
           ADD 821 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte wÑhlen Sie:" AT VDU-LP.
           CALL "CAUP" USING "1008391001" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1 PERFORM AR-LIST
               WHEN 2
               WHEN 3 PERFORM STD-LIST
               WHEN 4 PERFORM AUF-LOE
               WHEN 5 PERFORM LOE-AK-DEB
               WHEN OTHER GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       LOE-AK-DEB SECTION.
       A.  IF WM-DATUM NOT = 20030106 GO Z.
           OPEN I-O AUFKOPF.
           MOVE LOW-VALUES TO DE-SATZ AK-SATZ.
           START DEBITOR KEY > DE-KEY INVALID STOP RUN.
           START AUFKOPF KEY > AK-KEY INVALID STOP RUN.
       C.  READ AUFKOPF NEXT AT END GO G.
           DISPLAY AK-NUM AT 2020.
           MOVE 0 TO AK-UST.
           REWRITE AK-SATZ.
           GO C.
       G.  READ DEBITOR NEXT AT END GO X.
           DISPLAY DE-KTONR AT 2120.
           MOVE 0 TO DE-UST.
           MOVE SPACE TO DE-UID.
           REWRITE DE-SATZ.
           GO G.
       X.  CLOSE AUFKOPF.
       Z.  EXIT.
      ******************************************************************
       STD-LIST SECTION.
       A.  OPEN I-O STUNDEN.
           OPEN INPUT AUFKOPF.
           MOVE 0 TO WM-OPEN WZ-ZEILEN WZ-SEITE WH-MOD
           CALL "CAUP" USING "0707100460000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Stundenlistung " with highlight AT VDU-LP.
           IF WH-PG = 3 DISPLAY " Stundenlistung " with highlight
               AT VDU-LP " mit Lîschung " with highlight
                   foreground-color 4.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "vom:           bis:          " AT VDU-LP.
           CALL "CAUP" USING "1102086006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WX-DATUM = 0 GO C.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       E.  CALL "CAUP" USING "1102236006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WX-DATUM = 0 GO C.
           MOVE WX-DATUM TO WS-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WS-DATUM < WV-DATUM GO C.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET GO G.
           MOVE 97 TO AR-RAGRP.
           MOVE 0 TO AR-NUM WS-STD.
           START ARTIKEL KEY > AR-CKEY INVALID KEY GO X.
       I.  READ ARTIKEL NEXT IGNORE LOCK AT END GO X.
           MOVE AR-NUM TO GS-NUM.
           MOVE 0 TO GS-DATUM GS-AUF.
           MOVE GS-NUM TO WV-NUM.
           START STUNDEN KEY not < GS-AKEY INVALID GO I.
       K.  READ STUNDEN NEXT IGNORE LOCK AT END PERFORM MANSUM GO I.
           IF GS-NUM not = WV-NUM PERFORM MANSUM GO I.
           PERFORM GS-KO.
           MOVE GS-AUF TO DRS-AUFNUM AK-NUM.
           MOVE GS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-DATUM.
           READ AUFKOPF IGNORE LOCK INVALID GO K.
           MOVE AK-BEZ TO DRS-BEZ.
           INSPECT DRS-BEZ REPLACING ALL "#" BY ",".
           MOVE AK-RENUM TO DRS-RENR.
           ADD GS-ANZ TO WS-STD.
           MOVE GS-ANZ TO DRS-STD.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY DRS-STR(1:50) AT VDU-LP.
           PERFORM DRUCK.
           IF WH-PG = 3 DELETE STUNDEN INVALID NEXT SENTENCE.
           GO K.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE STUNDEN.
           CLOSE AUFKOPF.
           PERFORM END-DRU.
       Z.  EXIT.
      ********************************************** Summe je Pers.Nr. *
       MANSUM SECTION.
       A.  MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           MOVE "Summe" TO DRS-STR(40:).
           MOVE WS-STD TO DRS-STD.
           PERFORM DRUCK.
           MOVE 70 TO WZ-ZEILEN.
       Z.  EXIT.
      ********************************************** Kopf Artikelliste *
       GS-KO SECTION.
       A.  IF WZ-ZEILEN > 69 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 59 MOVE "öbertrag" TO DRS-BEZ(40:)
                             MOVE WS-STD TO DRS-STD
                             PERFORM DRUCK
                             WRITE DRA-SATZ AFTER PAGE
                             MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 0 MOVE x"0100" TO WH-PX(1)
               MOVE x"000C" TO WH-PX(2)
               MOVE WE-WO(5) TO WM-WO
               MOVE WE-DRU(5) TO WM-DRU
               MOVE "LPT1" TO WH-DRUNAM
               IF WM-DRU = 1 or WM-WO = 0
                   MOVE "STUNDEN.LST" TO WH-DRUNAM;
                       IF WM-DRU = 1 MOVE 5 TO WH-P
                   end-if
               end-if
               PERFORM BEG-DRU.
           MOVE "S t u n d e n l i s t e  vom: " TO DRS-STR.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR(31:).
           MOVE "bis:" TO DRS-STR(41:).
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR(46:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
           MOVE "per:" TO DRS-STR(68:5).
           MOVE VDU-DATUM TO DRS-STR(68:8).
           MOVE "Seite: " TO DRS-STR(78:6).
           MOVE WK-SEITE TO DRS-STR(85:4).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           MOVE "Auftrag Rechng.  L-Datum  Bezeichnung:
      -        "                         Stunden " TO DRS-STR.
           MOVE AR-BEZA TO DRS-STR(40:30).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           IF WS-STD not = 0 MOVE "öbertrag" TO DRS-BEZ(40:)
                             MOVE WS-STD TO DRS-STD
                             PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       AUF-LOE SECTION.
       A.  OPEN I-O AUFKOPF AUFTRAG.
           CALL "CAUP" USING "0707100460000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lîschung Auftragsscheine " with highlight
               AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bis inklusive: " AT VDU-LP.
           COMPUTE WX-DATUM = WH-DATUM + 20000000.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WX-DATUM).
           ADD -366 TO WA-TAGE.
           COMPUTE WA-TAGE = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE WA-TAGE TO WC-DATUM WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WV-DATUM.
       B.  MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 218 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE WC-DATUM TO WH-WERT WZ-DATUM WV-DATUM.
           CALL "CAUP" USING "1102186006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO B.
           MOVE WZ-DATUM TO WV-DATUM WC-DATUM WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, < >= zurÅck, <Entf>= Start < >"
              AT 2301.
           CALL "CAUP" USING "0023450000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO B.
           IF not ENTF GO C.
010212     INITIALIZE AK-SATZ.
010212*    MOVE 4 TO AK-ART.
010212     START AUFKOPF KEY NOT < AK-AUFART INVALID GO X.
           START AUFKOPF KEY NOT < AK-NUM INVALID GO X.
       E.  READ AUFKOPF NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY AK-BEZ(1:50) AT VDU-LP.
           IF AK-DATUM > WC-DATUM GO E.
           MOVE AK-NUM TO AU-NUM.
           MOVE 0 TO AU-KAP AU-POS AU-TZ.
           START AUFTRAG KEY NOT < AU-KEY INVALID GO I.
       G.  READ AUFTRAG NEXT AT END GO I.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF AU-NUM not = AK-NUM GO I.
           DELETE AUFTRAG INVALID NEXT SENTENCE.
           GO G.
       I.  DELETE AUFKOPF INVALID NEXT SENTENCE.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE AUFKOPF AUFTRAG.
       Z.  EXIT.
      ************************************************* Artikelkatalog *
       AR-LIST SECTION.
       A.  CALL "CAUP" USING "0707100460000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikellistung " with highlight AT VDU-LP.
           PERFORM VONBIS.
           IF ESC GO Z.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start" AT 2301.
           CALL "CAUP" USING "0023301000" WH-CREG.
           IF ESC GO Z.
           IF NOT RETA GO G.
           PERFORM UNTER.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO Z.
       I.  READ ARTIKEL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE AR-NUM TO WD-ART.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ART AT VDU-LP " " AR-BEZA.
           PERFORM KB-STAT.
           IF ESC GO W.
           IF WS-TASTE = 5; IF AR-KAGRP = 2 and ar-ekp > ar-vkp
               display ar-ekp at 1210 ar-vkp 1230
               accept wx
               end-if GO I.
           IF AR-NUM > WV-ARNUM GO W.
           IF WM-ARNUM = 0 DIVIDE 1000 INTO AR-NUM GIVING WM-ARNUM.
           DIVIDE 1000 INTO AR-NUM GIVING WH-ARNUM.
           IF WH-ARNUM NOT = WM-ARNUM MOVE 88 TO WZ-ZEILEN
               MOVE 0 TO WM-ARNUM.
           PERFORM AR-KO.
           GO I.
       W.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       VONBIS SECTION.
       A.  CALL "CAUP" USING "168CLRFEN" WH-CREG.
           MOVE 0 TO WZ-ZEILEN WM-ARNUM WZ-SEITE.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Artikel:           bis Artikel:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Artikelnummer" AT 2301.
       C.  CALL "CAUP" USING "1002156006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO C.
           IF WH-NUM = 0 DISPLAY "ab Anfang" with highlight AT VDU-LP
                ELSE MOVE WH-NUM TO WD-ART
                     DISPLAY WD-ART with highlight AT VDU-LP.
           MOVE WH-NUM TO AR-NUM.
       E.  DISPLAY "<esc/>= zurÅck" AT 2301.
           CALL "CAUP" USING "1002386006" WH-CREG.
           IF ESC GO A.
           IF NOT RET GO E.
           IF WH-NUM = 0 DISPLAY "bis Ende" with highlight AT VDU-LP
                MOVE 999999 TO WH-NUM WV-ARNUM
                ELSE MOVE WH-NUM TO WD-ART WV-ARNUM
                     DISPLAY WD-ART with highlight AT VDU-LP.
           IF WV-ARNUM < AR-NUM DISPLAY "Unmîglich!" AT 2401
                PERFORM WEITER GO E.
       Z.  EXIT.
      ********************************************* Kopf Artikelliste *
       AR-KO SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO W.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-WO(5) TO WM-WO.
           MOVE WE-DRU(5) TO WM-DRU.
           MOVE "LPT1" TO WH-DRUNAM.
           IF WM-DRU = 1 or WM-WO = 0 MOVE "ARTIKLI.LST" TO WH-DRUNAM;
               IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "A r t i k e l l i s t e" TO DRA-SATZ.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
           MOVE "per:" TO DRA-SATZ(68:5).
           MOVE VDU-DATUM TO DRA-SATZ(68:8).
           MOVE "Seite: " TO DRA-SATZ(78:6).
           MOVE WK-SEITE TO DRA-SATZ(85:4).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRW-SATZ PERFORM DRUCK.
           MOVE "Art-Nr. Bezeichnung
      -        "             Preis Rab.ZR.Auf   Verkauf" TO DRW-SATZ.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRW-SATZ PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       W.  MOVE AR-NUM TO DRW-ARNUM.
           MOVE AR-BEZA TO DRW-BEZA.
           MOVE AR-BEZB TO DRW-BEZB.
           MOVE AR-PREIS TO DRW-PREIS.
           MOVE AR-RAB TO DRW-RAB.
           IF AR-ZRAB NOT = 0 MOVE AR-ZRAB TO DRW-ZRAB.
           IF AR-AUF NOT = 0 MOVE AR-AUF TO DRW-AUF.
           IF AR-FIXPR NOT = 0 MOVE "*" TO DRW-FX
           MOVE AR-VKP TO DRW-VKP.
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************************* Artikelkatalog *
       PR-WART SECTION.
       A.  MOVE " Preiswartung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "270818073000012" WH-CREG.
           DISPLAY "1 - Preiswartung" AT 1021.
           DISPLAY "< >" AT 1140.
           DISPLAY "2 - Rabattwartung" AT 1221.
       B.  DISPLAY "<ret-leer/esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0011411001" WH-CREG.
           IF ESC OR WH-NUM = 0 GO Z.
           IF not RET GO B.
           IF WH-NUM > 2 GO B.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CALL "CAUP" USING "0707100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE WH-NUM TO WH-MOD.
           EVALUATE WH-MOD
               WHEN 1 DISPLAY " Preiswartung " with highlight AT VDU-LP
               WHEN 2 DISPLAY " Rabattwartung" with highlight AT VDU-LP.
           PERFORM VONBIS.
           IF ESC GO Z.
           IF WH-MOD = 2 GO D.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Erhîhungsprozentsatz: " AT VDU-LP.
           DISPLAY "<tab>= Rabatt, <ret>= Preiserhîhung" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1003252205" WH-CREG.
           MOVE WH-WERT TO WD-UST WH-PREIS.
           DISPLAY WD-UST  with highlight AT VDU-LP.
           IF WH-WERT = 0 OR > 15,0 GO C.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
                GO A.
           IF NOT RET GO C.
           GO G.
      *-------------------------------------------> bei Rabattwartung <-
       D.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "neuer Rabattsatz: " AT VDU-LP.
           DISPLAY "<ret>= Rabatt, <esc>= Abbruch" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1003212002" WH-CREG.
           MULTIPLY -1 BY WH-WERT GIVING WH-RAB.
           MOVE WH-RAB TO WD-RAB.
           DISPLAY WD-RAB with highlight AT VDU-LP.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
                GO A.
           IF WH-WERT = 0 or not RET GO D.
       E.  ADD 330 VDU-ECK GIVING VDU-LP.
           DISPLAY "neuer zus. Rabatt: " AT VDU-LP.
           DISPLAY "<ret>= Rabatt, <esc>= Abbruch" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1003492002" WH-CREG.
           MULTIPLY -1 BY WH-WERT GIVING WK-RAB.
           MOVE WK-RAB TO WD-RAB.
           DISPLAY WD-RAB with highlight AT VDU-LP.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
                GO A.
           IF not RET GO E.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start" AT 2301.
           DISPLAY "<ret>= alles, < >= nur ohne, < >= nur mit Aufschlag"
                with highlight AT 2401.
           CALL "CAUP" USING "0023301000" WH-CREG.
           IF ESC GO Z.
           MOVE 0 TO WH-VAR.
           ADD 003 VDU-ECK GIVING VDU-LP.
           IF AUF MOVE 1 TO WH-VAR SET RET TO TRUE
                DISPLAY "nur ohne Auschlag" with highlight AT VDU-LP
           IF AB MOVE 2 TO WH-VAR SET RET TO TRUE
                DISPLAY "nur mit Auschlag" with highlight AT VDU-LP.
           IF NOT RET GO G.
           PERFORM UNTER.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO Z.
       I.  READ ARTIKEL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE AR-NUM TO WD-ART.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ART with highlight AT VDU-LP " "
               AR-BEZA with highlight foreground-color 6.
           PERFORM KB-STAT.
           IF ESC GO W.
           IF AR-NUM > WV-ARNUM GO W.
           EVALUATE WH-VAR
               WHEN 1 IF AR-AUF not = 0 GO I
               WHEN 2 IF AR-AUF = 0 GO I.
060103*    IF AR-AUF NOT = 0 GO I.             *> nicht mehr gewÅnscht!
           PERFORM AR-KO.
           SET PY TO 4.
       J.  MOVE AR-PRTAB(PY) TO AR-PRTAB(PY + 1).
           IF PY > 1 SET PY DOWN BY 1 GO J.
           MOVE AR-LIEF TO AR-ALIEF(1).
           MOVE AR-DATUM TO AR-ADAT(1).
           MOVE AR-RAB TO AR-ARAB(1).
           MOVE AR-ZRAB TO AR-AZRAB(1).
           MOVE AR-AUF TO AR-AAUF(1).
           MOVE 4 TO AR-SYM(1).
           MOVE AR-PREIS TO AR-APREIS(1).
           MOVE WH-DATUM TO AR-DATUM.
           IF WH-MOD = 2 MOVE 5 TO AR-SYM(1)
               MOVE WH-RAB TO AR-RAB
               MOVE WK-RAB TO AR-ZRAB GO K.
           COMPUTE AR-PREIS = AR-PREIS + (AR-PREIS * WH-PREIS / 100).
           MOVE AR-PREIS TO AR-VKP AR-EKP.
       K.  IF AR-RAB NOT = 0 COMPUTE WH-WERT = AR-PREIS * AR-RAB / 100
               ADD WH-WERT AR-PREIS GIVING AR-EKP.
           IF AR-ZRAB NOT = 0 COMPUTE WH-WERT = AR-EKP * AR-ZRAB / 100
               ADD WH-WERT TO AR-EKP.
           DIVIDE 10 INTO AR-VKP GIVING AR-VKP ROUNDED.
           MULTIPLY 10 BY AR-VKP.
           MOVE AR-PREIS TO DRW-PREIS.
           MOVE AR-RAB TO DRW-RAB.
           IF AR-ZRAB NOT = 0 MOVE AR-ZRAB TO DRW-ZRAB.
           IF AR-AUF NOT = 0 MOVE AR-AUF TO DRW-AUF
               COMPUTE AR-VKP = AR-PREIS + (AR-PREIS * AR-AUF / 100).
           IF AR-FIXPR NOT = 0 MOVE "*" TO DRW-FX.
           MOVE AR-VKP TO DRW-VKP.
           PERFORM DRUCK.
           REWRITE AR-SATZ.
           GO I.
       W.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
       Z.  EXIT.
      ******************************************** Listung Vkf-UmsÑtze *
       VK-LIST SECTION.
       A.  CALL "CAUP" USING "0710100540000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " VerkÑuferliste " WITH REVERSE-VIDEO
               FOREGROUND-COLOR is 3 AT VDU-LP.
       B.  DISPLAY "<esc>= Abbruch, <ret>= Vkf.-Nr., <ret-leer>= Start '
      -        "alle'" AT 2301.
           DISPLAY "< >= Hilfe, <#>= 'alle' mit lîschen" AT 2401.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Vkf.Nr.:                        " AT VDU-LP.
           CALL "CAUP" USING "1003122002" WH-CREG.
           IF HELP CALL "CAUP" USING "190019002001" WH-CREG GO B.
           IF ESC GO Z.
           IF KIST GO G.
           IF NOT RET GO B.
           IF WH-NUM = 0 GO G.
           ADD WH-NUM 11 GIVING WH-KEY.
       E.  READ KONSTANT INVALID PERFORM NO-REC GO B.
           IF ZUGRIF PERFORM BESETZT GO E.
           PERFORM VK-DRU.
           GO B.
       G.  MOVE 11 TO WH-KEY.
           PERFORM UNTER.
           MOVE "VKFLIST.LST" TO WH-DRUNAM.
           START KONSTANT KEY > WH-KEY INVALID KEY PERFORM NO-REC GO Z.
       H.  READ KONSTANT NEXT AT END GO B.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF WH-KEY > 40 GO B.
           PERFORM KB-STAT.
           IF ESC GO B.
           PERFORM VK-DRU.
           REWRITE KO-SATZ.
           GO H.
       Z.  EXIT.
      ******************************************************************
       VK-DRU SECTION.
       A.  MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE "VKFLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 6 TO WZ-SCHALT.
           MOVE "VerkÑuferumsÑtze                per:" TO DRA-SATZ(8:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(45:8).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-SATZ(8:45).
           PERFORM DRUCK.
           MOVE KO-NAME TO DRA-SATZ(8:).
           MOVE KO-NUM TO WD-RAB.
           MOVE WD-RAB TO DRA-SATZ(36:2).
           ADD VDU-ECK 314 GIVING VDU-LP.
           DISPLAY KO-NAME WITH HIGHLIGHT AT VDU-LP.
           MOVE 4 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           MOVE "Grp.:" TO DRV-SATZ(9:5).
       D.  SET WX TO 1.
           MOVE 0 TO WS-BET.
       E.  MOVE WX TO DRV-GRP.
           MOVE KO-UMSATZ(WX) TO DRV-BET.
           PERFORM DRUCK.
           ADD KO-UMSATZ(WX) TO WS-BET.
           IF WS-TASTE = 6 MOVE 0 TO KO-UMSATZ(WX).
           IF WX < 15 ADD 1 TO WX GO E.
           MOVE "**" TO DRV-SATZ(16:2).
           MOVE WS-BET TO DRV-BET.
           PERFORM DRUCK.
           SUBTRACT KO-UMSATZ(1) FROM WS-BET.
           ADD WS-BET KO-UMSATZ(17) GIVING DRV-BET.
           ADD KO-UMSATZ(17) WS-BET GIVING WH-WERT.
           COMPUTE DRV-PROZ = WS-BET / WH-WERT * 100.
           MOVE 1 TO WX.
       G.  IF WS-TASTE = 6 MOVE 0 TO KO-UMSATZ(WX);
               IF WX < 20 SET WX UP BY 1 GO G.
           PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      *****************************************************************
       GAB-LIS SECTION.
       A.  CALL "CAUP" USING "0710300533000" WH-CREG.
           ADD 304 VDU-ECK GIVING VDU-LP.
           MOVE "AUFTRAG.LST" TO WH-DRUNAM.
           DISPLAY "Auftragsliste bis:" AT VDU-LP.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 323 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       B.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE WH-DATUM TO WH-WERT WZ-DATUM.
           CALL "CAUP" USING "1103236006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO B.
           MOVE WZ-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD VDU-ECK 414 GIVING VDU-LP.
           MOVE 0 TO AK-NUM.
           START AUFKOPF KEY > AK-NUM INVALID KEY GO Z.
       D.  READ AUFKOPF NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF AK-DATUM > WV-DATUM GO D.
           IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO E.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
           MOVE "Offene AuftrÑge bis:                       per:"
               TO DRA-SATZ(2:).
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(23:8).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(50:8).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-SATZ(2:56).
           PERFORM DRUCK.
       E.  MOVE AK-NUM TO DRB-NUM.
           MOVE AK-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRB-DATUM.
           INSPECT AK-BEZ REPLACING ALL "#" BY ",".
           MOVE AK-BEZ TO DRB-BEZ.
           DISPLAY DRB-NUM with highlight AT VDU-LP.
           IF AK-RENUM NOT = 0 MOVE "*" TO DRB-SATZ(10:1).
           PERFORM DRUCK.
           GO D.
       W.  PERFORM END-DRU.
       Z.  EXIT.
