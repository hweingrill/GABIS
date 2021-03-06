      $SET LINKCOUNT"128" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GABANZ.
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  P3800.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
           COPY GABSEART.CPY.
           COPY GABSEDEB.CPY.
           COPY GABSESTA.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSEATX.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY GABARTIK.CPY.
       COPY GABDEBIT.CPY.
       COPY GABSTATI.CPY.
       COPY GABANBOT.CPY.
      ***************************************************** Textdatei *
       FD  ANTEXT       EXTERNAL       LABEL RECORD STANDARD.
       01  TE-SATZ                     PIC X(60).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 15.
           03  WH-END                  PIC 99   COMP.
               88  LOW-END VALUE 0.    88  HIGH-END VALUE 99.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WB-LP                   PIC 9999.
           03  WH-L                    PIC 99.
           03  WI                      PIC 99       COMP.
           03  WV-TXKEY                PIC 9(5).
           03  WS-MENGEN               PIC S9(9)  OCCURS 2  COMP-3.
           03  WH-MEH                  PIC 99       COMP.
           03  WH-RAB                  PIC S999V9   COMP-3.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WD-MGA                  PIC ZZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZZ9,9-.
           03  WD-MGC                  PIC ZZZ9,99-.
           03  WD-MG                   PIC X(8).
           03  WD-JAHR                 PIC 9999.
           03  WD-PZ                   PIC ZZ9-.
           03  WD-POS                  PIC Z9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ARNUM                PIC Z9.9999.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "DEBITORE.FII" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING "GABISTAT.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON ANTEXT.
       A.  CALL "CADECL" USING WH-TXKEY       WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON AUFTRAG.
       A.  CALL "CADECL" USING "GABISAUF.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
      *    CALL X"B0" USING WU-FUNC WU-KEYTAB.
           IF WL-CA = 01 PERFORM DEB-ANZ GO W.
           IF WL-CA = 02 PERFORM ART-ANZ GO W.
           IF WL-CA = 10 PERFORM STA-ANZ GO W.
           IF WL-CA = 15 PERFORM PREIANZ GO X.
           IF WL-CA = 20 PERFORM TEXTANZ GO W.
           IF WL-CA = 30 PERFORM AUFSUCH GO W.
           IF WL-CA = 31 PERFORM AUFSUCH GO W.
           GO Z.
       W.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
           IF NOT RET GO A.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, <ret-leer>/< >= weiterlisten,
      -        "< >= zur�cklisten" AT 2301.
           IF FINE DISPLAY "***** keine Daten mehr *****"
               with highlight AT 2450.
           DISPLAY "<esc> oder <#>= Ende" AT 2401.
           CALL "CAUP" USING "0023711001" WH-CREG.
           MOVE 1 TO WR.
           IF ESC OR KIST MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 0; IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO A.
       Z.  EXIT.
      ******************************************************************
       ALLANZ SECTION.
       A.  ADD 202 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS WITH HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
       Z.  EXIT.
      *****************************************************************
       ART-ANZ SECTION.
       A.  CALL "CAUP" USING "0702151162001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Artikel " WITH HIGHLIGHT AT VDU-REL.
           PERFORM ALLANZ.
           START ARTIKEL KEY NOT < AR-AKEY INVALID PERFORM NO-REC
                GO Z.
       E.  READ ARTIKEL NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF AR-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM AR-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO AR-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO AR-KEY GO Z.
           IF AB GO E.
       N.  READ ARTIKEL PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF AR-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM AR-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       AR-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           DISPLAY AR-AKEY AT VDU-REL.
           ADD 16 TO VDU-REL.
           DISPLAY AR-BEZA WITH HIGHLIGHT AT VDU-REL.
           ADD 32 TO VDU-REL.
           MOVE AR-NUM TO WD-ARNUM.
           DISPLAY WD-ARNUM AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE AR-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *****************************************************************
       DEB-ANZ SECTION.
       A.  CALL "CAUP" USING "0703011180001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Kunden " WITH HIGHLIGHT AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           START DEBITOR KEY NOT < DE-MCODE INVALID PERFORM NO-REC GO Z.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF DE-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM DEB-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO DE-KTONR GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO DE-KEY
               MOVE DE-KTONR TO WH-WERT WH-NUM GO Z.
           IF AB GO E.
       N.  READ DEBITOR PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF DE-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM DEB-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       DEB-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           ADD 10 TO VDU-REL.
           DISPLAY WT-TX WITH SIZE 53 HIGHLIGHT AT VDU-REL.
           ADD 55 TO VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE DE-KEY TO WT-KEY(WX).
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF WT-NK(WH-MEH) = 2 DIVIDE 100 INTO WH-ANZ GIVING WD-MGC
               MOVE WD-MGC TO WD-MG.
           IF WT-NK(WH-MEH) = 1 DIVIDE 10 INTO WH-ANZ GIVING WD-MGB
               MOVE WD-MGB TO WD-MG.
           IF WT-NK(WH-MEH) = 0 MOVE WH-ANZ TO WD-MGA
               MOVE WD-MGA TO WD-MG.
       Z.  EXIT.
      ******************************************************************
       STA-ANZ SECTION.
       A.  CALL "CAUP" USING "0701531826020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Umsatzstatistik " WITH HIGHLIGHT AT VDU-REL.
           ADD 203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Mo" AT VDU-REL.
           ADD 208 VDU-ECK GIVING VDU-REL.
           DISPLAY "Mengen" AT VDU-REL.
           ADD 217 VDU-ECK GIVING VDU-REL.
           DISPLAY "VJ-Mengen" AT VDU-REL.
           ADD 403 VDU-ECK GIVING VDU-REL.
           ADD AR-MEH 1 GIVING WH-MEH.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 12
               MOVE WI TO WD-POS
               DISPLAY WD-POS WITH HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
           ADD 1703 VDU-ECK GIVING VDU-REL
           DISPLAY "Su" AT VDU-REL.
           MOVE WH-DATUM TO WZ-DATUM.
           SUBTRACT 1 FROM WZ-JAHR GIVING WC-JAHR.
           MOVE AR-NUM TO ST-ARNUM.
       D.  READ STATISTIK IGNORE LOCK INVALID INITIALIZE ST-SATZ GO H.
           IF ST-JAHR = WZ-JAHR GO H.
           IF ST-JAHR > WZ-JAHR GO H.
           SET SY TO 1.
       E.  MOVE ST-MENGEN(SY) TO ST-MENGEN(SY + 12).
           MOVE 0 TO ST-MENGEN(SY).
           IF SY < 12 SET SY UP BY 1 GO E.
       H.  PERFORM STA-DIS.
           DISPLAY "weiter mit <ret/tab>" with highlight AT 2401.
           CALL "CAUP" USING "0024220000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       STA-DIS SECTION.
       A.  MOVE 0 TO WS-MENGEN(1) WS-MENGEN(2).
           ADD 302 VDU-ECK GIVING VDU-REL.
           DISPLAY "������������������������" AT VDU-REL.
           ADD 309 VDU-ECK GIVING VDU-REL.
           ADD ST-JAHR 1900 GIVING WD-JAHR.
           DISPLAY " (" AT VDU-REL WD-JAHR WITH HIGHLIGHT ") ".
           ADD 1602 VDU-ECK GIVING VDU-REL.
           DISPLAY "������������������������" AT VDU-REL.
           ADD 408 VDU-ECK GIVING VDU-REL.
           SET SY TO 1.
       E.  IF ST-MENGEN(SY) NOT = 0 MOVE ST-MENGEN(SY) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MG AT VDU-REL
           ELSE DISPLAY "    -" AT VDU-REL.
           ADD 9 TO VDU-REL.
           IF ST-MENGEN(SY + 12) NOT = 0
               MOVE ST-MENGEN(SY + 12) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MG AT VDU-REL
           ELSE DISPLAY "    -" AT VDU-REL.
           SUBTRACT 9 FROM VDU-REL.
           ADD ST-MENGEN(SY) TO WS-MENGEN(1).
           ADD ST-MENGEN(SY + 12) TO WS-MENGEN(2).
           IF SY < 12 SET SY UP BY 1
               ADD 100 TO VDU-REL GO E.
           ADD 200 TO VDU-REL.
           MOVE WS-MENGEN(1) TO WH-ANZ.
           PERFORM MG.
           DISPLAY WD-MG AT VDU-REL.
           ADD 9 TO VDU-REL.
           MOVE WS-MENGEN(2) TO WH-ANZ.
           PERFORM MG.
           DISPLAY WD-MG AT VDU-REL.
       Z.  EXIT.
      ************************************************ anzeigen Preise *
       PREIANZ SECTION.
       A.  CALL "CAUP" USING "0716010760002" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Preiskartei " WITH HIGHLIGHT AT VDU-REL.
           SET PY TO 1.
           ADD 102 VDU-ECK GIVING VDU-REL.
       C.  IF AR-ADAT(PY) = 0 GO D.
           ADD 100 TO VDU-REL.
           MOVE AR-ADAT(PY) TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           DISPLAY VDU-DATUM AT VDU-REL.
           ADD 9 TO VDU-REL.
           DISPLAY AR-ALIEF(PY) WITH HIGHLIGHT AT VDU-REL.
           ADD 7 TO VDU-REL.
           MOVE AR-APREIS(PY) TO WD-PREIS WH-ANZ.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-REL.
           MOVE AR-ARAB(PY) TO WD-PZ.
           ADD 11 TO VDU-REL.
           DISPLAY WD-PZ WITH HIGHLIGHT AT VDU-REL.
           MOVE AR-AZRAB(PY) TO WD-PZ.
           ADD 4 TO VDU-REL.
           DISPLAY WD-PZ WITH HIGHLIGHT AT VDU-REL.
           ADD 6 TO VDU-REL.
           IF AR-ARAB(PY) NOT = 0 MOVE AR-ARAB(PY) TO WH-RAB
               COMPUTE WH-ANZ = WH-ANZ + (WH-RAB * WH-ANZ / 100).
           IF AR-AZRAB(PY) NOT = 0 MOVE AR-AZRAB(PY) TO WH-RAB
               COMPUTE WH-ANZ = WH-ANZ + (WH-RAB * WH-ANZ / 100).
           MOVE WH-ANZ TO WD-PREIS.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-REL.
           ADD 13 TO VDU-REL.
           IF AR-AAUF(PY) NOT = 0 MOVE AR-AAUF(PY) TO WD-PZ
               DISPLAY WD-PZ WITH HIGHLIGHT AT VDU-REL
           ELSE DISPLAY "  -" AT VDU-REL.
           ADD 4 TO VDU-REL.
           IF AR-SYM(PY) = 2 DISPLAY "of" AT VDU-REL.
           IF AR-SYM(PY) = 3 DISPLAY "�n" AT VDU-REL.
           IF AR-SYM(PY) = 4 DISPLAY "pe" AT VDU-REL.
           IF AR-SYM(PY) = 5 DISPLAY "%e" AT VDU-REL.
           SUBTRACT 54 FROM VDU-REL.
       D.  IF PY < 5 SET PY UP BY 1 GO C.
           MOVE 1 TO WI.
           MOVE VDU-REL TO WB-LP.
           ADD 258 VDU-ECK GIVING VDU-REL.
       E.  MOVE WI TO WD-POS.
           IF AR-ADAT(WI) NOT = 0 DISPLAY WD-POS AT VDU-REL.
           IF WI < 5 ADD 1 TO WI ADD 100 TO VDU-REL GO E.
           MOVE WB-LP TO VDU-REL.
       G.  DISPLAY "Preiswartung: <ret>= Preis-Nr., <ret-leer>= akt. Lie
      -         "f., <#>= zus. Lief. < >" AT 2301.
           DISPLAY "<esc> = Abbruch, <tab>= Statistik, <>= �nderung, < 
      -         ">= Hilfe" AT 2401.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "0023741001" WH-CREG.
           IF HELP CALL "CAUP" USING "190007001000" WH-CREG GO G.
           IF WH-NUM > 5 GO G.
           IF TABL PERFORM STA-ANZ GO G.
           MOVE 0 TO WX.
           IF RET AND WH-NUM > 0 MOVE WH-NUM TO WX;
               IF AR-ADAT(WX) = 0 GO G.
           IF RET OR KIST OR ESC OR WOLI GO Z.
           GO G.
       Z.  EXIT.
      **************************************** anlegen / �ndern Texte *
       TEXTANZ SECTION.
       A.  CALL "CAUP" USING "0705031464000" WH-CREG.
           MOVE WT-TX TO WH-TXKEY.
           OPEN INPUT ANTEXT.
           IF WF-SB = 13 OPEN I-O ANTEXT.
           CLOSE ANTEXT.
       B.  OPEN I-O ANTEXT.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Angebotstexte " WITH HIGHLIGHT AT VDU-REL.
           ADD 20 TO VDU-REL.
           DISPLAY " " AT VDU-REL WH-TXKEY " ".
           MOVE 0 TO WK-TXKEY WV-TXKEY WH-L.
       D.  MOVE 0 TO WI WX.
      *----------------------------------------------------> anzeigen <-
       E.  ADD 1 TO WX WK-TXKEY.
           READ ANTEXT IGNORE LOCK INVALID INITIALIZE TE-SATZ
                MOVE 10 TO WF-STATUS GO G.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  IF WI NOT = 12 ADD 1 TO WI.
           COMPUTE VDU-LP = 103 + WX * 100 + VDU-ECK.
           DISPLAY TE-SATZ WITH HIGHLIGHT AT VDU-LP.
           IF WI NOT = 12;
                IF WX = 12 CALL "CAUP" USING "17SCROLLF" WH-CREG
                    MOVE 11 TO WX GO E ELSE GO E.
       G.  IF WV-TXKEY NOT = 0 GO Q.
           DISPLAY "< >= auf, <tab>= anzeigen, <esc>= Ende" AT 2301.
           DISPLAY "<#>= in letzter Zeile = Zeilenl�schung" AT 2401.
           MOVE TE-SATZ TO WT-TX.
           COMPUTE VDU-LP = 103 + WX * 100.
           CALL "CAUP" USING "1200000160" WH-CREG.
           IF KIST AND WH-L = 99 GO I.
           IF ESC GO X.
           IF AUF GO L.
           IF RET GO K.
           IF NOT TABL GO G.
           IF FINE CALL "CAUP" USING "16LOEFEN" WH-CREG
               MOVE 0 TO WK-TXKEY GO D.
           MOVE 1 TO WI.
       H.  IF WX = 12 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 11 TO WX.
           GO E.
       I.  MOVE SPACE TO TE-SATZ WT-TX.
           COMPUTE VDU-LP = 103 + WX * 100 + VDU-ECK.
           DISPLAY TE-SATZ WITH HIGHLIGHT AT VDU-LP.
           DELETE ANTEXT INVALID KEY GO G.
           SET FINE TO TRUE.
           GO G.
       K.  MOVE WT-TX TO TE-SATZ.
           COMPUTE VDU-LP = 103 + WX * 100 + VDU-ECK.
           DISPLAY TE-SATZ WITH HIGHLIGHT AT VDU-LP.
           WRITE TE-SATZ INVALID REWRITE TE-SATZ.
           GO H.
       L.  IF WK-TXKEY > 1 SUBTRACT 1 FROM WK-TXKEY WX.
           IF FINE MOVE 99 TO WH-L ELSE MOVE 0 TO WH-L.
           IF WX = 0 MOVE WK-TXKEY TO WV-TXKEY
               SUBTRACT 1 FROM WK-TXKEY GO D.
           GO R.
       Q.  MOVE 1 TO WX.
           MOVE WV-TXKEY TO WK-TXKEY.
           MOVE 0 TO WV-TXKEY.
       R.  READ ANTEXT IGNORE LOCK INVALID GO X.
           GO G.
       X.  CLOSE ANTEXT.
       Z.  EXIT.
      *****************************************************************
       AUFSUCH SECTION.
       A.  CALL "CAUP" USING "0703011180001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Auftr�ge " WITH HIGHLIGHT AT VDU-REL.
           ADD 50 TO VDU-REL.
           IF WL-CA = 30 DISPLAY " Bezeichnung " AT VDU-REL
                    ELSE DISPLAY " Kundennummern " AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO AK-AKEY.
           IF WL-CA = 31 START AUFKOPF KEY NOT < AK-KTONR INVALID GO X.
           IF WL-CA = 30 START AUFKOPF KEY NOT < AK-AKEY INVALID GO X.
       E.  READ AUFKOPF NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF AK-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM AUF-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO AK-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO AK-KEY GO Z.
           IF AB MOVE 1 TO WI GO E.
       N.  READ AUFKOPF PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF AK-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM AUF-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       X.  PERFORM NO-REC.
           MOVE 0 TO AK-NUM.
       Z.  EXIT.
      ******************************************************************
       AUF-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE AK-NUM TO WD-ARNUM.
           IF AK-RENUM NOT = 0 DISPLAY WD-ARNUM AT VDU-REL
           ELSE DISPLAY WD-ARNUM WITH HIGHLIGHT AT VDU-REL.
           DIVIDE 10 INTO AK-KTONR GIVING WD-KTO.
           ADD 8 TO VDU-REL.
           DISPLAY WD-KTO AT VDU-REL.
           MOVE AK-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           ADD 9 TO VDU-REL.
           IF AK-RENUM NOT = 0 DISPLAY WT-TX WITH SIZE 43 AT VDU-REL
           ELSE DISPLAY WT-TX WITH SIZE 43 HIGHLIGHT AT VDU-REL.
           ADD 44 TO VDU-REL.
           MOVE AK-AKEY TO WT-TX.
           CALL "CAUP" USING "21CONV15" WH-CREG.
           DISPLAY WT-TX with highlight SIZE 13 AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE AK-KEY TO WT-KEY(WX).
       Z.  EXIT.
