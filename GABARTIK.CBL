      $SET LINKCOUNT"128" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GABARTIK.
      ******************************************************************
      *               Anlage und énderung Artikeldatei                 *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEART.CPY.
           COPY GABSESTA.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY GABARTIK.CPY.
       COPY GABSTATI.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WH-PRTAB.
               05 WH-LIEF              PIC X(5).
               05 FILLER               PIC X(26).
           03  WV-PRTAB.
               05 WV-LIEF              PIC X(5).
               05 WV-DAT               PIC 9(6)       COMP.
               05 WV-PREIS             PIC S9(7)V99   COMP-3.
               05 WV-RAB               PIC S999V9     COMP-3.
               05 WV-ZRAB              PIC S99V9      COMP-3.
               05 WV-AUF               PIC S999V99    COMP-3.
           03  WM-PRTAB                PIC X(21).
           03  WZ-ECK                  PIC 9999      COMP  OCCURS 2.
           03  WI                      PIC 99        COMP.
           03  WK-TASTE                PIC 99        COMP.
           03  WV-ANUM                 PIC 9(6)      COMP.
           03  WV-BNUM                 PIC 9(6)      COMP.
           03  WV-ARNUM                PIC 9(6)      COMP.
           03  WM-ARNUM                PIC 9(6)      COMP.
           03  WD-UST                  PIC Z9,99-.
           03  WD-PZ                   PIC Z9.
           03  WD-KZ                   PIC 9.
           03  WD-ARNUM                PIC Z9.9999.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WD-APREIS               PIC ZZZ.ZZ9,99-.
           03  WD-GRP                  PIC Z9.
           03  WD-RAB                  PIC ZZ9-.
           03  WD-ZUS                  PIC ZZ-.
           03  WD-MIN                  PIC ZZ9,9-.
           03  WX-GK                   PIC 9999    COMP.
           03  WM-MERKER               PIC 9       COMP   VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WR-P                    PIC 99.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-Z SECTION.         USE AFTER ERROR PROCEDURE ON ZUSATZ.
       A.  CALL "CADECL" USING "GABISARZ.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING "GABISTAT.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 50
               WHEN 51
               WHEN 52 PERFORM ARTIK
               WHEN 54
               WHEN 53 PERFORM PR-WART
               WHEN 55 PERFORM ARTNUM
                       MOVE WH-CREG TO WL-CREG.
       Y.  CANCEL "GABANZ".
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       ARTIK SECTION.
       A.  MOVE 0 TO WM-MERKER WI.
           IF WL-CA = 51 CALL "CAUP" USING "0716010780000" WH-CREG
                    ELSE CALL "CAUP" USING "0708010780000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikelverwaltung " WITH HIGHLIGHT AT VDU-LP.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           IF AR-NUM > 0 AND < 15 MOVE 1 TO WI
                MULTIPLY AR-NUM BY 10000 GIVING AR-NUM.
           PERFORM AR-BILD.
           IF WL-CA = 51 OR WL-CA = 52 GO M.
       C.  DISPLAY "<ret>= Anl./Aend., alpha+<ret>= suchen, <esc>= Ende"
               AT 2301.
           DISPLAY "nÑchster freier Artikel = Nr.+<#>, <>= letzter Arti
      -        "kel" AT 2401.
           ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY "              " AT VDU-LP.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1202120114" WH-CREG.
           IF WOLI MOVE 0 TO WM-MERKER GO P.
           IF ESC MOVE 0 TO AR-NUM GO X.
           PERFORM LIESART
               IF FEHLER GO B.
       M.  MOVE 0 TO WM-MERKER.
           MOVE WH-NUM TO AR-NUM.
       P.  READ ARTIKEL INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO P.
           PERFORM AR-BILD.
           IF NOT ANLAGE PERFORM AR-ANZ.
           MOVE AR-TAB TO WV-PRTAB.
           PERFORM AR-ANL.
           UNLOCK ARTIKEL.
           IF WL-CA NOT = 51 AND WL-CA NOT = 52 GO B.
           IF ESC MOVE 0 TO AR-NUM.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK ARTIKEL.
       Z.  EXIT.
      ****************************************** Artikelumnummerierung *
       ARTNUM SECTION.
       A.  CALL "CAUP" USING "0708090867000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikelnummernverwltg." AT VDU-LP.
       C.  DISPLAY "von Artikel:         bis Artikel:         auf Artike
      -        "l:" AT 1011.
           DISPLAY "<esc>= Abbruch, <tab>= Artikelnr." AT 2301.
           UNLOCK ARTIKEL.
           UNLOCK STATISTIK.
           CALL "CAUP" USING "0010246006" WH-CREG.
           IF ESC GO X.
           IF NOT RETA GO C.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WD-ARNUM WM-ARNUM WH-WERT.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT 1024.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT 1045.
       E.  CALL "CAUP" USING "0010456006" WH-CREG.
           IF WOLI GO C.
           IF ESC GO X.
           IF NOT RETA GO C.
           MOVE WH-WERT TO WD-ARNUM WV-ARNUM.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT 1045.
           IF WM-ARNUM > WV-ARNUM GO E.
       F.  CALL "CAUP" USING "0010666006" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RETA GO F.
           IF WH-NUM = 0 GO F.
           MOVE WH-NUM TO WD-ARNUM AR-NUM.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT 1066.
       G.  DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Start" AT 2301.
           CALL "CAUP" USING "0023430000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE AR-NUM TO WH-NUM.
           COMPUTE WV-ANUM = WV-ARNUM - WM-ARNUM + 1.
           COMPUTE AR-NUM = WH-NUM + WV-ANUM.
           IF AR-NUM > 999999 GO H.
           MOVE WH-NUM TO AR-NUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO I.
           READ ARTIKEL NEXT AT END GO I.
           IF ZUGRIF PERFORM BESETZT GO G.
           COMPUTE WV-BNUM = AR-NUM - WH-NUM.
           IF WV-BNUM NOT < WV-ANUM GO I.
       H.  DISPLAY "kein Platz vorhanden," AT 2401.
           PERFORM WEITER GO F.
       I.  MOVE WM-ARNUM TO AR-NUM ST-ARNUM ZU-NUM.
       K.  READ ARTIKEL INVALID GO R.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE AR-NUM TO WD-ARNUM.
           DISPLAY WD-ARNUM AT 1238.
           DELETE ARTIKEL.
           MOVE WH-NUM TO AR-NUM.
           COMPUTE AR-GRP = AR-NUM / 10000.
           WRITE AR-SATZ.
      *----------------------------------------------> Statistik-Satz <-
       L.  READ STATISTIK INVALID GO N.
           IF ZUGRIF PERFORM BESETZT GO L.
           DELETE STATISTIK INVALID GO N.
           MOVE AR-NUM TO ST-ARNUM.
           WRITE ST-SATZ INVALID REWRITE ST-SATZ.
      *-------------------------------------------------> Zusatztexte <-
       N.  READ ZUSATZ INVALID GO R.
           IF ZUGRIF PERFORM BESETZT GO N.
           DELETE ZUSATZ INVALID GO R.
           MOVE AR-NUM TO ZU-NUM.
           WRITE ZU-SATZ INVALID REWRITE ZU-SATZ.
       R.  IF WV-ANUM > 1 SUBTRACT 1 FROM WV-ANUM
               ADD 1 TO WM-ARNUM
               ADD 1 TO WH-NUM GO I.
           GO C.
       X.  CALL "CAUP" USING "08FENST" WH-CREG.
       Z.  EXIT.
      ***************************************** Lesen nach Artikel-Nr. *
       LIESART SECTION.
       A.  MOVE WT-TX TO WV-MCODE AR-AKEY.
           MOVE WX-TASTE TO WK-TASTE.
           IF KIST MOVE 5 TO WX-TASTE.
           IF NOT TABL GO G.
           CALL "CAUP" USING "1500006070" WH-CREG.
           IF WH-NUM = 0 GO X.
           IF WK-TASTE = 6 GO D.
           MOVE WH-NUM TO AR-NUM.
       C.  READ ARTIKEL IGNORE LOCK KEY AR-KEY NOT INVALID GO Z.
           GO Z.
       D.  MOVE WH-NUM TO AR-NUM WV-ARNUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO Z.
       E.  READ ARTIKEL NEXT IGNORE LOCK AT END MOVE 999999 TO AR-NUM.
           IF AR-NUM = WV-ARNUM ADD 1 TO WV-ARNUM GO E.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE WV-ARNUM TO WH-NUM.
           GO Z.
      *----------------------------------------> lesen nach Matchcode <-
       G.  READ ARTIKEL IGNORE LOCK KEY AR-AKEY INVALID
                MOVE 0 TO AR-NUM GO H.
           IF WF-STATUS NOT = 02 GO K.
       H.  CALL "GABANZ" USING "02ARTSUCH" WH-CREG.
       K.  MOVE AR-NUM TO WH-NUM.
           IF AR-NUM NOT = 0 GO Z.
       X.  MOVE 99 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       AR-BILD SECTION.
       A.  CALL "CAUP" USING "16LOEFENST" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikel:          Mcode:                Bez.:"
               AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "KGrp:    RGrp:    Herst:                Bez.:"
               AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Meh:     Ust:   % Ur-Nr:             min.:        Tx
      -        ":   Barvkp:" AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY " Datum  Lief.       Preis  Rab  ZR   Netto-Ekp   Auf
      -        "         Vkp     Fix-Vkp" AT VDU-LP.
           ADD 602 VDU-ECK GIVING VDU-LP.
           IF ANLAGE INITIALIZE AR-SATZ
               MOVE WH-NUM TO AR-NUM.
           MOVE AR-NUM TO WH-NUM WD-ARNUM.
           ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
      *****************************************************************
       AR-ANZ SECTION.
       A.  ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ARNUM WITH HIGHLIGHT AT VDU-LP.
           ADD 228 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-AKEY WITH HIGHLIGHT AT VDU-LP.
           ADD 249 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZA WITH HIGHLIGHT AT VDU-LP.
           ADD 349 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZB WITH HIGHLIGHT AT VDU-LP.
           ADD 309 VDU-ECK GIVING VDU-LP.
           MOVE AR-KAGRP TO WD-GRP.
           DISPLAY WD-GRP WITH HIGHLIGHT AT VDU-LP.
           ADD 318 VDU-ECK GIVING VDU-LP.
           MOVE AR-RAGRP TO WD-GRP.
           DISPLAY WD-GRP WITH HIGHLIGHT AT VDU-LP.
           ADD 328 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-HERST WITH HIGHLIGHT AT VDU-LP.
           ADD 408 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-MEH(AR-MEH + 1) WITH HIGHLIGHT AT VDU-LP.
           ADD 417 VDU-ECK GIVING VDU-LP.
           MOVE WT-UST(AR-UST) TO WD-PZ.
           DISPLAY WD-PZ WITH HIGHLIGHT AT VDU-LP.
           ADD 428 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-URNR WITH HIGHLIGHT AT VDU-LP.
           MOVE AR-MIN TO WD-MIN.
           ADD 446 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-MIN WITH HIGHLIGHT AT VDU-LP.
           ADD 457 VDU-ECK GIVING VDU-LP.
           MOVE AR-FOLGE TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           PERFORM PREIDIS.
           ADD 669 VDU-ECK GIVING VDU-LP.
           IF AR-FIXPR NOT = 0 MOVE AR-FIXPR TO WD-PREIS
               DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP
               ELSE DISPLAY "      -    " AT VDU-LP.
           MOVE AR-BVKP TO WD-PREIS.
           ADD 469 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       AR-ANL SECTION.
       A.  IF ANLAGE DISPLAY "Artikel fehlt!" AT 2401 GO B.
           IF WL-CA = 52 GO N.
           IF WH-PG NOT = 2 GO O.
       B.  DISPLAY "<esc>= Abbruch" AT 2301.
           IF WI = 1 MOVE 0 TO WR-P MOVE 1 TO WX
               DIVIDE 10000 INTO AR-NUM GIVING AR-GRP
               MOVE AR-GRP TO AR-RAGRP AR-KAGRP AR-MOGRP GO C.
           MOVE AR-AKEY TO WT-TX.
           CALL "CAUP" USING "1202280114" WH-CREG.
           IF WOLI OR ESC OR KIST GO Z.
           IF WT-TX = SPACE DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO B.
           MOVE WT-TX TO AR-AKEY.
           DISPLAY AR-AKEY with highlight AT VDU-LP.
       C.  MOVE AR-BEZA TO WT-TX.
           CALL "CAUP" USING "1202490130" WH-CREG.
           IF WOLI OR ESC OR KIST GO B.
           IF WT-TX = SPACE DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO C.
           MOVE WT-TX TO AR-BEZA.
           DISPLAY AR-BEZA with highlight AT VDU-LP.
       D.  MOVE AR-BEZB TO WT-TX.
           CALL "CAUP" USING "1203490130" WH-CREG.
           IF WOLI OR ESC OR KIST GO C.
           MOVE WT-TX TO AR-BEZB.
           DISPLAY AR-BEZB with highlight AT VDU-LP.
           IF WI = 1 GO H.
       E.  MOVE AR-KAGRP TO WH-WERT.
           DISPLAY "Kalkulationsgruppe" AT 2301.
           CALL "CAUP" USING "1003092002" WH-CREG.
           IF ESC GO V.
           IF WOLI GO D.
           IF FEHLER GO E.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WD-GRP AR-KAGRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
       F.  MOVE AR-RAGRP TO WH-WERT.
           DISPLAY "Rabattgruppe" AT 2301.
           CALL "CAUP" USING "1003182002" WH-CREG.
           IF ESC GO V.
           IF WOLI GO E.
           IF FEHLER GO F.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WD-GRP AR-RAGRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
       G.  DISPLAY "Hersteller" AT 2301.
           MOVE AR-HERST TO WT-TX.
           CALL "CAUP" USING "1203280110" WH-CREG.
           IF WOLI OR ESC OR KIST GO F.
           IF WT-TX = SPACE DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO G.
           MOVE WT-TX TO AR-HERST.
           DISPLAY AR-HERST with highlight AT VDU-LP.
           MOVE 0 TO WR-P.
           MOVE 1 TO WX.
       H.  DISPLAY "<tab>= anzeigen Mengeneinheiten" AT 2301.
           MOVE 2401 TO VDU-LP.
       I.  DISPLAY WR-P AT VDU-LP ":".
           ADD 3 TO VDU-P.
           IF WT-MEH(WX) NOT = SPACE AND WX NOT = 10
               DISPLAY WT-MEH(WX) WITH HIGHLIGHT AT VDU-LP ","
           IF VDU-P < 70 ADD 1 TO WX
               ADD 1 TO WR-P
               ADD 5 TO VDU-P GO I.
       J.  MOVE AR-MEH TO WH-WERT.
           CALL "CAUP" USING "1004082002" WH-CREG.
           IF TABL; IF WR-P > 10 MOVE 0 TO WR-P
                                 MOVE 1 TO WX GO H
                            ELSE MOVE 10 TO WR-P
                                 MOVE 11 TO WX GO H.
           IF ESC GO V.
           IF WOLI AND WI = 0 GO G.
           IF NOT RET GO J.
           IF WH-WERT > 19 GO J.
           MOVE WH-WERT TO AR-MEH.
           DISPLAY WT-MEH(AR-MEH + 1) with highlight AT VDU-LP.
       K.  DISPLAY "Ust-Kennzeichen 1-6" AT 2301.
           MOVE AR-UST TO WH-WERT.
           CALL "CAUP" USING "1004161001" WH-CREG.
           DISPLAY " " AT VDU-LP.
           ADD 1 TO VDU-LP.
           IF ESC GO V.
           IF WOLI MOVE 0 TO WR-P
                   MOVE 1 TO WX GO H.
           IF FEHLER GO K.
           MOVE WH-WERT TO AR-UST WD-KZ.
           IF AR-UST < 1 OR AR-UST > 6 GO K.
           MOVE WT-UST(AR-UST) TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       M.  DISPLAY "Minuten" AT 2301.
           MOVE AR-MIN TO WH-WERT.
           CALL "CAUP" USING "1004463105" WH-CREG.
           IF ESC GO V.
           IF WOLI GO K.
           IF FEHLER GO M.
           MOVE WH-WERT TO AR-MIN WD-MIN.
           DISPLAY WD-MIN with highlight AT VDU-LP.
       N.  DISPLAY "Zusatzbeschreibung f. Angebot" AT 2301.
           MOVE AR-FOLGE TO WH-WERT.
           CALL "CAUP" USING "1004571001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO M.
           IF FEHLER GO N.
           IF WH-WERT > 1 GO N.
           MOVE WH-WERT TO AR-FOLGE WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF AR-FOLGE = 1 PERFORM FOLGETX.
           IF WL-CA = 52 GO Y.
       O.  DISPLAY "Preisdatum" AT 2301.
           IF AR-DATUM = 0 MOVE WH-DATUM TO AR-DATUM.
           IF WH-PG = 4 MOVE WH-DATUM TO WC-DATUM WH-WERT
               ELSE MOVE AR-DATUM TO WC-DATUM WH-WERT.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 602 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WH-WERT TO WZ-DATUM.
           CALL "CAUP" USING "1106026006" WH-CREG.
           IF ESC GO V.
           IF WOLI; IF WH-PG = 2 GO N ELSE GO O.
           IF WZ-DATUM = 0 GO O.
           MOVE WZ-DATUM TO AR-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       P.  DISPLAY "Lieferant" AT 2301.
           IF WH-PG = 5 DISPLAY "<ret-leer>= Stammlieferant" AT 2401.
           MOVE AR-LIEF TO WT-TX.
           CALL "CAUP" USING "1206110105" WH-CREG.
           MOVE WT-TX TO AR-LIEF.
           DISPLAY AR-LIEF with highlight AT VDU-LP.
           IF AR-LIEF = SPACE MOVE WH-LIEF TO AR-LIEF
               DISPLAY AR-LIEF with highlight AT VDU-LP.
           IF AR-LIEF = SPACE GO P.
       Q.  DISPLAY "Einkaufspreis" with highlight AT 2301
      * " < >= Europre
      *        "is"  with highlight foreground-color 6.
           IF WH-PG = 6 DISPLAY "kein Preis = lîschen" AT 2401.
           MOVE AR-PREIS TO WH-WERT WD-APREIS.
           CALL "CAUP" USING "1006186209" WH-CREG.
           IF ESC GO V.
           IF WOLI GO O.
           IF FEHLER GO Q.
           MOVE WH-WERT TO AR-PREIS WD-PREIS.
      *    IF AB DISPLAY "Eur: " with highlight foreground-color 6
      *              AT 0713 WD-PREIS with highlight foreground-color 6
      *         COMPUTE WH-WERT rounded = AR-PREIS * 13,7603
      *         MOVE WH-WERT TO WD-PREIS AR-PREIS
      *         DISPLAY WD-PREIS with highlight AT 0000
      *         DISPLAY WD-APREIS with highlight AT VDU-LP
      *    else DISPLAY WD-PREIS with highlight AT VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
       R.  DISPLAY "Rabatt bei Bruttopreis" AT 2301.
           MOVE AR-RAB TO WH-WERT.
           CALL "CAUP" USING "1006302003" WH-CREG.
      *    CALL "CAUP" USING "1307010780" WH-CREG.
      *    MOVE AR-PREIS TO WD-PREIS.                     *>EURO
      *    ADD 0618 VDU-ECK GIVING VDU-LP.                *> "
      *    DISPLAY WD-PREIS with highlight AT VDU-LP.     *> "
           ADD 0630 VDU-ECK GIVING VDU-LP.                *>EURO
           IF ESC GO V.
           IF WOLI GO Q.
           IF FEHLER GO R.
           MOVE WH-WERT TO WX-GK.
           IF WX-GK > 98 DISPLAY "nicht mîglich" AT 2401
               PERFORM WEITER GO R.
           MULTIPLY -1 BY WX-GK GIVING WH-WERT.
           MOVE WH-WERT TO AR-RAB WD-ZUS.
           DISPLAY WD-ZUS with highlight AT VDU-LP.
       S.  DISPLAY "Zusatzrabatt" AT 2301.
           MOVE AR-ZRAB TO WH-WERT.
           CALL "CAUP" USING "1006342003" WH-CREG.
           IF ESC GO V.
           IF WOLI GO R.
           IF FEHLER GO S.
           MOVE WH-WERT TO WX-GK.
           IF WX-GK > 99 DISPLAY "nicht mîglich" AT 2401
               PERFORM WEITER GO T.
           MULTIPLY -1 BY WX-GK GIVING WH-WERT AR-ZRAB WD-ZUS.
           DISPLAY WD-ZUS with highlight AT VDU-LP.
           MOVE AR-PREIS TO AR-EKP AR-VKP.
           IF AR-RAB NOT = 0 COMPUTE WH-WERT = AR-RAB * AR-EKP / 100
               ADD WH-WERT AR-PREIS GIVING AR-EKP.
           IF AR-ZRAB NOT = 0 COMPUTE WH-WERT = AR-EKP * AR-ZRAB / 100
               ADD WH-WERT TO AR-EKP.
           MOVE AR-EKP TO WD-PREIS.
           ADD 639 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS AT VDU-LP.
           IF AR-AUF > 0
               COMPUTE AR-VKP = (AR-PREIS * AR-AUF / 100) + AR-PREIS.
           DIVIDE 10 INTO AR-VKP GIVING AR-VKP ROUNDED.
           MULTIPLY 10 BY AR-VKP.
           MOVE AR-VKP TO WD-PREIS.
           ADD 657 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
       T.  DISPLAY "Aufschlag" AT 2301.
           MOVE AR-AUF TO WH-WERT.
           CALL "CAUP" USING "1006523004" WH-CREG.
           IF ESC GO V.
           IF WOLI GO S.
           IF FEHLER GO T.
           MOVE WH-WERT TO WX-GK.
           MOVE WH-WERT TO AR-AUF WD-RAB.
           DISPLAY WD-RAB with highlight AT VDU-LP.
           IF AR-AUF > 0
               COMPUTE AR-VKP = (AR-PREIS * AR-AUF / 100) + AR-PREIS
           ELSE MOVE AR-PREIS TO AR-VKP
               DISPLAY "    " AT VDU-LP.
           DIVIDE 10 INTO AR-VKP GIVING AR-VKP ROUNDED.
           MULTIPLY 10 BY AR-VKP.
           MOVE AR-VKP TO WD-PREIS.
           ADD 657 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           IF ANLAGE GO U.
           IF WH-PG > 4 GO V.
       U.  DISPLAY "Sonderpreisvormerkung" AT 2301.
           MOVE AR-FIXPR TO WH-WERT.
           CALL "CAUP" USING "1006696209" WH-CREG.
           IF ESC GO V.
           IF WOLI GO T.
           IF FEHLER GO U.
           MOVE WH-WERT TO AR-FIXPR.
           IF AR-FIXPR NOT = 0 MOVE AR-FIXPR TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT VDU-LP
               ELSE DISPLAY "      -    " AT VDU-LP.
           COMPUTE AR-BVKP = AR-VKP + (AR-VKP * WT-UST(AR-UST) / 100).
           IF NOT ANLAGE; IF WH-PG NOT = 2 GO V.
           MOVE AR-BVKP TO WD-PREIS.
           ADD 469 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
       V.  IF WH-PG = 2 DISPLAY "<tab>= loeschen, <ret>= speichern, <esc
      -        ">= Abbruch" AT 2301
               CALL "CAUP" USING "0023511001" WH-CREG ELSE
           DISPLAY "<ret>= speichern, <esc> = Abbruch" AT 2301
               CALL "CAUP" USING "0023351001" WH-CREG.
           IF ESC UNLOCK ARTIKEL GO Z.
           IF WOLI GO O.
           IF TABL; IF WH-PG = 2 PERFORM AR-LOE GO Z.
           IF NOT RET GO V.
           IF WI = 1 GO Z.
           IF WH-PG = 6; IF AR-PREIS NOT = 0 GO X
               ELSE PERFORM PREILOE GO Y.
           IF WH-PG = 5 AND AR-PREIS = 0 GO Y.
           SET PY TO 1.
           MOVE AR-TAB TO WM-PRTAB.
           IF ANLAGE MOVE AR-TAB TO WH-PRTAB GO Y.
           IF WM-PRTAB = WV-PRTAB GO Y.
           SET PY TO 4.
       W.  MOVE AR-PRTAB(PY) TO AR-PRTAB(PY + 1).
           IF PY > 1 SET PY DOWN BY 1 GO W.
       X.  IF WH-PG > 4 MOVE AR-LIEF TO AR-ALIEF(PY)
               MOVE AR-DATUM TO AR-ADAT(PY)
               MOVE AR-PREIS TO AR-APREIS(PY)
               MOVE AR-RAB TO AR-ARAB(PY)
               MOVE AR-ZRAB TO AR-AZRAB(PY)
               MOVE AR-AUF TO AR-AAUF(PY).
           IF WH-PG = 4 MOVE 3 TO AR-SYM(PY)
               MOVE WV-LIEF  TO AR-ALIEF(PY)
               MOVE WV-DAT   TO AR-ADAT(PY)
               MOVE WV-PREIS TO AR-APREIS(PY)
               MOVE WV-RAB   TO AR-ARAB(PY)
               MOVE WV-ZRAB  TO AR-AZRAB(PY)
               MOVE WV-AUF   TO AR-AAUF(PY).
           IF WH-PG = 5 MOVE 2 TO AR-SYM(PY).
       Y.  IF WH-PG > 4 MOVE WH-PRTAB TO AR-TAB.
           COMPUTE AR-GRP = AR-NUM / 10000.
           IF WE-MOD = "RH" MOVE 99 TO AR-MET
                       else MOVE 00 TO AR-MET.
           WRITE AR-SATZ INVALID REWRITE AR-SATZ.
           UNLOCK ARTIKEL.
           MOVE 0 TO WM-MERKER.
           IF AR-FOLGE = 1 GO Z.
           MOVE AR-NUM TO ZU-NUM.
           DELETE ZUSATZ INVALID GO Z.
       Z.  EXIT.
      ******************************************************************
       FOLGETX SECTION.
       A.  CALL "CAUP" USING "0704361544010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikelbeschreibung " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "ƒƒƒƒ Artikelzeilenbreite ƒƒƒƒƒ" with reverse-video
                AT VDU-LP.
           MOVE AR-NUM TO ZU-NUM.
           READ ZUSATZ IGNORE LOCK INVALID INITIALIZE ZU-SATZ
               MOVE AR-NUM TO ZU-NUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 12
               COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK
               DISPLAY ZU-BEZ(WX) with highlight AT VDU-LP.
           MOVE 1 TO WX.
       C.  DISPLAY "<esc>= Abbruch, <ret> Zeilenende, < >= auf, <#>= Tex
      -        "tende" AT 2301.
           DISPLAY "kein Text = lîschen" AT 2401.
           MOVE ZU-BEZ(WX) TO WT-TX.
           COMPUTE VDU-LP = WX * 100 + 203.
           CALL "CAUP" USING "1200030140" WH-CREG.
           IF KIST MOVE WT-TX TO ZU-BEZ(WX) GO W.
           IF AUF; IF WX > 1 ADD -1 TO WX GO C.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE WT-TX TO ZU-BEZ(WX).
           DISPLAY ZU-BEZ(WX) with highlight AT VDU-LP.
           IF WX < 12 ADD 1 TO WX GO C.
           IF ZU-TX NOT = SPACE GO W.
           DELETE ZUSATZ INVALID GO X.
           GO X.
       W.  REWRITE ZU-SATZ INVALID WRITE ZU-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************** loeschen aus Preiskartei *
       PREILOE SECTION.
       A.  MOVE AR-PRTAB(PY + 1) TO AR-PRTAB(PY).
           IF PY < 4 SET PY UP BY 1 GO A.
           INITIALIZE AR-PRTAB(5).
       Z.  EXIT.
      ********************************************** loeschen Artikel *
       AR-LOE SECTION.
       A.  DELETE ARTIKEL INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO A.
           INITIALIZE AR-SATZ.
           MOVE AR-NUM TO ZU-NUM.
           DELETE ZUSATZ INVALID GO W.
       W.  DISPLAY "** gelîscht **" AT 2301.
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************  Preiswartung Artikel *
       PR-WART SECTION.
       A.  MOVE 0 TO WM-MERKER.
           IF WL-CA NOT = 54 MOVE "     Preiswartung" TO WK-GEB
               CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "0708010780000" WH-CREG.
           CALL "CAUP" USING "1315012580" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Preiswartung " with highlight AT VDU-LP.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           PERFORM AR-BILD.
           IF WL-CA = 54 GO F.
       C.  UNLOCK ARTIKEL.
           DISPLAY "<ret>= Matchcode, <tab>= Art.Nr, alpha+<ret>= suchen
      -       ", <esc>= Ende" AT 2301.
           DISPLAY "nÑchster freier Artikel = Nr.+<#>" AT 2401.
           IF AR-NUM NOT = 0 DISPLAY ", <>= letzter Artikel" AT 2434.
           ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY "              " AT VDU-LP.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1202120114" WH-CREG.
           IF WOLI; IF AR-NUM = 0 GO C ELSE GO D.
           IF ESC MOVE 0 TO AR-NUM GO X.
           PERFORM LIESART
               IF FEHLER GO C.
           MOVE WH-NUM TO AR-NUM.
       D.  MOVE 0 TO WM-MERKER.
       F.  UNLOCK ARTIKEL.
           READ ARTIKEL INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO F.
           PERFORM AR-BILD.
           IF ANLAGE MOVE AR-TAB TO WV-PRTAB
              MOVE 5 TO WH-PG
              PERFORM AR-ANL
              IF ESC GO B.
           PERFORM AR-ANZ.
           MOVE VDU-ECK TO WZ-ECK(1).
       G.  CALL "GABANZ" USING "15PREIANZ" WH-CREG.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
               IF WL-CA = 54 GO X else GO B.
           MOVE AR-TAB TO WH-PRTAB WV-PRTAB.
           IF WOLI CALL "CAUP" USING "08CLOFEN" WH-CREG
               MOVE 2 TO WH-PG
               PERFORM AR-ANL
               IF AR-NUM = 0 GO B ELSE GO D.
           MOVE VDU-ECK TO WZ-ECK(2).
           IF WX NOT = 0 SET PY TO WX
               MOVE 6 TO WH-PG GO I.
           IF KIST MOVE 5 TO WH-PG
              INITIALIZE AR-TAB
              MOVE WH-DATUM TO AR-DATUM
              ADD 200 WZ-ECK(1) GIVING VDU-ECK GO J.
           MOVE WZ-ECK(1) TO VDU-ECK.
           MOVE 4 TO WH-PG.
           GO K.
       I.  ADD 200 WZ-ECK(1) GIVING VDU-ECK.
           MOVE AR-ALIEF(PY)  TO AR-LIEF.
           MOVE AR-ADAT(PY)   TO AR-DATUM.
           MOVE AR-APREIS(PY) TO AR-PREIS AR-EKP.
           MOVE AR-ARAB(PY)   TO AR-RAB.
           MOVE AR-AZRAB(PY)  TO AR-ZRAB.
           MOVE AR-AAUF(PY)   TO AR-AUF.
           IF AR-RAB NOT = 0
               COMPUTE AR-EKP = AR-EKP + (AR-RAB * AR-EKP / 100).
           IF AR-ZRAB NOT = 0
               COMPUTE AR-EKP = AR-EKP + (AR-ZRAB * AR-EKP / 100).
           IF AR-AUF > 0
               COMPUTE AR-VKP = (AR-PREIS * AR-AUF / 100) + AR-PREIS
           ELSE MOVE AR-PREIS TO AR-VKP.
           DIVIDE 10 INTO AR-VKP GIVING AR-VKP ROUNDED.
           MULTIPLY 10 BY AR-VKP.
       J.  PERFORM PREIDIS.
       K.  PERFORM AR-ANL.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE WZ-ECK(1) TO VDU-ECK.
           IF WL-CA not = 54 GO F.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK ARTIKEL.
       Z.  EXIT.
      **************************************************** Preiszeile *
       PREIDIS SECTION.
       A.  MOVE AR-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 602 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM WITH HIGHLIGHT AT VDU-LP.
           ADD 611 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-LIEF AT VDU-LP.
           MOVE AR-PREIS TO WD-PREIS.
           ADD 618 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
           MOVE AR-RAB TO WD-ZUS.
           ADD 630 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ZUS WITH HIGHLIGHT AT VDU-LP.
           MOVE AR-ZRAB TO WD-ZUS.
           ADD 634 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ZUS WITH HIGHLIGHT AT VDU-LP.
           MOVE AR-EKP TO WD-PREIS.
           ADD 639 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS AT VDU-LP.
           ADD 652 VDU-ECK GIVING VDU-LP.
           IF AR-AUF NOT = 0 MOVE AR-AUF TO WD-RAB
               DISPLAY WD-RAB AT VDU-LP
               ELSE DISPLAY "    " AT VDU-LP.
           MOVE AR-VKP TO WD-PREIS.
           ADD 657 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
