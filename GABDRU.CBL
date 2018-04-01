      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GABDRU.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEKON.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSEART.CPY.
           COPY GABSEATX.CPY.
           SELECT XML-DAT    ASSIGN TO WH-GABNAM
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
           COPY GABSEDRU.CPY.
           SELECT ABLAGE     ASSIGN TO WH-ABLNAM
                             ORGANIZATION RECORD SEQUENTIAL
                             FILE STATUS WF-STATUS.
           SELECT XMLDRUCK   ASSIGN TO WH-XMLNAM
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABKONST.CPY.
       COPY GABANBOT.CPY.
       COPY GABARTIK.CPY.
      ******************************************************************
       FD  ABLAGE                      LABEL RECORD OMITTED.
       01  ABA-SATZ.
           03 ABA-SCHALT               PIC 99.
           03 ABA-REST                 PIC X(132).
      ******************************************************************
       FD  XMLDRUCK                    LABEL RECORD OMITTED.
       01  XM-SATZ                     PIC X(256).
      ****************************************** Hilfsdatei f. Reorg. *
       FD  XML-DAT                     LABEL RECORD STANDARD.
       01  XD-SATZ                     PIC X(256).
      ***************************************************** Textdatei *
       FD  ANTEXT       EXTERNAL       LABEL RECORD STANDARD.
       01  TE-SATZ                     PIC X(60).
      ******************************** P3800 = PRINTER **** PC = LPT1 *
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-NO                  PIC XX.
           03  DRA-CD.
               05 DRA-KTONR            PIC ZZ.ZZ9,9.
               05 DRA-ARNUM            PIC ZZZZ.ZZ9-.
               05 DRA-ART              PIC ZZZ9-.
               05 DRA-MON              PIC ZZZ9/99-.
               05 FILLER               PIC X(210).
       01  DRB-SATZ.
           03  FILLER                  PIC XXX.
           03  DRB-KO.
               05 FILLER               PIC XXX.
               05 DRB-TX.
                  07 FILLER            PIC X(10).
                  07 DRB-ADR.
                     09 FILLER         PIC X(30).
                     09 DRB-RENUM      PIC Z.ZZZ9-.
                     09 DRB-STR        PIC X.
                     09 DRB-KTONR      PIC ZZ.ZZ9,9.
                     09 FILLER         PIC X(6).
                     09 DRB-ORT        PIC X(27).
                     09 FILLER         PIC X(90).
       01  DRC-SATZ.
           03  FILLER                  PIC X(7).
           03  DRC-ARNUM               PIC Z9.9999-.
           03  FILLER                  PIC X.
           03  DRC-BEZ.
               05 FILLER               PIC X(31).
               05 DRC-MENGE            PIC X(12).
               05 DRC-MEH              PIC XXX.
               05 DRC-PREIS            PIC ZZZZ.ZZ9,99-.
               05 FILLER               PIC X.
           03  DRC-PROZ                PIC Z9-.
           03  DRC-STR.
               05 DRC-BETRAG           PIC Z.ZZZ.ZZ9,99-.
           03  DRC-UST                 PIC ZZ9.
       01  DRD-SATZ.
           03  FILLER                  PIC X(10).
           03  DRD-MWST.
               05  FILLER              PIC X(18).
               05  DRD-PROZ            PIC Z9,99.
               05  DRD-PZ              PIC XX.
               05  DRD-BEZ             PIC X(19).
               05  DRD-BASIS           PIC Z.ZZZ.ZZ9,99-.
               05  FILLER              PIC X(18).
               05  DRD-STR             PIC X(13).
       01  DRE-SATZ.
           03  FILLER                  PIC X(16).
           03  DRE-BEZ                 PIC X(72).
       01  DRV-SATZ.
           03  FILLER                  PIC X(7).
           03  DRV-TX.
               05 DRV-SK1              PIC 9.
               05 DRV-TX1              PIC X(35).
               05 DRV-TG1              PIC Z9.
               05 DRV-TX2              PIC X(12).
               05 DRV-TGN              PIC Z9.
               05 DRV-TX3              PIC X(7).
               05 DRV-BET              PIC ZZ.ZZ9,99-.
               05 DRV-TX4              PIC X(3).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-P                    PIC 99        COMP.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WM-MERKER               PIC 99        COMP.
                      88 ANLAGE   VALUE 1 3.   88 AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 99        COMP.
           03  WZ-SEITE                PIC 99        COMP.
           03  WZ-SCHALT               PIC 99        COMP.
           03  WZ-ZEILEN               PIC 99V9      COMP.
           03  WH-XMLNAM               PIC X(24).
           03  WX-TOP                  PIC 9999V9.
           03  WM-TOP                  PIC 9999.           *> Start-Top
           03  WX-LEFT                 PIC 9999.
           03  WH-GABNAM               PIC X(15).
           03  WH-SIZE                 PIC 99        COMP.
           03  WH-TX                   PIC X(51).
           03  WH-MOD                  PIC 99        COMP.
               88  ANBDRU   VALUE 1.    88  MATDRU    VALUE 2.
               88  AUFRE    VALUE 3.    88  AUFDRU    VALUE 4.
               88  BEIDES   VALUE 5.
           03  WL                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WQ                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IP                      PIC 99        COMP.
           03  IQ                      PIC 99        COMP.
           03  WV-ARNUM                PIC 9(6)      COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WH-L                    PIC 99        COMP.
           03  WV-KAP                  PIC 99        COMP.
           03  WV-AEND                 PIC 99        COMP.
               88 AUFEND    VALUE 1.
           03  WD-ANUM                 PIC 99999.
           03  WD-LFNUM                PIC ZZ.ZZ9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZZ9,9-.
           03  WD-MGC                  PIC ZZZ9,99-.
           03  WD-MG                   PIC X(8).
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WD-ARTIK                PIC X(30).
           03  WD-KZ                   PIC 9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WT-BASIS                PIC S9(7)V99   COMP-3 OCCURS 6.
           03  WH-USTKZ                PIC 9.
           03  WE-PREIS                PIC S9(5)V99   COMP-3.
           03  WD-EUR                  PIC ZZ.ZZ9,99-.
           03  WH-BET                  PIC S9(9)V99   COMP-3.
           03  WS-TEIL                 PIC S9(9)V99   COMP-3.
           03  WK-BET                  PIC S9(9)V99   COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WS-RAB                  PIC S9(9)V99   COMP-3.
           03  WS-NET                  PIC S9(9)V99   COMP-3.
           03  WS-KAPSUM               PIC S9(9)V99   COMP-3.
           03  WH-MWST                 PIC S9(9)V99   COMP-3.
           03  WH-MEH                  PIC 99         COMP.
           03  WM-KAP                  PIC 99         COMP.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-MA                PIC 99         COMP.
               05 WM-KO                PIC 99         COMP.
               05 WM-PR                PIC 99         COMP.
               05 WM-RB                PIC 99         COMP.
           03  WK-ANR                  PIC 9.
      *-----------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-Z SECTION.         USE AFTER ERROR PROCEDURE ON ZUSATZ.
       A.  CALL "CADECL" USING "GABISARZ.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON ANTEXT.
       A.  CALL "CADECL" USING WH-TXKEY       WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON AUFTRAG.
       A.  CALL "CADECL" USING "GABISAUF.DAT" WH-CREG.
       DECL-J SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON XML-DAT.
       A.  CALL "CADECL" USING WH-GABNAM WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 10 PERFORM AUFDRUCK.
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
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 21 GO Z.
       B.  CALL "CAUP" USING "140701248000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
           SUBTRACT 1 FROM WH-L.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       PAGEDRU SECTION.
       A.  WRITE DRA-SATZ BEFORE PAGE.
           IF WM-DRU = 1 MOVE 99 TO ABA-SCHALT
                         MOVE SPACE TO ABA-REST
                         WRITE ABA-SATZ.
           MOVE SPACE TO DRA-SATZ.
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
       XML-ZEIL SECTION.
       A.  MOVE "<text top= ""1000"" left= ""0330"">" TO XM-SATZ.
           COMPUTE WX-TOP = (WZ-ZEILEN + WZ-SCHALT - 1) * 42,5 + WM-TOP.
           MOVE WX-TOP(1:4) TO XM-SATZ(13:4).
           MOVE WX-LEFT TO XM-SATZ(26:4).
           IF IP = 5 MOVE " align=""right"">" TO XM-SATZ(31:).
           WRITE XM-SATZ.
      *    ADD 42,5 TO WX-TOP.
           MOVE "<p>" TO XM-SATZ.
           MOVE DRA-SATZ(3:) TO XM-SATZ(4:).
           INSPECT XM-SATZ REPLACING ALL "ö" BY "‹" "ô" BY "÷"
                              "é" BY "ƒ" "Å" BY "¸" "î" BY "ˆ"
                              "Ñ" BY "‰" "·" BY "ﬂ" "&" BY "+".
           PERFORM VARYING wr from 96 by -1 until wr = 10
               or xm-satz(wr:1) not = " " end-perform.
           add 1 to wr.
           MOVE "</p>" TO XM-SATZ(wr:).
      *    MOVE "</p>" TO XM-SATZ(96:).
           WRITE XM-SATZ.
           MOVE "</text>" TO XM-SATZ.
           WRITE XM-SATZ.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
           IF ANBDRU; IF IP = 1 PERFORM XML-ZEIL.
           IF WM-DRU = 1 MOVE DRA-SATZ(3:) TO DRA-SATZ(1:).
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           IF WM-DRU = 1 MOVE WZ-SCHALT TO ABA-SCHALT
                         MOVE DRA-SATZ TO ABA-REST
                         WRITE ABA-SATZ.
           MOVE SPACE TO DRA-SATZ ABA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
       B.  IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:)
               PERFORM PAGEDRU.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ ABA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER ABLAGE.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           PERFORM DRU-OK.
           IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER
                IF WM-WO = 9 OPEN EXTEND ABLAGE
                end-if
           else OPEN EXTEND DRUCKER ABLAGE.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT WZ-SEITE.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE "E" TO DRA-SATZ
               MOVE WE-STG(WH-P) TO DRA-SATZ(3:)
               MOVE 0 TO ABA-SCHALT
               MOVE DRA-SATZ TO ABA-REST
               WRITE ABA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           IF WM-DRU = 1 GO X.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
       X.  MOVE SPACE TO DRA-SATZ ABA-SATZ.
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF WT-NK(WH-MEH) = 2 DIVIDE 100 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ2 WD-MGC
               MOVE WD-MGC TO WD-MG
               MOVE WD-ANZ2 TO WD-MENGE.
           IF WT-NK(WH-MEH) = 1 DIVIDE 10 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ1 WD-MGB
               MOVE WD-MGB TO WD-MG
               MOVE WD-ANZ1 TO WD-MENGE.
           IF WT-NK(WH-MEH) = 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
               MOVE WD-MGA TO WD-MG
               MOVE WD-ANZ TO WD-MENGE.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ******************************************************************
       AUFDRUCK SECTION.
       A.  CALL "CAUP" using "1307012480000" WH-CREG.
           MOVE 1 TO WH-KEY.
           MOVE 0 TO IP.
       B.  READ KONSTANT NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE KO-MERK TO WK-M.
           MOVE AK-NUM TO AU-NUM.
           MOVE 0 TO AU-KAP AU-POS AU-TZ WS-BET.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN WM-OPEN.
           DISPLAY "1 = Angebotsdruck"  AT 1025
                   "2 = Materialschein" AT 1225
                   "                               < >" AT 1325
                   "3 = Rechnung lt. Auftrag" AT 1425
                   "4 = Auftragsschein" AT 1625
           CALL "CAUP" USING "1500006006" WH-CREG.     *> f. Voreingabe
           IF WH-NUM > 0 and < 5 GO D.
       C.  DISPLAY "<ret>= Auswahl, <ret-leer>= Abbruch" AT 2301.
           CALL "CAUP" USING "0013571001" WH-CREG.
       D.  IF ESC or WH-NUM = 0 GO Z.
           MOVE "GABKOPF.XML" TO WH-GABNAM.
           MOVE WH-NUM TO WH-MOD WD-KZ.
           DISPLAY WD-KZ WITH HIGHLIGHT AT 1357.
           IF WH-NUM > 5 or NOT RET MOVE 0 TO WH-NUM
                GO C.
           IF MATDRU or AUFDRU PERFORM MATDRUCK GO Z.
           IF AUFRE MOVE 49 TO WH-PG
               MOVE "GABRECH 99AUTFAK" TO WT-TX GO G.
           START AUFTRAG KEY > AU-KEY INVALID GO Z.
           MOVE 7 TO VDU-L.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE 0 TO WH-P.
           MOVE WE-DRU(2) TO WM-DRU.            *> 2 = Auftragsformular
           MOVE WE-KOPF(2) TO WM-KOPF.
           MOVE WE-WO(2) TO WM-WO.
           MOVE "KOPIE.LST" TO WH-ABLNAM.
           IF WM-DRU = 1 MOVE 10 TO WH-P
                         MOVE "AUFTRAG.LST" TO WH-DRUNAM.
           IF WM-DRU = 0 or WM-WO = 9 MOVE "LPT1" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "ANxxxxx.XML" TO WH-XMLNAM.
           MOVE AK-NUM TO WD-ANUM
           MOVE WD-ANUM TO WH-XMLNAM(3:5).
           MOVE "E-ANBOT.XML" TO WH-XMLNAM.
           OPEN OUTPUT XMLDRUCK.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6) WV-KAP WL
                WH-NUM WH-MWST WS-BET WS-RAB WS-NET WS-TEIL WS-KAPSUM
                INITIALIZE AK-SUMMEN.
           PERFORM FAKADR.
       E.  READ AUFTRAG NEXT NO LOCK AT END MOVE 0 TO WV-KAP AU-NUM.
           IF AU-NUM NOT = AK-NUM MOVE 0 TO WV-KAP GO F.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WV-KAP = 0 MOVE AU-KAP TO WV-KAP.
       F.  IF AU-KAP NOT = WV-KAP;
               IF WL = 0 PERFORM KAP-SUM
               else IF WS-TEIL NOT = 0
                        MOVE "T e i l s u m m e " TO DRC-SATZ(26:18)
                        MOVE WS-TEIL TO DRC-BETRAG
                        MOVE WS-KAPSUM TO DRC-PREIS
                        PERFORM DRUCK
                        MOVE 0 TO WL WS-TEIL WS-KAPSUM
                        MOVE AU-KAP TO WV-KAP.
           IF AU-NUM NOT = AK-NUM GO W.
           PERFORM ZEIDRUCK.
           GO E.
       G.  START AUFTRAG KEY > AU-KEY INVALID GO X.
       H.  READ AUFTRAG NEXT NO LOCK AT END GO X.
           IF AU-NUM NOT = AK-NUM GO X.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF AK-RENUM = 0 GO Z.
           DISPLAY "bereits fakturiert Re.Nr.:        / " AT 0701.
           MOVE AK-RENUM TO WD-LFNUM.
           DISPLAY WD-LFNUM WITH HIGHLIGHT AT 0728.
           MOVE AK-REDAT TO WC-DATUM.
           IF AK-REDAT = 0 MOVE AK-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM WITH HIGHLIGHT AT 0737.
           DISPLAY "<ret>= Abbruch, <#>= Stornorechnung" AT 2301.
           CALL "CAUP" USING "0007470000" WH-CREG.
           IF KIST MOVE 49 TO WH-PG
               MOVE "GABRECH 98AUTFAK" TO WT-TX GO Z.
           MOVE 1 TO WH-PG.
           GO Z.
       W.  CALL "CAUP" using "1307012480000" WH-CREG.
           IF WS-KAPSUM NOT = 0 PERFORM KAP-SUM.
           PERFORM AUFENDE.
           GO Z.
       X.  MOVE 1 TO WH-PG.
           UNLOCK AUFTRAG.
       Z.  EXIT.
      *****************************************************************
       MATDRUCK SECTION.
       A.  START AUFTRAG KEY > AU-KEY INVALID GO Z.
           MOVE 0 TO WM-KAP.
           MOVE 7 TO VDU-L.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           EVALUATE TRUE
               WHEN MATDRU MOVE WE-DRU(4) TO WM-DRU
                           MOVE WE-KOPF(4) TO WM-KOPF
                           MOVE WE-WO(4) TO WM-WO
                           MOVE "MATDRU.LST" TO WH-DRUNAM
               WHEN AUFDRU MOVE WE-DRU(3) TO WM-DRU
                           MOVE WE-KOPF(3) TO WM-KOPF
                           MOVE WE-WO(3) TO WM-WO
                           MOVE "AUFDRU.LST" TO WH-DRUNAM.
           MOVE 0 TO WH-P.
           IF WM-DRU = 1 MOVE 10 TO WH-P
                MOVE "KOPIE.LST" TO WH-ABLNAM.
           IF WM-DRU = 0 or WM-WO = 9 MOVE "LPT1" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           PERFORM FAKADR.
           MOVE 0 TO WV-AEND.
       C.  READ AUFTRAG NEXT IGNORE LOCK AT END GO W.
           IF AU-NUM NOT = AK-NUM GO W.
           IF AU-ART = 0 and AU-POS = 0 GO C.
           IF WM-KAP = 0 MOVE AU-KAP TO WM-KAP.
           IF WM-KAP not = AU-KAP PERFORM LEERZEIL
               MOVE 70 TO WZ-ZEILEN
               MOVE AU-KAP TO WM-KAP.
           PERFORM UEBERTRAG.
           IF AU-ART = 1 and AU-POS = 0 PERFORM KOPFTEXT
               IF FINE GO W
               else GO Q.
      *----------------------------------------> Ausdruck einer Zeile <-
           IF AU-ARNUM > 0 MOVE AU-ARNUM TO WV-ARNUM
               COMPUTE DRC-ARNUM = AU-KAP * 10000 + AU-POS.
           IF AU-TZ > 0; IF AU-ART > 5 GO F
               ELSE GO C.
           MOVE AU-BEZ(1) TO WT-ADR.
           MOVE AU-BEZ(2) TO WR-ADR(2).
           IF WR-ADR(1) NOT = SPACE MOVE WR-ADR(1) TO DRC-BEZ.
       E.  IF WR-ADR(2) NOT = SPACE PERFORM DRUCK
               MOVE 0 TO WZ-SCHALT
               MOVE "(s0p0s0b6T" TO DRB-TX(3:)
               PERFORM DRUCK
               MOVE 0 TO WZ-SCHALT
               IF MATDRU MOVE "≥" TO DRB-TX(57:1) DRB-TX(61:1)
                   DRB-TX(65:1) DRB-TX(69:1) DRB-TX(73:1) DRB-TX(77:1)
                   DRB-TX(81:1) DRB-TX(85:1) DRB-TX(89:1) DRB-TX(1:1)
               end-if
               IF AUFDRU MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1)
                   DRB-TX(73:1) DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                   DRB-TX(89:1) DRB-TX(1:1)
               end-if
               PERFORM DRUCK
               MOVE WR-ADR(2) TO DRC-BEZ.
       F.  IF AU-ANZ NOT = 0 MOVE AU-ANZ TO WH-ANZ
               ADD 1 AU-MEH GIVING WH-MEH
               PERFORM MG
               DISPLAY WD-MG WITH HIGHLIGHT AT VDU-LP
               IF AU-KAGRP NOT = 1 MOVE WD-MENGE TO DRC-MENGE
               END-IF MOVE WT-MEH(WH-MEH) TO DRC-MEH
                   IF AU-ART = 9 MOVE "*A*" TO DRC-SATZ(64:3)
                   end-if
                      IF MATDRU MOVE "≥" TO DRB-TX(57:1)DRB-TX(61:1)
                           DRB-TX(65:1) DRB-TX(69:1) DRB-TX(73:1)
                           DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                           DRB-TX(89:1) DRB-TX(1:1)
                      end-if
                      IF AUFDRU MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1)
                           DRB-TX(73:1) DRB-TX(77:1) DRB-TX(81:1)
                           DRB-TX(85:1) DRB-TX(89:1) DRB-TX(1:1)
                           COMPUTE WD-PREIS ROUNDED = AU-PREIS +
                               (AU-PREIS * WT-UST(AU-UST) / 100)
                           MOVE WD-PREIS TO DRB-TX(58:11)
                      end-if
                   PERFORM DRUCK.
       Q.  MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¬" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1).
           IF MATDRU MOVE "¬" TO DRB-TX(61:1) DRB-TX(65:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           IF WZ-ZEILEN > 57,5 MOVE ALL "ƒ" TO DRB-TX
                               MOVE "Ÿ" TO DRB-TX(89:)
                               MOVE "¿" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           GO C.
       W.  SET AUFEND TO TRUE.
           PERFORM LEERZEIL.
           PERFORM DUPLOS.
           PERFORM END-DRU.
           MOVE 4 TO AK-ART.
           REWRITE AK-SATZ.
           DELETE FILE ABLAGE.
           UNLOCK AUFTRAG.
       Z.  EXIT.
      ******************************************************************
       KOPFTEXT SECTION.
       A.  MOVE AU-BEZ(1) TO DRC-BEZ.
           MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                   DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                   DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           IF AU-BEZ(2) = SPACE GO C.
           MOVE AU-BEZ(2) TO DRC-BEZ.
           MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                       DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
       C.  READ AUFTRAG NEXT IGNORE LOCK AT END GO Z.
           IF AU-NUM NOT = AK-NUM SET FINE TO TRUE GO Z.
           IF AU-ART = 1 and AU-POS = 0 GO A.
           READ AUFTRAG PREVIOUS IGNORE LOCK AT END GO Z.
       Z.  EXIT.
      ******************************************************************
       LEERZEIL SECTION.
       A.  IF AUFDRU and AUFEND and WZ-ZEILEN < 39 GO G.
       C.  IF WZ-ZEILEN > 58; IF AUFDRU AND AUFEND GO G
                              else GO X.
           MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                       DRB-TX(89:1) DRB-TX(1:1).
           IF MATDRU MOVE "≥" TO DRB-TX(61:1) DRB-TX(65:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¬" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1).
           IF MATDRU MOVE "¬" TO DRB-TX(61:1) DRB-TX(65:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           ADD 0,5 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 58 MOVE ALL "ƒ" TO DRB-TX
                               MOVE "Ÿ" TO DRB-TX(89:)
                               MOVE "¿" TO DRB-TX(1:1).
           PERFORM DRUCK.
           GO C.
      *-------------------------------> Leerzeilen bei Auftragsschein <-
       G.  PERFORM UEBERTRAG.
           IF WZ-ZEILEN > 49 GO Q.
           MOVE "≥" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
                       DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¬" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                       DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1).
           IF WZ-ZEILEN > 48,5 MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           GO G.
      *--------------------------------------> Fu· bei Auftragsschein <-
       Q.  MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRB-TX(3:) PERFORM DRUCK.
           MOVE "≥" TO DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p12v0s2b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Altmaterial-Entsorgung: WC - WCDE - WCSPK - WT - BW - K
      -         "LSP - SP_____ lt. ARM _____ Stk." TO DRB-TX(6:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRB-TX(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRB-TX(3:) PERFORM DRUCK.
           MOVE "≥" TO DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p12v0s2b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Sonstige Altmaterial-Entsorgung:" TO DRB-TX(6:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRB-TX(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¬" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(72:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE "≥" TO DRB-TX(54:1) DRB-TX(64:1)
                       DRB-TX(72:1) DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p12v0s2b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Arbeitszeit wird von WerkstÑtte zu WerkstÑtte verrechne
      -        "t." TO DRB-TX(6:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p10v0s1b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "von - bis" TO DRB-TX(113:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Std." TO DRB-TX(133:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "N a m e" TO DRB-TX(153:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRA-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "≈" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(72:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           MOVE "¬" TO DRB-TX(33:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE "≥" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(33:1)
                       DRB-TX(72:1) DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p10v0s1b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Unterschift des Kunden" TO DRB-TX(15:).
           PERFORM DRUCK.
           MOVE "Monteur-Std. mit Auto" TO DRB-TX(69:).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRA-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "≈" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(72:1)
                       DRB-TX(33:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "√" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE "≥" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(33:1)
                       DRB-TX(72:1) DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p10v0s1b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Monteur-Std. ohne Auto" TO DRB-TX(69:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRA-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "√" TO DRB-TX(33:1).
           MOVE ALL "ƒ" TO DRB-TX(34:).
           MOVE "≈" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(72:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "≥" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE "≥" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(33:1)
                       DRB-TX(72:1) DRB-TX(89:1) DRB-TX(1:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p10v0s1b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Helfer- /Lehrlingsstd." TO DRB-TX(69:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRA-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "√" TO DRB-TX(33:1).
           MOVE ALL "ƒ" TO DRB-TX(34:).
           MOVE "¡" TO DRB-TX(54:1) DRB-TX(64:1) DRB-TX(72:1).
           MOVE "¥" TO DRB-TX(89:).
           MOVE "≥" TO DRB-TX(1:1).
           PERFORM DRUCK.
           ADD 0,5 TO WZ-ZEILEN.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRB-TX(3:) PERFORM DRUCK.
           MOVE "≥" TO DRB-TX(89:1) DRB-TX(1:1) DRB-TX(33:1).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s1p12v0s2b4101T" TO DRB-SATZ(3:) PERFORM DRUCK.
           MOVE "MONT.-NR." TO DRB-TX(59:).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "KM HIN-RETOUR:" TO DRB-TX(105:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b4101T" TO DRA-SATZ(3:) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK.        *> 1/2 Zeile
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRB-TX.
           MOVE "¡" TO DRB-TX(33:1).
           MOVE "Ÿ" TO DRB-TX(89:).
           MOVE "¿" TO DRB-TX(1:1).
           PERFORM DRUCK.
       X.  PERFORM FUSSTEXT.
       Z.  EXIT.
      ******************************************************** Fu·text *
       FUSSTEXT SECTION.
       A.  MOVE 0 TO WZ-SCHALT.
           MOVE "&l4C(s0p16.6h0b0s0T" TO DRA-CD PERFORM DRUCK.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Die Ware bleibt bis zur vollstÑndigen Bezahlung unser E
      -      "igentum. Im Åbrigen gelten die Bedingungen der Bundes-"
                TO DRB-TX(3:).
           PERFORM DRUCK.
           MOVE "wirtschaftskammer Sektion Gas-, Wasserinstallateure und
      -         " Heizungsbauer ôsterreichs. " TO DRB-TX(3:)
               PERFORM DRUCK
           MOVE 0 TO WZ-SCHALT.
           MOVE "&l9,6C(s0p12h0b0s6T" TO DRA-CD PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       DUPLOS SECTION.
       A.  PERFORM PAGEDRU.
300405     IF ANBDRU ADD 10 TO WH-MOD.
      *     GO Z.
           CLOSE ABLAGE.
           OPEN INPUT ABLAGE WITH LOCK.
           ADD 2 TO WM-DRU.
       C.  READ ABLAGE AT END GO Q.
           MOVE ABA-REST TO DRA-SATZ.
           IF ABA-SCHALT = 99 WRITE DRA-SATZ BEFORE PAGE GO C.
           MOVE ABA-SCHALT TO WZ-SCHALT.
           PERFORM DRUCK.
           GO C.
       Q.  ADD -2 TO WM-DRU.
           CLOSE ABLAGE.
           DELETE FILE ABLAGE.
           OPEN OUTPUT ABLAGE.
           IF WH-MOD > 10 ADD -10 TO WH-MOD.
       Z.  EXIT.
      *****************************************************************
       FAKADR SECTION.
       A.  IF ANBDRU MOVE 4 TO WZ-SCHALT.
           IF MATDRU or AUFDRU MOVE 4 TO WZ-SCHALT.
           IF WM-KOPF = 0 PERFORM DRUCK GO G.
      *------------------------------------> Kopf aus Konstantendatei <-
           MOVE 2 TO WH-KEY.
       B.  READ KONSTANT IGNORE LOCK.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 0 GO C.
           EVALUATE WH-KEY
               WHEN 2 MOVE "(s1p22v4s3b4101T" TO DRA-CD
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE KO-KOPF TO DRB-SATZ(15:)
                      PERFORM OB-TEILG
               WHEN 3 MOVE "(s0p0s3b6T" TO DRA-CD PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(15:)
               WHEN 4 MOVE "(s0p0s1b6T" TO DRA-CD PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(15:)
               WHEN 5 MOVE "(s0p0s0b0T" TO DRA-CD PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(12:).
           GO E.
       C.  MOVE X"1B21" TO DRA-SATZ(1:).
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRA-SATZ(3:2) PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(4:)
               WHEN 3 MOVE X"3100" TO DRA-SATZ(3:2) PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(4:)
               WHEN 4 MOVE KO-KOPF TO DRB-SATZ(4:)
               WHEN 5 MOVE X"0100" TO DRA-SATZ(3:2) PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(12:).
       E.  PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO B.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "&l6D" TO DRA-CD.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRB-SATZ(7:).
           PERFORM DRUCK.
           MOVE "Firmenbuch: 321/Leibnitz Handelsgericht GRAZ"
               TO DRB-SATZ(51:).
           PERFORM DRUCK.
           MOVE 0 TO WQ.
           MOVE AK-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           INSPECT WT-BEZ REPLACING ALL "##" BY ", " ALL "#" BY ",".
           IF ANBDRU PERFORM XMLKOPF.
      *--------------------------------------> ab hier ohne Kopfdruck <-
       G.  IF WZ-SEITE > 0 MOVE 1 TO WZ-SCHALT
               MOVE AK-BEZ TO DRB-SATZ(8:)
               INSPECT DRB-SATZ REPLACING ALL "#" BY ","
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 80
                   IF DRB-SATZ(WY:3) = ",,," ADD 2 WY GIVING WR
                          MOVE DRB-SATZ(WR:) TO DRB-SATZ(WY:)
                   end-if
                   IF DRB-SATZ(WY:2) = ",," ADD 1 WY GIVING WR
                          MOVE DRB-SATZ(WR:) TO DRB-SATZ(WY:)
               end-perform
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE "Seite: " TO DRB-SATZ(20:7)
               ADD 1 TO WZ-SEITE
               MOVE WZ-SEITE TO WD-TG
               MOVE WD-TG TO DRB-SATZ(27:3)
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               GO F
           else MOVE 1 TO WZ-SEITE.
           MOVE 4 TO WZ-SCHALT.
           IF AUFDRU or MATDRU MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
       F.  MOVE 0 TO WZ-SCHALT ABA-SCHALT.
           IF WM-DRU = 1 MOVE "(s1p18v0s0b4101TK o p i e(s0p0s0b4101T"
                            TO ABA-REST(68:)
               WRITE ABA-SATZ
               MOVE SPACE TO ABA-SATZ.
           IF WZ-SEITE > 1 MOVE 2 TO WZ-SCHALT GO H.
           MOVE WT-TXT(AK-ANR + 1) TO DRB-ADR.
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRB-ADR
               IF not AUFDRU and not MATDRU PERFORM DRUCK
               else IF DRB-ADR not = SPACE PERFORM DRUCK.
           MOVE 4 TO WZ-SCHALT.
           IF AUFDRU or MATDRU MOVE 2 TO WZ-SCHALT.
       H.  IF WM-DRU = 0 MOVE X"1B213100" TO DRB-KO
                    else MOVE "(s1p16v0s2b4101T" TO DRB-KO.
           PERFORM DRUCK.
           EVALUATE TRUE
               WHEN ANBDRU MOVE "Angebot" TO DRB-TX(4:)
                           MOVE 35 TO WX
               WHEN MATDRU MOVE "Materialschein" TO DRB-TX(4:)
                           MOVE 27 TO WX
               WHEN AUFDRU MOVE "Auftragsschein" TO DRB-TX(4:)
                           MOVE 27 TO WX.
           MOVE 0 TO WZ-SCHALT PERFORM DRUCK.
           IF WM-DRU = 0 MOVE X"1B210100" TO DRA-SATZ(1:4)
                    else MOVE "(s0p0s0b0T" TO DRB-KO.
           MOVE 0 TO WZ-SCHALT PERFORM DRUCK.
           MOVE "Nr:" TO DRB-ADR(26:).
           MOVE AK-NUM TO DRB-RENUM.
           MOVE "/" TO DRB-STR.
           DIVIDE 10 INTO AK-KTONR GIVING DRB-KTONR.
           MOVE DRB-SATZ(42:) TO DRB-SATZ(WX:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-GZDAT TO DRB-ORT.
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
300405*        MOVE "(s0p0s1b6T" TO DRA-CD
               MOVE "(s0p0s1b0T" TO DRA-CD
               PERFORM DRUCK.
           MOVE 7 TO WH-KEY.
       I.  READ KONSTANT NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE KO-KOPF TO DRB-TX.
           IF WH-KEY = 8;
              EVALUATE TRUE
                  WHEN ANBDRU MOVE "Position" TO DRB-TX(2:8)
                  WHEN MATDRU
                  WHEN AUFDRU MOVE SPACE TO DRB-TX(58:31)
                              MOVE "Ret" TO DRB-TX(82:3)
                              MOVE "Ges" TO DRB-TX(86:3)
                              MOVE "≥" TO DRB-TX(57:1) DRB-TX(61:1)
                                          DRB-TX(65:1) DRB-TX(69:1)
                                          DRB-TX(73:1) DRB-TX(77:1)
                                          DRB-TX(81:1) DRB-TX(85:1)
                              IF AUFDRU
                                  MOVE "Preis inkl." TO DRB-TX(58:11).
           IF WH-KEY = 7 and MATDRU MOVE "ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒ
      -        "ƒø" TO DRB-TX(58:32).
           IF WH-KEY = 7 and AUFDRU MOVE "ƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒƒ¬ƒƒ
      -        "ƒø" TO DRB-TX(58:32).
           IF WH-KEY = 9 and MATDRU MOVE "ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒ
      -        "ƒ¥" TO DRB-TX(58:32)
               MOVE "≈" TO DRB-TX(57:1)
               MOVE "√" TO DRB-TX(1:1).
           IF WH-KEY = 9 and AUFDRU MOVE "ƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒƒ≈ƒƒ
      -        "ƒ¥" TO DRB-TX(58:32)
               MOVE "≈" TO DRB-TX(57:1)
               MOVE "√" TO DRB-TX(1:1).
           PERFORM DRUCK.
           IF WH-KEY < 9 ADD 1 TO WH-KEY GO I.
           IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
               MOVE "(s0p0s0b0T" TO DRA-CD
               PERFORM DRUCK.
           IF ANBDRU MOVE 1 TO IP.
       Z.  EXIT.
      ******************************************************************
       XMLKOPF SECTION.
       A.  OPEN INPUT XML-DAT.
           MOVE 0 TO WM-TOP.
       C.  READ XML-DAT AT END GO X.
           IF XD-SATZ = SPACE GO C.
           IF XD-SATZ(1:7) = ";Toppos" MOVE 9999 TO WM-TOP
                                       GO C.
           IF WM-TOP = 9999 and XD-SATZ(19:4) = "top="
               MOVE XD-SATZ(24:4) TO WM-TOP WX-TOP.
           IF WZ-SEITE > 0;
               IF XD-SATZ(1:5) = "<?xml" or "<docu" GO C.
           IF XD-SATZ(1:10) = "</document" GO X.
           MOVE XD-SATZ TO XM-SATZ.
           IF XD-SATZ(33:10) = "<p>$Zeilen" GO X.
           IF XD-SATZ(33:9) = "<p>$Name " MOVE 1 TO WQ.
      *        MOVE "<p> </p>" TO XM-SATZ(1:) GO Q.
           IF WQ > 0 MOVE "<p>" TO XM-SATZ(1:)
               IF WH-GABNAM = "GABKOPF.XML"
                   MOVE WR-ADR(WQ) TO XM-SATZ(4:)
               else MOVE WT-BEZ TO XM-SATZ(4:)
               end-if
               INSPECT XM-SATZ REPLACING ALL "ö" BY "‹" "ô" BY "÷"
                                  "é" BY "ƒ" "Å" BY "¸" "î" BY "ˆ"
                                  "Ñ" BY "‰" "·" BY "ﬂ"
               MOVE "</p>" TO XM-SATZ(96:)
               ADD 1 TO WQ
               IF WQ = 6 or WH-GABNAM not = "GABKOPF.XML"
                   MOVE 0 TO WQ
      *            MOVE "</text>" TO XM-SATZ
      *            WRITE XM-SATZ
               end-if
               GO Q.
           IF XD-SATZ(33:7) not = "<p>Nr.:" GO Q.
           MOVE "<p>Nr:" TO DRB-ADR(23:).
           MOVE AK-NUM TO DRB-RENUM.
           MOVE "/" TO DRB-STR.
           DIVIDE 10 INTO AK-KTONR GIVING DRB-KTONR.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-GZDAT TO DRB-ADR(66:).
           MOVE "</p>" TO DRB-ADR(93:).
           MOVE DRB-ADR TO XM-SATZ(8:).
           MOVE SPACE TO DRA-SATZ.
       Q.  WRITE XM-SATZ.
           GO C.
       X.  CLOSE XML-DAT.
       Z.  EXIT.
      ************************************************ ob Druckteilung *
       OB-TEILG SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 80
               IF DRA-SATZ(WX:1) = "*" MOVE DRA-SATZ(WX:) TO WT-TX
                   MOVE "(s12v0s3b4101T" TO DRA-SATZ(WX:)
                   ADD 15 TO WX
                   MOVE WT-TX(2:) TO DRA-SATZ(WX:)
                   MOVE 84 TO WX.
       Z.  EXIT.
      *****************************************************************
       UEBERTRAG SECTION.
       A.  IF WZ-ZEILEN < 57 GO Z.
           IF WS-BET not = 0;
               IF IP = 1 MOVE 2 TO IP
                     MOVE ALL x"2d" TO DRC-STR
                     MOVE 1615 TO WX-LEFT
                     PERFORM XML-ZEIL
                else MOVE ALL "ƒ" TO DRC-STR.
           MOVE 1 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF IP = 2 MOVE 1 TO IP
                     MOVE 700 TO WX-LEFT.
           MOVE "ö b e r t r a g  " TO DRC-SATZ(35:18).
           IF WS-BET NOT = 0 MOVE WS-BET TO DRC-BETRAG.
           IF WL = 1 SUBTRACT WS-TEIL FROM WS-BET GIVING DRC-BETRAG.
           IF MATDRU or AUFDRU
               MOVE "Fortsetzung nÑchste Seite" TO DRC-SATZ(35:).
           PERFORM DRUCK.
           IF WM-DRU = 1 PERFORM DUPLOS
           else PERFORM PAGEDRU.
           IF ANBDRU MOVE "<page/>" TO XM-SATZ
                     WRITE XM-SATZ
                     MOVE 0 TO IP.
           MOVE 0 TO WZ-ZEILEN.
           MOVE "GABUEB.XML" TO WH-GABNAM.
           PERFORM FAKADR.
           MOVE "ö b e r t r a g  " TO DRC-SATZ(35:18).
           IF WS-BET NOT = 0 MOVE WS-BET TO DRC-BETRAG.
           IF WL = 1 SUBTRACT WS-TEIL FROM WS-BET GIVING DRC-BETRAG.
           IF MATDRU or AUFDRU MOVE "Fortsetzung" TO DRC-SATZ(35:)
               MOVE "≥" TO DRB-TX(1:1) DRB-TX(57:1) DRB-TX(69:1)
                           DRB-TX(73:1) DRB-TX(77:1) DRB-TX(81:1)
                           DRB-TX(85:1) DRB-TX(89:1)
               PERFORM DRUCK
               MOVE 0 TO WZ-SCHALT
               MOVE "=" TO DRA-SATZ(3:) PERFORM DRUCK     *> 1/2 Zeile
               MOVE 0 TO WZ-SCHALT
               MOVE ALL "ƒ" TO DRB-TX
               MOVE "¬" TO DRB-TX(57:1) DRB-TX(69:1) DRB-TX(73:1)
                           DRB-TX(77:1) DRB-TX(81:1) DRB-TX(85:1)
               IF MATDRU MOVE "¬" TO DRB-TX(61:1) DRB-TX(65:1)
               end-if
               MOVE "¥" TO DRB-TX(89:)
               MOVE "√" TO DRB-TX(1:1)
               ADD 0,5 TO WZ-ZEILEN.
           PERFORM DRUCK.
       Z.  EXIT.
      ****************************************** Ausdruck einer Zeile **
       ZEIDRUCK SECTION.
       A.  PERFORM UEBERTRAG.
           MOVE 0 TO WE-PREIS.
           IF AU-BEZ(1) not = SPACE MOVE 320 TO WX-LEFT.
           IF AU-POS = 0 MOVE 1 TO WZ-SCHALT;
               IF AU-TZ = 0 MOVE 2 TO WZ-SCHALT;
                   IF WZ-ZEILEN > 55 MOVE 60 TO WZ-ZEILEN GO A.
           IF AU-ARNUM > 0 MOVE AU-ARNUM TO WV-ARNUM
               MOVE 150 TO WX-LEFT
               COMPUTE DRC-ARNUM = AU-KAP * 10000 + AU-POS.
           IF WZ-SEITE < 2; IF WZ-ZEILEN < 25 MOVE 1 TO WZ-SCHALT.
           MOVE AU-BEZ(1) TO WT-ADR.
           MOVE AU-BEZ(2) TO WR-ADR(2).
           IF WR-ADR(1) NOT = SPACE MOVE WR-ADR(1) TO DRC-BEZ.
           IF WR-ADR(2) NOT = SPACE PERFORM DRUCK
               MOVE WR-ADR(2) TO DRC-BEZ
               MOVE 320 TO WX-LEFT.
           IF AU-FOLGE = 0 OR AU-FOLGE =  32 GO H.
           MOVE AU-ARNUM TO ZU-NUM.
       D.  READ ZUSATZ INVALID GO H.
           IF ZUGRIF PERFORM BESETZT GO D.
           PERFORM VARYING IX FROM 15 BY -1 UNTIL IX = 0
               OR ZU-BEZ(IX) NOT = SPACE CONTINUE.
           IF IX = 0 GO H.
           ADD 1 TO IX.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI = IX
               IF DRC-BEZ NOT = SPACE MOVE 320 TO WX-LEFT
                                      PERFORM DRUCK
                                      PERFORM UEBERTRAG
               end-if MOVE ZU-BEZ(WI) TO DRC-BEZ.
       H.  IF AU-POS = 0; IF AU-TZ = 0 MOVE AU-MEH TO WL
               END-IF GO X.
           IF AU-ART < 6 GO X.
           IF AU-ANZ NOT = 0 MOVE AU-ANZ TO WH-ANZ
               ADD 1 AU-MEH GIVING WH-MEH
               PERFORM MG
               MOVE WD-MENGE TO DRC-MENGE
               MOVE WT-MEH(WH-MEH) TO DRC-MEH
               COMPUTE WE-PREIS rounded = AU-PREIS / 13,7603
               MOVE AU-PREIS TO DRC-PREIS.
           IF AU-RAB NOT = 0 MULTIPLY -1 BY AU-RAB GIVING DRC-PROZ.
           IF AU-ART = 9 MOVE "alterantiv" TO DRC-STR GO Q.
           MOVE AU-BET TO DRC-BETRAG WK-BET
           ADD AU-BET TO WS-BET WS-TEIL.
           IF AU-KAGRP > 1 ADD AU-BET TO WS-RAB;
               IF AK-RAB NOT = 0
                   COMPUTE WH-WERT = WK-BET * AK-RAB / 100
                   SUBTRACT WH-WERT FROM WK-BET.
           ADD WK-BET TO WS-NET.
           SUBTRACT AU-BET FROM WS-NET.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
           SET AX TO AU-KAGRP.
           IF AX > 15 SET AX TO 15.
           ADD WK-BET TO AK-SUM(AX).
           ADD WK-BET TO WT-BASIS(AU-UST).
           ADD WK-BET TO WS-KAPSUM.
           MULTIPLY WH-ANZ BY AU-EKP GIVING WH-BET ROUNDED.
           IF AX > 1 ADD WH-BET TO AK-SUM(17).
       Q.  MOVE WT-UST(AU-UST) TO DRC-UST.
       X.  IF WL = 1 MOVE SPACES TO DRC-SATZ(64:32);
               IF AU-ART = 9 MOVE "alterantiv" TO DRC-STR.
           PERFORM DRUCK.
           IF AU-POS = 0 MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       AUFENDE SECTION.
       A.  MOVE 0 TO WX-TASTE.
           IF IP = 1 MOVE 2 TO IP
                     MOVE ALL x"2d" TO DRC-STR
                     MOVE 1615 TO WX-LEFT
                     PERFORM XML-ZEIL.
           MOVE ALL "ƒ" TO DRC-STR.
           PERFORM DRUCK.
           IF IP = 2 MOVE 5 TO IP
                     MOVE WS-BET TO DRC-BETRAG
                     MOVE 1880 TO WX-LEFT
                     PERFORM XML-ZEIL
                     MOVE 2 TO IP.
           MOVE WS-BET TO DRC-BETRAG.
           PERFORM DRUCK.
       B.  IF VDU-L > 16 PERFORM B IN BS-ZEIL GO B.
           MOVE VDU-L TO WH-L.
           MOVE 66 TO VDU-P.
           MOVE ALL "ƒ" TO DRC-STR.
           DISPLAY DRC-STR AT VDU-LP.
           MOVE SPACE TO DRA-SATZ.
           PERFORM BS-ZEIL.
           MOVE WS-BET TO WD-BET.
           MOVE 66 TO VDU-P.
           DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP.
           PERFORM BS-ZEIL.
           IF IP = 2 MOVE 1 TO IP
               MOVE 570 TO WX-LEFT.
           IF AK-RAB NOT = 0; IF WS-RAB NOT = 0
               MOVE "SONDERNACHLASS VON" TO DRD-BEZ
               MOVE AK-RAB TO DRD-PROZ
               MOVE "%" TO DRD-PZ
               MOVE WS-NET TO DRC-BETRAG WD-BET WH-BET
               MOVE WS-RAB TO DRD-BASIS
               DISPLAY DRD-MWST
               MOVE 66 TO VDU-P
               DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP
               PERFORM DRUCK
               PERFORM BS-ZEIL
               ADD WH-BET TO WS-BET.
       C.  MOVE 1 TO WX.
           MOVE WZ-ZEILEN TO WZ.
       F.  IF WT-BASIS(WX) = 0 GO G.
           MOVE WT-BASIS(WX) TO WH-BET.
           COMPUTE WH-BET ROUNDED = WT-BASIS(WX) * WT-UST(WX) / 100.
           IF WH-BET = 0 MOVE SPACE TO DRD-SATZ GO G.
           MOVE WT-UST(WX) TO DRD-PROZ.
           MOVE "%" TO DRD-PZ.
           MOVE "MEHRWERTSTEUER VON" TO DRD-BEZ.
           MOVE WT-BASIS(WX) TO DRD-BASIS.
           MOVE WH-BET TO DRC-BETRAG WD-BET.
           ADD WH-BET TO WH-MWST.
           ADD WH-BET TO WS-BET.
           DISPLAY DRD-MWST.
           MOVE 66 TO VDU-P.
           DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
       G.  IF WX < 6 ADD 1 TO WX GO F.
           MOVE WS-BET TO WK-BET AK-SUM(16).
           IF WZ-ZEILEN = WZ GO L.
           IF IP = 1 MOVE 2 TO IP
                     MOVE ALL x"2d" TO DRC-STR
                     MOVE 1615 TO WX-LEFT
                     PERFORM XML-ZEIL.
           MOVE ALL "ƒ" TO DRC-STR.
           DISPLAY DRC-STR AT VDU-LP.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
           IF WH-PG = 2 MOVE WH-TX TO DRB-ADR.
           MOVE WS-BET TO DRC-BETRAG WD-BET.
           DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP.
           IF IP = 2 MOVE 5 TO IP
                     MOVE WS-BET TO DRC-BETRAG
                     MOVE 1880 TO WX-LEFT
                     PERFORM XML-ZEIL
                     MOVE 2 TO IP.
           PERFORM DRUCK.
           PERFORM BS-ZEIL.
       L.  MOVE ALL "=" TO DRC-STR.
           IF IP = 2 MOVE 1 TO IP
                     MOVE 1615 TO WX-LEFT.
           MOVE 0 TO WS-BET.
           PERFORM DRUCK.
           PERFORM DRUCK.
           MOVE 160 TO WX-LEFT.
      *------------------------------------------> Angebotsschlu·text <-
           IF AK-TXKEY = SPACE GO O.
           MOVE AK-TXKEY TO WH-TXKEY.
           OPEN INPUT ANTEXT.
           IF WF-STATUS not = "00" GO O.
       M.  READ ANTEXT NEXT NO LOCK AT END CLOSE ANTEXT GO O.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE TE-SATZ TO DRE-BEZ.
           PERFORM UEBERTRAG.
           PERFORM DRUCK.
           GO M.
       O.  MOVE AK-KOND TO WH-KK.
           MOVE 2 TO WZ-SCHALT.
           IF IP not = 0 ADD 42,5 TO WX-TOP.
           IF WH-TGN = 0 GO R.
           IF WH-SK1 = 0 GO Q.
           MOVE " % Skonto bei Zahlung innerhalb von    Tagen, das sind
      -        "Eur" TO DRV-TX.
           MOVE WH-SK1 TO DRV-SK1.
           MOVE WH-TG1 TO DRV-TG1.
           COMPUTE WH-WERT = WK-BET * WH-SK1 / 100.
           MOVE WH-WERT TO DRV-BET.
           PERFORM DRUCK.
           MOVE "ansonsten erbitten wir Ihre Zahlung innerhalb von    Ta
      -        "gen ohne Abzug." TO DRV-TX.
       Q.  IF WH-SK1 = 0 MOVE "          Wir erbitten Ihre Zahlung inner
      -        "halb von    Tagen ohne Abzug." TO DRV-TX.
           MOVE WH-TGN TO DRV-TGN.
           PERFORM DRUCK.
       R.  IF WM-DRU = 1 PERFORM DUPLOS.
           PERFORM END-DRU.
           REWRITE AK-SATZ.
      *    MOVE "</text>" TO XM-SATZ.
      *    WRITE XM-SATZ.
           MOVE "</document>" TO XM-SATZ.
           WRITE XM-SATZ.
           CLOSE XMLDRUCK.
           IF not ANBDRU DELETE FILE ABLAGE.
      *    else MOVE 0 TO WH-MOD
      *         MOVE "ABLAGE.LST" TO WH-DRUNAM
      *         PERFORM DUPLOS.
       Z.  EXIT.
      ************************************************** Kapitelsummen *
       KAP-SUM SECTION.
       A.  MOVE "Kapitelsumme:" TO DRC-SATZ(26:13).
           MOVE WS-KAPSUM TO DRC-PREIS.
           MOVE 510 TO WX-LEFT.
           PERFORM DRUCK.
           MOVE 0 TO WS-KAPSUM.
           MOVE AU-KAP TO WV-KAP.
       Z.  EXIT.
