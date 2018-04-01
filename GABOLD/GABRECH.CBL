      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABRECH.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                               CURSOR IS VDU-AB.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEART.CPY.
           COPY GABSEDEB.CPY.
           COPY GABSEKON.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSESTA.CPY.
           SELECT BUCHFAKT   ASSIGN TO "GABISFIB.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BF-KEY
                             ALTERNATE KEY BF-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
           SELECT ABLAGE     ASSIGN TO WH-ABLNAM
                             ORGANIZATION RECORD SEQUENTIAL
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
       COPY GABARTIK.CPY.
       COPY GABKONST.CPY.
       COPY GABSTATI.CPY.
       COPY GABANBOT.CPY.
       COPY GABBUCH.CPY.
      ******************************************************************
       FD  ABLAGE                      LABEL RECORD OMITTED.
       01  ABA-SATZ.
           03 ABA-SCHALT               PIC 99.
           03 ABA-REST                 PIC X(132).
      ******************************************************************
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
           03  FILLER                  PIC X(16).
           03  DRB-ADR.
               05 FILLER               PIC X(30).
               05 DRB-RENUM            PIC ZZ.ZZ9-.
               05 DRB-STR              PIC X.
               05 DRB-KTONR            PIC ZZ.ZZ9,9.
               05 FILLER               PIC X(10).
               05 DRB-ORT              PIC X(26).
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
               05 FILLER               PIC X(50).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-P                    PIC 99      COMP.
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88 AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  VDU-AB                  PIC 9999.
           03  VR REDEFINES VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WK-TASTE                PIC 99        COMP.
           03  WH-VON                  PIC 9999      COMP.
           03  WK-VON                  PIC 9999      COMP.
           03  WX-FIL                  PIC X(37).
           03  WH-TX                   PIC X(51).
           03  WK-TX                   PIC X(51).
           03  WK-RM                   PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
               88  MANUEL   VALUE 0.
           03  WK-MON                  PIC 9(6)      COMP.
           03  WX-M                    PIC 99        COMP.
           03  WX-J                    PIC 99        COMP-X.
           03  WX-R                    PIC 99        COMP.
           03  WH-RAB                  PIC S99V9     COMP-3.
           03  WK-RAB                  PIC S99V9     COMP-3.
           03  WV-ARNUM                PIC 9(6)      COMP.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WH-ABDAT                PIC 9(6)      COMP.
           03  WH-BISDAT               PIC 9(6)      COMP.
           03  WD-BASIS                PIC ZZZ.ZZ9,99-.
           03  WD-PZ                   PIC Z9,99.
      *    03  WE-PREIS                PIC S9(5)V99   COMP-3.
           03  WS-SUMMEN.
               05 WS-SUM               PIC S9(7)V99   COMP-3
                                           OCCURS 19 INDEXED AY.
               05 WS-ERL               PIC S9(7)V99   COMP-3 OCCURS 19.
           03  WH-ANZ                  PIC S9(8)V99  COMP-3.
           03  WA-ANZ                  PIC 9(6)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(6).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9(5)V9.
           03  WH-ANZ2                 PIC S9(4)V99.
           03  WH-MENGE                PIC 9(5)      COMP-3.
           03  WH-ART                  PIC 99        COMP-3.
           03  WH-L                    PIC 99        COMP.
           03  WH-VKF                  PIC 99        COMP.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZZ9,9-.
           03  WD-MGC                  PIC ZZZ9,99-.
           03  WD-MG                   PIC X(8).
           03  WD-ARTIK                PIC X(30).
           03  WD-ART                  PIC ZZ.9999.
           03  WD-PROZ                 PIC Z9.
           03  WD-KZ                   PIC 9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WH-PREIS                PIC 9(6)V999   COMP-3.
           03  WT-BASIS                PIC S9(7)V99   COMP-3 OCCURS 6.
           03  WH-USTKZ                PIC 9.
           03  WH-BET                  PIC S9(9)V99   COMP-3.
           03  WK-BET                  PIC S9(9)V99   COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WS-NET                  PIC S9(9)V99   COMP-3.
           03  WS-RAB                  PIC S9(9)V99   COMP-3.
           03  WH-MWST                 PIC S9(9)V99   COMP-3.
           03  WS-VKF                  PIC S9(9)V99   COMP-3.
           03  WH-MEH                  PIC 99         COMP.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-MA                PIC 99         COMP.
               05 WM-KO                PIC 99         COMP.
               05 WM-PR                PIC 99         COMP.
               05 WM-RB                PIC 99         COMP.
           03  WK-ANR                  PIC 99         COMP.
      *-----------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WX-ZEI                  PIC X(80).
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
      *--------------------------------------------> check_file_exist <-
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)    COMP-X.
               05 WF-TT                PIC 99      COMP-X.
               05 WF-MO                PIC 99      COMP-X.
               05 WF-YY                PIC 9999    COMP-X.
               05 WF-HH                PIC 99      COMP-X.
               05 WF-HO                PIC 99      COMP-X.
               05 WF-SS                PIC 99      COMP-X.
               05 WF-HS                PIC 99      COMP-X.
           03  WK-UID                  PIC X(15).
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON AUFTRAG.
       A.  CALL "CADECL" USING "GABISAUF.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "DEBITORE.FII" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING "GABISTAT.DAT" WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON BUCHFAKT.
       A.  CALL "CADECL" USING "GABISFIB.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE WE-KOPF(1) TO WM-KOPF.
           MOVE WE-WO(1) TO WM-WO.
           MOVE "FAKTURA.LST" TO WH-DRUNAM.
           MOVE "ABLAG.LST" TO WH-ABLNAM.
           IF WM-DRU = 0 or WM-WO = 9 MOVE "LPT1" TO WH-DRUNAM.
           EVALUATE WL-CA
               WHEN 98
               WHEN 99 PERFORM AUTFAKT
               WHEN 20 PERFORM FAKTURA.
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
       A.  MOVE 0000 TO VDU-AB
           DISPLAY ", " AT VDU-AB
           DISPLAY "weiter mit <ret>: " WITH HIGHLIGHT AT VDU-AB.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT VDU-AB.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 20 GO Z.
       B.  CALL "CAUP" USING "1407012180000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
           SUBTRACT 1 FROM WH-L.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden" AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       PAGEDRU SECTION.
       A.  WRITE DRA-SATZ AFTER PAGE.
           IF WM-DRU = 1 MOVE 99 TO ABA-SCHALT
                         MOVE SPACE TO ABA-REST
                         WRITE ABA-SATZ.
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
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
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
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  PERFORM PAGEDRU.
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
           else OPEN EXTEND DRUCKER ABLAGE.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ
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
      *****************************************************************
       DISP-MENGE SECTION.
       A.  MOVE WH-ANZ TO WA-ANZ.
           IF WT-NK(WH-MEH) = 0 GO C.
           IF WT-NK(WH-MEH) = 1 GO B.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ2 ON SIZE ERROR GO X.
           MOVE WH-ANZ2 TO WD-ANZ2 WD-MGC.
           MOVE WD-ANZ2 TO WD-MENGE.
           MOVE WD-MGC  TO WD-MG.
           GO E.
       B.  IF WR-A2 NOT = 0 GO X.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ1 ON SIZE ERROR GO X.
           MOVE WH-ANZ1 TO WD-ANZ1 WD-MGB.
           MOVE WD-ANZ1 TO WD-MENGE.
           MOVE WD-MGB  TO WD-MG.
           GO E.
       C.  IF WH-ANZ > 999999 OR WH-ANZ < -999999 GO X.
           IF WR-A2 NOT = 0 AND WR-A1 NOT = 0 GO X.
           MOVE WH-ANZ TO WD-ANZ WD-MGA.
           MOVE WD-ANZ TO WD-MENGE.
           MOVE WD-MGA TO WD-MG.
       E.  DISPLAY WD-MG WITH HIGHLIGHT AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER
               MOVE 99 TO WX-TASTE.
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
      ************************************************ Auftragsfaktura *
       AUTFAKT SECTION.
       A.  DISPLAY " Auftragsfakturierung" AT 0101.
           MOVE 0 TO WK-RM AU-POS AU-TZ AU-KAP WX-R WZ-SEITE.
           IF WL-CA = 98 MOVE 1 TO WK-RM
               MOVE 99 TO WL-CA.
           MOVE 1 TO WH-MOD.
           CALL "CAUP" USING "1307102580" WH-CREG.
           MOVE AK-KTONR TO DE-KTONR.
       B.  READ DEBITOR NO LOCK INVALID MOVE AK-AKEY TO DE-MCODE.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE AK-ANR TO WK-ANR.
           MOVE AK-BEZ TO DE-BEZ.
           MOVE AK-RAB TO WK-RAB.
           MOVE AK-VKF TO WH-VKF.
           MOVE DE-UID TO WK-UID.
           PERFORM FAKTKOPF.
           MOVE 7 TO VDU-L.
           MOVE WH-RENUM TO AK-RENUM.
           MOVE WM-DATUM TO AK-REDAT.
           START AUFTRAG KEY NOT < AU-KEY INVALID KEY GO Z.
       C.  READ AUFTRAG NEXT AT END GO X.
           IF AU-NUM NOT = AK-NUM GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
      *    MOVE 0 TO WE-PREIS.
       D.  PERFORM UEBERTRAG.
           IF AU-POS = 0 MOVE 1 TO WZ-SCHALT;
               IF AU-TZ = 0 MOVE 2 TO WZ-SCHALT;
                   IF WZ-ZEILEN > 55 MOVE 60 TO WZ-ZEILEN GO D.
           IF WZ-SEITE < 2; IF WZ-ZEILEN < 25 MOVE 1 TO WZ-SCHALT.
      *----------------------------------------> Ausdruck einer Zeile <-
           IF AU-ARNUM > 0 MOVE AU-ARNUM TO WV-ARNUM AR-NUM
               COMPUTE DRC-ARNUM = AU-KAP * 10000 + AU-POS.
           MOVE AU-BEZ(1) TO WT-ADR.
           MOVE AU-BEZ(2) TO WR-ADR(2).
           IF WR-ADR(1) NOT = SPACE MOVE WR-ADR(1) TO DRC-BEZ.
           IF WR-ADR(2) NOT = SPACE PERFORM DRUCK
               MOVE WR-ADR(2) TO DRC-BEZ.
           IF AU-POS = 0 GO Q.
           IF AU-ART < 6 GO Q.
           IF WK-RM = 1 MULTIPLY -1 BY AU-ANZ
                        MULTIPLY -1 BY AU-BET.
           IF AU-ANZ NOT = 0 MOVE AU-ANZ TO WH-ANZ
               ADD 1 AU-MEH GIVING WH-MEH
               PERFORM MG
               MOVE WD-MENGE TO DRC-MENGE
               MOVE WT-MEH(WH-MEH) TO DRC-MEH
      *        COMPUTE WE-PREIS rounded = AU-PREIS * 13,7603
               MOVE AU-PREIS TO DRC-PREIS.
           IF AU-RAB NOT = 0 MULTIPLY -1 BY AU-RAB GIVING DRC-PROZ.
           IF AU-BET = 0 AND AU-ANZ = 0 GO Q.
           MOVE AU-BET TO DRC-BETRAG WK-BET.
           ADD AU-BET TO WS-BET.
           IF AU-KAGRP > 1 ADD AU-BET TO WS-RAB;
               IF AK-RAB NOT = 0
                   COMPUTE WH-WERT = WK-BET * AK-RAB / 100
                   SUBTRACT WH-WERT FROM WK-BET.
           ADD WK-BET TO WS-NET.
           SUBTRACT AU-BET FROM WS-NET.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
           SET AX AY TO AU-KAGRP.
           IF AX > 15 SET AX AY TO 15.
           ADD WK-BET TO AK-SUM(AX) WS-ERL(AX).
           ADD WK-BET TO WT-BASIS(AU-UST).
           MOVE WT-UST(AU-UST) TO DRC-UST.
           MOVE AU-EKP TO AR-EKP.
           PERFORM ADDSTAT.
       Q.  PERFORM DRUCK.
           IF AU-POS = 0 MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WH-RAB.
           GO C.
       X.  PERFORM FAKTENDE.
           IF WM-DRU = 1 PERFORM DUPLOS.
           UNLOCK AUFTRAG.
           MOVE WS-BET TO AK-SUM(16).                   *> Summe gesamt
           IF WK-RM = 1 MOVE 0 TO AK-RENUM.
           REWRITE AK-SATZ.
           UNLOCK AUFKOPF.
       Y.  MOVE "GABAUF  15AUFT" TO WT-TX.
       Z.  EXIT.
      ********************************************* man. Fakturierung *
       FAKTURA SECTION.
       A.  MOVE "  Rechnungen" TO WK-GEB.
           MOVE 0 TO WM-DATUM.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM PERPRUEF.
           IF ESC GO W.
       C.  DISPLAY "Kunden-Nr.:          " AT 0301.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           MOVE 0 TO DE-KTONR WM-KTONR WV-ARNUM WH-MOD
                     WZ-SEITE WZ-ZEILEN.
           MOVE LOW-VALUES TO AK-SATZ.
           DISPLAY "<esc>= Ende, alpha+<ret>= Kunden suchen, <ret>= Kund
      -        "en-Nr., <>= zurÅck" AT 2301.
           DISPLAY "'*'+Kd.Nr.+<ret>= man. Adresse, <Bild- >= Druckabfra
      -        "ge" AT 2401.
           CALL "CAUP" USING "0003136010" WH-CREG.
           IF HELP CALL "CAUP" USING "190011000908" WH-CREG.
           IF ESC GO W.
           IF WOLI GO B.
           IF BAUF PERFORM OB-ABLAG GO C.
           IF NOT RET GO C.
           IF FEHLER GO C.
           MOVE WV-MCODE TO WH-X.
           IF WH-X = "*" MOVE SPACE TO WV-MCODE.
           IF WH-NUM = 0; IF WV-MCODE = SPACE
               DISPLAY "Kunden-Nr. muss sein" AT 2401
               PERFORM WEITER GO C
           ELSE CALL "GABANZ" USING "01DEBSUCH" WH-CREG
                CANCEL "GABANZ".
           MOVE WH-NUM TO DE-KTONR WD-KZ.
           IF WD-KZ NOT = 2 GO C.
           MOVE 0 TO WM-MERKER.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
       D.  READ DEBITOR WITH NO LOCK NOT INVALID GO F.
           MOVE 1 TO WM-MERKER.
           CALL "GABDEBIT" USING "61DEBANL" WH-CREG.
           CANCEL "GABDEBIT".
           IF DE-KTONR = 0 GO B.
           IF WH-X NOT = "*" GO D.
       F.  IF ZUGRIF PERFORM BESETZT GO B.
           MOVE DE-BEZ TO WT-BEZ.
           IF ESC GO B.
           DISPLAY "Kunden-Nr.:" AT 0301.
           DISPLAY WD-KTO WITH HIGHLIGHT AT 0313.
           MOVE DE-BEZ TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           CALL "CAUP" USING "1304010480" WH-CREG.
           DISPLAY WX-ZEI AT 0401.
           MOVE 0 TO WX-R.
           MOVE DE-ANREDE TO WK-ANR.
       H.  DISPLAY "<ret-leer>= Rechnung, <esc>= Abbruch, <ret>= Rechnun
      -        "gsrabatt: " AT 2301.
931006*    DISPLAY "<Bild- >= zur Karteieintragung" AT 2401.
       K.  IF WX-R = 3 DISPLAY "*" WITH HIGHLIGHT AT 0279.
           CALL "CAUP" USING "0023632002" WH-CREG.
           IF ESC GO B.
           IF KIST MOVE 3 TO WX-R.
           IF TABL MOVE DE-KTONR TO WH-NUM
                CALL "GABDEBIT" USING "52DEBANZ" WH-CREG
                CANCEL "GABDEBIT" GO H.
931006*    IF BAB MOVE 49 TO WH-PG
931006*           MOVE "GABKART GABREC" TO WT-TX
931006*           CALL "CAUP" USING "0710100316000" WH-CREG
931006*           ADD 202 VDU-ECK GIVING VDU-LP
931006*           DISPLAY " bitte warten " AT VDU-LP
931006*           GO Z.
           IF NOT RET GO H.
           MOVE 0 TO WK-RAB.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO WK-RAB WD-PROZ
               DISPLAY "  Rabatt: " AT 0468
               DISPLAY WD-PROZ WITH HIGHLIGHT AT 0478 "%".
           MOVE DE-ANREDE TO WK-ANR.
           PERFORM FAKTKOPF.
           PERFORM FAKTZEIL.
           PERFORM FAKTENDE.
           GO B.
      *------------------------------------------> Ende Fakturierung <-
       W.  IF WM-DRU = 1 PERFORM DUPLOS.
       Z.  EXIT.
      ********************************* ob alte Ablage-Kopie vorhanden *
       OB-ABLAG SECTION.
       A.  IF WM-DRU not = 1 GO Z.
           IF WM-OPEN = 1 CLOSE DRUCKER
               MOVE 0 TO WM-OPEN.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING "ABLAG.LST " WH-FDET.
           IF WF-SIZE = 0 GO Z.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
           OPEN INPUT ABLAGE.
           PERFORM DUPLOS.
       Z.  EXIT.
      ******************************************************************
       DUPLOS SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           CLOSE ABLAGE.
           OPEN INPUT ABLAGE WITH LOCK.
           ADD 2 TO WM-DRU.
       C.  READ ABLAGE AT END GO Q.
           MOVE ABA-REST TO DRA-SATZ.
           IF ABA-SCHALT = 99 WRITE DRA-SATZ AFTER PAGE GO C.
           MOVE ABA-SCHALT TO WZ-SCHALT.
           PERFORM DRUCK.
           GO C.
       Q.  CLOSE ABLAGE.
           DELETE FILE ABLAGE.
           IF WM-DRU = 1 PERFORM END-DRU
           else CLOSE DRUCKER ABLAGE
                MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ************************************************ Rechnungsdatum *
       PERPRUEF SECTION.
       A.  DISPLAY "Re-Datum: " AT 0339.
           IF WM-DATUM NOT = 0 MOVE WM-DATUM TO WZ-DATUM GO D.
       B.  DISPLAY "<ret-leer>= letztes Datum, <esc>= Abbruch, <ret>= Da
      -        "tum" AT 2301.
           MOVE WH-DATUM TO WZ-DATUM WC-DATUM WM-DATUM.
           CALL "CAUP" USING "0103496006" WH-CREG.
           IF ESC GO Z.
           IF WZ-DATUM = 0 GO B.
           MOVE WZ-DATUM TO WM-DATUM.
       D.  MOVE WZ-JAHR TO WX-J.
           MOVE WH-DATUM TO WZ-DATUM.
           IF WX-J < (WZ-JAHR - 1) OR WX-J > WZ-JAHR GO B.
           IF WX-J < 20 ADD 100 TO WX-J.
           MOVE WZ-MONAT TO WX-M.
           MOVE WM-DATUM TO WZ-DATUM WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0349.
       I.  DISPLAY "Vkf.:   " AT 0359.
           DISPLAY "Bezeichnung:" AT 0446.
           CALL "CAUP" USING "0103652002" WH-CREG.
           IF ESC OR WOLI GO B.
           IF WH-NUM = 0 GO I.
           IF NOT RET GO I.
           MOVE WH-NUM TO WH-VKF AK-VKF.
           DISPLAY WH-VKF AT 0365.
           COMPUTE WH-KEY = WH-VKF + 11.
       K.  READ KONSTANT WITH NO LOCK NOT INVALID GO M.
           INITIALIZE KO-VSATZ.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0204590120" WH-CREG.
           IF ESC GO I.
           MOVE WT-TX TO KO-NAME.
           MOVE WH-VKF TO KO-NUM.
           WRITE KO-SATZ.
           UNLOCK KONSTANT.
       M.  IF ZUGRIF PERFORM BESETZT GO K.
           DISPLAY KO-NAME WITH HIGHLIGHT AT 0459.
       Z.  EXIT.
      ******************************************************************
       FAKTKOPF SECTION.
       A.  MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           IF WM-DRU = 0 MOVE "LPT1" TO WH-DRUNAM
                         MOVE 0 TO WH-P
                    else MOVE 10 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6)
                WH-NUM WH-MWST WS-BET WH-RAB WS-RAB WS-NET WS-VKF
                INITIALIZE AK-SUMMEN
                INITIALIZE WS-SUMMEN.
      *-----------------------------------------> Datum fÅr Statistik <-
           MOVE WM-DATUM TO WZ-DATUM.
           MOVE WZ-JAHR TO WX-J.
           MOVE WH-DATUM TO WZ-DATUM.
           IF WX-J < 20 ADD 100 TO WX-J.
           MOVE WZ-MONAT TO WX-M.
           MOVE 1 TO WH-KEY.
       C.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE KO-TX TO WK-TX.
           MOVE KO-MERK TO WK-M.
           IF WX-R < 2 ADD 1 TO KO-RENUM.
           MOVE KO-RENUM TO WH-RENUM.
           REWRITE KO-SATZ.
           MOVE 6 TO WH-KEY.
       D.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF KO-ERST = 0 MOVE WH-RENUM TO KO-ERST
               MOVE WM-DATUM TO KO-DATUM.
           IF WX-R < 2 REWRITE KO-SATZ.
           UNLOCK KONSTANT.
           PERFORM FAKADR.
           IF WL-CA NOT = 99
               MOVE WH-RENUM TO WD-NUM
               CALL "CAUP" USING "1300000080" WH-CREG
               DISPLAY " Re-Nr.:" AT 0321
               DISPLAY "Re-Datum: " AT 0339
               DISPLAY WD-NUM WITH HIGHLIGHT AT 0330
               MOVE WM-DATUM TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM WITH HIGHLIGHT AT 0349.
           DISPLAY "Artikel B e z e i c h n u n g            Menge Meh
      -        "    Preis  %  Eur Betrag  U" AT 0501.
           MOVE 6 TO VDU-L.
           MOVE ALL "ƒ" TO WX-ZEI.
           DISPLAY WX-ZEI AT 0601.
       Z.  EXIT.
      *****************************************************************
       FAKADR SECTION.
       A.  IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT ABA-SCHALT
               MOVE "&l26a8H" TO ABA-REST         *> gelbe Kopie Fach 1
               WRITE ABA-SATZ
               ADD 5 TO WM-DRU                     *> nicht bei Kopie
               MOVE "&l26a1H" TO DRA-CD           *> w. Original Fach 2
               PERFORM DRUCK
               ADD -5 TO WM-DRU.
           IF WM-KOPF = 0 MOVE 7 TO WZ-SCHALT GO G.
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
           MOVE "&l6D" TO DRA-CD.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRB-SATZ(7:).
           PERFORM DRUCK.
           IF WZ-SEITE = 1 MOVE "Firmenbuch: 321/Leibnitz Handelsgericht
      -        " GRAZ" TO DRB-SATZ(51:).
       G.  PERFORM DRUCK.
           MOVE 4 TO WZ-SCHALT.
           IF WZ-SEITE > 0 MOVE 2 TO WZ-SCHALT
               MOVE "Seite: " TO DRB-SATZ(20:7)
               ADD 1 TO WZ-SEITE
               MOVE WZ-SEITE TO WD-TG
               MOVE WD-TG TO DRB-SATZ(27:3)
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
           else MOVE 1 TO WZ-SEITE.
           PERFORM DRUCK.
           MOVE DE-BEZ TO WT-BEZ.
           SET FX TO 5.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(2) TO WX-FIL.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT ABA-SCHALT.
           IF WM-DRU = 1 MOVE "(s1p18v0s0b4101TK o p i e(s0p0s0b4101T"
                            TO ABA-REST(68:)
               WRITE ABA-SATZ
               MOVE SPACE TO ABA-SATZ.
           MOVE WT-TXT(WK-ANR + 1) TO DRB-ADR.
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRB-ADR
               PERFORM DRUCK.
           MOVE 4 TO WZ-SCHALT.
           MOVE "Nr:" TO DRA-SATZ(27:4).
           IF WX-R NOT = 2 MOVE WH-RENUM TO DRB-RENUM
               MOVE "/" TO DRB-STR.
           DIVIDE 10 INTO DE-KTONR GIVING DRB-KTONR.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-GZDAT TO DRB-ORT.
           MOVE DRB-SATZ(47:) TO DRB-SATZ(32:).
           MOVE DRA-SATZ(27:) TO WT-TX.
           IF WM-DRU not = 1 MOVE X"1B213100" TO DRA-SATZ(1:4)
               MOVE "Rechnung" TO DRB-SATZ(14:)
               IF AK-UST = 1 MOVE "Teilrechnung" TO DRB-SATZ(9:)
               end-if
               IF AK-UST = 2 MOVE "Schlu·rechnung" TO DRB-SATZ(7:)
               end-if
               MOVE X"1B210100" TO DRA-SATZ(22:4)
               MOVE WT-TX TO DRA-SATZ(27:)
           else EVALUATE AK-UST
                WHEN 0 MOVE "(s1p18v0s2b4101TRechnung(s0p0s0b4101T"
                              TO DRA-SATZ(13:)
                WHEN 1 MOVE "(s1p18v0s2b4101TTeilrechnung(s0p0s0b4101T
      -                    "" TO DRA-SATZ(07:)
                WHEN 2 MOVE "(s1p18v0s2b4101TSchlu·rechnung(s0p0s0b410
      -                    "1T" TO DRA-SATZ(07:)
                end-evaluate
                MOVE WT-TX TO DRA-SATZ(54:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b6T" TO DRA-CD PERFORM DRUCK.
           MOVE 7 TO WH-KEY.
       I.  READ KONSTANT WITH NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF WL-CA = 99 AND WH-KEY = 8
               MOVE "Position" TO DRB-SATZ(9:8).
           MOVE KO-KOPF TO DRB-SATZ(7:).
           PERFORM DRUCK.
           IF WH-KEY < 9 ADD 1 TO WH-KEY GO I.
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p0s0b0T" TO DRA-CD PERFORM DRUCK.
       X.  PERFORM DRUCK.
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
       A.  IF WZ-ZEILEN < 58 GO Z.
           IF WS-BET NOT = 0 MOVE ALL "ƒ" TO DRC-STR.
           MOVE 1 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE "          U e b e r t r a g  " TO DRC-BEZ.
           IF WS-BET NOT = 0 MOVE WS-BET TO DRC-BETRAG.
           PERFORM DRUCK.
           PERFORM PAGEDRU.
           MOVE 0 TO WZ-ZEILEN.
           PERFORM FAKADR.
           MOVE "          U e b e r t r a g  " TO DRC-BEZ.
           IF WS-BET NOT = 0 MOVE WS-BET TO DRC-BETRAG.
           PERFORM DRUCK.
       Z.  EXIT.
      ***************************************** Lesen nach Artikel-Nr. *
       LIESART SECTION.
       A.  MOVE WX-TASTE TO WK-TASTE.
           IF KIST MOVE 5 TO WX-TASTE.
           IF NOT TABL GO G.
           CALL "CAUP" USING "1500006070" WH-CREG.
           IF WH-NUM = 0 GO Y.
           IF WK-TASTE = 6 GO D.
           MOVE WH-NUM TO AR-NUM.
       B.  READ ARTIKEL WITH NO LOCK KEY AR-KEY NOT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  MOVE VDU-LP TO VDU-AB.
           CALL "GABARTIK" USING "51ARTANL" WH-CREG.
           CANCEL "GABARTIK".
           MOVE VDU-AB TO VDU-LP.
           GO X.
       D.  MOVE WH-NUM TO AR-NUM WV-ARNUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO C.
       E.  READ ARTIKEL NEXT WITH NO LOCK AT END MOVE 999999 TO AR-NUM.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF AR-NUM = WV-ARNUM ADD 1 TO WV-ARNUM GO E.
           MOVE WV-ARNUM TO WH-NUM AR-NUM.
           GO C.
      *----------------------------------------> lesen nach Matchcode <-
       G.  READ ARTIKEL WITH NO LOCK KEY AR-AKEY INVALID
               MOVE 0 TO AR-NUM GO H.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF WF-STATUS NOT = 02
               MOVE AR-NUM TO WH-NUM GO X.
       H.  CALL "GABANZ" USING "02ARTSUCH" WH-CREG.
           CANCEL "GABANZ".
           MOVE AR-NUM TO WH-NUM.
           IF AR-NUM NOT = 0 GO B.
       X.  IF AR-NUM NOT = 0 GO Z.
       Y.  MOVE 99 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       FAKTZEIL SECTION.
       A.  PERFORM BS-ZEIL.
           MOVE VDU-L TO WH-L.
           PERFORM UEBERTRAG.
       B.  IF VDU-L > WH-L PERFORM B IN BS-ZEIL
               ADD 1 TO WH-L GO B.
           MOVE WH-L TO VDU-L.
           MOVE SPACE TO DRC-SATZ WT-ADR.
           UNLOCK ARTIKEL.
           MOVE 0 TO WK-RM.
           MOVE 1 TO VDU-P.
           CALL "CAUP" USING "1300012480" WH-CREG.
           DISPLAY "<ret>= Matchcode, <tab>= Art.Nr., <esc>= Ende, <#>=
      -        "Text" AT 2301.
           DISPLAY "nÑchster freier Artikel = Nr.+<#>, < >= Hilfe"
               AT 2401.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0200010114" WH-CREG.
           IF HELP CALL "CAUP" USING "19001200138" WH-CREG.
           IF ESC GO Z.
           IF RET AND WT-TX = SPACE GO B.
           MOVE WT-TX TO WV-MCODE AR-AKEY.
           IF KIST AND AR-AKEY = SPACE PERFORM ZUS-TEXT;
               IF VDU-L = WH-L GO B ELSE GO A.
           PERFORM LIESART
               IF FEHLER GO B.
       C.  IF VDU-L > 16 PERFORM B IN BS-ZEIL GO C.
       D.  MOVE AR-NUM TO WD-ART.
           MOVE 1 TO VDU-P.
           CALL "CAUP" USING "1300000080" WH-CREG.
           DISPLAY WD-ART AT VDU-LP.
           MOVE 9 TO VDU-P.
           MOVE AR-BEZA TO WR-ADR(1).
           MOVE AR-BEZB TO WR-ADR(2).
           SET TX TO 1.
           SUBTRACT 100 FROM VDU-LP GIVING VDU-AB.
           CALL "CAUP" USING "1300002480" WH-CREG.
           IF WT-ADR = SPACE GO F.
       E.  IF WR-ADR(TX) NOT = SPACE ADD 100 TO VDU-AB
               MOVE WR-ADR(TX) TO WD-ARTIK
               DISPLAY WD-ARTIK WITH HIGHLIGHT AT VDU-AB.
           IF TX < 6 SET TX UP BY 1 GO E ELSE GO G.
       F.  DISPLAY "Artikelbez.: <ret>= Zeilenende, <#>= Textende"
               AT 2301.
           CALL "CAUP" USING "0200090630" WH-CREG.
           IF WOLI OR ESC GO B.
       G.  MOVE WH-L TO VDU-L.
           MOVE 9 TO VDU-P.
           SUBTRACT 100 FROM VDU-LP.
           SET TX TO 1.
       H.  IF WR-ADR(TX) NOT = SPACE ADD 100 TO VDU-LP
               MOVE WR-ADR(TX) TO WD-ARTIK
               CALL "CAUP" USING "1300000080" WH-CREG
               DISPLAY WD-ARTIK WITH HIGHLIGHT AT VDU-LP
               IF TX < 6 SET TX UP BY 1 GO H.
           MOVE 0 TO WH-WERT WH-ANZ.
       I.  PERFORM VORANZ.
           DISPLAY "<ret>= Menge, <ret-leer>= zum Betrag, <tab>= Artikel
      -        " anzeigen"  AT 2301.
           MOVE WH-ANZ TO WH-WERT.
           IF WT-NK(WH-MEH) = 0 CALL "CAUP" USING "0000406007" WH-CREG.
           IF WT-NK(WH-MEH) = 1 CALL "CAUP" USING "0000405107" WH-CREG.
           IF WT-NK(WH-MEH) = 2 CALL "CAUP" USING "0000404207" WH-CREG.
           IF TABL MOVE AR-NUM TO WH-NUM
                CALL "GABARTIK" USING "52ARTANZ" WH-CREG
                CANCEL "GABARTIK" GO I.
           IF WOLI OR ESC MOVE 40 TO VDU-P
                CALL "CAUP" USING "1300000080" WH-CREG
                MOVE WH-L TO VDU-L GO F.
           IF FEHLER GO I.
           IF WK-RM = 1 MULTIPLY WH-WERT BY -1 GIVING WH-WERT.
           MOVE WH-WERT TO WH-ANZ.
           MOVE 40 TO VDU-P.
           IF WH-ANZ = 0 MOVE SPACE TO DRC-MEH
               CALL "CAUP" USING "1300000080" WH-CREG GO K.
           PERFORM DISP-MENGE.
           IF ESC GO H.
           IF WT-MEH(AR-MEH + 1) NOT = SPACE GO L.
       J.  DISPLAY "Mengeneinheiten-Kz. lt. Vorlaufdatei" AT 2301.
           MOVE 48 TO VDU-P.
           CALL "CAUP" USING "0000001001" WH-CREG.
           IF WOLI OR ESC GO I.
           IF NOT RET GO J.
           ADD 1 WH-NUM GIVING WH-MEH.
           IF WH-MEH > 20 GO J.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           IF WH-ANZ NOT = 0 GO L.
       K.  DISPLAY "<ret>= Betrag, <F1>= zur Menge" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "0000667210" WH-CREG.
           IF WOLI OR ESC GO I.
           IF FEHLER GO K.
           MOVE WH-WERT TO WH-BET.
           MOVE 0 TO WH-RAB.
           GO Q.
      *------------------------------------> Preis lt. Datei anzeigen <-
       L.  MOVE 63 TO VDU-P.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
       M.  DISPLAY "Preis: <ret-leer>= Wiederholung, <ret>= Eingabe"
               AT 2301.
           MOVE 52 TO VDU-P.
           MOVE WH-PREIS TO WD-PREIS WH-WERT.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
           CALL "CAUP" USING "0000526209" WH-CREG.
           IF WOLI OR ESC CALL "CAUP" USING "130000080" WH-CREG GO I.
           IF FEHLER GO M.
           MOVE WH-WERT TO WH-PREIS WD-PREIS.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
      *------------------------------------------------> Zeilenrabatt <-
       N.  MOVE 63 TO VDU-P.
           MOVE 0 TO WH-WERT.
           DISPLAY "Rabatt: <ret-leer>= Wiederholung, <ret>= Eingabe"
               AT 2301.
           CALL "CAUP" USING "0000632002" WH-CREG.
           IF WOLI OR ESC GO M.
           IF FEHLER GO N.
           MOVE WH-WERT TO WD-PROZ WH-RAB.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP
           ELSE DISPLAY "  " AT VDU-LP.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
       Q.  PERFORM ERLOEKTO.
           IF ESC GO B.
           IF WOLI; IF WH-ANZ = 0 GO K ELSE GO N.
           MOVE AR-NUM TO WV-ARNUM.
           SUBTRACT WH-ANZ FROM AR-STAND.
           MOVE 0 TO WH-RAB.
           GO A.
       Z.  EXIT.
      ******************************************************************
       VORANZ SECTION.
       A.  MOVE 48 TO VDU-P.
           ADD AR-MEH 1 GIVING WH-MEH.
           CALL "CAUP" USING "1300000080" WH-CREG.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           MOVE 52 TO VDU-P.
           MOVE AR-VKP TO WH-PREIS.
           MOVE WH-PREIS TO WD-PREIS WH-WERT.
           DISPLAY WD-PREIS WITH HIGHLIGHT AT VDU-LP.
           IF AR-FIXPR NOT = 0 MOVE AR-FIXPR TO WD-PREIS
               ADD 1 TO VDU-L
               DISPLAY WD-PREIS AT VDU-LP
               SUBTRACT 1 FROM VDU-L.
           MOVE 63 TO VDU-P.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ERLOEKTO SECTION.
       A.  MOVE WH-BET TO WD-BET WK-BET.
           IF WL-CA = 99 MOVE AU-UST TO WX WH-USTKZ
               ADD WH-BET TO WS-BET
               MOVE WS-BET TO WD-BET GO G.
           MOVE 66 TO VDU-P.
           DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP.
           MOVE AR-UST TO WX.
       B.  MOVE WX TO WH-USTKZ.
           MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
           IF NOT MANUEL GO E.
           DISPLAY "<ret>= Zeile ok., <F1>= zurueck, <esc>= Abbruch, <ta
      -       "b>= Ust-Kz (1-6)" AT 2301.
           CALL "CAUP" USING "0000791001" WH-CREG.
           IF ESC OR WOLI GO Z.
           MOVE WH-NUM TO WH-USTKZ.
           IF RET; IF WH-USTKZ > 0
               DISPLAY "keine Eingabe erlaubt" AT 2401
               PERFORM WEITER GO B
           ELSE MOVE WX TO WH-USTKZ GO E.
           IF NOT TABL GO B.
           IF WH-USTKZ > 0 AND WH-USTKZ < 7 MOVE WH-USTKZ TO WX GO B.
       D.  DISPLAY "Ust-Kz falsch" AT 2401.
           PERFORM WEITER.
           GO B.
       E.  IF WX = 0 GO D.
           MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
           ADD WH-BET TO WS-BET.
           MOVE WS-BET TO WD-BET.
           DISPLAY WD-BET WITH HIGHLIGHT AT 0366.
       G.  IF AR-KAGRP > 1 ADD WK-BET TO WS-RAB;
               IF WK-RAB NOT = 0
                   COMPUTE WH-WERT = WK-BET * WK-RAB / 100
                   SUBTRACT WH-WERT FROM WK-BET.
           ADD WK-BET TO WS-NET.
           SUBTRACT WH-BET FROM WS-NET.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
           SET AX AY TO AR-KAGRP.
           IF AX > 15 SET AX AY TO 15.
           ADD WK-BET TO AK-SUM(AX) WS-ERL(AX)          *> Einzelerlîse
           ADD WK-BET TO WT-BASIS(WX).                  *> Bmg-Ust
           PERFORM ZEIDRUCK.
           PERFORM ADDSTAT.
       Z.  EXIT.
      *********************************** Addition in Artikelstatistik *
       ADDSTAT SECTION.
       A.  MOVE AR-NUM TO ST-ARNUM.
           MULTIPLY AR-EKP BY WH-ANZ GIVING WH-WERT.
           IF AY > 1 ADD WH-WERT TO WS-SUM(17) AK-SUM(17).
           SUBTRACT WH-WERT FROM WK-BET GIVING WH-WERT.
           ADD WH-WERT TO WS-SUM(AY).
           IF WT-NK(WH-MEH) = 2 MULTIPLY 100 BY WH-ANZ.
           IF WT-NK(WH-MEH) = 1 MULTIPLY  10 BY WH-ANZ.
       Q.  READ STATISTIK INVALID KEY INITIALIZE ST-SATZ
               MOVE WX-J TO ST-JAHR
               MOVE AR-NUM TO ST-ARNUM.
           IF ZUGRIF PERFORM BESETZT GO Q.
           IF WX-J NOT > ST-JAHR GO S.
           SET SY TO 1.
       R.  MOVE ST-MENGEN(SY) TO ST-MENGEN(SY + 12).
           MOVE 0 TO ST-MENGEN(SY).
           IF SY < 12 SET SY UP BY 1 GO R.
           MOVE WX-J TO ST-JAHR.
       S.  SET SY TO WX-M.
           IF WX-J = ST-JAHR
               ADD WH-ANZ TO ST-MENGEN(SY) GIVING ST-MENGEN(SY) GO W.
           IF WX-J = (ST-JAHR - 1)
             ADD WH-ANZ TO ST-MENGEN(SY + 12) GIVING ST-MENGEN(SY + 12).
       W.  WRITE ST-SATZ INVALID KEY REWRITE ST-SATZ.
           UNLOCK STATISTIK.
       Z.  EXIT.
      ****************************************** Ausdruck einer Zeile **
       ZEIDRUCK SECTION.
       A.  MOVE AR-NUM TO WH-NUM DRC-ARNUM.
      *    MOVE 0 TO WE-PREIS.
           IF WR-ADR(1) NOT = SPACE MOVE WR-ADR(1) TO DRC-BEZ.
           SET TX TO 2.
       C.  IF WR-ADR(TX) NOT = SPACE PERFORM DRUCK
               MOVE WR-ADR(TX) TO DRC-BEZ
               IF TX < 6 SET TX UP BY 1 GO C.
           IF WH-ANZ NOT = 0 MOVE WD-MENGE TO DRC-MENGE
               MOVE WT-MEH(WH-MEH) TO DRC-MEH
      *        COMPUTE WE-PREIS rounded = WH-PREIS * 13,7603
               MOVE WH-PREIS TO DRC-PREIS.
           IF WH-RAB NOT = 0 MULTIPLY -1 BY WH-RAB GIVING DRC-PROZ.
           MOVE WH-BET TO DRC-BETRAG.
           MOVE WT-UST(WH-USTKZ) TO DRC-UST.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       FAKTENDE SECTION.
       A.  MOVE 0 TO WX-TASTE.
           IF WZ-ZEILEN > 54 MOVE 60 TO WZ-ZEILEN
               PERFORM UEBERTRAG.
           MOVE ALL "ƒ" TO DRC-STR.
           PERFORM DRUCK.
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
           IF WK-RAB NOT = 0; IF WS-RAB NOT = 0
               MOVE "SONDERNACHLASS VON" TO DRD-BEZ
               MOVE WK-RAB TO DRD-PROZ
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
           IF WX-R = 2 OR WX-R = 9 GO G.
           MOVE WT-BASIS(WX) TO WH-BET.
           COMPUTE WH-BET ROUNDED = WT-BASIS(WX) * WT-UST(WX) / 100.
           IF WH-BET = 0 MOVE SPACE TO DRD-SATZ GO G.
           MOVE WT-UST(WX) TO DRD-PROZ.
           MOVE "%" TO DRD-PZ.
           MOVE "MEHRWERTSTEUER VON" TO DRD-BEZ.
           MOVE WT-BASIS(WX) TO DRD-BASIS.
           MOVE WH-BET TO DRC-BETRAG WD-BET.
           ADD WH-BET TO WH-MWST.
           IF AK-UST = 0 ADD WH-BET TO WS-BET
               DISPLAY DRD-MWST AT 0000
               MOVE 66 TO VDU-P
               DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP
               PERFORM BS-ZEIL
               PERFORM DRUCK.
           MOVE SPACE TO DRA-SATZ.
       G.  IF WX < 6 ADD 1 TO WX GO F.
       H.  MOVE WS-BET TO WH-BET.
      *-> Rechnungss. f. Ueberleitung - Tagessumme USt: 8 * Summe: 9 <-
       K.  IF WZ-ZEILEN = WZ GO L.
           MOVE ALL "ƒ" TO DRC-STR.
           DISPLAY DRC-STR AT VDU-LP.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
           MOVE WK-TX TO DRC-BEZ.
           MOVE WS-BET TO DRC-BETRAG WD-BET.
           MOVE WH-BET TO WS-BET.
           DISPLAY WD-BET WITH HIGHLIGHT AT VDU-LP.
           PERFORM DRUCK.
           PERFORM BS-ZEIL.
       L.  MOVE WH-BET TO WS-BET.
      *---------------------------------------------> Euroinformation <-
           IF WX-R < 3 MOVE ALL "=" TO DRC-STR.
           IF AK-UST > 0 MOVE "öbergang der Steuerschuld gem   19 Abs. 1
      -        " lit a UST. UID: " TO DRC-SATZ(8:57)
               MOVE WK-UID TO DRC-SATZ(66:13).
           PERFORM DRUCK.
           IF AK-UST > 0 PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
              IF WT-BASIS(WX) not = 0 MOVE WT-BASIS(WX) TO WD-BASIS
                  MOVE "Anzuwendender Steuersatz:" TO DRC-SATZ(8:)
                  MOVE WT-UST(WX) TO WD-PZ
                  MOVE WD-PZ TO DRC-SATZ(33:5)
      *Anzuwendener Steuersatz: 20,oo% von Eur
                  MOVE "% von Eur" TO DRC-SATZ(39:6)
                  MOVE WD-BASIS TO DRC-SATZ(48:)
                  PERFORM DRUCK.
           IF MANUEL PERFORM ZUS-TEXT.
           PERFORM SKTO.
           COMPUTE WH-KEY = WH-VKF + 11.
           IF WM-DRU = 0 GO M.
           MOVE "(s0p16.6h0b0s0T" TO DRA-CD PERFORM DRUCK.
           MOVE "Die Ware bleibt bis zur vollstÑndigen Bezahlung unser E
      -        "igentum. Im Åbrigen gelten die Bedingungen der Bundes-"
                TO DRV-TX.
           PERFORM DRUCK.
           MOVE "wirtschaftskammer Sektion Gas-, Wasserinstallateure und
      -         " Heizungsbauer ôsterreichs. " TO DRV-TX PERFORM DRUCK
           MOVE 0 TO WZ-SCHALT.
           MOVE "(s0p12h0b0s3T" TO DRA-CD PERFORM DRUCK.
      *-----------------------------------------> Addition VerkÑufer <-
       M.  READ KONSTANT INVALID KEY INITIALIZE KO-VSATZ.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE 1 TO WX.
       N.  ADD WS-SUM(WX) TO KO-UMSATZ(WX).
           IF WX < 17 ADD 1 TO WX GO N.
           MOVE WH-VKF TO KO-NUM.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           UNLOCK KONSTANT.
      *--------------------------------------> Addition Erloessummen <-
       O.  MOVE 6 TO WH-KEY.
           READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO O.
           MOVE 1 TO WX.
       P.  ADD WT-BASIS(WX) TO KO-ERLOES(WX).            *> Bmg-Ust 1-6
           IF WX < 6 ADD 1 TO WX GO P.
           MOVE 1 TO WX.
       Q.  ADD AK-SUM(WX) TO KO-ERLOES(WX + 7).          *> Erlîse
           IF WX < 15 ADD 1 TO WX GO Q.
           ADD WH-MWST   TO KO-ERLOES(23) WS-ERL(18)     *> Ust
           ADD WS-BET    TO KO-ERLOES(24) WS-ERL(19)     *> Brutto
           IF WX-R > 2 GO S.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           UNLOCK KONSTANT.
           IF WX-R < 2 PERFORM VERBUCHE.
       S.  UNLOCK KONSTANT.
           PERFORM PAGEDRU.
           IF MANUEL GO Z.
           CALL "CAUP" USING "1307012480" WH-CREG.
       Z.  EXIT.
      ********************************************************* Skonto *
       SKTO SECTION.
       A.  MOVE 2 TO WZ-SCHALT.
           IF WL-CA = 99 MOVE AK-KOND TO WH-KK;
               IF WH-TGN = 0 GO Z ELSE GO H.
           MOVE 2 TO WH-SK1.
           MOVE 8 TO WH-TG1.
           MOVE 24 TO WH-TGN.
           MOVE 0 TO WH-SK2 WH-TG2.
       C.  DISPLAY "Skonto:  2% Skontotage:  8 Nettotage: 24" AT 2111.
           DISPLAY "<ret>= Skontoprozentsatz" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "0021201001" WH-CREG.
           MOVE WH-WERT TO WH-SK1 WD-PROZ.
           IF WH-WERT = 0
                DISPLAY "                           " AT 2110 GO G.
           IF NOT RET GO C.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT 2119.
       E.  DISPLAY "<ret>= Skontotage" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "0021352002" WH-CREG.
           IF NOT RET GO C.
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO WH-TG1 WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT 2135.
       G.  DISPLAY "<ret>= Nettotage" AT 2301.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "0021492002" WH-CREG.
           IF NOT RET GO G.
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO WH-TGN WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT 2149.
       H.  IF WH-SK1 = 0 GO K.
           MOVE " % Skonto bei Zahlung innerhalb von    Tagen, das sind
      -        "Eur" TO DRV-TX.
           MOVE WH-SK1 TO DRV-SK1.
           MOVE WH-TG1 TO DRV-TG1.
           COMPUTE WH-WERT = WS-BET * WH-SK1 / 100.
           MOVE WH-WERT TO DRV-BET.
           PERFORM DRUCK.
           MOVE "ansonsten erbitten wir Ihre Zahlung innerhalb von    Ta
      -        "gen ohne Abzug." TO DRV-TX.
       K.  IF WH-SK1 = 0 MOVE "          Wir erbitten Ihre Zahlung inner
      -        "halb von    Tagen ohne Abzug." TO DRV-TX.
           MOVE WH-TGN TO DRV-TGN.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       VERBUCHE SECTION.
       A.  OPEN I-O BUCHFAKT.
           MOVE 99999 TO BF-KEY.
           START BUCHFAKT KEY < BF-KEY INVALID MOVE 1 TO WH-BUKEY GO E.
       C.  READ BUCHFAKT PREVIOUS NO LOCK AT END
                         MOVE 1 TO WH-BUKEY GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 1 BF-KEY GIVING WH-BUKEY.
       E.  INITIALIZE BF-SATZ.
           MOVE WH-BUKEY TO BF-KEY.
           MOVE DE-KTONR TO BF-KTONR.
           MOVE WM-DATUM TO BF-DAT.
           MOVE SPACE TO BF-TX.
           MOVE WH-MWST TO BF-MW.
           MOVE WT-BASIS(1) TO BF-B1.
           MOVE WT-BASIS(2) TO BF-B2.
           MOVE WS-BET TO BF-BET.
           MOVE WE-ARSYM TO BF-SY.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE WT-BASIS(WX) TO BF-SUM(WX).             *> Bmg-Ust
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 19
               MOVE WS-ERL(WX) TO BF-SUM(WX + 6).           *> Erlîse
           MOVE 1 TO BF-VM.
           MOVE 0 TO BF-Z.
           EVALUATE AK-UST
               WHEN 1 MOVE 5 TO BF-VM                   *> TR frei ı 19
                      MOVE WE-TRSYM TO BF-SY
                      MOVE 1 TO BF-Z
               WHEN 2 MOVE 6 TO BF-VM                   *> SR frei ı 19
                      MOVE WE-SRSYM TO BF-SY
                      MOVE 1 TO BF-Z.
           MOVE 0 TO BF-SH.
           IF WS-BET < 0 MOVE WE-GSSYM TO BF-SY.
           MOVE WH-RENUM TO BF-REN.
           MOVE WH-KK TO BF-KOND.
           IF WT-BASIS(1) NOT = 0 MOVE 1 TO BF-U1.
           IF WT-BASIS(2) NOT = 0 MOVE 2 TO BF-U2.
           MOVE "Eur" TO BF-FSYM.                 *> ab hier EURO-Werte
           MOVE 100 TO BF-KURS.
           MOVE BF-BET TO BF-FWBET.
           WRITE BF-SATZ.
      *-------------------------------------------> KundenÅberleitung <-
           MOVE "K" TO BF-SA.
           MOVE DE-MCODE TO BF-MCODE.
           MOVE DE-KOND TO BF-KOND.
           MOVE DE-ANREDE TO BF-ANREDE.
           MOVE DE-BEZ TO BF-BEZ.
           MOVE DE-TEL TO BF-TEL.
       G.  ADD 1 TO BF-KEY.
           WRITE BF-SATZ INVALID GO G.
           CLOSE BUCHFAKT.
       Z.  EXIT.
      ******************************************************************
       ZUS-TEXT SECTION.
       A.  SUBTRACT WZ-ZEILEN FROM 54 GIVING WD-TG.
           IF WZ-ZEILEN NOT < 54 DISPLAY "kein weiterer Text mîglich"
               AT 2401 PERFORM WEITER GO Z.
           DISPLAY "<tab>= Leezeile, <ret-leer>= Rechnungsende
      -        "*  noch freie Zeilen: " AT 2301 WD-TG.
           MOVE SPACE TO WT-TX.
       C.  MOVE 9 TO VDU-P.
           CALL "CAUP" USING "0200000150" WH-CREG.
           IF RET; IF WT-TX NOT = SPACE GO D ELSE GO Z.
           IF WOLI GO Z.
           IF NOT TABL GO A.
       D.  MOVE WT-TX TO DRC-BEZ.
           PERFORM DRUCK.
           PERFORM BS-ZEIL
           GO A.
       Z.  EXIT.
