      $SET LINKCOUNT"192" ANS85"SYNTAX" BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABSTAM.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEKON.CPY.
           COPY GABSEDRU.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY GABKONST.CPY.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-MA                PIC 99         COMP.
               05 WM-KO                PIC 99         COMP.
               05 WM-PR                PIC 99         COMP.
               05 WM-RB                PIC 99         COMP.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WS-BET                  PIC 9(9)V99 COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WD-APZ                  PIC Z99,9-.
           03  WD-UST                  PIC Z9,99-.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-PZ                   PIC Z9.
           03  WD-POS                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WH-PX                   PIC XX       OCCURS 2.
      *-----------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-ART                  PIC 99.
       COPY GABEXT.CPY.
      *----------------------------------------------------------------*
      *LINKAGE SECTION.
      *01  WL-CALL                     PIC X(15).
      *01  WL-CREG                     PIC X(1152).
      *PROCEDURE DIVISION using WL-CALL WL-CREG.
      *DECLARATIVES.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           CALL X"B0" USING WU-FUNC WU-KEYTAB.
           MOVE " Stammdatenverwaltung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM STAMVERW.
       X.  MOVE WH-CREG TO WL-CREG.
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
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 99 GO A.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      *********************************************** Formularauswurf *
       AUSWURF SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           WRITE DRA-SATZ AFTER PAGE.
           IF WF-STATUS = 99 GO A.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE SPACE TO DRA-SATZ.
           MOVE X"1B21" TO DRA-SATZ(1:2).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           MOVE SPACE TO DRA-SATZ.
           MOVE X"1B43" TO DRA-SATZ(1:2).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ****************************************** Stammdatenverwaltung *
       STAMVERW SECTION.
       A.  UNLOCK KONSTANT.
           DISPLAY "1 - Kunden"              AT 0714
                   "2 - Artikel"             AT 0014
                   "3 - Artikelnummern"      AT 0014
                   "4 - Mengeneinheiten"     AT 0014
                   "5 - Ust-SÑtze"           AT 0014
                   "6 - Rechnungstext"       AT 0014
                   "7 - Kopf / Code"         AT 0014
                   "8 - sonst. Konstanten"   AT 0744
                   "9 - Rechnungskopf"       AT 0044
                   "10 - Anredetexte"        AT 0043
                   "11 - VerkÑufer"          AT 0043
                   "12 - Fibuverzeichnis"    AT 0043
                   "13 - Erlîskonten"        AT 0043
                   "14 - Druckeinstellung"   AT 0043
                   "bitte wÑhlen Sie:     " with highlight AT 1629.
           DISPLAY "leer = Programmende" AT 2301.
       C.  CALL "CAUP" USING "0016472002" WH-CREG.
           IF ESC or ENDE SET RET TO TRUE MOVE 0 TO WH-NUM.
           IF NOT RET GO A.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 MOVE 9 TO WH-PG GO Z
               WHEN 1 CALL "GABDEBIT" USING "60DEBIT" WH-CREG
                      CANCEL "GABDEBIT"
               WHEN 2 CALL "GABARTIK" USING "50ARTIK" WH-CREG
                      CANCEL "GABARTIK"
               WHEN 3 CALL "GABARTIK" USING "55ARTNU" WH-CREG
                      CANCEL "GABARTIK"
               WHEN 4 PERFORM MENGEN
               WHEN 5
               WHEN 6
               WHEN 7
               WHEN 8 GO D
               WHEN 9 GO S
               WHEN 10 PERFORM ANR
               WHEN 11 PERFORM VKF
               WHEN 12 PERFORM VERZEICH
               WHEN 13 PERFORM ERLOESE
               WHEN 14 PERFORM DRUPAR.
           GO A.
       D.  MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF WH-PG = 5 PERFORM USTSATZ GO A.
           IF WH-PG = 6 PERFORM USTSATZ GO A.
           IF WH-PG = 8 GO J.
      *------------------------------> Aender Firmenkopf u. Codewort <-
       F.  MOVE KO-FIRMA TO WT-TX.
           CALL "CAUP" USING "0201270122" WH-CREG.
           IF NOT RET GO F.
           MOVE WT-TX TO KO-FIRMA.
           IF KO-FIRMA = SPACE GO F.
           DISPLAY KO-FIRMA AT 0127.
           CALL "CAUP" USING "0705430528000" WH-CREG.
           DISPLAY " Codewort " with highlight AT 0545.
       G.  DISPLAY "Altes Codewort:" AT 0645.
           DISPLAY KO-SPERRE AT 0663.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           DISPLAY "Neues Codewort:" AT 0745.
           ACCEPT WX-CODE(1) AT 0763.
           IF NOT RET GO G.
           MOVE WX-CODE(1) TO KO-SPERRE.
       H.  DISPLAY "Kontrolleingabe:" AT 0845.
           ACCEPT WX-CODE(2) AT 0863.
           IF NOT RET GO H.
           IF WX-CODE(2) NOT = WX-CODE(1)
               DISPLAY "Eingaben nicht gleich" AT 0445 GO G.
       I.  REWRITE KO-SATZ.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           GO A.
      *----------------------------------------> sonstige Konstanten <-
       J.  CALL "CAUP" USING "0704010644000" WH-CREG.
           DISPLAY " sonst. Konstanten " with highlight AT 0403.
           DISPLAY "letzte Rechnung.......:" AT 0503.
           DISPLAY "Rechnungsort..........:" AT 0603.
           DISPLAY "Variante..............:" AT 0703.
           DISPLAY "Kopfdruck.............:" AT 0803.
           MOVE KO-RENUM TO WD-NUM.
           DISPLAY WD-NUM with highlight foreground-color 2 AT 0528.
           DISPLAY KO-ORT  with highlight foreground-color 3 AT 0627.
           MOVE KO-MERK TO WK-M.
           MOVE WM-MA TO WD-KZ.
           DISPLAY WD-KZ AT 0727.
           PERFORM KOART.
       L.  MOVE KO-RENUM TO WH-WERT.
           DISPLAY "<ret-leer>= keine énderung" AT 2301.
           CALL "CAUP" USING "0005285005" WH-CREG.
           IF FEHLER GO L.
           MOVE WH-WERT TO KO-RENUM WD-NUM.
           DISPLAY WD-NUM with highlight foreground-color 2 AT 0528.
       M.  MOVE KO-ORT TO WT-TX.
           CALL "CAUP" USING "0206270115" WH-CREG.
           MOVE WT-TX TO KO-ORT.
           IF WOLI GO L.
           IF KO-ORT = SPACE GO M.
           DISPLAY KO-ORT  with highlight foreground-color 3 AT 0627.
       P.  MOVE WM-MA TO WH-WERT.
           DISPLAY "Variante: 0 - 9" AT 2301.
           CALL "CAUP" USING "0007271001" WH-CREG.
           IF WOLI GO M.
           IF FEHLER GO P.
           MOVE WH-WERT TO WM-MA WD-KZ.
           DISPLAY WD-KZ AT 0727.
       Q.  MOVE WM-KO TO WH-WERT.
           DISPLAY "Kopfdruck: 0 = nein, 1 = mit Re-Kopf, 2 = mit Ma-Kop
      -        "f, 3 = beides" AT 2301.
           CALL "CAUP" USING "0008271001" WH-CREG.
           IF WOLI GO P.
           IF FEHLER OR WH-WERT > 3 GO Q.
           MOVE WH-WERT TO WM-KO.
           PERFORM KOART.
       R.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO R1.
           IF NOT RET GO R.
           MOVE WK-M TO KO-MERK.
           REWRITE KO-SATZ.
       R1. CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           GO A.
      *----------------------------------------------> Rechnungskopf <-
       S.  CALL "CAUP" USING "0707010680000" WH-CREG.
           DISPLAY " Rechnungskopf * 4 Zeilen " with highlight AT 0703.
           DISPLAY "<esc>= Abbruch" AT 2301.
           MOVE 2 TO WH-KEY.
       T.  READ KONSTANT WITH NO LOCK INVALID MOVE SPACE TO KO-SATZ
               MOVE WH-KEY TO KO-NUM
               WRITE KO-SATZ.
           IF ZUGRIF PERFORM BESETZT GO T.
           COMPUTE VDU-REL = WH-KEY * 100 + 2 + VDU-ECK.
           DISPLAY KO-KOPF WITH SIZE 78 HIGHLIGHT AT VDU-REL.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO T.
           MOVE 2 TO WH-KEY.
       U.  READ KONSTANT WITH NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO U.
           COMPUTE VDU-L = WH-KEY + 6.
           MOVE KO-KOPF TO WT-TX.
           CALL "CAUP" USING "0200020178" WH-CREG.
           IF ESC GO Y.
           MOVE WT-TX TO KO-KOPF.
           REWRITE KO-SATZ.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO U.
       V.  DISPLAY "<ret>= weiter, <tab>= Testdruck " AT 2301.
           CALL "CAUP" USING "0023331001" WH-CREG.
           IF RET GO Y.
           IF NOT TABL GO V.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE 2 TO WH-KEY.
       W.  READ KONSTANT WITH NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO W.
           MOVE X"1B21" TO DRA-SATZ(1:2).
           IF WH-KEY = 2 MOVE X"3B00" TO DRA-SATZ(3:2) PERFORM DRUCK.
           IF WH-KEY = 3 MOVE X"3100" TO DRA-SATZ(3:2) PERFORM DRUCK.
           IF WH-KEY = 5 MOVE X"0100" TO DRA-SATZ(3:2) PERFORM DRUCK.
           IF WH-KEY = 5 MOVE KO-KOPF TO DRA-SATZ(7:)
               ELSE MOVE KO-KOPF TO DRA-SATZ(3:).
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO W.
           MOVE ALL "ƒ" TO DRA-SATZ(7:89).
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           MOVE 7 TO WH-KEY.
       X.  READ KONSTANT WITH NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO X.
           MOVE KO-KOPF TO DRA-SATZ(7:).
           PERFORM DRUCK.
           IF WH-KEY < 9 ADD 1 TO WH-KEY GO X.
           MOVE 4 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 2 TO WH-KEY.
           GO T.
       Y.  PERFORM AUSWURF.
           GO R1.
       Z.  EXIT.
      ******************************************************************
       KOART SECTION.
       A.  MOVE WM-KO TO WD-KZ.
           DISPLAY WD-KZ AT 0827.
           IF WM-KO = 0 DISPLAY "kein Kopf" AT 0832.
           IF WM-KO = 1 DISPLAY "mit Re-Kopf" AT 0832.
           IF WM-KO = 2 DISPLAY "mit Ma-Kopf" AT 0832.
           IF WM-KO = 3 DISPLAY "beides" AT 0832.
       Z.  EXIT.
      ******************************************************** Anrede *
       ANR SECTION.
       A.  MOVE 10 TO WH-KEY.
           READ KONSTANT INVALID GO Y.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0706601117020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Anredetexte " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr. T e x t e" AT VDU-REL.
           ADD 99 TO VDU-REL.
           DISPLAY "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" AT VDU-REL.
           MOVE 1 TO WX.
       F.  COMPUTE VDU-REL = 304 + WX * 100 + VDU-ECK.
           SUBTRACT 1 FROM WX GIVING WD-KZ.
           DISPLAY WD-KZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Ende, <>= zurÅck" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 307.
           MOVE WT-TXT(WX) TO WT-TX.
           CALL "CAUP" USING "1200000109" WH-CREG.
           IF ESC GO L.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF FEHLER GO H.
           MOVE WT-TX TO WT-TXT(WX).
           COMPUTE VDU-REL = 307 + WX * 100 + VDU-ECK.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX GO H.
       L.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF NOT RET GO L.
           MOVE WH-TABTX TO KO-ANR.
           REWRITE KO-SATZ.
       X.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 7
               MOVE KO-AN(WX) TO WT-TXT(WX).
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************* Erlîskontenzuordnung *
       ERLOESE SECTION.
       A.  MOVE 41 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-ESATZ
               WRITE KO-SATZ GO A.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0703502028010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Erlîskonten " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Nr. Konto-Nr.           " with reverse-video
                AT VDU-LP.
           MOVE 1 TO WX.
       F.  COMPUTE VDU-LP = 204 + WX * 100 + VDU-ECK.
           MOVE WX TO WD-PZ.
           DISPLAY WD-PZ AT VDU-LP.
           ADD 4 TO VDU-LP.
           DIVIDE 10 INTO KO-KTONR(WX) GIVING WD-KTO.
           IF KO-KTONR(WX) NOT = 0
               DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WX > 15 ADD 9 TO VDU-LP
               DISPLAY " frei ı 19" AT VDU-LP.
           IF WX < 17 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Ende, <>= zurÅck" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 208.
           MOVE KO-KTONR(WX) TO WH-WERT.
           CALL "CAUP" USING "1000005107" WH-CREG.
           IF ESC GO L.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF FEHLER GO H.
           MOVE WH-WERT TO KO-KTONR(WX).
           DIVIDE 10 INTO WH-WERT GIVING WD-KTO.
           IF WH-WERT NOT = 0 DISPLAY WD-KTO with highlight AT VDU-LP
                         else DISPLAY "         " AT VDU-LP.
           IF WX > 15 ADD 9 TO VDU-LP
               DISPLAY " frei ı 19" AT VDU-LP.
           IF WX < 17 ADD 1 TO WX GO H.
       L.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO Y.
           IF NOT RET GO L.
           REWRITE KO-SATZ.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************* VerkÑufertexte *
       VKF SECTION.
       A.  CALL "CAUP" USING "0707231239000" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " VerkÑufer " with highlight AT VDU-REL.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.: " AT VDU-REL.
       C.  DISPLAY "<esc>= Ende, <ret>= Vkf., < >= Hilfe, < >= Druck, <t
      -        "ab>= anzeigen" AT 2301.
           DISPLAY "Vkfnr. 1 - 29 mîglich" with highlight AT 2401.
           CALL "CAUP" USING "1102082002" WH-CREG.
           IF HELP CALL "CAUP" USING "190022000001" WH-CREG GO C.
           IF WS-TASTE = 4 CALL "GABLIST" USING "30VKLI" WH-CREG
               CANCEL "GABLIST" GO C.
           IF ESC GO Y.
           IF WH-NUM > 29 GO C.
           IF TABL GO P.
           IF NOT RET OR WH-NUM = 0 GO C.
           MOVE WH-NUM TO KO-NUM WD-PZ.
           ADD WH-NUM 11 GIVING WH-KEY.
       D.  READ KONSTANT NOT INVALID GO E.
           INITIALIZE KO-VSATZ.
       E.  DISPLAY WD-PZ with highlight AT VDU-LP.
           IF KO-NAME NOT = SPACE GO G.
       F.  MOVE KO-NAME TO WT-TX.
           DISPLAY "keine Bezeichnung = lîschen" AT 2401.
           CALL "CAUP" USING "1202110120" WH-CREG.
           MOVE WT-TX TO KO-NAME.
           IF ESC GO B.
       G.  COMPUTE VDU-LP = 211 + VDU-ECK.
           DISPLAY KO-NAME with highlight AT VDU-LP.
           MOVE 1 TO WX.
           MOVE 0 TO WS-BET.
       H.  COMPUTE VDU-REL = 304 + WX * 100 + VDU-ECK.
           IF WX > 8 SUBTRACT 782 FROM VDU-REL.
           MOVE WX TO WD-PZ.
           DISPLAY WD-PZ AT VDU-REL ":".
           ADD 3 TO VDU-REL.
           MOVE KO-UMSATZ(WX) TO WD-BET.
           ADD KO-UMSATZ(WX) TO WS-BET.
           DISPLAY WD-BET with highlight AT VDU-REL.
           IF WX < 15 ADD 1 TO WX GO H.
           MOVE WS-BET TO WD-BET.
           ADD 100 TO VDU-REL.
           DISPLAY WD-BET with highlight AT VDU-REL.
           SUBTRACT 3 FROM VDU-REL.
           DISPLAY "**" AT VDU-REL.
           COMPUTE VDU-REL = 305 + VDU-ECK.
           SUBTRACT KO-UMSATZ(1) FROM WS-BET.
           ADD KO-UMSATZ(17) WS-BET GIVING WH-WERT WD-BET.
           DISPLAY WD-BET AT VDU-REL.
           ADD 18 TO VDU-REL.
           COMPUTE WD-APZ = WS-BET / WH-WERT * 100.
           DISPLAY WD-APZ AT VDU-REL "%".
       K.  DISPLAY "<ret>= speichern, <esc>= Abbruch, <>= NamensÑnderun
      -        "g" AT 2301.
           CALL "CAUP" USING "0023550000" WH-CREG.
           IF ESC UNLOCK KONSTANT GO B.
           IF WOLI GO F.
           IF NOT RET GO K.
           IF KO-NAME = SPACE GO L.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           GO B.
       L.  DELETE KONSTANT INVALID GO B.
           GO B.
      *---------------------------------------------------> auflisten <-
       P.  MOVE 0 TO WR WX.
           CALL "CAUP" USING "16LOEFENST" WH-CREG.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM = 0 ADD 1 TO WH-NUM.
           ADD 11 WH-NUM GIVING WH-KEY.
           START KONSTANT KEY NOT < WH-KEY INVALID KEY GO B.
       Q.  ADD 1 TO WX.
           READ KONSTANT NEXT WITH NO LOCK AT END GO R.
           IF ZUGRIF PERFORM BESETZT GO Q.
           ADD 1 TO WR.
           IF WH-KEY > 40 SET FINE TO TRUE GO R.
           IF WR NOT = 10 GO S.
       R.  COMPUTE VDU-LP = 103 + WX * 100.
           DISPLAY "<tab>= weiter anzeigen, <esc/ret>= Ende" AT 2301.
           CALL "CAUP" USING "1000000000" WH-CREG.
           IF ESC GO B.
           IF NOT RETA GO R.
           MOVE 1 TO WR.
           IF RET GO B.
           IF FINE GO P.
       S.  IF WX = 10 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 9 TO WX.
           MOVE KO-NUM TO WD-PZ.
           COMPUTE VDU-REL = VDU-ECK + 104 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KO-NAME with highlight AT VDU-REL.
           GO Q.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       USTSATZ SECTION.
       A.  IF WH-PG = 5 GO D.
           CALL "CAUP" USING "0709080355000" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Rechnungstext " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY KO-TX with highlight AT VDU-REL.
       B.  DISPLAY "<ret>= Aenderung / Anlage, <esc>= Abbruch" AT 2301.
           MOVE KO-TX TO WT-TX.
           CALL "CAUP" USING "1202030151" WH-CREG.
           MOVE WT-TX TO KO-TX.
           IF ESC GO X.
           IF NOT RET GO B.
           REWRITE KO-SATZ.
           GO X.
       D.  CALL "CAUP" USING "0704081017020" WH-CREG.
           DISPLAY " Ust-Saetze " with highlight AT 0411.
           DISPLAY "Nr.    %-Satz" AT 0510.
           DISPLAY "ƒƒƒƒƒƒƒƒƒƒƒƒƒ" AT 0610.
           MOVE 1 TO WX.
       F.  ADD 6 WX GIVING VDU-L.
           MOVE WX TO WD-KZ.
           MOVE 11 TO VDU-P.
           DISPLAY WD-KZ AT VDU-LP.
           MOVE KO-UST(WX) TO WD-UST.
           MOVE 17 TO VDU-P.
           DISPLAY WD-UST with highlight AT VDU-LP.
           IF WX < 6 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Ende, <>= zurueck" AT 2301.
           ADD 6 WX GIVING VDU-L.
           MOVE KO-UST(WX) TO WH-WERT.
           CALL "CAUP" USING "0000002205" WH-CREG.
           IF ESC GO L.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF AB GO I.
           IF FEHLER GO H.
           MOVE 17 TO VDU-P.
           MOVE WH-WERT TO KO-UST(WX) WD-UST.
           DISPLAY WD-UST with highlight AT VDU-LP.
       I.  IF WX < 6 ADD 1 TO WX GO H.
       L.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF NOT RET GO L.
           REWRITE KO-SATZ.
           MOVE 1 TO WX.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       MENGEN SECTION.
       A.  CALL "CAUP" USING "0705401434020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Mengeneinheiten " with highlight AT VDU-REL.
           MOVE 10 TO WH-KEY.
       B.  READ KONSTANT INVALID KEY GO X.
           IF ZUGRIF PERFORM BESETZT GO B.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.  Text NK     Nr.  Text NK" AT VDU-REL.
           ADD 0303 VDU-ECK GIVING VDU-REL.
           DISPLAY "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ" AT VDU-REL.
           MOVE 0 TO WH-ART.
       C.  ADD 1 WH-ART GIVING WX.
           ADD 03 VDU-EL WX GIVING VDU-L.
           IF WX < 11 ADD 02 VDU-EP GIVING VDU-P
                 ELSE ADD 19 VDU-EP GIVING VDU-P
                      SUBTRACT 10 FROM VDU-L.
           MOVE WH-ART TO WD-POS.
           DISPLAY WD-POS AT VDU-LP.
           ADD 6 TO VDU-P.
           DISPLAY KO-MEH(WX) with highlight AT VDU-LP.
           MOVE KO-NK(WX) TO WD-KZ.
           ADD 6 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 1 TO WH-ART.
           IF WX < 20 ADD 1 TO WX GO C.
           MOVE 1 TO WX.
       D.  DISPLAY "  = auf,   = ab, <esc>= Ende, <ret>= Meh" AT 2301.
           ADD 03 WX GIVING VDU-RL.
           IF WX < 11 MOVE 08 TO VDU-RP
                 ELSE MOVE 25 TO VDU-RP
                      SUBTRACT 10 FROM VDU-RL.
           MOVE VDU-REL TO VDU-LP.
           MOVE KO-MEH(WX) TO WT-TX.
           CALL "CAUP" USING "1200000103" WH-CREG.
           MOVE WT-TX TO KO-MEH(WX).
           IF ESC GO K.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO D.
           IF AB  GO I.
           IF NOT RET GO D.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY KO-MEH(WX) with highlight AT VDU-LP.
           ADD 6 TO VDU-REL.
           MOVE VDU-REL TO VDU-LP.
       F.  DISPLAY "Nachkomma bei Mengeneingaben (0 - 2)" AT 2301.
           MOVE KO-NK(WX) TO WH-WERT.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI GO D.
           IF ESC GO K.
           IF NOT RET GO F.
           IF WH-WERT > 2 GO F.
           MOVE WH-WERT TO KO-NK(WX) WD-KZ.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       I.  IF WX < 20 ADD 1 TO WX GO D.
       K.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************** Druckereinstllung im Netz *
       DRUPAR SECTION.
       A.  MOVE 11 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KOD-SATZ.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0710100850000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " wo drucken? " with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK
               EVALUATE WX
                   WHEN 1 DISPLAY "Rechnung"    AT VDU-LP
                   WHEN 2 DISPLAY "Angebot"     AT VDU-LP
                   WHEN 3 DISPLAY "Auftrag"     AT VDU-LP
                   WHEN 4 DISPLAY "Mat.-Schein" AT VDU-LP
                   WHEN 5 DISPLAY "Listungen"   AT VDU-LP
               end-evaluate
               MOVE KOD-DRU(WX) TO WD-KZ
               ADD 17 TO VDU-LP
               DISPLAY WD-KZ with highlight foreground-color 2 AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE KOD-KOPF(WX) TO WD-KZ
               DISPLAY WD-KZ with highlight foreground-color 3 AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE KOD-WO(WX) TO WD-KZ
               DISPLAY WD-KZ with highlight foreground-color 6
                   AT VDU-LP.
           MOVE 1 TO WX.
      *-------------------------------------------> Anlage / énderung <-
       G.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "auf: 0= Nadel- 1= Laserdrucker" with highlight
               AT 2401.
           MOVE KOD-DRU(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 220.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF; IF WX > 1 ADD -1 TO WX GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF WH-WERT > 1 GO G.
           MOVE WH-WERT TO KOD-DRU(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 2 AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "0= ohne 1= mit Kopf" with highlight AT 2401.
           MOVE KOD-KOPF(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 225.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF not RET GO I.
           IF WH-WERT > 1 GO G.
           MOVE WH-WERT TO KOD-KOPF(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 3 AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "0= Druckdatei, 9= Direktdruck" with highlight
               AT 2401.
           MOVE KOD-WO(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 230.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF WH-WERT > 9 GO K.
           MOVE WH-WERT TO KOD-WO(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 6 AT VDU-LP.
           IF WX < 5 ADD 1 TO WX.
           GO G.
       W.  REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       VERZEICH SECTION.
       A.  CALL "CAUP" USING "0710100353000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " FibudateiverknÅpfung " with highlight AT VDU-LP.
       C.  MOVE 11 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KOD-SATZ
               MOVE WH-KEY TO KO-NUM.
           IF ZUGRIF PERFORM BESETZT GO X.
           IF KOD-VERZ = SPACE MOVE "F:\FIBU\FIBUKONS.DAT" TO KOD-VERZ.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Verzeichnis:" AT VDU-LP.
           ADD 216 VDU-ECK GIVING VDU-LP.
           DISPLAY KOD-VERZ with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Verzeichnis und Bezeichnung d
      -        ". FIBU-Konstantendatei" AT 2301.
           MOVE KOD-VERZ TO WT-TX.
           CALL "CAUP" USING "1202160135" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WT-TX TO KOD-VERZ.
           DISPLAY KOD-VERZ with highlight AT VDU-LP.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
