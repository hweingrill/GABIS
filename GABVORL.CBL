      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABVORL.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SECDAT     ASSIGN TO DATEI
                             ORGANIZATION SEQUENTIAL.
           COPY GABSEART.CPY.
           COPY GABSEDEB.CPY.
           COPY GABSEKON.CPY.
           COPY GABSESTA.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSEDRU.CPY.
           SELECT BUCHUNG    ASSIGN TO "FIBUEBER.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BU-KEY
                             ALTERNATE KEY BU-RKEY DUPLICATES
                             LOCK MODE IS EXCLUSIVE
                             FILE STATUS WF-STATUS.
           SELECT BUCHFAKT   ASSIGN TO "GABISFIB.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BF-KEY
                             LOCK MODE IS EXCLUSIVE
                             ALTERNATE KEY BF-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT KONSFIBU   ASSIGN TO WN-FKON
                             ORGANIZATION RELATIVE, ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
       COPY GABARTIK.CPY.
       COPY GABKONST.CPY.
       COPY GABSTATI.CPY.
       COPY GABANBOT.CPY.
       COPY GABBUCH.CPY.
      ************************** BuchsÑtze fÅr öberleitung (FIBUDATEI) *
       FD  BUCHUNG      external       LABEL RECORD STANDARD.
       01  BU-SATZ.
           03  BU-KEY                  PIC 9(6)     COMP.
           03  BU-RKEY.
               05 BU-SA                PIC X.
               05 BU-REN               PIC 9(7)     COMP.
           03  BU-KTONR                PIC 9(6)     COMP.
           03  BU-KOND                 PIC 9(13)    COMP.
           03  BU-DAT                  PIC 9(6)     COMP.
           03  BU-SY                   PIC 99       COMP.
           03  BU-GK                   PIC 9(6)     COMP.
           03  BU-Z                    PIC 9        COMP.
           03  BU-VM                   PIC 99       COMP.
           03  BU-RF                   PIC 9(7)     COMP.
           03  BU-U1                   PIC 9        COMP.
           03  BU-U2                   PIC 9        COMP.
           03  BU-MW                   PIC S9(7)V99 COMP-3.
           03  BU-B1                   PIC S9(9)V99 COMP-3.
           03  BU-B2                   PIC S9(9)V99 COMP-3.
           03  BU-SH                   PIC S9       COMP-3.
           03  BU-BET                  PIC S9(9)V99 COMP-3.
           03  BU-KST                  PIC S9(5)    COMP-3.
           03  BU-TX                   PIC X(40).
           03  BU-SKTOBAS              PIC S9(9)V99 COMP-3.
           03  BU-VALDAT               PIC 9(6)     COMP.
      *--------------------------------------------> fÅr FremdwÑhrung <-
           03  BU-FWBET                PIC S9(9)V99  COMP.
           03  BU-KURS                 PIC 9(4)V9(7) COMP.
           03  BU-FSYM                 PIC XXX.
      *------------------ BU-SA= "K" ---------------> KontenÅbernahme <-
       01  BU-KSATZ.
           03  FILLER                  PIC X(19).
           03  BU-MCODE                PIC X(8).
           03  BU-ANREDE               PIC 99       COMP.
           03  BU-BEZ                  PIC X(110).
           03  BU-TEL                  PIC X(35).
      ************************************* Unternehmerstammdaten FIBU *
       FD  KONSFIBU     external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  KF-SATZ.
           03  KF-NUM                  PIC 999     COMP-3.
           03  KF-FIRMA                PIC X(22).
           03  KF-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KF-JONUM                PIC 9(6)    COMP.
           03  KF-PER                  PIC 9(6)    COMP.
           03  KF-WJAHR                PIC 9(6)    COMP    OCCURS 2.
           03  KF-SPERRE               PIC X(6).
           03  KF-ORT                  PIC X(15).
           03  KF-MERK                 PIC XXXX.
           03  KF-DRU                  PIC 99      COMP.
           03  KF-ANR.
               05 KF-AN                PIC X(9)            OCCURS 7.
           03  KF-TABK.
               05 KF-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *-----------------------------------------> Symbole WH-KEY = 10 <-
       01  KF-MSATZ.
           03  FILLER                  PIC X(5).
           03  KF-TS.
               05 KF-SYM               PIC XX            OCCURS 20.
               05 KF-SKZ               PIC 9999    COMP  OCCURS 20.
      ************************************************ Security-Datei *
       FD  SECDAT.
       01  SEC-SATZ                    PIC X(128).
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
       01  DRL-SATZ.
           03  DRL-NO                  PIC X(6).
           03  DRL-STR.
               05 DRL-REN                 PIC ZZ.ZZ9-.
               05 DRL-KTO                 PIC ZZ.ZZ9,9-.
               05 DRL-BEZ                 PIC X(47).
               05 DRL-BET                 PIC ZZ.ZZZ.ZZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC X(12).
           03  DRS-TEXT.
               05  DRS-BASTX           PIC X(13).
               05  DRS-KZ              PIC 9/.
               05  DRS-UST             PIC 99.
               05  DRS-PZ              PIC X.
           03  DRS-KTONR               PIC ZZ.ZZ9,9.
           03  DRS-BETRAG              PIC Z.ZZZ.ZZ9,99-.
       01  DRT-SATZ.
           03  FILLER                  PIC X(12).
           03  DRT-TX.
               05 DRT-TEXT             PIC X(35).
               05 DRT-NR               PIC ZZZZ9-.
               05 DRT-BIS              PIC X(5).
               05 DRT-NR1              PIC ZZZZ9.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-MA                PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-PR                PIC 99        COMP.
               05 WM-RB                PIC 99        COMP.
           03  WH-P                    PIC 99        COMP.
           03  WH-PX                   PIC XX               OCCURS 2.
           03  WZ-SCHALT               PIC 99        COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99        COMP-3 VALUE ZERO.
           03  WS-BET                  PIC S9(9)V99  COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9         COMP-3 VALUE ZERO.
           03  WH-TX                   PIC X(51).
           03  WD-BKZ                  PIC 9(7).
           03  WD-APZ                  PIC Z99,9-.
           03  WD-UST                  PIC Z9,99-.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PZ                   PIC Z9.
           03  WD-POS                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WT-KTONR                PIC 9(6)      COMP   OCCURS 17.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WK-BUKEY                PIC 9(6)      COMP.
           03  WH-SEC                  PIC 9(14)     COMP-X.
           03  WH-SYMT                 PIC 9999.
           03  WR-Y REDEFINES WH-SYMT.
               05 WH-SH                PIC 9.
               05 WH-UST               PIC 9.
               05 WH-OP                PIC 9.
               05 WH-DIV               PIC 9.
           03  WS-SUM                  PIC S9(9)V99   COMP OCCURS 25.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-Z SECTION.         USE AFTER ERROR PROCEDURE ON ZUSATZ.
       A.  CALL "CADECL" USING "GABISARZ.DAT" WH-CREG.
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
       A.  CALL "CADECL" USING "GABISFIB.DAT " WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON BUCHUNG.
       A.  CALL "CADECL" USING "FIBUEBER.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM VORLAUF.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE " - G A B I S - 10.07" TO WK-GEB.
           IF WH-PG = 9 GO B.
           PERFORM LADE-DRU.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
      *    MOVE WH-DATUM TO WM-DATUM.
           DISPLAY "MODUS" UPON ENV-SET.
           ACCEPT WE-MOD FROM ENV-WERT.
           IF WE-MOD not = "RH" MOVE SPACE TO WE-MOD.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           call "gabcvde" using "10" wh-creg
           cancel "gabcvde".
           OPEN I-O DEBITOR.
           OPEN I-O ARTIKEL.
           OPEN I-O ZUSATZ.
           OPEN I-O STATISTIK.
           OPEN I-O KONSTANT.
      *    MOVE "GABDEBIT.DAT" TO DATEI PERFORM SEC.
      *    MOVE "GABISTAT.DAT" TO DATEI PERFORM SEC.
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK not INVALID GO J.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "bitte Kopf anlegen -" AT 2401.
           INITIALIZE KO-SATZ.
       C.  ACCEPT KO-FIRMA with highlight AT 0127.
           IF KO-FIRMA = SPACE GO C.
           DISPLAY KO-FIRMA with highlight AT 0127.
           MOVE "gabis" TO KO-SPERRE.
           MOVE 20 TO KO-UST(1).
           MOVE 10 TO KO-UST(2).
           MOVE 30 TO KO-UST(3).
           MOVE 0 TO KO-UST(4) KO-UST(5) KO-UST(6).
           MOVE SPACE TO KO-ORT KO-TX.
           MOVE 1 TO KO-RENUM KO-ABNUM KO-NUM WH-KEY.
           WRITE KO-SATZ.
           INITIALIZE KO-MSATZ.
           MOVE "FIRMA    SG. FAM. SG. HERRNSG. FRAU AN DAS   AN DIE
      -        "AN" TO KO-ANR.
           MOVE 10 TO WH-KEY KO-NUM.
           MOVE "Stkm  m2 kg LtrStdmalTubGarRolDosBoxSetFl SckStrStg
      -        "      " TO KO-TM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
              MOVE 0 TO KO-NK(WX).
           MOVE 2 TO KO-NK(2) KO-NK(3) KO-NK(4) KO-NK(5) KO-NK(6).
           WRITE KO-SATZ.
           INITIALIZE KO-SSATZ.
           MOVE 6 TO WH-KEY KO-NUM.
           MOVE 0 TO KO-ERST KO-DATUM.
           PERFORM VARYING WK FROM 1 BY 1 UNTIL WK > 24
               MOVE 0 TO KO-ERLOES(WK).
           WRITE KO-SATZ.
           MOVE SPACE TO KO-SATZ.
           MOVE 7 TO KO-NUM WH-KEY.
           MOVE "⁄ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒ¬ƒƒ
      -        "ƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒø" TO KO-KOPF.
           WRITE KO-SATZ.
           MOVE 8 TO KO-NUM WH-KEY.
           MOVE "≥ Artikel≥ B e z e i c h n u n g         ≥    Menge ≥Me
      -        "h≥     Preis ≥     Eur Betrag ≥U%≥" TO KO-KOPF.
           WRITE KO-SATZ.
           MOVE 9 TO KO-NUM WH-KEY.
           MOVE "¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒ
      -        "ƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒŸ" TO KO-KOPF.
           WRITE KO-SATZ.
           GO B.
       J.  MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-MERK TO WK-M.
           MOVE KO-ORT TO WT-TX.
           MOVE KO-TX TO WH-TX.
           SET TY FY TO 15.
       K.  IF WR-TX(TY) not = SPACE GO L.
           IF TY > 1 SET TY DOWN BY 1 GO K.
       L.  MOVE WR-TX(TY) TO WR-TX(FY).
           IF TY not = FY MOVE SPACE TO WR-TX(TY).
           IF TY > 1 SET TY FY DOWN BY 1 GO L.
           MOVE WT-TX TO VDU-ORT.
           MOVE WH-DATUM TO WZ-DATUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
           PERFORM KONDIT.
           MOVE 10 TO WH-KEY.
           READ KONSTANT IGNORE LOCK.
           MOVE KO-ANR TO WH-TABTX.
           MOVE KO-TM TO WT-TM.
           MOVE 11 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KOD-SATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               MOVE KOD-PARAM(WX) TO WE-PARAM(WX).
           CALL "CAUP" USING "06KOPF" WH-CREG.
       Q.  IF WE-MOD = "RH"
              DISPLAY WE-MOD with highlight background-color 4 AT 0177.
           DISPLAY "Programmwahl"           AT 0524
                   "ƒƒƒƒƒƒƒƒƒƒƒƒ"           AT 0024
                   "1 - Angebote"           AT 0024
                   "2 - Rechnungen"         AT 0024
                   "3 - Summendruck"        AT 0024
                   "4 - Preiswartung"       AT 0024
                   "5 - Preise"             AT 0024
                   "6 - Stammdaten"         AT 0024
                   "7 - Listungen"          AT 0024
                   "8 - Druckprogramm"      AT 0024
                   "9 - Adressenverwaltung" AT 0024
                   "10 - DatumsÑnderung"    AT 0023
                   " bitte wÑhlen Sie:   " with highlight AT 1939.
           DISPLAY "leer = Programmende" AT 2301.
           CALL "CAUP" USING "0019582002" WH-CREG.
       R.  IF ESC MOVE 0 TO WH-PG WH-NUM
               SET RET TO TRUE.
           IF not RET GO Q.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG = 66 CALL "GABREG" USING "20DE-MCOD" WH-CREG
                         CANCEL "GABREG" GO B.
           IF WH-PG = 0 GO T.
       S.  IF WX-CODE(2) = WX-CODE(1) GO T.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC MOVE 0 TO WH-PG GO R.
           MOVE WT-TX TO WX-CODE(1).
           GO S.
       T.  EVALUATE WH-PG
               WHEN 0 CLOSE DEBITOR ARTIKEL ZUSATZ KONSTANT STATISTIK
                      CALL "CAUP" USING "1301012480000" WH-CREG
                      DISPLAY " Programm beendet " AT 1231
                      STOP RUN
               WHEN 1 MOVE "GABAUF  10AUFT"    TO WT-TX
               WHEN 2 MOVE "GABRECH 20FAKT"    TO WT-TX
               WHEN 3 PERFORM TAGESSUMMEN GO A
               WHEN 4 MOVE "GABLIST 10PR-WART" TO WT-TX
               WHEN 5 MOVE "GABARTIK53PREISWT" TO WT-TX
               WHEN 6 MOVE "GABSTAM STAM"      TO WT-TX
               WHEN 7 MOVE "GABLIST 20AR-LIST" TO WT-TX
               WHEN 8 MOVE "CADRUCK 40DRUCK"   TO WT-TX
               WHEN 9 MOVE "GABKART GABADR"    TO WT-TX
               WHEN 10 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG GO Q.
       Z.  EXIT.
      *****************************************************************
       sec section.
       a.  open i-o secdat.
           read secdat at end go x.
           compute wh-sec = 92837465 * 970328 + 28473703.
           if sec-satz(23:14) = wh-sec go x.
           move sec-satz(23:6) to wz-datum.
           add 4 to wz-monat.
           if wz-monat > 12 add 1 to wz-jahr
               subtract 12 from wz-monat.
           if wz-datum > wh-datum go x.
           display "Security-Error (007) - " at 2401.
           display "weiter mit <ret>" at 2424.
           call "CAUP" using "0124416006" WH-CREG.
           compute wv-datum = wh-datum + 4711 - 470000.
           if wv-datum not = wh-num stop run.
           move wh-sec to sec-satz(23:14).
           rewrite sec-satz.
       x.  close secdat.
       z.  exit.
      *****************************************************************
       KONDIT SECTION.
       A.  INITIALIZE WH-TABK.
           MOVE 11 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KOD-SATZ
               MOVE 11 TO KO-NUM
               MOVE "F:\FIBU\FIBUKONS.DAT" TO KOD-VERZ
               WRITE KO-SATZ GO A.
           IF KOD-VERZ(2:1) not = ":" GO Z.
           MOVE KOD-VERZ TO WN-FKON WN-BUEB.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX = 0
               OR WN-BUEB(WX:1) = "\" CONTINUE.
           ADD 1 TO WX.
           MOVE "FIBUEBER.DAT" TO WN-BUEB(WX:).
           OPEN INPUT KONSFIBU.
           IF WF-STATUS not = "00" GO Z.
           MOVE 1 TO WH-KEY.
       C.  READ KONSFIBU IGNORE LOCK INVALID GO Q.
           MOVE KF-TABK TO WH-TABK.
      *-----------------------------------------> Symboltabelle lesen <-
           MOVE 10 TO WH-KEY.
       E.  READ KONSFIBU IGNORE LOCK INVALID GO Q.
           MOVE 99 TO WE-ARSYM WE-GSSYM.
           PERFORM VARYING WX FROM 0 BY 1 UNTIL WX > 19
               MOVE KF-SKZ(WX + 1) TO WH-SYMT
               EVALUATE WH-DIV
                   WHEN 1 MOVE WX TO WE-ARSYM
                   WHEN 2 MOVE WX TO WE-GSSYM
                   WHEN 8 MOVE WX TO WE-TRSYM
                   WHEN 9 MOVE WX TO WE-SRSYM.
           IF WE-ARSYM = 99 OR WE-GSSYM = 99 DISPLAY "Symbolkz. f. AR un
      -        "d/oder GU nicht angelegt" AT 2401 PERFORM WEITER
               MOVE 0 TO WE-ARSYM WE-GSSYM.
       Q.  CLOSE KONSFIBU.
       Z.  EXIT.
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
           IF WF-STATUS = 27 GO A.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      *********************************************** Formularauswurf *
       AUSWURF SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           WRITE DRA-SATZ AFTER PAGE.
           IF WF-STATUS = 27 GO A.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  PERFORM AUSWURF.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
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
      ************************************************ Read Konstanten *
       READ-KONS SECTION.
       A.  MOVE 41 TO WH-KEY.
           READ KONSTANT NO LOCK INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 17
               MOVE KO-KTONR(WX) TO WT-KTONR(WX).
           MOVE 1 TO WX WH-KEY.
       C.  READ KONSTANT NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE KO-RENUM TO WH-RENUM.
           MOVE 6 TO WH-KEY.
       D.  READ KONSTANT IGNORE LOCK.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 17
               IF KO-ERLOES(WX + 6) not = 0 AND WT-KTONR(WX) = 0 GO X.
           GO Z.
       X.  DISPLAY "Erlîskonten nicht angelegt" with highlight
               AT 2401 PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************************************
       TAGESSUMMEN SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "270510066000012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Summendruck " with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Summen bis inkl.:          " AT VDU-LP.
           MOVE WM-DATUM TO WC-DATUM WS-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           ADD 321 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       D.  MOVE WS-DATUM TO WH-WERT WZ-DATUM.
           CALL "CAUP" USING "1103216006" WH-CREG.
           IF ESC GO X.
           IF not RET GO D.
           IF WX-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM.
       E.  DISPLAY "Summendruck erwÅnscht?: <ret>= ja, <esc>= nein < >"
               AT 2301.
           CALL "CAUP" USING "0023490000" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
           PERFORM READ-KONS.
           IF ESC GO X.
      *    MOVE WM-DATUM TO WS-DATUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 25
               MOVE 0 TO WS-SUM(WX).
           OPEN I-O BUCHFAKT.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE WE-KOPF(1) TO WM-KOPF.
           MOVE WE-WO(1) TO WM-WO.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           MOVE "TAGSUM.LST" TO WH-DRUNAM.
           IF WM-DRU = 0 or WM-WO = 9 MOVE "LPT1" TO WH-DRUNAM.
           OPEN I-O BUCHUNG.
           MOVE 999999 TO BU-KEY.
           MOVE 0 TO WK-BUKEY.
           START BUCHUNG KEY not < BU-KEY INVALID GO J.
       I.  READ BUCHUNG NEXT AT END GO J.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE BU-KEY TO WK-BUKEY.
       J.  PERFORM ARLIST.
           IF WM-OPEN = 0 GO X.
           PERFORM SABU.
           CLOSE BUCHUNG.
           CLOSE BUCHFAKT.
           DELETE FILE BUCHFAKT.
           PERFORM AUSWURF.
       K.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 24
               MOVE 0 TO KO-ERLOES(WX).
           MOVE 0 TO KO-DATUM KO-ERST.
           REWRITE KO-SATZ.
       X.  MOVE 9 TO WH-PG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* öberleitung neuer Adressen *
       NEUE-ADR SECTION.
       A.  MOVE 0 TO WZ-SEITE WS-BET BF-REN BF-KEY.
           MOVE "K" TO BF-SA.
           START BUCHFAKT KEY > BF-RKEY INVALID GO Z.
       B.  READ BUCHFAKT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
      *-----------------------------------------> SammelbuchungssÑtze <-
           IF BF-SA not = "K" GO B.
           MOVE BF-SATZ TO BU-SATZ.
           WRITE BU-SATZ INVALID REWRITE BU-SATZ.
           GO B.
       Z.  EXIT.
      ***************************************** Rechnungsausgangsliste *
       ARLIST SECTION.
       A.  PERFORM NEUE-ADR.
           MOVE 0 TO WZ-SEITE WS-BET BF-REN BF-KEY.
           MOVE SPACE TO BF-SA.
           START BUCHFAKT KEY > BF-RKEY INVALID GO Z.
       B.  READ BUCHFAKT NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO B.
           COMPUTE WV-DATUM = BF-DAT + 20000000.
           IF WV-DATUM > WS-DATUM GO B.
      *-----------------------------------------> SammelbuchungssÑtze <-
           IF BF-SA not = " " GO B.
           PERFORM BEG-DRU.
           MOVE BF-KTONR TO WD-KZ.
           IF WD-KZ not = 2 GO Q.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-SEITE = 0 GO C.
           IF WZ-ZEILEN > 0 GO G.
           IF WM-KO = 0 MOVE 12 TO WZ-SCHALT
                   else IF WM-DRU = 1 MOVE 1 TO WZ-SCHALT
                                 else MOVE 4 TO WZ-SCHALT.
       C.  MOVE "Rechnungsliste" TO DRL-STR.
           MOVE "per: " TO DRL-STR(53:5).
           MOVE VDU-DATUM TO DRL-STR(58:8).
           ADD 1 TO WZ-SEITE.
           MOVE "Seite: " TO DRL-STR(68:7).
           MOVE WZ-SEITE TO WD-POS.
           MOVE WD-POS TO DRL-STR(75:3).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR.
           PERFORM DRUCK.
           MOVE "Re-Nr.   Kd-Nr. Name
      -       "               Betrag" TO DRL-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  MOVE BF-BET TO DRL-BET.
           ADD BF-BET TO WS-BET.
           MOVE BF-REN TO DRL-REN.
           DIVIDE 10 INTO BF-KTONR GIVING DRL-KTO.
           MOVE BF-KTONR TO DE-KTONR.
       H.  READ DEBITOR IGNORE LOCK INVALID KEY MOVE SPACES TO DE-BEZ.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           MOVE WT-BEZ TO DRL-BEZ(4:).
           MOVE BF-SATZ TO BU-SATZ.
           MOVE BF-FWBET TO BU-FWBET.
           MOVE BF-KURS TO BU-KURS.
           MOVE BF-FSYM TO BU-FSYM.
           MOVE BF-DAT TO BU-VALDAT.
           EVALUATE BF-VM
               WHEN 1 PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 25
                          ADD BF-SUM(WX) TO WS-SUM(WX)
                      end-perform
                      MOVE 0 TO WS-SUM(22) WS-SUM(23)
               WHEN 5 MOVE WT-KTONR(16) TO BU-GK            *> Ustfrei
                      ADD BF-B1 BF-B2 TO WS-SUM(22)
                      ADD BF-SUM(25) TO WS-SUM(25)
                      MOVE "Tu " TO DRL-BEZ(1:2)
                      MOVE 9 TO BU-VM BU-VM
                      MOVE 1 TO BU-Z
               WHEN 6 MOVE WT-KTONR(17) TO BU-GK
                      ADD BF-B1 BF-B2 TO WS-SUM(23)
                      ADD BF-SUM(25) TO WS-SUM(25)
                      MOVE "Su " TO DRL-BEZ(1:2)
                      MOVE 9 TO BU-VM BU-VM
                      MOVE 1 TO BU-Z.
           IF BU-SKTOBAS(1:) = SPACE MOVE 0 TO BU-SKTOBAS.
       I.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO I.
           PERFORM DRUCK.
           GO B.
      *----------------------------------------------------> Listende <-
       Q.  IF WZ-SEITE = 0 GO Z.
           IF WF-STATUS = "10" GO U.
           GO S.
       R.  READ BUCHFAKT NEXT AT END GO U.
           IF ZUGRIF PERFORM BESETZT GO R.
       S.  IF BF-SA not = " " GO R.
           MOVE BF-KTONR TO WD-KZ.
           IF WD-KZ not = 0 GO R.
           MOVE BF-SATZ TO BU-SATZ.
           MOVE BF-FWBET TO BU-FWBET.
           MOVE BF-KURS TO BU-KURS.
           MOVE BF-FSYM TO BU-FSYM.
           MOVE 0 TO BU-VALDAT.
       T.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO T.
           GO R.
       U.  MOVE ALL "ƒ" TO DRL-STR.
           MOVE SPACE TO DRL-NO.
           PERFORM DRUCK.
           MOVE "S u m m e" TO DRL-BEZ.
           MOVE WS-BET TO DRL-BET.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       SABU SECTION.
       A.  PERFORM ERLSUM.
           MOVE 7 TO WZ.
           MOVE 999999 TO BU-KEY.
           MOVE 0 TO WK-BUKEY.
           START BUCHUNG  KEY < BU-KEY INVALID GO E.
       C.  READ BUCHUNG PREVIOUS IGNORE LOCK AT END GO E.
           MOVE BU-KEY TO WK-BUKEY.
           MOVE BU-DAT TO WS-DATUM.
           MOVE WH-RENUM TO DRT-NR1.
           MOVE "bis:" TO DRT-BIS.
           MOVE KO-ERST  TO DRT-NR.
       E.  IF WS-SUM(WZ) = 0 GO K.
           INITIALIZE BU-SATZ.
           MOVE WT-KTONR(WZ - 6) TO BU-KTONR BU-REN.
           MOVE WS-DATUM TO BU-DAT.
           MOVE DRT-TX(36:) TO BU-TX.
           MOVE 19 TO BU-SY.
           MOVE 1 TO BU-SH.
           MOVE WS-SUM(WZ) TO BU-BET.
       G.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO G.       *> Sammelbuchung Protokoll
       K.  IF WZ < 22 ADD 1 TO WZ GO E.
           MOVE SPACE TO DRT-SATZ.
       Z.  EXIT.
      ********************************************** Umsatzaufstellung *
       ERLSUM SECTION.
       A.  MOVE "Tagessummen per:" TO DRT-TEXT.
           MOVE WH-RENUM TO DRT-NR1.
           MOVE KO-ERST  TO DRT-NR.
           MOVE WS-DATUM TO WC-DATUM.
           MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
           MOVE VDU-DATUM TO DRT-SATZ(30:8).
           MOVE "von Re:" TO DRT-SATZ(40:8).
           MOVE "bis:" TO DRT-BIS.
           IF WM-KO = 0 MOVE 12 TO WZ-SCHALT
               else MOVE 4 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-TX.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 1 TO WX.
       F.  IF WS-SUM(WX) not = 0 MOVE WX TO DRS-KZ
                MOVE "%" TO DRS-PZ
                MOVE WT-UST(WX) TO DRS-UST
                MOVE WS-SUM(WX) TO DRS-BETRAG
                IF WX = 1 MOVE "UST-Gruppe:" TO DRS-TEXT(2:11)
                end-if
                PERFORM DRUCK.
           IF WX < 6 ADD 1 TO WX GO F.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE "Erlîsgruppe:" TO DRS-TEXT.
           MOVE 7 TO WX.
       G.  IF WS-SUM(WX) = 0 GO H.
           MOVE WS-SUM(WX) TO DRS-BETRAG.
           IF WX = 24 MOVE "Umsatzsteuer" TO DRS-TEXT.
           IF WX = 25 MOVE "Gesamtsumme " TO DRS-TEXT.
           IF WX < 24 SUBTRACT 7 FROM WX GIVING DRS-UST
                      COMPUTE DRS-KTONR = WT-KTONR(WX - 6) / 10.
           PERFORM DRUCK.
       H.  IF WX < 25 ADD 1 TO WX GO TO G.
       Z.  EXIT.
      ************************************* Steuerzeichen LASER laden **
       LADE-DRU SECTION.
      *                                   1  *> quer A4 /10" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p10h0b0s3T" TO WE-STG(1).
      *                                   2  *> quer A4 /12" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p12h0b0s3T" TO WE-STG(2).
      *                                   3  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p14.6h0b0s3T" TO WE-STG(3).
      *                                   4  *> hoch A4 /10" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p10h0b0s3T" TO WE-STG(4).
      *                                   5  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p12h0b0s3T" TO WE-STG(5).
      *                                   6  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p14.6h0b0s3T" TO WE-STG(6).
      *                                   7  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p16.6h0b0s6T" TO WE-STG(7).
      *                                   8  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l3a6d0O(s0p12h0b0s3T" TO WE-STG(8).
      *                                   9  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p20.0h0b0s6T" TO WE-STG(9).
      *                                  10  *> hoch A4 /12" 5 Zeilen <-
           MOVE "&l26a9.6c0O(s0p12h0b0s3T" TO WE-STG(10).
      *    &la: Format, c: 6 Zeilen, 1/0O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: StÑrke s: Schrift T: Schrift
       Z.  EXIT.
