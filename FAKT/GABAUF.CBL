      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GABAUF.
       DATE-COMPILED.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  P3800.
       OBJECT-COMPUTER.  P3800.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEART.CPY.
           COPY GABSEDEB.CPY.
           COPY GABSEKON.CPY.
           COPY GABSEAUF.CPY.
           COPY GABSESTD.CPY.
           SELECT SAUFTRAG   ASSIGN TO "R:\FAKT\GABISAUF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD IS SAU-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT SAUFKOPF   ASSIGN TO "R:\FAKT\GABIKOPF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY IS SAK-KEY
                             ALTERNATE RECORD SAK-AKEY WITH DUPLICATES
                             ALTERNATE RECORD SAK-KTONR WITH DUPLICATES
                             FILE STATUS IS WF-STATUS.
           COPY GABSEATX.CPY.
           SELECT HILFPOS    ASSIGN TO "GABHILF.DAT"
                             ORGANIZATION RECORD SEQUENTIAL
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
       COPY GABARTIK.CPY.
       COPY GABKONST.CPY.
       COPY GABANBOT.CPY.
       COPY GABSAUFD.CPY.
       COPY GABSTDFD.CPY.
      ******************************************* Hilfsdatei f. Reorg. *
       FD  HILFPOS                     LABEL RECORD STANDARD.
       01  HI-SATZ                     PIC X(124).
      ****************************************************** Textdatei *
       FD  ANTEXT       EXTERNAL       LABEL RECORD STANDARD.
       01  TE-SATZ                     PIC X(60).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-L                    PIC 99.
           03  WL-REST                 PIC 9(11).
       COPY WHCREG.CPY.
       01  WM-BILD.
           03  WB-ATTR                 PIC X(2000).
           03  WB-ZEICH                PIC X(2000).
       01  WH-REG.
           03  WX-PARM.
               05 WX-L                 PIC 99   COMP-X.
               05 WX-P                 PIC 99   COMP-X.
           03  WX-PS REDEFINES WX-PARM PIC 9999 COMP.
           03  WX-LG                   PIC 9999 COMP.
           03  WT-ERFTAB.
               05 WT-KEY               PIC X(7)      OCCURS 17.
           03  WH-PARA.
               05 WH-PX                PIC 9(4)    COMP  OCCURS 3.
           03  WX-GK                   PIC 9(4)    COMP.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88 AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  VDU-AB                  PIC 9999.
           03  VR REDEFINES VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  VDU-SL                  PIC S99     COMP-3.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
      *--------------------------> fuer Datumsaufbereitung im DATUM <--
           03  WK-TASTE                PIC 99        COMP.
           03  WH-VON                  PIC 9999      COMP.
           03  WK-VON                  PIC 9999      COMP.
           03  WX-FIL                  PIC X(37).
           03  WH-TX                   PIC X(51).
           03  WH-MOD                  PIC 99        COMP.
               88  AUFDRU   VALUE 1.    88  MATDRU    VALUE 2.
               88  AUFRE    VALUE 3.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WX-M                    PIC 99        COMP.
           03  WX-J                    PIC 9999      COMP.
           03  WX-R                    PIC 99        COMP.
           03  WH-RAB                  PIC S99V9     COMP-3.
           03  WV-ARNUM                PIC 9(6)      COMP.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WV-NUM                  PIC 9(8)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WH-STD                  PIC S9(6)     COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WA-ANZ                  PIC 9(6)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(6).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9(5)V9.
           03  WH-ANZ2                 PIC S9(4)V99.
           03  WH-L                    PIC 99        COMP.
           03  WH-KAP                  PIC 99        COMP.
           03  WV-KAP                  PIC 99        COMP.
           03  WM-KAP                  PIC 99        COMP.
           03  WV-POS                  PIC 9999      COMP.
           03  WH-AUKEY                PIC X(8).
           03  WM-NEU                  PIC 99        COMP.
           03  WD-LFNUM                PIC ZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-BKZ                  PIC 9(7).
           03  WD-PZ                   PIC 9,9-.
           03  WD-RAB                  PIC Z9-.
           03  WD-UST                  PIC Z9,99-.
           03  WD-APZ                  PIC ZZ9,9-.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZZ9,9-.
           03  WD-MGC                  PIC ZZZ9,99-.
           03  WD-MG                   PIC X(8).
           03  WD-ARTIK                PIC X(30).
           03  WD-POS                  PIC ZZ99.
           03  WD-ART                  PIC ZZ.9999.
           03  WD-AUF                  PIC ZZ/ZZZ9.
           03  WD-AKNUM                PIC ZZZZ/ZZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-KAP                  PIC Z9.
           03  WD-PROZ                 PIC Z9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WH-PREIS                PIC 9(6)V999   COMP-3.
           03  WT-BASIS                PIC S9(7)V99   COMP-3 OCCURS 6.
           03  WH-USTKZ                PIC 9.
           03  WH-BET                  PIC S9(9)V99   COMP-3.
           03  WS-TEIL                 PIC S9(9)V99   COMP-3.
           03  WK-BET                  PIC S9(9)V99   COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WS-RAB                  PIC S9(9)V99   COMP-3.
           03  WS-NET                  PIC S9(9)V99   COMP-3.
           03  WH-MWST                 PIC S9(9)V99   COMP-3.
           03  WH-MEH                  PIC 99         COMP.
           03  WH-REST                 PIC X(37).
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-MA                PIC 99         COMP.
               05 WM-KO                PIC 99         COMP.
               05 WM-PR                PIC 99         COMP.
               05 WM-RB                PIC 99         COMP.
      *--------------------------------------> Adapt-feld aus FIBU <--
           03  WH-ANR                  PIC 99         COMP.
           03  WK-ANR                  PIC 9.
      *-----------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WH-KB                   PIC 99    COMP.
           03  WX-ZEI                  PIC X(57).
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
      *------------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(60).
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "DEBITORE.FII" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON ANTEXT.
       A.  CALL "CADECL" USING WH-TXKEY       WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON AUFTRAG.
       A.  CALL "CADECL" USING "GABISAUF.DAT" WH-CREG.
       DECL-J SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON HILFPOS.
       A.  CALL "CADECL" USING "GABIHILF.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WH-PG = 49 MOVE 1 TO WH-PG GO D.
           OPEN I-O AUFTRAG.
           OPEN I-O AUFKOPF.
           OPEN I-O STUNDEN.
       D.  PERFORM ANBOT.
           IF WH-PG = 49 GO X.
           CLOSE AUFTRAG AUFKOPF STUNDEN.
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
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
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
       E.  DISPLAY WD-MG with highlight AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross," AT 2401
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
      *********************** anzeigen eines vorhandenen Auftragkopfes *
       KOPFANZ SECTION.
       A.  MOVE AK-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0332.
           DIVIDE 10 INTO AK-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0355.
           DISPLAY "    Vkf.:   " AT 0363.
           DISPLAY AK-VKF AT 0373.
           MOVE AK-BEZ TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI AT 0401.
           IF AK-VKF = 0 GO Z.
           ADD AK-VKF 11 GIVING WH-KEY.
       B.  READ KONSTANT NO LOCK NOT INVALID
               DISPLAY "  " AT 0456
               DISPLAY KO-NAME with highlight AT 0459.
           IF ZUGRIF PERFORM BESETZT GO B.
       Z.  EXIT.
      **************************************** nÑchster freier Auftrag *
       FREIER SECTION.
       A.  COMPUTE AK-NUM = WX-J * 10000 + 9999.
           START AUFKOPF KEY < AK-NUM INVALID GO X.
       C.  READ AUFKOPF PREVIOUS NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE WM-DATUM TO WZ-DATUM.
           DIVIDE 10000 INTO AK-NUM GIVING WC-TAG.
           IF WC-TAG not = WZ-JAHR COMPUTE AK-NUM = WX-J * 10000.
           ADD 1 TO AK-NUM.
           GO Z.
       X.  MOVE 0 TO AK-NUM.
           PERFORM NO-REC.
       Z.  EXIT.
      ***************************************** Auftragsreorganisation *
       AUREG SECTION.
       A.  MOVE WS-TASTE TO WK-TASTE.
           OPEN OUTPUT HILFPOS.
           MOVE 0 TO AU-KAP AU-POS WH-KAP WV-KAP WM-KAP.
           MOVE AK-NUM TO AU-NUM.
       B.  CALL "CAUP" using "1307012480000" WH-CREG.
           IF WK-TASTE = 12 DISPLAY "Auftrag wird reorganisiert - bitte
      -        "warten" AT 1010 GO J.
           IF WK-TASTE = 13 DISPLAY "Auftragsduplikat von Auftrag:
      -        "     " AT 1010 GO C.
           IF WK-TASTE = 14 DISPLAY "Auftragslîschung" AT 1010.
           DISPLAY "wirklich lîschen?" with highlight AT 2301
              "<esc>= Start, <ret>= Abbruch" AT 2319.
           CALL "CAUP" USING "0023480000" WH-CREG.
           IF RET MOVE 0 TO WK-TASTE GO W.
           IF NOT ESC GO B.
           GO J.
       C.  DISPLAY "<esc>= Abbruch, <ret>= von Auftrags-Nr." AT 2301.
           CALL "CAUP" USING "0010406006" WH-CREG.
           IF ESC MOVE 0 TO WK-TASTE GO W.
           IF NOT RET OR WH-NUM = 0 GO C.
           IF WH-NUM < 10000 COMPUTE WH-NUM = WX-J * 10000 + WH-NUM.
           MOVE WH-NUM TO AU-NUM WD-AUF WV-NUM.
           IF WV-NUM < 999999;
               IF WV-NUM > 500000 ADD 19000000 TO WV-NUM
                             else ADD 20000000 TO WV-NUM.
           DISPLAY WD-AUF with highlight AT 1040.
       D.  CALL "CAUP" USING "1312012480" WH-CREG.
           IF WK-TASTE = 13 DISPLAY "ab Position: " AT 1227.
           DISPLAY "<ret-leer>= gesamter Auftrag, 01 - 99 = Kapitel"
               AT 2301.
           DISPLAY "<esc>= Ende, <#>= wie <ret> mit Mengeneingabe"
               AT 2401.
           CALL "CAUP" USING "0012402002" WH-CREG.
           MOVE 0 TO WH-MOD.
           IF KIST CALL "CAUP" USING "1500002002" WH-CREG
               MOVE 1 TO WH-MOD.
           IF WOLI GO B.
           IF ESC MOVE 0 TO WK-TASTE GO W.
           IF NOT RET AND NOT KIST GO D.
           MOVE 6 TO VDU-L.
           IF WH-NUM > 0 AND < 99 GO P.
           MOVE 0 TO AU-TZ AU-POS AU-KAP WV-KAP.
           MOVE AU-KEY TO WH-AUKEY.
           DISPLAY "gesamter Auftrag" with highlight AT 1240.
       J.  START AUFTRAG KEY NOT < AU-KEY INVALID PERFORM NO-REC GO W.
       K.  READ AUFTRAG NEXT AT END GO M.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF WK-TASTE = 12 OR 14 GO L.
           IF AU-NUM NOT = WV-NUM GO M.
           IF WV-KAP NOT = 0; IF WV-KAP NOT = AU-KAP GO M.
           WRITE HI-SATZ FROM AU-SATZ.
           DISPLAY AU-KAP AT 1425 " - " AU-POS.
           GO K.
       L.  IF AU-NUM NOT = AK-NUM GO M.
           WRITE HI-SATZ FROM AU-SATZ.
           DISPLAY AU-KAP AT 1425 " - " AU-POS.
           DELETE AUFTRAG.
           GO K.
       M.  UNLOCK AUFTRAG.
           IF WK-TASTE = 14 GO V.
           CLOSE HILFPOS.
           OPEN INPUT HILFPOS.
           MOVE 0 TO WH-VON.
931006*    move 1 to wh-kap.
       O.  READ HILFPOS AT END GO V.
           MOVE HI-SATZ TO AU-SATZ.
           IF WK-TASTE = 13 OR 15;
                IF WM-KAP NOT = 0 MOVE WH-KAP TO AU-KAP end-if
                   IF AU-KAP NOT = WH-KAP MOVE 0 TO WH-VON
                      MOVE AU-KAP TO WH-KAP.
931006     if wk-taste = 12;                               *> nur Reorg
               if au-kap not = wh-kap
                   if au-pos = 0 move 0 to wh-von
931006                  move au-kap to wh-kap.
           MOVE AK-NUM TO AU-NUM.
           IF AU-POS NOT = 0; IF AU-TZ = 0 ADD 10 TO WH-VON.
           MOVE WH-VON TO AU-POS.
931006     move wh-kap to au-kap.
           IF WK-TASTE NOT = 12 PERFORM KOPMENG
               EVALUATE TRUE
                   WHEN ENTF GO O
                   WHEN ESC GO V.
           WRITE AU-SATZ INVALID KEY GO O.
           GO O.
       P.  DISPLAY "Kapitel:    auf Kapitel:   " AT 1227.
           MOVE WH-NUM TO AU-KAP WM-KAP WH-WERT WV-KAP.
           DISPLAY AU-KAP with highlight AT 1236.
           MOVE 0 TO AU-POS AU-TZ.
           MOVE WV-NUM TO AU-NUM.
           MOVE AU-KEY TO WH-AUKEY.
       Q.  DISPLAY "<>= zurÅck, <esc>= Abbruch, <ret-leer>= gleiches Ka
      -        "pitel" AT 2301.
           DISPLAY "<#>= wie <ret> mit Mengeneingabe" AT 2401.
           CALL "CAUP" USING "0012522002" WH-CREG.
           MOVE 0 TO WH-MOD.
           IF KIST CALL "CAUP" USING "1500002002" WH-CREG
               MOVE 1 TO WH-MOD.
           IF WOLI GO D.
           IF ESC GO B.
           IF NOT RET AND NOT KIST OR WH-WERT = 0 GO Q.
           MOVE WH-NUM TO WH-KAP AU-KAP.
           DISPLAY WH-KAP with highlight AT 1252.
           MOVE AK-NUM TO AU-NUM.
           MOVE 0 TO AU-POS AU-TZ.
           START AUFTRAG KEY > AU-KEY INVALID GO S.
       R.  READ AUFTRAG NEXT AT END GO S.
           IF ZUGRIF PERFORM BESETZT GO R.
           IF AU-KAP = WH-KAP AND AK-NUM = AU-NUM
               MOVE WM-KAP TO WH-NUM
               DISPLAY "bereits vorhanden" AT 2401 PERFORM WEITER GO P.
       S.  MOVE WH-AUKEY TO AU-KEY.
           MOVE 15 TO WK-TASTE.
           GO J.
       V.  IF WK-TASTE NOT = 14 GO W.
           DELETE AUFKOPF.
       W.  CLOSE HILFPOS.
           IF WK-TASTE = 15 OPEN OUTPUT HILFPOS
               MOVE 13 TO WK-TASTE
               CALL "CAUP" USING "1307012480000" WH-CREG
               DISPLAY "Auftragsduplikat von Auftrag:" AT 1010
               MOVE WV-NUM TO WD-AUF WH-NUM AU-NUM
               DISPLAY WD-AUF with highlight AT 1040
               MOVE 6 TO VDU-L WH-L
               GO D.
           UNLOCK AUFTRAG.
           CALL "CAUP" USING "1307012480000" WH-CREG.
       Z.  EXIT.
      ************************************ anzeigen und Mengeneingaben *
       KOPMENG SECTION.
       A.  IF VDU-L > 19 PERFORM B IN BS-ZEIL GO A.
           ADD 1 TO VDU-L.
           PERFORM AUF-ANZ.
           IF AU-ARNUM NOT = 0 MOVE AU-ARNUM TO WV-ARNUM.
           PERFORM AUF-REST.
           IF WH-MOD = 0 OR AU-ANZ = 0 GO Z.
           MOVE 40 TO VDU-P.
           IF WH-ANZ NOT = 0 PERFORM DISP-MENGE.
       B.  DISPLAY "<ret>= Mengeneingabe, <Entf>= lîschen, <esc>= Abbruc
      -        "h" AT 2301.
           MOVE WH-ANZ TO WH-WERT.
           IF WT-NK(WH-MEH) = 0 CALL "CAUP" USING "0000406007" WH-CREG.
           IF WT-NK(WH-MEH) = 1 CALL "CAUP" USING "0000405107" WH-CREG.
           IF WT-NK(WH-MEH) = 2 CALL "CAUP" USING "0000404207" WH-CREG.
           IF ENTF; IF AU-BEZ(2) NOT = SPACE ADD -1 TO VDU-L
                    end-if
                    CALL "CAUP" USING "1300013480" WH-CREG
                    MOVE WH-AUKEY TO AU-KEY
                    ADD -1 TO VDU-L
                    GO Z.
           IF ESC GO Z.
           IF NOT RET GO B.
           MOVE WH-WERT TO WH-ANZ.
           PERFORM DISP-MENGE.
           PERFORM BETRAG.
           IF WT-NK(WH-MEH) = 2 MULTIPLY WH-ANZ BY 100 GIVING WH-ANZ.
           IF WT-NK(WH-MEH) = 1 MULTIPLY WH-ANZ BY 100 GIVING WH-ANZ.
           MOVE WH-ANZ TO AU-ANZ.
           PERFORM AUF-REST.
       D.  MOVE 63 TO VDU-P.
           MOVE AU-RAB TO WH-WERT.
           DISPLAY "Rabatt: <ret-leer>= Wiederholung, <ret>= Eingabe"
               AT 2301.
           CALL "CAUP" USING "0000632002" WH-CREG.
           IF WOLI GO B.
           MOVE WH-WERT TO WD-PROZ AU-RAB.
           IF AU-RAB NOT = 0 MOVE AU-RAB TO WD-PROZ
               DISPLAY WD-PROZ with highlight AT VDU-LP
           ELSE DISPLAY "  " AT VDU-LP.
           COMPUTE AU-BET = AU-BET - (AU-BET * AU-RAB / 100).
           PERFORM AUF-REST.
       Z.  EXIT.
      *************************************************** Kopfanzeigen *
       ANZKO SECTION.
       A.  CALL "CAUP" using "1305012480000" WH-CREG.
           IF AK-DATUM = 0 GO Z.
       C.  DISPLAY "Angebotsrabatt:" AT 0501.
           DISPLAY "Skonto:   % Skontotage:    Nettotage:   " AT 0521.
           DISPLAY "Angebotstext:" AT 0601.
           DISPLAY "Aufschlag auf Netto:    " AT 0651.
           DISPLAY "Ust-Kz.:          " AT 0701.
           MOVE AK-RAB TO WD-RAB.
           DISPLAY WD-RAB with highlight AT 0517.
           MOVE AK-KOND TO WH-KK.
           MOVE WH-SK1 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0529.
           MOVE WH-TG1 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0545.
           MOVE WH-TGN TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0559.
           DISPLAY AK-TXKEY with highlight AT 0615.
           MOVE AK-AUF TO WD-PROZ
           DISPLAY WD-PROZ with highlight AT 0672.
           MOVE AK-UST TO WD-KZ.
           DISPLAY WD-KZ AT 0710 "-".
           EVALUATE AK-UST
               WHEN 0 DISPLAY "normal  " with highlight AT 0000
               WHEN 1 DISPLAY "TR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6
               WHEN 2 DISPLAY "SR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6.
      *-------------------------------------------> Kalkulationswerte <-
           SET AX TO 1.
           MOVE 0 TO WH-BET.
       D.  SET WS TO AX.
           SET WI TO AX.
           SUBTRACT 1 FROM WS.
           DIVIDE 3 INTO WS GIVING VDU-L REMAINDER WS.
           COMPUTE VDU-P = WS * 22 + 10.
           ADD 10 TO VDU-L.
           MOVE WI TO WD-KAP.
           DISPLAY WD-KAP AT VDU-LP ":".
           MOVE AK-SUM(AX) TO WD-BET.
           ADD AK-SUM(AX) TO WH-BET.
           ADD 5 TO VDU-LP.
           IF AK-SUM(16) NOT = 0 DISPLAY WD-BET AT VDU-LP.
           IF AX < 15 SET AX UP BY 1 GO D.
           DISPLAY "Summe:" AT 0801.
           MOVE WH-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 0815 "o. Mwst.".
           MOVE AK-SUM(16) TO WD-BET.
           DISPLAY WD-BET with highlight AT 0837 "m. Mwst.".
           SUBTRACT AK-SUM(1) FROM WH-BET.
           DIVIDE WH-BET INTO AK-SUM(17) GIVING WH-WERT.
           COMPUTE WD-APZ = 100 - WH-WERT * 100.
           DISPLAY WD-APZ AT 0862 "% o.Lohn".
       Z.  EXIT.
      *************************************************** Kunden lesen *
       READ-KUNDE SECTION.
       A.  MOVE AK-KTONR TO DE-KTONR.
           READ DEBITOR IGNORE LOCK INVALID NEXT SENTENCE.
       Z.  EXIT.
      ****************************************** Kopfanlage/ -Ñnderung *
       AUF-KOPF SECTION.
       A.  IF WM-MERKER = 1 INITIALIZE WH-KK
               MOVE 3 TO WH-SK1
               MOVE 8 TO WH-TG1
               MOVE 24 TO WH-TGN
               MOVE WH-KK TO AK-KOND
           ELSE PERFORM READ-KUNDE
                PERFORM ANZKO
                GO P.
       B.  IF AK-DATUM = 0 MOVE WM-DATUM TO WC-DATUM
                      ELSE MOVE AK-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0332.
           MOVE WC-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103326006" WH-CREG.
           IF ESC OR WOLI GO Z.
           IF WZ-DATUM = 0 GO B.
           MOVE WZ-DATUM TO WM-DATUM AK-DATUM.
           DISPLAY VDU-DATUM with highlight AT 0332.
       C.  DISPLAY "<esc>= Ende, alpha+<ret>= Kunden suchen, <ret>= Kund
      -        "en-Nr. " AT 2301.
           DISPLAY "'*'+ Kd.Nr.+<ret>= man. Adresse" AT 2401.
           DIVIDE 10 INTO AK-KTONR GIVING WH-NUM.
           MOVE WH-NUM TO WH-WERT.
           CALL "CAUP" USING "0003556010" WH-CREG.
           IF WOLI GO B.
           IF ESC GO Z.
           IF NOT RET GO C.
           MOVE WV-MCODE TO WH-X.
           IF WH-X = "*" MOVE SPACE TO WV-MCODE
               MOVE "$" TO WH-X.
           IF WH-NUM = 0; IF WV-MCODE NOT = SPACE
               CALL "GABANZ" USING "01DEBSUCH" WH-CREG
               CANCEL "GABANZ"
               MOVE WH-NUM TO WH-WERT.
           MOVE WH-NUM TO DE-KTONR WD-KZ.
           IF WD-KZ NOT = 2 GO C.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0355.
           IF AK-KTONR NOT = DE-KTONR GO E.
           IF AK-BEZ NOT = SPACE GO G.
       E.  READ DEBITOR IGNORE LOCK INVALID
               CALL "GABDEBIT" USING "61DEBANL" WH-CREG
               CANCEL "GABDEBIT"
               IF DE-KTONR = 0 GO C
               else IF WH-X NOT = "$" GO E.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE DE-ANREDE TO WH-ANR.
           MOVE DE-BEZ TO WT-BEZ.
           IF ESC GO B.
           MOVE DE-KTONR TO AK-KTONR.
           MOVE DE-MCODE TO AK-AKEY.
           MOVE DE-BEZ TO WX-ZEI AK-BEZ.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI AT 0401.
           MOVE DE-REKTO TO AK-REKTO.
           MOVE WH-ANR TO AK-ANR.
       G.  PERFORM ANZKO.
           PERFORM VKF.
           IF ESC OR WOLI GO C.
           IF WM-MERKER = 1 MOVE 0 TO AK-RAB WM-MERKER GO L.
       H.  MOVE AK-RAB TO WH-WERT.
           DISPLAY "<ret>= Rabatt, <esc>= Abbruch, <>= énderung,"
               AT 2301.
           DISPLAY "zusÑtzlicher Materialgesamtrabatt" AT 2401.
           CALL "CAUP" USING "0005172002" WH-CREG.
           IF WOLI GO G.
           IF ESC GO C.
           IF NOT RET GO H.
           MOVE WH-WERT TO AK-RAB WD-RAB.
           DISPLAY WD-RAB with highlight AT 0517.
      *-----------------------------------------> Zahlungskonditionen <-
       I.  DISPLAY "<ret>= Skontoprozentsatz" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "0005301001" WH-CREG.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-SK1 WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0529.
           IF WH-WERT = 0 MOVE 0 TO WH-TG1 WD-PROZ
               DISPLAY WD-PROZ with highlight AT 0545 GO K.
       J.  DISPLAY "<ret>= Skontotage" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "0005452002" WH-CREG.
           IF WOLI GO I.
           IF NOT RET OR WH-WERT = 0 GO L.
           MOVE WH-WERT TO WH-TG1 WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0545.
       K.  DISPLAY "<ret>= Nettotage" AT 2301.
           DISPLAY "keine Nettotage = ohne Skontoklausel" AT 2401.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "0005592002" WH-CREG.
           IF WOLI GO J.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-TGN WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0559.
           MOVE WH-KK TO AK-KOND.
      *-----------------------------------------------> Angebotstexte <-
       L.  DISPLAY "Angebotstext:" AT 0601.
           DISPLAY "<ret-leer>= kein Text, <ret>= Textname, <tab>= anzei
      -        "gen" AT 2401.
           DISPLAY AK-TXKEY with highlight AT 0615.
           MOVE AK-TXKEY TO WT-TX.
           CALL "CAUP" USING "0206150120" WH-CREG.
           IF WOLI GO K.
           MOVE WT-TX TO AK-TXKEY WH-TXKEY.
           DISPLAY AK-TXKEY with highlight AT 0615.
           IF WT-TX = SPACE DISPLAY "                    " AT 0615
               GO N.
           IF TABL CALL "GABANZ" USING "20ANTEXT" WH-CREG
               CANCEL "GABANZ" GO L.
           OPEN INPUT ANTEXT.
           IF WF-SB = 13 PERFORM NO-REC GO L.
           CLOSE ANTEXT.
       N.  MOVE AK-AUF TO WH-WERT.
           DISPLAY "<ret>= Aufschlag auf Netto" AT 2301.
           CALL "CAUP" USING "0006722002" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO L.
           IF NOT RET GO N.
           MOVE WH-WERT TO AK-AUF WD-PROZ.
           DISPLAY WD-PROZ with highlight AT 0672.
      *-------------------------------------> Kennzeichen Fakturenart <-
       O.  IF ANLAGE MOVE DE-UST TO AK-UST.
           MOVE AK-UST TO WH-WERT.
           MOVE WH-WERT TO AK-UST WD-KZ.
           DISPLAY WD-KZ AT 0710 "-".
           EVALUATE AK-UST
               WHEN 0 DISPLAY "normal  " with highlight AT 0000
               WHEN 1 DISPLAY "TR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6
               WHEN 2 DISPLAY "SR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6.
           DISPLAY "<ret>= Umsatzsteuer, 0= normal, 1= TR frei ı19, 2= S
      -        "R frei ı19)" AT 2301.
           DISPLAY "< >= Ust. vom Kundenkto. holen" with highlight
               AT 2401.
           CALL "CAUP" USING "0007101001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO N.
           IF AB MOVE DE-UST TO WH-WERT
                 SET RET TO TRUE.
           IF not RET GO O.
           IF WH-WERT > 2 GO O.
           MOVE WH-WERT TO AK-UST WD-KZ.
           DISPLAY WD-KZ AT VDU-LP "-".
           EVALUATE AK-UST
               WHEN 0 DISPLAY "normal" with highlight AT 0000
                      CALL "CAUP" USING "1300180040" WH-CREG
               WHEN 1 DISPLAY "TR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6
               WHEN 2 DISPLAY "SR frei ı19" with highlight
                           foreground-color 4 AT 0000 " Uid: "
                      DE-UID with highlight foreground-color 6.
           EVALUATE AK-UST
               WHEN 0; IF DE-UST = 1 DISPLAY "Kunde = Ustfrei"
                        with highlight foreground-color 4 AT 2401
                        PERFORM WEITER
               WHEN 1; IF DE-UID = " " DISPLAY "UID-Nummer fehlt"
                        with highlight foreground-color 4 AT 2401
                        PERFORM WEITER.
      *---------------------------------------------------------------->
           MOVE 0 TO WM-MERKER.
       P.  DISPLAY "<ret-leer>= Ende, <>= énderung, < >= Hilfe, <Bild- 
      -        ">= zur Kartei < >" AT 2301.
           DISPLAY "<Nr+ret>= update (1-rechnen, 2-neue Preise, 3-Ekp-Pr
      -        "eise)" AT 2401.
           CALL "CAUP" USING "0023681001" WH-CREG.
           MOVE WS-TASTE TO WX-TASTE.
           IF AUF CALL "CAUP" USING "190002001408" WH-CREG GO P.
           IF RET; IF WH-NUM > 0 AND < 4 MOVE 0 TO AU-KAP
               PERFORM DURCH
               PERFORM ANZKO GO P.
           IF WOLI; IF AK-RENUM = 0 GO H
                    else DISPLAY "Beretis fakturiert!" AT 2401
                         PERFORM WEITER
                         GO P.
           IF BAB MOVE 49 TO WH-PG
                  MOVE AK-KTONR TO DE-KTONR
                  MOVE "GABKART GABAUF" TO WT-TX
                  CALL "CAUP" USING "0710100316000" WH-CREG
                  ADD 202 VDU-ECK GIVING VDU-LP
                  DISPLAY " bitte warten " AT VDU-LP
                  GO Z.
           IF NOT RET GO P.
       Z.  EXIT.
      ******************************************************************
       VKF SECTION.
       A.  DISPLAY "    Vkf.:" AT 0363.
           DISPLAY "  " AT 0456.
           MOVE AK-VKF TO WH-WERT.
           CALL "CAUP" USING "0103732002" WH-CREG.
           IF ESC OR WOLI GO X.
           IF NOT RET GO A.
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO AK-VKF.
           DISPLAY AK-VKF AT 0373.
           COMPUTE WH-KEY = AK-VKF + 11.
       K.  READ KONSTANT NOT INVALID GO M.
           INITIALIZE KO-VSATZ.
           MOVE SPACE TO WT-TX.
           DISPLAY "Bezeichnung:" AT 2401.
           CALL "CAUP" USING "0224140120" WH-CREG.
           IF ESC GO A.
           MOVE WT-TX TO KO-NAME.
           MOVE AK-VKF TO KO-NUM.
           WRITE KO-SATZ.
       M.  IF ZUGRIF PERFORM BESETZT GO K.
           DISPLAY KO-NAME with highlight AT 0459.
       X.  UNLOCK KONSTANT.
       Z.  EXIT.
      *************************************************** durchrechnen *
       DURCH SECTION.
       A.  MOVE 0 TO AU-POS AU-TZ WS-BET WK-BET WS-RAB
               WT-BASIS(1) WT-BASIS(2) WT-BASIS(3)
               WT-BASIS(4) WT-BASIS(5) WT-BASIS(6).
           MOVE WH-NUM TO WH-MOD.
           MOVE AK-NUM TO AU-NUM.
           INITIALIZE AK-SUMMEN.
           START AUFTRAG KEY NOT < AU-KEY INVALID PERFORM NO-REC GO Z.
       C.  READ AUFTRAG NEXT NO LOCK AT END GO R.
           IF AK-NUM NOT = AU-NUM GO R.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WH-MOD = 60; IF WV-KAP NOT = 0;
               IF WV-KAP NOT = AU-KAP GO R.
           IF AU-ARNUM > 0 MOVE AU-ARNUM TO WV-ARNUM AR-NUM.
           IF AU-POS = 0 OR AU-ART < 6 GO C.
           IF WH-MOD = 1 GO G.
       D.  READ ARTIKEL NO LOCK INVALID KEY GO G.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE AR-VKP TO AU-PREIS.
           MOVE AR-EKP TO AU-EKP.
           IF WH-MOD = 3 MOVE AU-EKP TO AU-PREIS.
           MOVE AR-MIN TO AU-MIN.
           MOVE AR-GRP TO AU-GRP.
           MOVE AR-KAGRP TO AU-KAGRP.
           MOVE AR-RAGRP TO AU-RAGRP.
           MOVE AR-MOGRP TO AU-MOGRP.
       G.  IF AK-AUF = 0 GO J.
           IF AU-KAGRP < 2 GO J.
           IF AK-AUF NOT = 0 COMPUTE AU-PREIS ROUNDED =
                   AU-EKP * (AK-AUF + 100) / 1000
               MULTIPLY AU-PREIS BY 10 GIVING AU-PREIS.
       J.  IF AU-ART = 9 GO Q.
           MOVE AU-ANZ TO WH-ANZ.
           ADD 1 AU-MEH GIVING WH-MEH.
           PERFORM MG.
           IF WH-MOD = 60
               MULTIPLY AU-MIN BY WH-ANZ GIVING WH-BET ROUNDED
               ADD WH-BET TO WS-BET GO C.
           PERFORM BETRAG.
       Q.  REWRITE AU-SATZ.
           GO C.
       R.  IF WH-MOD = 60 GO Z.
           MOVE 1 TO WX.
       S.  COMPUTE WH-BET ROUNDED = WT-BASIS(WX) * WT-UST(WX) / 100.
           ADD WH-BET TO AK-SUM(16).
           IF WX < 6 ADD 1 TO WX GO S.
       W.  REWRITE AK-SATZ.
       Z.  EXIT.
      ********************************************** Betrag ausrechnen *
       BETRAG SECTION.
       A.  MULTIPLY WH-ANZ BY AU-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * AU-RAB / 100).
           MOVE WH-BET TO AU-BET.
           IF AU-KAGRP > 1 AND AK-RAB NOT = 0
                   COMPUTE WH-WERT = WH-BET * AK-RAB / 100
                   SUBTRACT WH-WERT FROM WH-BET.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
           SET AX TO AU-KAGRP.
           IF AX > 15 SET AX TO 15.
           IF AU-ART = 6
               ADD WH-BET TO AK-SUM(AX) WT-BASIS(AU-UST) AK-SUM(16).
           MULTIPLY WH-ANZ BY AU-EKP GIVING WH-BET ROUNDED.
           IF AX > 1 AND AU-ART = 6 ADD WH-BET TO AK-SUM(17).
       Z.  EXIT.
      ******************************************* altes Bild abstellen *
       OLDPIC SECTION.
      *A.  MOVE WT-ERFTAB TO HI-ERFTAB.
           MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_READ_SCR_CHATTRS" USING
                WX-PS WB-ZEICH WB-ATTR WX-LG.
      *    CALL "CAUP" USING "220000SCRAB" WH-CREG.
       Z.  EXIT.
      ********************************** altes Fenster wieder ausgeben *
       ALTBILD SECTION.
       A.  MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_WRITE_SCR_CHATTRS"
               USING WX-PS WB-ZEICH WB-ATTR WX-LG.
      *    MOVE HI-ERFTAB TO WT-ERFTAB.
       Z.  EXIT.
      ********************************************* Auftragsverwaltung *
       ANBOT SECTION.
       A.  MOVE " Auftragsverwaltung" TO WK-GEB.
           MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           COMPUTE WX-J = WX-DATUM / 10000.
       B.  IF WL-CA = 16 CALL "CAUP" USING "08CLOFEN" WH-CREG
                         MOVE 0 TO WM-MERKER
                         MOVE 0 TO WL-CA GO F.   *> zurÅck aus Adr.Verw.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           MOVE 0 TO DE-KTONR WM-KTONR WV-ARNUM WH-MOD WZ-SEITE.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6)
                WH-NUM WH-MWST WS-BET WH-RAB WS-RAB WS-NET
                INITIALIZE AK-SUMMEN.
           MOVE 1 TO WH-KAP WX-M.
           DISPLAY "Kunden-Nr. immer mit ',2' eingeben!" AT 2401.
           DISPLAY "A-Nr.+<ret>  = Auftragsnummer" AT 0625
                   "<ret-leer>   = nÑchste Auftragsnummer" AT 0725
                   "K-Nr.+<ret>  = suchen nach Kunden-Nr." AT 0925
                   "alpha-<ret>  = suchen nach Auftragsbez." AT 1025
                   "< - leer>   = letzte Auftragsnummer" AT 1225
                   "<tab>        = Listung" AT 1325
                   "<Einf>       = Pdf-Datei-Erstellung" AT 1525
                   "<Pos-1>      = DatenÅbernahme" AT 1625
                   "bitte wÑhlen Sie: " with highlight AT 1825.
           DISPLAY "<esc/#>= Ende" AT 2301.
           MOVE SPACE TO WT-TX WV-MCODE.
           CALL "CAUP" USING "0218430115" WH-CREG.
           IF APOS PERFORM DATUEBER GO B.
           IF ESC OR KIST GO Z.
           IF EINF PERFORM XML-DRU GO B.
           IF WOLI GO D.
           IF TABL CALL "GABLIST" USING "80AUFLIST" WH-CREG
                   CANCEL "GABLIST" GO A.
           IF NOT RET GO B.
           MOVE 0 TO WH-WERT WH-NUM.
           MOVE WT-TX TO WV-MCODE.
           IF WV-MCODE = SPACE PERFORM FREIER GO D.
           CALL "CAUP" USING "1500006107" WH-CREG.
           IF WH-NUM = 0 CALL "GABANZ" USING "30AUFSUCH" WH-CREG
                         CANCEL "GABANZ" GO D.
           IF WH-NUM = WH-WERT;
               IF WK < 5 COMPUTE AK-NUM = WX-J * 10000 + WH-NUM
               else COMPUTE AK-NUM = 19000000 + WH-NUM
                    IF WH-NUM < 800000 ADD 1000000 TO AK-NUM
                    end-if
               end-if GO D.
      *--------------------------------> ob lesen nach Auftragsnummer <-
           MOVE WH-NUM TO AK-KTONR.
           CALL "GABANZ" USING "31AUFKUND" WH-CREG.
           CANCEL "GABANZ".
       D.  IF AK-NUM = 0 GO B.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Auftrags-Nr.:           Datum:          " AT 0301.
           DISPLAY "Kunden-Nr.:           " AT 0343.
           MOVE AK-NUM TO WD-AUF WH-NUM.
           DISPLAY WD-AUF with highlight AT 0315.
           MOVE AK-NUM TO WD-AKNUM.
           DISPLAY WD-AKNUM with foreground-color 6 AT 0270.
           MOVE 1 TO WM-MERKER.
       E.  READ AUFKOPF NOT INVALID MOVE 0 TO WM-MERKER
               PERFORM KOPFANZ GO L.
           IF ZUGRIF PERFORM BESETZT GO B.
           INITIALIZE AK-SATZ.
           MOVE WH-NUM TO AK-NUM.
       F.  PERFORM AUF-KOPF.
           CALL "CAUP" using "1305012480000" WH-CREG.
           IF ESC OR WOLI GO B.
       L.  DISPLAY WD-KTO with highlight AT 0355.
           MOVE AK-BEZ TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI AT 0401.
      *-------------------------------> Kennzeichen externe Erfassung <-
           EVALUATE AK-ART
               WHEN 0
               WHEN 10 MOVE 10 TO AK-ART
               WHEN 4  MOVE 14 TO AK-ART.
           WRITE AK-SATZ INVALID REWRITE AK-SATZ.
           IF WH-PG = 49 GO Z.                *> verzweigen in Adr.Verw.
       M.  DISPLAY "Posit.  B e z e i c h n u n g            Menge Meh
      -        "    Preis  %      Betrag  U" AT 0501.
           MOVE 6 TO VDU-L WH-L.
           DISPLAY ALL "ƒƒ" with SIZE 80 AT 0601.
           IF AK-RENUM NOT = 0 MOVE AK-RENUM TO WD-LFNUM
               DISPLAY " Re.:        v.          " AT 0630
               DISPLAY WD-LFNUM with highlight AT 0636
               MOVE AK-REDAT TO WC-DATUM
               IF AK-REDAT = 0 MOVE AK-DATUM TO WC-DATUM
               END-IF PERFORM DATDREH
                      DISPLAY VDU-DATUM AT 0646.
       N.  PERFORM BS-ZEIL.
           DISPLAY "<esc>= Abbruch, <>= KopfÑnderung, < >= Druck, Nr.+<
      -        " >= Kap.Nr." AT 2301.
           DISPLAY "<ret>= Pos.Nr., <#/Entf>= lîschen, <tab> = anzeigen,
      -        " <Einf>= einfÅgen" AT 2401.
           CALL "CAUP" USING "0023656006" WH-CREG.
           PERFORM OLDPIC.
           MOVE VDU-RES TO VDU-LP.
           MOVE 0 TO WL WI WS WH-STD.
           MOVE WS-TASTE TO WX-TASTE.
           IF AUF CALL "CAUP" USING "1500006006" WH-CREG
               ADD -1 TO VDU-L
               IF WH-NUM > 00 AND < 99 MOVE WH-NUM TO WH-KAP WD-RAB
                   DISPLAY "K.: " with highlight AT 0533 WD-RAB GO N
               ELSE CALL "CAUP" USING "190001001508" WH-CREG GO N.
           MOVE WS-TASTE TO WX-TASTE.
           IF KIST OR TABL CALL "CAUP" USING "1500006006" WH-CREG.
           IF AB SET RET TO TRUE
               CALL "GABDRU" USING "10AUFDRUCK" WH-CREG
               CANCEL "GABDRU"
               IF WH-PG = 49 GO Z ELSE GO B.
           MOVE WS-TASTE TO WX-TASTE.
           IF EINF SET SF2 TO TRUE
                   MOVE WX-TASTE TO WS-TASTE.
           IF SF1 OR SF2 PERFORM AUREG GO N.
           IF SF3 PERFORM AUREG GO B.
           IF WH-KAP = 0 MOVE AU-KAP TO WH-KAP.
           IF RET; IF WH-NUM > 0 AND < 99 MOVE WH-NUM TO WH-KAP
               PERFORM KAPITEL
               IF ESC PERFORM ALTBILD
                      ADD -1 TO VDU-L GO N
               else GO N.
           MOVE WH-KAP TO WD-RAB.
           DISPLAY "K.: " with highlight AT 0533 WD-RAB.
           IF WOLI GO F.
           IF ESC GO B.
           IF WORE GO N.
           IF ENTF SET KIST TO TRUE.
           IF TABL MOVE 0 TO WV-KAP WV-POS
               PERFORM AUFLIST GO N.
           IF NOT RET AND NOT KIST GO N.
           MOVE AK-NUM TO AU-NUM.
           IF WH-NUM = 0; IF KIST GO N else GO Q.
      *----------------------------------------> Pos. wird eingegeben <-
           MOVE WH-NUM TO AU-POS.
           DIVIDE 10000 INTO WH-NUM GIVING AU-KAP.
           IF AU-KAP = 0 MOVE WH-KAP TO AU-KAP.
           IF KIST; IF WH-NUM > 0 AND < 99 MOVE WH-NUM TO WH-KAP AU-KAP
               MOVE 0 TO AU-POS.
           MOVE 0 TO AU-TZ.
           MOVE AU-KEY TO WH-AUKEY.
       O.  READ AUFTRAG INVALID INITIALIZE AU-SATZ.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF KIST PERFORM LOESCH GO N.
           PERFORM AUFZEIL.
           GO N.
      *-----------------------------------> autom. nÑchste freie Pos. <-
       Q.  MOVE WH-KAP TO AU-KAP.
           MOVE 9999 TO AU-POS.
           START AUFTRAG KEY < AU-KEY INVALID MOVE 0 TO AU-POS GO T.
       S.  READ AUFTRAG PREVIOUS IGNORE LOCK AT END
               MOVE 0 TO AU-POS GO T.
       T.  IF AU-NUM NOT = AK-NUM or AU-KAP NOT = WH-KAP
               MOVE 0 TO AU-POS.
           MOVE AK-NUM TO AU-NUM.
           ADD 10 TO AU-POS.
           MOVE WH-KAP TO AU-KAP.
           MOVE 0 TO AU-TZ.
           MOVE AU-KEY TO WH-AUKEY.
           INITIALIZE AU-SATZ.
           PERFORM AUFZEIL.
           IF WX-TASTE = 50 GO Q.
           GO N.
       Z.  EXIT.
      ******************************************************************
       AUFZEIL SECTION.
       A.  MOVE VDU-L TO WH-L VDU-SL.
       B.  IF VDU-L > WH-L PERFORM B IN BS-ZEIL
               ADD 1 TO WH-L GO B.
           MOVE WH-L TO VDU-L.
           CALL "CAUP" USING "1300012480" WH-CREG.
           IF AU-NUM NOT = 0 MOVE 0 TO WV-POS WV-KAP
                MOVE 1 TO WS
                PERFORM AUF-ANZ
                MOVE WH-L TO VDU-L
                MOVE AU-ANZ TO WH-STD
                MOVE AU-ARNUM TO AR-NUM WH-NUM WV-ARNUM GO D
                ELSE MOVE WH-AUKEY TO AU-KEY.
           UNLOCK ARTIKEL.
           MOVE 1 TO VDU-P.
           DISPLAY AU-KAP with highlight AT VDU-LP.
           MOVE 4 TO VDU-P.
           DISPLAY AU-POS AT VDU-LP.
       C.  DISPLAY "<ret>= Matchcode, <tab>= Art.Nr., < >= Hilfe, <esc>=
      -        " Ende" AT 2301.
           DISPLAY "nÑchster freier Artikel = Nr.+<#>, <#>= Stundensumme
      -        " je Kapitel" AT 2401.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0200090114" WH-CREG.
           MOVE WX-TASTE TO WK-TASTE.
           IF HELP CALL "CAUP" USING "190004001608" WH-CREG GO C.
           IF ESC GO X.
           MOVE WT-TX TO WV-MCODE AR-AKEY.
           IF KIST CALL "CAUP" USING "1500006070" WH-CREG
               IF WH-NUM = 0 MOVE 60 TO WH-NUM
               MOVE WH-KAP TO AU-KAP WV-KAP
               PERFORM DURCH
               DIVIDE 60 INTO WS-BET GIVING WD-BET
               DISPLAY WD-BET with highlight AT 0363 "Std"
               MOVE WH-AUKEY TO AU-KEY GO C.
           MOVE WK-TASTE TO WX-TASTE.
           PERFORM LIESART.
           IF FEHLER GO C.
           MOVE 0 TO WH-ANZ.
       D.  IF VDU-L > 18 subtract wh-l from vdu-sl
              PERFORM B IN BS-ZEIL
              add wh-l to vdu-sl GO D.
           CALL "CAUP" USING "1303630380" WH-CREG.
           IF AR-NUM NOT = 0 MOVE AR-NUM TO WD-ART AU-ARNUM WV-ARNUM
               DISPLAY WD-ART with highlight AT 0372.
           MOVE 9 TO VDU-P.
           SET TX TO 1.
           IF WS = 0 MOVE AR-BEZA TO AU-BEZ(1) WT-ADR
                     MOVE AR-BEZB TO AU-BEZ(2) WR-ADR(2).
           DISPLAY AU-BEZ(1) WITH SIZE 30 HIGHLIGHT AT VDU-LP.
           IF AU-BEZ(2) NOT = SPACE ADD 100 TO VDU-LP
               DISPLAY AU-BEZ(2) WITH SIZE 30 HIGHLIGHT AT VDU-LP.
           IF WS = 0 OR AU-ART > 5 GO G.
       F.  PERFORM MEHRZEIL.
           IF WOLI OR ESC; IF WL = 1 SET RET TO TRUE ELSE GO X.
       G.  PERFORM VORANZ.
           UNLOCK ARTIKEL.
           MOVE 40 TO VDU-P.
           IF WH-ANZ NOT = 0 PERFORM DISP-MENGE.
           PERFORM STD-EINGABE
               IF ESC; IF WL = 1 GO G ELSE GO X.
           DISPLAY "<ret>= Menge" AT 2301.
           DISPLAY "<tab>= Artikel anzeigen u. Preiswartung, , <#>= Arti
      -        "kelzusatztext" AT 2401.
           MOVE WH-ANZ TO WH-WERT.
           IF WT-NK(WH-MEH) = 0 CALL "CAUP" USING "0000406007" WH-CREG.
           IF WT-NK(WH-MEH) = 1 CALL "CAUP" USING "0000405107" WH-CREG.
           IF WT-NK(WH-MEH) = 2 CALL "CAUP" USING "0000404207" WH-CREG.
      *----------------------------------------------> Artikelabfrage <-
           IF TABL MOVE AU-ARNUM TO WH-NUM
               CALL "GABARTIK" USING "54PRWART" WH-CREG
               CANCEL "GABARTIK" GO G.
           IF KIST MOVE AU-ARNUM TO WH-NUM
               CALL "GABARTIK" USING "52ARTIK" WH-CREG
               CANCEL "GABARTIK" GO G.
           IF ESC; IF WL = 1 GO G ELSE GO X.
           IF WOLI CALL "CAUP" USING "1300402480" WH-CREG GO F.
           MOVE WH-WERT TO WH-ANZ.
           IF NOT RET GO G.
           PERFORM DISP-MENGE.
           IF WT-NK(WH-MEH) = 2 MULTIPLY WH-ANZ BY 100 GIVING WH-ANZ.
           IF WT-NK(WH-MEH) = 1 MULTIPLY WH-ANZ BY 100 GIVING WH-ANZ.
           MOVE WH-ANZ TO AU-ANZ.
           IF WT-MEH(AR-MEH + 1) NOT = SPACE GO L.
       J.  DISPLAY "Mengeneinheiten-Kz. lt. Vorlaufdatei" AT 2301.
           MOVE 48 TO VDU-P.
           CALL "CAUP" USING "0000001001" WH-CREG.
           IF WOLI OR ESC PERFORM MG GO G.
           IF NOT RET OR WH-ANZ = 0 GO J.
           MOVE WH-NUM TO AU-MEH.
           ADD 1 AU-MEH GIVING WH-MEH.
           IF WH-MEH > 20 GO J.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
      *------------------------------------> Preis lt. Datei anzeigen <-
       L.  MOVE 63 TO VDU-P.
           IF AU-RAB NOT = 0 MOVE AU-RAB TO WD-PROZ
               DISPLAY WD-PROZ with highlight AT VDU-LP.
       M.  DISPLAY "Preis: <ret-leer>= Wiederholung, <ret>= Eingabe"
               AT 2301.
           MOVE 52 TO VDU-P.
           MOVE WH-PREIS TO WD-PREIS WH-WERT.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           CALL "CAUP" USING "0000526209" WH-CREG.
           IF WOLI OR ESC MOVE AU-ANZ TO WH-ANZ
               PERFORM MG
               CALL "CAUP" USING "1300522480" WH-CREG GO G.
           IF FEHLER GO M.
           MOVE WH-WERT TO WH-PREIS WD-PREIS AU-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
      *------------------------------------------------> Zeilenrabatt <-
       N.  MOVE 63 TO VDU-P.
           MOVE AU-RAB TO WH-WERT.
           DISPLAY "Rabatt: <ret-leer>= Wiederholung, <ret>= Eingabe"
               AT 2301.
           CALL "CAUP" USING "0000632002" WH-CREG.
           IF WOLI OR ESC GO M.
           IF FEHLER GO N.
           MOVE WH-WERT TO WD-PROZ WH-RAB AU-RAB.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ with highlight AT VDU-LP
           ELSE DISPLAY "  " AT VDU-LP.
           MOVE AU-ANZ TO WH-ANZ.
           PERFORM MG.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
       Q.  PERFORM ERLOEKTO.
           IF ESC GO X.
           IF WOLI GO N.
           MOVE AR-FOLGE TO AU-FOLGE.
           WRITE AU-SATZ INVALID REWRITE AU-SATZ.
           MOVE 0 TO WH-RAB.
           PERFORM ADD-STD.
           GO Z.
       X.  MOVE WH-L TO VDU-L.
           CALL "CAUP" USING "1300012480" WH-CREG.
           SUBTRACT 1 FROM WH-L.
           MOVE WH-L TO VDU-L.
       Z.  EXIT.
      ************************************************ Stunden-Eingabe *
       STD-EINGABE SECTION.
       A.  SET RET TO TRUE.
           EVALUATE WS
               WHEN 0 IF AR-RAGRP not = 98 GO Z
               WHEN 1 IF AU-RAGRP not = 98 GO Z.
           MOVE "0700500320000" TO WH-CALL.
           COMPUTE WL-L = VDU-L - 1.
           CALL "CAUP" USING WH-CALL WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Leistung " with highlight foreground-color 6
               AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Datum: " AT VDU-LP.
           IF WS = 1 MOVE AU-DATUM TO WZ-DATUM GO D.
           IF WH-MODAT = 0 GO E.
           MOVE WH-MODAT TO WZ-DATUM.
       D.  CALL "CAUP" USING "05DATPRF" WH-CREG.
           ADD 210 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       E.  DISPLAY "<ret>= " AT 2301 "Leistungsdatum" with highlight
                foreground-color 6.
           CALL "CAUP" USING "1102106006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WX-DATUM = 0 GO C.
           MOVE WX-DATUM TO AU-DATUM.
           IF WS = 0 MOVE AU-DATUM TO WH-MODAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************ Stunden adieren *
       ADD-STD SECTION.
       A.  IF AU-RAGRP not = 98 GO Z.
           MOVE AU-ARNUM TO GS-NUM.
           MOVE AK-NUM TO GS-AUF.
       C.  READ STUNDEN INVALID MOVE 0 TO GS-ANZ GS-DATUM.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE AK-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WX-DATUM TO GS-DATUM.
           COMPUTE WH-ANZ = AU-ANZ - WH-STD.
           PERFORM MG.
           ADD WH-ANZ TO GS-ANZ.
           REWRITE GS-SATZ INVALID WRITE GS-SATZ.
       Z.  EXIT.
      *************************************** mehrzeiliger Artikeltext *
       MEHRZEIL SECTION.
       A.  MOVE AU-POS TO WV-POS.
      *------------------------------------------> Achtung auf au-min <-
           IF AU-BEZ(2) NOT = SPACE SUBTRACT 1 FROM VDU-L GIVING WH-L
               ELSE MOVE VDU-L TO WH-L.
           MOVE WH-L TO VDU-L.
       B.  IF AU-ARNUM NOT = 0 MOVE AU-ARNUM TO WV-ARNUM.
           IF AU-ART > 5 MOVE AU-REST TO WH-REST
               MOVE 1 TO WL.
           MOVE AU-BEZ(1) TO WT-ADR.
           MOVE AU-BEZ(2) TO WR-ADR(2).
           MOVE 9 TO VDU-P.
           MOVE VDU-L TO WH-L.
       C.  IF VDU-L > 20 PERFORM B IN BS-ZEIL;
               SUBTRACT 1 FROM VDU-SL GO C.
           IF KIST GO G.
           IF AU-BEZ(1) NOT = SPACE
               DISPLAY AU-BEZ(1) with highlight AT VDU-LP.
           IF AU-BEZ(2) NOT = SPACE ADD 100 VDU-LP GIVING VDU-AB
               DISPLAY AU-BEZ(2) with highlight AT VDU-AB.
       F.  DISPLAY "Text: <ret>= Zeilenende, <#>= Textende" AT 2301.
           CALL "CAUP" USING "0200090230" WH-CREG.
           IF ESC; IF WL = 1 SET KIST TO TRUE ELSE GO X.
           IF AU-TZ = 0 MOVE WV-ARNUM TO AU-ARNUM.
       G.  MOVE 9 TO VDU-P.
           PERFORM OB-TX-OK.
           MOVE WR-ADR(1) TO AU-BEZ(1) WD-ARTIK.
           IF AU-TZ = 0; IF AU-BEZ(1) = SPACE
               AND AU-BEZ(2) = SPACE GO F.
           SUBTRACT 1 FROM WH-L GIVING VDU-L.
           ADD 100 TO VDU-LP.
           DISPLAY WD-ARTIK with highlight AT VDU-LP.
           MOVE WR-ADR(2) TO AU-BEZ(2) WD-ARTIK.
           IF WD-ARTIK NOT = SPACE ADD 100 TO VDU-LP
               DISPLAY WD-ARTIK with highlight AT VDU-LP.
           IF WOLI MOVE WH-L TO VDU-L GO T.
           IF AU-TX = SPACE; IF AU-TZ > WI
               SUBTRACT 1 FROM VDU-L
               SET KIST TO TRUE GO T
               ELSE GO F.
      *------------------------------------------> weitere Textzeilen <-
       K.  IF KIST AND AU-TZ = 0 AND WS = 0 GO V.
           INITIALIZE AU-REST.
           IF AU-TZ = 0 MOVE WV-ARNUM TO AU-ARNUM.
           REWRITE AU-SATZ INVALID WRITE AU-SATZ MOVE 1 TO WL.
           IF AU-TZ = 99 DISPLAY "kein weiterer Text mîglich" AT 2401
               PERFORM WEITER GO X.
       L.  subtract wh-l from vdu-sl.
           PERFORM BS-ZEIL.
           add wh-l to vdu-sl.
           ADD 1 TO AU-TZ.
       R.  MOVE AU-KEY TO WH-AUKEY.
           READ AUFTRAG INVALID INITIALIZE AU-SATZ GO S.
           IF ZUGRIF PERFORM BESETZT GO R.
           IF AU-NUM NOT = AK-NUM OR AU-POS NOT = WV-POS
               INITIALIZE AU-SATZ GO S.
           MOVE AU-TZ TO WI.
       S.  MOVE WH-AUKEY TO AU-KEY.
           GO B.
      *-----------------------------------------------------------------
       T.  IF AU-TZ = 0 GO C.
           START AUFTRAG KEY < AU-KEY INVALID GO C.
       U.  READ AUFTRAG PREVIOUS.
           IF ZUGRIF PERFORM BESETZT GO U.
       V.  IF VDU-L < 8 CALL "CAUP" USING "1307012480" WH-CREG GO R.
           IF KIST GO X.
           IF AU-BEZ(2) NOT = SPACE SUBTRACT 100 FROM VDU-LP.
           IF AU-BEZ(1) NOT = SPACE SUBTRACT 100 FROM VDU-LP.
           GO R.
       X.  IF VDU-SL > 6 MOVE VDU-SL TO WH-L
               ELSE MOVE 6 TO WH-L.
           IF ESC MOVE VDU-SL TO VDU-L
               ELSE MOVE WH-REST TO AU-REST
                    IF AU-TZ = 0 MOVE WV-ARNUM TO AU-ARNUM
                        ELSE MOVE 0 TO AU-ARNUM.
           UNLOCK AUFTRAG.
       Z.  EXIT.
      ********************************* ob Textbeginn ohne Leerzeichen *
       OB-TX-OK SECTION.
       A.  IF WR-ADR(1) = SPACE GO B.
           IF WR-ADR(1)(1:1) = space
               MOVE WR-ADR(1)(2:) TO WR-ADR(1)(1:)
               GO A.
       B.  IF WR-ADR(2) = SPACE GO Z.
       C.  IF WR-ADR(2)(1:1) = space
               MOVE WR-ADR(2)(2:) TO WR-ADR(2)(1:)
               GO C.
       Z.  EXIT.
      *************************************** mehrzeiliger Kapiteltext *
       KAPITEL SECTION.
       A.  MOVE VDU-L TO WH-L.
           PERFORM BS-ZEIL.
           SUBTRACT 1 FROM VDU-L GIVING VDU-SL.
           MOVE VDU-L TO WH-L.
           MOVE 0 TO AU-POS AU-TZ WI.
           MOVE WH-KAP TO AU-KAP.
           MOVE AK-NUM TO AU-NUM.
           MOVE 2 TO VDU-P.
           DISPLAY AU-KAP AT VDU-LP.
       B.  MOVE AU-KEY TO WH-AUKEY.
           READ AUFTRAG INVALID INITIALIZE AU-SATZ.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE WH-AUKEY TO AU-KEY.
           MOVE 1 TO AU-ART.
           IF WI = 9 MOVE WH-MEH TO AU-MEH.
           MOVE AU-BEZ(1) TO WT-ADR.
           MOVE AU-BEZ(2) TO WR-ADR(2).
           MOVE 9 TO VDU-P.
           MOVE VDU-L TO WH-L.
       C.  IF VDU-L > 20 PERFORM B IN BS-ZEIL;
               SUBTRACT 1 FROM VDU-SL GO C.
           IF KIST GO G.
           IF AU-BEZ(1) NOT = SPACE
               DISPLAY AU-BEZ(1) with highlight AT VDU-LP.
           IF WI = 0 GO L.
       D.  IF AU-BEZ(2) NOT = SPACE ADD 100 VDU-LP GIVING VDU-AB
               DISPLAY AU-BEZ(2) with highlight AT VDU-AB.
       F.  DISPLAY "Text: <ret>= Zeilenende, <#>= Textende" AT 2301.
           CALL "CAUP" USING "0200090230" WH-CREG.
           IF ESC GO X.
       G.  MOVE 9 TO VDU-P.
           SUBTRACT 1 FROM WH-L GIVING VDU-L.
           MOVE WR-ADR(1) TO AU-BEZ(1) WD-ARTIK.
           ADD 100 TO VDU-LP.
           DISPLAY WD-ARTIK with highlight AT VDU-LP.
           MOVE WR-ADR(2) TO AU-BEZ(2) WD-ARTIK.
           IF WD-ARTIK NOT = SPACE ADD 100 TO VDU-LP
               DISPLAY WD-ARTIK with highlight AT VDU-LP.
           IF WOLI GO J.
           IF AU-TX = SPACE SUBTRACT 1 FROM VDU-L GO Z.
      *------------------------------------------> weitere Textzeilen <-
       I.  WRITE AU-SATZ INVALID REWRITE AU-SATZ.
           IF AU-TZ = 99 DISPLAY "kein weiterer Text mîglich" AT 2401
               PERFORM WEITER GO X.
           subtract wh-l from vdu-sl.
           PERFORM BS-ZEIL.
           add wh-l to vdu-sl.
           ADD 1 TO AU-TZ.
           GO B.
      *-----------------------------------------------------------------
       J.  MOVE WH-L TO VDU-L.
           IF AU-TZ = 0 GO C.
           START AUFTRAG KEY < AU-KEY INVALID GO C.
       K.  READ AUFTRAG PREVIOUS.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF VDU-L < 8 CALL "CAUP" USING "1307012480" WH-CREG GO B.
           IF AU-BEZ(2) NOT = SPACE SUBTRACT 100 FROM VDU-LP.
           IF AU-BEZ(1) NOT = SPACE SUBTRACT 100 FROM VDU-LP.
           GO B.
      *----------------------------------------> Eingabe Kapitelpreis <-
       L.  MOVE AU-MEH TO WD-KZ.
       M.  MOVE 5 TO VDU-P.
           IF AU-MEH = 1 DISPLAY "1/K" with highlight AT VDU-LP
                    ELSE DISPLAY "0/E" with highlight AT VDU-LP.
       N.  DISPLAY "0 = Einzelpreise, 1 = Kapitelpreise" AT 2301.
           MOVE AU-MEH TO WH-WERT.
           CALL "CAUP" USING "0000051001" WH-CREG.
           IF ESC GO X.
           IF not RET GO N.
           MOVE WH-WERT TO AU-MEH WH-MEH.
           IF AU-MEH > 1 GO N.
           PERFORM M.
           MOVE 9 TO VDU-P WI.
           IF AU-MEH = 0 GO D.
           IF AU-TX = SPACE GO O.
           REWRITE AU-SATZ.
       O.  START AUFTRAG KEY > AU-KEY INVALID GO Q.
       P.  READ AUFTRAG NEXT NO LOCK AT END GO Q.
           IF AU-NUM NOT = AK-NUM OR AU-KAP NOT = WH-KAP GO Q.
           IF ZUGRIF PERFORM BESETZT GO P.
           IF AU-ART NOT = 9 GO P.
           DISPLAY AU-KAP with highlight AT 2401.
           DISPLAY AU-POS WITH BEEP AT 2404.
           DISPLAY "ACHTUNG - Alternativposition" AT 2409.
           PERFORM WEITER.
           GO P.
       Q.  MOVE WH-AUKEY TO AU-KEY GO B.
       X.  IF VDU-SL > 6 MOVE VDU-SL TO WH-L
               ELSE MOVE 6 TO WH-L.
           IF ESC MOVE VDU-SL TO VDU-L.
           UNLOCK AUFTRAG.
       Z.  EXIT.
      **************************************** anzeigen von Positionen *
       AUFLIST SECTION.
       A.  CALL "CAUP" using "1307012480000" WH-CREG.
           IF WH-NUM > 0 AND WH-NUM < 99
               MULTIPLY WH-NUM BY 10000 GIVING WH-NUM.
           MOVE AK-NUM TO AU-NUM.
           MOVE WH-NUM TO AU-POS.
           MOVE 0 TO AU-TZ.
           DIVIDE 10000 INTO WH-NUM GIVING AU-KAP.
           IF AU-POS NOT = 0; IF AU-KAP = 0 GO Z.
           START AUFTRAG KEY NOT < AU-KEY INVALID PERFORM NO-REC GO Z.
       B.  MOVE 19 TO WH-L.
           MOVE 1 TO WR.
           MOVE 6 TO VDU-L.
       C.  READ AUFTRAG NEXT WITH NO LOCK AT END GO Z.
           IF AU-NUM NOT = AK-NUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WR < 9 GO E.
           IF VDU-L < 20 GO E.
       D.  DISPLAY "<esc> oder <#>= Ende, <ret-leer>= weiterlisten"
               AT 2301.
           CALL "CAUP" USING "0023480000" WH-CREG.
           IF ESC OR KIST MOVE 0 TO AU-NUM GO Z.
           IF NOT RET GO D.
           MOVE 2 TO WR.
       E.  IF VDU-L < WH-L ADD 100 TO VDU-LP GO K.
       F.  IF VDU-L > 19 PERFORM B IN BS-ZEIL GO F.
           MOVE 19 TO WH-L.
           ADD 100 TO VDU-LP.
       K.  PERFORM AUF-ANZ.
           PERFORM AUF-REST.
           GO C.
       Z.  EXIT.
      **************************************** anzeigen einer Position *
       AUF-ANZ SECTION.
       A.  IF WV-POS NOT = AU-POS OR WV-KAP NOT = AU-KAP
               IF AU-POS = 0 MOVE 2 TO VDU-P
                   DISPLAY AU-KAP AT VDU-LP
                   PERFORM M IN KAPITEL
               ELSE MOVE 1 TO VDU-P
                   DISPLAY AU-KAP with highlight AT VDU-LP
                   MOVE 4 TO VDU-P
                   DISPLAY AU-POS AT VDU-LP
               END-IF MOVE AU-POS TO WV-POS
                   MOVE AU-KAP TO WV-KAP
                   MOVE AU-BEZ(1) TO WT-ADR
                   MOVE AU-BEZ(2) TO WR-ADR(2).
           MOVE 9 TO VDU-P.
           CALL "CAUP" using "1300002480" WH-CREG.
           DISPLAY AU-BEZ(1) with highlight AT VDU-LP.
           IF AU-BEZ(2) NOT = SPACE ADD 100 TO VDU-LP
               DISPLAY AU-BEZ(2) with highlight AT VDU-LP.
       Z.  EXIT.
      ************************************** anzeigen der Restposition *
       AUF-REST SECTION.
       A.  IF AU-ANZ NOT = 0 MOVE AU-ANZ TO WH-ANZ
               MOVE 40 TO VDU-P
               ADD 1 AU-MEH GIVING WH-MEH
               PERFORM MG
               DISPLAY WD-MG with highlight AT VDU-LP
               IF WT-MEH(WH-MEH) NOT = SPACE
                   MOVE 48 TO VDU-P
                   DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           IF AU-PREIS NOT = 0 MOVE 52 TO VDU-P
               MOVE AU-PREIS TO WH-PREIS WD-PREIS
               DISPLAY WD-PREIS with highlight AT VDU-LP.
           MOVE 0 TO WH-RAB.
           IF AU-RAB NOT = 0 MOVE AU-RAB TO WD-PROZ
               MOVE 63 TO VDU-P
               DISPLAY WD-PROZ with highlight AT VDU-LP.
           MOVE 66 TO VDU-P.
           ADD 1 TO WR.
           IF AU-BET = 0 GO Z.
           IF AU-ART = 9 DISPLAY "alternativ" AT VDU-LP
               ELSE MOVE AU-BET TO WD-BET
                    DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE AU-UST TO WH-USTKZ.
           MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
       Z.  EXIT.
      ***************************************** Lesen nach Artikel-Nr. *
       LIESART SECTION.
       A.  MOVE WX-TASTE TO WK-TASTE.
           IF KIST MOVE 5 TO WX-TASTE.
           IF WX-TASTE = 98 GO B.
           IF NOT TABL GO G.
           CALL "CAUP" USING "1500006070" WH-CREG.
           IF WH-NUM = 0 GO Y.
           IF WK-TASTE = 6 GO D.
           MOVE WH-NUM TO AR-NUM.
       B.  READ ARTIKEL NO LOCK KEY AR-KEY NOT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  MOVE VDU-LP TO VDU-AB.
           CALL "GABARTIK" USING "51ARTANL" WH-CREG.
           CANCEL "GABARTIK".
           MOVE VDU-AB TO VDU-LP.
           GO X.
       D.  MOVE WH-NUM TO AR-NUM WV-ARNUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO C.
       E.  READ ARTIKEL NEXT NO LOCK AT END MOVE 999999 TO AR-NUM.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF AR-NUM = WV-ARNUM ADD 1 TO WV-ARNUM GO E.
           MOVE WV-ARNUM TO WH-NUM AR-NUM.
           GO C.
      *----------------------------------------> lesen nach Matchcode <-
       G.  READ ARTIKEL NO LOCK KEY AR-AKEY
               INVALID MOVE 0 TO AR-NUM GO H.
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
      ********************************* lîschen von Auftragspositionen *
       LOESCH SECTION.
       A.  IF AU-POS = 0 AND AU-KAP = 0 GO Z.
           CALL "CAUP" USING "0719100350000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Positionslîschung " with highlight AT VDU-LP.
           MOVE AU-POS TO WV-POS.
           MOVE AU-KAP TO WV-KAP.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "von Pos.:         bis Pos.:" AT VDU-LP.
           DISPLAY "<esc>= ja, <ret>= Abbruch, <F1>= Hilfe" AT 2301.
           DISPLAY "0 + <esc-leer>= bis Kapitelende, <esc-leer>= nur die
      -        "se Position" AT 2401.
           ADD 10 TO VDU-LP.
           DISPLAY AU-KAP with highlight AT VDU-LP " "
                   AU-POS with highlight.
           COMPUTE WH-WERT = AU-KAP * 10000 + AU-POS.
           CALL "CAUP" USING "1002316006" WH-CREG.
           IF HELP CALL "CAUP" USING "190006001708" WH-CREG GO B.
           IF RET GO Y.
           IF NOT ESC GO B.
      *------------------------------------------------> bis Position <-
           SET RET TO TRUE.
           CALL "CAUP" USING "1500006070" WH-CREG.
           if wv-mcode(1:5) = "0    "
               DISPLAY "Kapitelende" with highlight AT VDU-LP
      *    IF WH-WERT = 0 DISPLAY "Kapitelende" with highlight AT VDU-LP
               MOVE 9999 TO WV-POS GO F.
           DIVIDE 10000 INTO WH-WERT GIVING WV-KAP REMAINDER WV-POS.
           IF WV-KAP = 0 MOVE WH-KAP TO WV-KAP.
           IF WH-WERT > 0 AND < 99 MOVE WH-WERT TO WH-KAP WV-KAP
               MOVE 0 TO AU-POS.
           DISPLAY WV-KAP with highlight AT VDU-LP " "
                   WV-POS with highlight AT 0000.
           COMPUTE WH-STD =  AU-ANZ * -1.
           PERFORM ADD-STD.
           GO F.
       D.  READ AUFTRAG NEXT AT END GO X.
           IF AU-NUM NOT = AK-NUM OR AU-KAP NOT = WV-KAP GO X.
           IF AU-POS > WV-POS GO X.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE AU-ANZ TO WH-STD.
           PERFORM ADD-STD.
       F.  DELETE AUFTRAG INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO F.
           GO D.
       X.  MOVE 0 TO AK-SUM(16) AK-SUM(17).
           REWRITE AK-SATZ.
           UNLOCK AUFTRAG.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       VORANZ SECTION.
       A.  IF WS = 1 MOVE 0 TO AR-FIXPR
               MOVE AU-PREIS TO WH-PREIS
               MOVE AU-UST TO WH-USTKZ
               MOVE AU-ANZ TO WH-WERT WH-ANZ
               ADD 1 AU-MEH GIVING WH-MEH
               PERFORM AUF-REST
           ELSE ADD AR-MEH 1 GIVING WH-MEH
               MOVE AR-MEH TO AU-MEH
               MOVE AR-MIN TO AU-MIN
               MOVE AR-VKP TO WH-PREIS
               MOVE AR-EKP TO AU-EKP
               MOVE AR-GRP TO AU-GRP
               MOVE AR-KAGRP TO AU-KAGRP
               MOVE AR-RAGRP TO AU-RAGRP
               MOVE AR-MOGRP TO AU-MOGRP
               MOVE AR-UST TO WH-USTKZ AU-UST.
           MOVE AU-REST TO WH-REST.
           MOVE 48 TO VDU-P.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           MOVE 52 TO VDU-P.
           MOVE WH-PREIS TO WD-PREIS WH-WERT.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           IF AR-FIXPR NOT = 0 MOVE AR-FIXPR TO WD-PREIS
               ADD 1 TO VDU-L
               DISPLAY WD-PREIS AT VDU-LP
               SUBTRACT 1 FROM VDU-L.
           MOVE 63 TO VDU-P.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ERLOEKTO SECTION.
       A.  MOVE WH-BET TO WD-BET WK-BET AU-BET.
           MOVE 66 TO VDU-P.
           IF AU-ART = 9 DISPLAY "alternativ   " AT VDU-LP
                    ELSE DISPLAY WD-BET with highlight AT VDU-LP.
       B.  MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
           DISPLAY "<ret>= Zeile ok., <>= zurÅck, < >= Hilfe, <#>= alte
      -        "rnativ" AT 2301.
           IF WL = 0 DISPLAY ", <esc>= Abbruch" AT 2359.
           CALL "CAUP" USING "0000791001" WH-CREG.
           MOVE WS-TASTE TO WX-TASTE.
           IF HELP CALL "CAUP" USING "190005001808" WH-CREG GO B.
           IF ESC; IF WL = 1 DISPLAY "nicht mîglich" AT 2401 GO B
                ELSE GO Z.
           IF WOLI GO Z.
           MOVE 66 TO VDU-P.
           IF KIST; IF AU-ART = 9 MOVE 6 TO AU-ART
                    ELSE MOVE 9 TO AU-ART.
           IF AU-ART < 6 MOVE 6 TO AU-ART.
           IF AU-ART = 9 DISPLAY "alternativ   " AT VDU-LP
                    ELSE DISPLAY WD-BET with highlight AT VDU-LP.
           IF NOT RET GO B.
           IF AU-ART = 6 GO E.
           MOVE AU-SATZ TO WT-BEZ.
           MOVE 0 TO AU-POS AU-TZ.
       C.  READ AUFTRAG NO LOCK INVALID KEY MOVE 0 TO AU-MEH.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF AU-MEH = 1 MOVE WT-BEZ TO AU-SATZ
               DISPLAY "keine Alternativposition erlaubt,"
                  WITH BEEP AT 2401
               PERFORM WEITER GO B.
           MOVE WT-BEZ TO AU-SATZ.
      *---------------------------------> bei Mehrplatzsystem sperren <-
       E.  IF AU-ART = 9 GO Z.
           ADD WH-BET TO WS-BET.
           IF AU-KAGRP > 1 ADD WK-BET TO WS-RAB;
               IF AK-RAB NOT = 0
                   COMPUTE WH-WERT = WK-BET * AK-RAB / 100
                   SUBTRACT WH-WERT FROM WK-BET.
           ADD WK-BET TO WS-NET.
           SUBTRACT AU-BET FROM WS-NET.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
           SET AX TO AU-KAGRP.
           IF AX > 15 SET AX TO 15.
           ADD WK-BET TO AK-SUM(AX) WT-BASIS(WH-USTKZ).
           IF AK-SUM(16) NOT = 0 MOVE 0 TO AK-SUM(16) AK-SUM(17)
               REWRITE AK-SATZ.
       Z.  EXIT.
      ******************************************************************
       XML-DRU SECTION.
       A.  MOVE "PDFPRINT.BAT" TO WD-UPON.
           DISPLAY LOW-VALUE AT 2001.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ***************************** öbernahme Daten aus ext. Erfassung *
       DATUEBER SECTION.
       A.  CALL "CAUP" USING "270510056000014" WH-CREG.
           OPEN I-O SAUFKOPF SAUFTRAG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " DatenÅbernahme " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <Einfg>= Start < >" AT 2301.
       C.  CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC GO X.
           IF not EINF GO C.
           MOVE LOW-VALUES TO SAK-SATZ.
           MOVE 10 TO AK-ART.
           START SAUFKOPF INVALID GO X.
       G.  READ SAUFKOPF NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.

       X.  CLOSE SAUFKOPF SAUFTRAG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
