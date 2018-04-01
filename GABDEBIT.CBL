      $SET LINKCOUNT"128" ANS85"SYNTAX" BOUND AUTOLOCK
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GABDEBIT.
      ******************************************************************
      *              Anlage und énderung Kundendatei                   *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEDEB.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
      *---------------------------------------> Adapt.Feld aus FIBU <--
           03  WH-ANR               PIC 99        COMP.
           03  WD-KZ                   PIC 9.
           03  WH-DEKEY                PIC X(6).
           03  WK-ANR                  PIC 9.
      *----------------------------------------> Konditionentabelle <--
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-PZ                   PIC 9,9.
           03  WD-PROZ                 PIC Z9,9.
      *-----------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WM-MERKER               PIC 9       COMP   VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
       COPY GABEXT.CPY.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "GABDEBIT.DAT" WH-CREG.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 60 PERFORM DEBIT GO Z.
           IF WL-CA = 61 PERFORM DEBIT GO Z.
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
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  ADD 415 VDU-ECK GIVING VDU-REL.
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
               DISPLAY WR-ADR(WR) WITH SIZE 34 HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
       Z.  EXIT.
      ******************************************************************
       DEBIT SECTION.
       A.  CALL "CAUP" USING "0711011280000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Verwaltung Kunden " with highlight AT VDU-REL.
       C.  ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.:" AT VDU-REL.
           IF WL-CA = 61 GO P.
           DISPLAY "<ret>= Anl./Aend., alpha+<ret>= suchen, <tab>=naechs
      -        "te freie Kd.Nr., <esc>= Ende" AT 2301.
           DISPLAY "alpha+<Einf>= Str.-Suche" AT 2401.
           CALL "CAUP" USING "1002089009" WH-CREG.
           ADD 208 VDU-ECK GIVING VDU-REL.
           DISPLAY "       " AT VDU-REL.
           IF ESC GO X.
           IF FEHLER GO C.
           IF RET or EINF GO M.
           MOVE HIGH-VALUE TO DE-KEY.
           START DEBITOR KEY < DE-KEY INVALID MOVE 2 TO DE-KTONR GO G.
       F.  READ DEBITOR PREVIOUS NO LOCK AT END MOVE 2 TO DE-KTONR.
           IF ZUGRIF PERFORM BESETZT GO F.
       G.  MOVE DE-KTONR TO WH-WERT.
           IF WH-WERT = 999992
               DISPLAY "hîchste mîgliche Nr. schon vergeben" AT 2401
               GO C.
           ADD 1 WH-WERT GIVING WH-NUM.
       M.  MOVE 0 TO WM-MERKER.
           IF EINF MOVE WH-MCODE TO WT-TX
               CALL "GABANZ" USING "01STRSUCH" WH-CREG
               CANCEL "GABANZ"
               GO N.
           IF WH-NUM = 0 CALL "GABANZ" USING "01DEBSUCH" WH-CREG
               CANCEL "GABANZ".
       N.  IF WH-NUM = 0 GO C.
           MOVE WH-WERT TO DE-KTONR.
       P.  READ DEBITOR INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO P.
           PERFORM DEB-ANL.
           IF WL-CA = 61; IF ESC MOVE 0 TO DE-KTONR GO X ELSE GO X.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK DEBITOR.
       Z.  EXIT.
      *****************************************************************
       DEB-ANL SECTION.
       A.  COMPUTE WH-WERT = DE-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-KTO with highlight AT VDU-REL.
           ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY "Konto-Nr.: " AT VDU-REL.
           ADD 302 VDU-ECK GIVING VDU-REL.
           DISPLAY "Anrede:" AT VDU-REL.
           ADD 402 VDU-ECK GIVING VDU-REL.
           DISPLAY "Bezeichnung:" AT VDU-REL.
           ADD 1002 VDU-ECK GIVING VDU-REL.
           DISPLAY "Rechnung an:" AT VDU-REL.
           ADD 1102 VDU-ECK GIVING VDU-REL.
           DISPLAY "Telefon:" AT VDU-REL.
           ADD 223 VDU-ECK GIVING VDU-REL.
           DISPLAY "Kurzname:" AT VDU-REL.
           ADD 550 VDU-ECK GIVING VDU-REL.
           DISPLAY "Ust:" AT VDU-REL.
           ADD 650 VDU-ECK GIVING VDU-REL.
           DISPLAY "UID:" AT VDU-REL.
           ADD 750 VDU-ECK GIVING VDU-REL.
           DISPLAY "Zahlungskondition:" AT VDU-REL.
           ADD 1050 VDU-ECK GIVING VDU-REL.
           IF ANLAGE GO B.
           MOVE DE-ANREDE TO WH-ANR WD-KZ.
           ADD 311 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-KZ AT VDU-REL.
           ADD 315 VDU-ECK GIVING VDU-REL.
           DISPLAY WT-TXT(WH-ANR + 1) with highlight AT VDU-REL.
           PERFORM HOLADR.
           ADD 235 VDU-ECK GIVING VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-REL.
           ADD 1015 VDU-ECK GIVING VDU-REL.
           DIVIDE 10 INTO DE-REKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-REL.
           ADD 555 VDU-ECK GIVING VDU-REL.
           MOVE DE-UST TO WD-KZ.
           DISPLAY WD-KZ AT VDU-REL "-".
           EVALUATE DE-UST
               WHEN 0 DISPLAY "normal  " with highlight AT 0000
               WHEN 1 DISPLAY "frei ı19" with highlight
                           foreground-color 4 AT 0000.
           ADD 655 VDU-ECK GIVING VDU-REL.
           DISPLAY DE-UID with highlight foreground-color 6 AT VDU-REL.
           ADD 271 VDU-ECK GIVING VDU-REL.
           PERFORM KOND-ANZ.
           ADD 1115 VDU-ECK GIVING VDU-REL.
           DISPLAY DE-TEL with SIZE 33 highlight AT VDU-REL.
           ADD 1055 VDU-ECK GIVING VDU-REL.
           GO C.
       B.  MOVE DE-KEY TO WH-DEKEY.
           INITIALIZE DE-SATZ.
           MOVE WH-DEKEY TO DE-KEY.
       C.  MOVE DE-ANREDE TO WH-ANR WH-WERT.
       D.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine énderung" AT 2301.
           MOVE 2401 TO VDU-REL.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
               SUBTRACT 1 FROM WY GIVING WK-ANR
               DISPLAY WK-ANR AT VDU-REL ":" WT-TXT(WY)
               ADD 10 TO VDU-REL.
           CALL "CAUP" USING "1003111001" WH-CREG.
           ADD 311 VDU-ECK GIVING VDU-REL.
           IF ESC OR WOLI GO Z.
           IF NOT RET OR WH-WERT > 6 GO D.
           MOVE WH-WERT TO WH-ANR WD-KZ.
           DISPLAY WD-KZ AT VDU-REL.
           ADD 315 VDU-ECK GIVING VDU-REL.
           DISPLAY WT-TXT(WH-ANR + 1) AT VDU-REL.
       E.  PERFORM HOLADR.
       F.  DISPLAY "<esc>= Abbruch, <>= zurÅck" AT 2301.
           CALL "CAUP" USING "1204150533" WH-CREG.
           MOVE WT-BEZ TO DE-BEZ.
           IF ESC GO D.
           IF WR-ADR(1) = SPACE
               DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO F.
           IF WH-X = "*" MOVE WH-ANR TO DE-ANREDE GO Z.
       G.  DISPLAY "<esc>= zurÅck, <ret-leer>= keine énderung" AT 2301.
           ADD 235 VDU-ECK GIVING VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-REL.
           CALL "CAUP" USING "1202350108" WH-CREG.
           IF ESC GO E.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-REL.
           IF WH-X = "$" MOVE WH-ANR TO DE-ANREDE GO Z.
       H.  DISPLAY "<esc>= Abbruch, <ret>= Ust (0-normal 1-frei ı19)"
               AT 2301.
           MOVE DE-UST TO WH-WERT.
           CALL "CAUP" USING "1005551001" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO G.
           IF not RET or WH-WERT > 1 GO H.
           MOVE WH-WERT TO DE-UST WD-KZ.
           MOVE DE-UST TO WD-KZ.
           DISPLAY WD-KZ AT VDU-LP "-".
           EVALUATE DE-UST
               WHEN 0 DISPLAY "normal  " with highlight AT 0000
               WHEN 1 DISPLAY "frei ı19" with highlight
                           foreground-color 4 AT 0000.
       I.  DISPLAY "<esc>= Abbruch, <ret>= UID-Nr." AT 2301.
           MOVE DE-UID TO WT-TX.
           CALL "CAUP" USING "1206550113" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO H.
           IF not RET GO I.
           MOVE WT-TX TO DE-UID.
           DISPLAY DE-UID with highlight foreground-color 6 AT VDU-LP.
           IF DE-UST = 1 and DE-UID = SPACE
               DISPLAY "ACHTUNG!" with highlight foreground-color 4
                   AT 2401 " UID-Nr. fehlt!" with highlight
               PERFORM WEITER.
      *-----------------------------------------> Zahlungskonditionen <-
       J.  MOVE DE-KOND TO WH-KK.
           IF DE-KOND NOT = 0 PERFORM KOND-ANZ.
           MOVE 0 TO WH-NUM WH-WERT.
           DISPLAY "0= man. Kond., 1 - 9 lt. Stamm" AT 2401.
           CALL "CAUP" USING "1007711001" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "12KOND" WH-CREG
               CANCEL "FIBSTAM" GO J.
           IF ESC GO Z.
           IF WOLI or AUF GO I.
           IF NOT RET GO J.
           MOVE WH-WERT TO WD-KZ WY.
           IF WH-WERT NOT = 0 DISPLAY WD-KZ with highlight AT VDU-LP
                         else DISPLAY " " AT VDU-LP.
           IF WH-MCODE = SPACE AND NOT ANLAGE
                PERFORM KOND-ANZ GO N.
           IF WY NOT = 0 MOVE WT-KONDIT(WY) TO DE-KOND
               PERFORM KOND-ANZ
           ELSE PERFORM MAN-KOND.
           IF WOLI GO I.
           IF ESC GO Z.
       N.  IF DE-REKTO = 0 MOVE DE-KTONR TO DE-REKTO.
           MOVE DE-REKTO TO WH-WERT.
           COMPUTE WD-KTO = DE-REKTO / 10.
           ADD 1015 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           CALL "CAUP" USING "1010155005" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO I.
           IF NOT RET GO N.
           MOVE WH-WERT TO DE-REKTO.
           IF DE-REKTO = 0 MOVE DE-KTONR TO DE-REKTO.
           COMPUTE WD-KTO = DE-REKTO / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       O.  MOVE DE-TEL TO WT-TX.
           ADD 1115 VDU-ECK GIVING VDU-LP.
           DISPLAY DE-TEL with highlight AT VDU-LP.
           CALL "CAUP" USING "1211150133" WH-CREG.
           MOVE WT-TX TO DE-TEL.
           DISPLAY DE-TEL with highlight AT VDU-LP.
           IF WOLI GO N.
           MOVE WH-ANR TO DE-ANREDE.
       R.  DISPLAY "<ret>= Eingaben ok, <esc>= Abbruch, <tab>= lîschen"
               AT 2301.
           CALL "CAUP" USING "0023531001" WH-CREG.
           IF WOLI GO O.
           IF ESC  GO Z.
           IF TABL GO Y.
           IF NOT RET GO R.
           IF WE-MOD = "RH" MOVE 99 TO DE-RES.
           IF ANLAGE WRITE DE-SATZ GO Z.
           IF AEND REWRITE DE-SATZ GO Z.
       Y.  DISPLAY "Wirklich loeschen? <ret>= Abbruch, <esc>= lîschen"
               AT 2301.
           CALL "CAUP" USING "0023511001" WH-CREG.
           IF RET GO R.
           IF NOT ESC GO Y.
           DELETE DEBITOR.
           DISPLAY "gelîscht!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************************************
       KOND-ANZ SECTION.
       A.  ADD 850 VDU-ECK GIVING VDU-LP.
           MOVE DE-KOND TO WH-KK.
           IF DE-KOND = 0
               DISPLAY " keine Skontierung   " AT VDU-LP
               ADD 100 TO VDU-LP
               DISPLAY "                     " AT VDU-LP GO Z.
           DISPLAY "                             " AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "                      " AT VDU-LP.
           SUBTRACT 100 FROM VDU-LP.
           IF DE-KOND = 9999999 MOVE 0 TO DE-KOND WH-KK.
           DISPLAY " %-1 Tg. %-2 Tg. TgN. " with reverse-video
               AT VDU-LP.
           ADD 101 TO VDU-LP.
           IF WH-SK1 NOT = 0 MOVE WH-SK1 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG1 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-SK2 NOT = 0 MOVE WH-SK2 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG2 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-TGN NOT = 0 MOVE WH-TGN TO WD-TG
               DISPLAY WD-TG with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************************** man. Konditionseingabe *
       MAN-KOND SECTION.
       A.  IF DE-KOND = 0 MOVE 9999999 TO DE-KOND.
           PERFORM KOND-ANZ.
           DISPLAY "<ret>= Skonto-1 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1009512103" WH-CREG.
           IF ESC OR WOLI GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO WH-SK1 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       F.  DISPLAY "<ret>= Tage-1, <esc>= Abbruch" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1009553003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO A.
           IF NOT RET GO F.
           MOVE WH-WERT TO WH-TG1 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= Skonto-2 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1009592103" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-SK2 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Tage-2, <esc>= Abbruch" AT 2301.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1009633003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO G.
           IF NOT RET GO H.
           MOVE WH-WERT TO WH-TG2 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       I.  DISPLAY "<ret>= Nettotage, <esc>= Abbruch" AT 2301.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1009673003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-TGN WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           MOVE WH-KK TO DE-KOND.
       Z.  EXIT.
