      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABKART.
      ******************************************************************
      *                -- Adressenverwaltung --                        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEDEB.CPY.
           COPY GABSEKON.CPY.
           SELECT KARTEIN  ASSIGN TO "GABKART.DAT"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS KA-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT DUPLETE  ASSIGN TO "DUPLO.ADI"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS DU-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT DEBALT   ASSIGN TO "C:\GABIS\GABDEBIT.DAT"
                           ORGANIZATION INDEXED  ACCESS DYNAMIC
                           RECORD KEY DA-KEY
                           ALTERNATE RECORD KEY DA-PKEY DUPLICATES
                           ALTERNATE RECORD KEY DA-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT KARTALT  ASSIGN TO "C:\GABIS\GABKART.ADI"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS KOL-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT EXPODAT  ASSIGN TO "ADREXP.DAT"
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS IS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
       COPY GABKONST.CPY.
      *********************************************** Karteibewegungen *
       FD  KARTEIN     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 287 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  KA-SATZ.
           03  KA-KEY.
               05 KA-ADNUM             PIC 9(6)     COMP.
               05 KA-DATUM             PIC 9(8)     COMP.
           03  KA-TXT                  PIC X(280).
      *****************************************************************
       FD  DEBALT       EXTERNAL       LABEL RECORDS STANDARD
                                       RECORD 256 CHARACTERS.
       01  DA-SATZ.
           03  DA-PKEY.
               05 DA-PLZL              PIC X(8).
               05 DA-KEY.
                  07 DA-KTONR          PIC 9(6)     COMP.
           03  DA-MCODE                PIC X(8).
           03  DA-ANREDE               PIC 99       COMP.
           03  DA-KOND                 PIC 9(13)    COMP.
           03  DA-BEZ                  PIC X(110).
           03  DA-TEL                  PIC X(35).
           03  DA-REKTO                PIC 9(6)     COMP.
           03  DA-KARTEI               PIC 99       COMP OCCURS 40.
           03  DA-RES                  PIC 99       COMP.
      *********************************************** Karteibewegungen *
       FD  KARTALT     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 286 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  KOL-SATZ.
           03  KOL-KEY.
               05 KOL-ADNUM             PIC 9(6)     COMP.
               05 KOL-DAT              PIC 9(6)     COMP.
           03  KOL-TXT                  PIC X(280).
      ************************************** Datei f. Duplettenfindung *
       FD  DUPLETE                     LABEL RECORD STANDARD
                                       RECORD 18 CHARACTERS.
       01  DU-SATZ.
           03  DU-KEY.
               05 DU-PLZL              PIC X(8).
               05 DU-TX                PIC X(4).
               05 DU-NR                PIC X(3).
           03  DU-NUM                  PIC 9(6)      COMP.
      *****************************************************************
       FD  EXPODAT                     LABEL RECORD OMITTED.
       01  EX-SATZ.
           03  EX-KOPF                 PIC X(100).
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-ADR                           OCCURS 4.
               05 DRA-PLZL             PIC X(9).
               05 FILLER               PIC X.
               05 DRA-TX.
                  07 FILLER            PIC X(15).
                  07 DRA-BNUM          PIC ZZZZZ9.
                  07 FILLER            PIC X(4).
               05 FILLER               PIC X(4).
       01  DRB-SATZ.
           03  FILLER                  PIC X(102).
           03  DRB-DATUM               PIC X(10).
           03  DRB-SEITX               PIC X(7).
           03  DRB-SEITE               PIC 999.
       01  DRC-SATZ.
           03  DRC-NUM                 PIC ZZZZZ9-.
           03  DRC-TXA                 PIC X(27).
           03  DRC-TXB                 PIC X(27).
           03  DRC-TXC                 PIC X(26).
           03  DRC-TXD                 PIC X(26).
       01  DRD-SATZ.
           03  FILLER                  PIC X(67).
           03  DRD-DATUM               PIC X(10).
           03  DRD-SEITX               PIC X(7).
           03  DRD-SEITE               PIC ZZ9.
       01  DRE-SATZ.
           03  DRE-NUM                 PIC ZZZZZ9-.
           03  DRE-TXA                 PIC X(25).
           03  DRE-TXB                 PIC X(25).
           03  DRE-TXC                 PIC X(25).
           03  DRE-TXD                 PIC X(25).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WM-EX                   PIC 99       COMP.
           03  WH-P                    PIC 99       COMP.
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WH-ENTSCHEID            PIC 9      VALUE 1.
               88  PLZL    VALUE 1.   88  NUM     VALUE 2.
               88  KUBEZ   VALUE 3.   88  ABILD   VALUE 1.
           03  WH-LAST                 PIC 9(6)     COMP  VALUE ZERO.
           03  VDU-KUBEZ.
               05 VDU-KZ               PIC X     OCCURS 25  INDEXED VX.
           03  WH-VONNR                PIC 9(6).
           03  WH-BISNR                PIC 9(6).
           03  WK-NUM                  PIC 9(7)    COMP.
           03  WV-KTONR                PIC 9(7)    COMP.
           03  WH-VONPLZL              PIC X(8).
           03  WH-BISPLZL              PIC X(8).
           03  WM-MERKER               PIC 9       COMP-3.
               88  ANLAGE     VALUE 1.
           03  WM-OPEN                 PIC 9       COMP-3.
           03  WI                      PIC 9       COMP.
           03  WK-S                    PIC 9       COMP.
           03  WV-VJ                   PIC 99      COMP.
           03  WV-BJ                   PIC 99      COMP.
           03  WD-J                    PIC 99.
           03  WD-I                    PIC 9.
           03  WH-L                    PIC 99      COMP.
           03  IX                      PIC 99      COMP.
           03  WS                      PIC 99      COMP.
           03  WS-JAHR                 PIC 99      COMP.
           03  WD-ETIKETT                        OCCURS 4.
               05  WD-BNUM             PIC 9(6)    COMP.
               05  WD-ANR              PIC 9       COMP.
               05  WD-ANZ              PIC 9999    COMP.
               05  WD-TX1              PIC X(25).
               05  WD-TX2              PIC X(25).
               05  WD-TX3.
                   07  WD-ANZ-GES      PIC ZZZZ9.
                   07  WD-RTX3         PIC X(20).
               05  WD-TX4.
                   07  WD-ANZ-PLZ      PIC ZZZZ9.
                   07  WD-RTX4         PIC X(20).
               05  WD-PLZL             PIC X(8).
           03  WK-PLZL                 PIC X(10).
           03  WS-ANZ-PLZ              PIC 9(5)    COMP-3  VALUE ZERO.
           03  WS-ANZ-GES              PIC 9(5)    COMP-3  VALUE ZERO.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 15.
           03  WH-DUSATZ               PIC X(15).
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WV-PLZL                 PIC X(8)          VALUE SPACES.
           03  WH-PLZL                 PIC 99999      VALUE ZERO.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  VDU-AB                  PIC 9999.
           03  VR REDEFINES VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
           03  WK-X                    PIC X(35).
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 999     COMP-3 VALUE ZERO.
           03  WZ-ZEIT.
               05  WZ-STUND            PIC 99.
               05  WZ-MINUTE           PIC 99.
               05  WZ-SEKUNDE          PIC 99.
               05  FILLER              PIC 99. 
           03  VDU-ZEIT.
               05  VDU-STUND           PIC 99.
               05  V1                  PIC X               VALUE ":".
               05  VDU-MINUTE          PIC 99.
               05  V2                  PIC X               VALUE ":".
               05  VDU-SEKUNDE         PIC 99.
           03  WH-KB                   PIC 99       COMP.
           03  WH-KART                 PIC 999      COMP.
           03  WV-ADNUM                PIC 9(6)     COMP.
           03  WD-BELEG                PIC ZZZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-X                    PIC X.
           03  DATEI                   PIC X(12).
           03  WH-TEXT.
               05 WR-TXT               PIC X(70)    OCCURS 4.
           03  WR-XT REDEFINES WH-TEXT PIC X  OCCURS 280 INDEXED XX.
           03  WH-KARTEI               PIC 99        COMP  OCCURS 40.
       01  WH-SATZ                     PIC X(100).
       01  WH-OFT                      PIC 9999      COMP VALUE 1.
       COPY "GABEXT.CPY".
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "GABDEBIT.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON DUPLETE.
       A.  CALL "CADECL" USING "DUPLO.ADI   " WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON KARTEIN.
       A.  CALL "CADECL" USING "ADRKART.ADI " WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "GABIKONS.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CREG TO WH-CREG.
           IF WL-CALL(1:4) ="KART" PERFORM VORLAUF
              IF ESC GO Z.
           MOVE " Adressenverwaltung" TO WK-GEB.
           OPEN I-O KARTEIN.
           IF WF-STATUS > "09" MOVE "ADRBEWEG.ADI" TO DATEI
               PERFORM ADRKOR.
           MOVE 0 TO WM-MERKER WK-S.
           IF WL-CALL(1:6) NOT = SPACE GO G.
       B.  PERFORM KOPF.
       C.  DISPLAY "Programmwahl" AT 0524
                   "ƒƒƒƒƒƒƒƒƒƒƒ-"        AT 0024
                   "1 - Bearbeiten"      AT 0724
                   "2 - Druck Etiketten" AT 0024
                   "3 - Duplettensuche"  AT 0024
                   "4 - Serienlîschung"  AT 0024
                   "5 - Druckereinstellung" AT 0024
                   "6 - Telefonkorrektur" AT 0024
                   "leer - Programmende" AT 0024
                   "bitte wÑhlen Sie: " with highlight AT 1539.
           CALL "CAUP" USING "0015571001" WH-CREG.
           IF ESC SET RET TO TRUE MOVE 0 TO WH-NUM.
           MOVE WH-NUM TO WH-PG.
           IF NOT RET GO C.
           IF WH-PG = 0 GO Y.
           IF WH-PG = 5 GO B.
           IF WH-PG = 6 PERFORM TELKOR GO B.
           IF WH-PG > 4 GO B.
       G.  MOVE SPACE TO WV-PLZL.
           MOVE 0 TO WM-OPEN.
           EVALUATE WH-PG
               WHEN 1
               WHEN 9
               WHEN 49 PERFORM BUCHEN
                       EVALUATE WH-PG
                           WHEN 49
                           WHEN 9 GO Y
                       end-evaluate
               WHEN 2 PERFORM ETIKETTEN
               WHEN 3 PERFORM DUPLETEN
               WHEN 4 PERFORM ADRLOE.
           GO B.
       Y.  CLOSE KARTEIN.
       Z.  EXIT PROGRAM.
           STOP RUN.
      ********************************************** Korrektur Telefon *
       TELKOR SECTION.
       A.  MOVE 0 TO DE-KTONR.
           MOVE " Telefonkorrektur" TO WK-GEB.
           PERFORM KOPF.
           PERFORM UNTER.
           START DEBITOR KEY NOT < DE-KEY.
       C.  READ DEBITOR NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO C.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 0810.
           PERFORM GETADR.
           IF WR-ADR(2) = SPACE AND WR-ADR(3) = SPACE GO C.
           IF WR-ADR(2) NOT = SPACE MOVE 2 TO WX
               PERFORM INSP
               IF WK-X IS NUMERIC GO E.
           IF WR-ADR(2) = SPACE AND WR-ADR(3) NOT = SPACE
               MOVE 3 TO WX
               PERFORM INSP.
           IF WK-X NOT NUMERIC GO C.
       E.  MOVE 1001 TO VDU-LP.
           PERFORM BILD.
           MOVE 1003 TO VDU-LP.
           PERFORM EINBLENDEN.
           PERFORM ABSTELLEN.
           MOVE WT-BEZ TO DE-BEZ.
           PERFORM KB-STAT.
      *G.  DISPLAY "<Ende>= nicht Ñndern, <ret>= énderung ok!" AT 2301.
      *    DISPLAY "<esc>= Abbruch" AT 2401.
      *    CALL "CAUP" USING "0023430000" WH-CREG.
           IF ESC GO Z.
      *    IF ENDE GO C.
      *    IF NOT RET GO G.
           REWRITE DE-SATZ.
           GO C.
       Z.  EXIT.
      ******************************************************************
       INSP SECTION.
       A.  MOVE WR-ADR(WX) TO WK-X.
           IF WK-X = SPACE GO Z.
           INSPECT WK-X REPLACING ALL SPACE BY "0", "t" BY "0"
               "T" BY "0", "/" BY "0", "-" BY "0",
               "e" BY "0", "l" BY "0", ":" BY "0"
               "E" BY "0", "L" BY "0", ":" BY "0".
           IF WK-X IS NUMERIC MOVE WR-ADR(WX) TO DE-TEL
                   MOVE SPACE TO WR-ADR(WX).
       Z.  EXIT.
      ******************************************************************
       ABSTELLEN SECTION.
       A.  SET BX TX TO 1.
           MOVE SPACE TO WT-BEZ.
       B.  MOVE WR-ADR(TX) TO WT-TX.
           SET TY TO 29.
           MOVE 1 TO WX.
       C.  IF WR-TX(TY) NOT = SPACE SET WX TO TY
               SET BX UP BY WX
               MOVE "#" TO WR-BEZ(BX)
               SET BX DOWN BY 1
               ADD 1 TO WX
               GO D.
           IF TY NOT = 1 SET TY DOWN BY 1 GO C.
           MOVE "#" TO WR-BEZ(BX).
           GO E.
       D.  MOVE WR-TX(TY) TO WR-BEZ(BX).
           IF TY NOT = 1 SET TY BX DOWN BY 1 GO D.
       E.  SET BX UP BY WX.
       F.  IF BX > 240 DISPLAY "Zuviele Zeichen eingegeben! " AT 2401
                PERFORM WEITER GO Z.
           IF TX < 5 SET TX UP BY 1 GO B.
       Z.  EXIT.
      ******************************************************************
       VORLAUF SECTION.
       A.  OPEN I-O DEBITOR.
           OPEN I-O KONSTANT.
           MOVE SPACE TO WH-CALL.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WH-DATUM TO WM-DATUM.
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID SET ESC TO TRUE GO Z.
           MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-ORT TO WT-TX.
           SET TY FY TO 15.
       K.  IF WR-TX(TY) NOT = SPACE GO L.
           IF TY > 1 SET TY DOWN BY 1 GO K.
       L.  MOVE WR-TX(TY) TO WR-TX(FY).
           IF TY NOT = FY MOVE SPACE TO WR-TX(TY).
           IF TY > 1 SET TY FY DOWN BY 1 GO L.
           MOVE WT-TX TO VDU-ORT.
           MOVE 10 TO WH-KEY.
       N.  READ KONSTANT IGNORE LOCK.
           MOVE KO-ANR TO WH-TABTX.
           MOVE KO-TM TO WT-TM.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       Z.  EXIT.
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
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      *****************************************************************
       KOPF SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           ACCEPT WZ-ZEIT FROM TIME.
           MOVE WZ-STUND TO VDU-STUND.
           MOVE WZ-MINUTE TO VDU-MINUTE.
           MOVE WZ-SEKUNDE TO VDU-SEKUNDE.
       E.  DISPLAY " " AT 0172 VDU-ZEIT.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
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
      ****************************** DruckerrÅckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhîhe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT WZ-SEITE.
       C.  MOVE X"1B21" TO DRA-SATZ(1:).
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
       ADRKOR SECTION.
       A.  PERFORM KOPF.
           DISPLAY "Reorganisation " AT 1010 DATEI with highlight
               " erforderlich".
           DISPLAY "Starten Sie bitte: " AT 1210 "REBUILD " with
               highlight DATEI.
       Z.  STOP RUN.
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
      ***************************************** Text 'Unterbrechnung' *
       UNTER SECTION.
       A.  DISPLAY "Unterbrechung mîglich" AT 2301.
       Z.  EXIT.
      ******************************************************************
       BILD SECTION.
       A.  CALL "CAUP" USING "1300002480" WH-CREG.
           ADD 013 VDU-LP GIVING VDU-REL.
           DISPLAY "Kurzbez/Nr:" AT VDU-REL.
           ADD 101 VDU-LP GIVING VDU-REL.
           DISPLAY "Anr.:             ( )" AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Name:" AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Zus.:" AT VDU-REL.
           ADD 200 TO VDU-REL.
           DISPLAY "Str.:" AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Ort :" AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Re-Ktnr.:" AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Tel.:" AT VDU-REL.
       Z.  EXIT.
      ************************************** autom. KUBEZ - ]bernahme *
       KUNBEZ SECTION.
       A.  PERFORM GETADR.
           MOVE WR-ADR(1) TO WH-MCODE VDU-KUBEZ.
           IF RET GO F.
           MOVE 1 TO WS.
       B.  IF VDU-KZ(WS) = SPACE ADD 1 TO WS GO D.
           IF WS < 25 SET WS UP BY 1 GO B.
       D.  MOVE VDU-KUBEZ(WS:) TO WH-MCODE.
       F.  PERFORM ALPHACONV.
           PERFORM RECONV.
           DISPLAY WH-MCODE with SIZE 8 highlight AT 0327.
       Z.  EXIT.
      ******************************************************************
       EINBLENDEN SECTION.
       A.  ADD 1 VDU-LP GIVING VDU-REL.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           ADD 24 VDU-LP GIVING VDU-REL.
           DISPLAY "        " AT VDU-REL.
           MOVE DE-MCODE TO WH-MCODE.
           PERFORM RECONV.
           DISPLAY WH-MCODE with highlight AT VDU-REL.
           ADD 105 VDU-LP GIVING VDU-REL.
           DISPLAY WT-TXT(DE-ANREDE + 1) with highlight AT VDU-REL.
           ADD 13 TO VDU-REL.
           MOVE DE-ANREDE TO WD-KZ.
           DISPLAY WD-KZ AT VDU-REL.
           IF WH-PG NOT = 6 PERFORM GETADR.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               COMPUTE VDU-REL = WX * 100 + VDU-LP + 105
               DISPLAY WR-ADR(WX) with highlight SIZE 33 AT VDU-REL.
           ADD 104 TO VDU-REL.
           IF DE-REKTO = 0 MOVE DE-KTONR TO DE-REKTO.
           DIVIDE 10 INTO DE-REKTO GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           ADD 96 TO VDU-REL.
           DISPLAY DE-TEL with SIZE 33 highlight AT VDU-REL.
           IF WH-PG NOT = 1 GO Z.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 40
               SUBTRACT 1 FROM WS GIVING WX
               DIVIDE 10 INTO WX GIVING VDU-RL REMAINDER VDU-RP
               COMPUTE VDU-RP = VDU-RP * 4 + 43
               COMPUTE VDU-RL = VDU-RL * 2 + 4
               DISPLAY WS AT VDU-REL
               ADD 100 TO VDU-REL
               IF DE-KARTEI(WS) NOT = 0
                   ADD 50 DE-KARTEI(WS) GIVING WD-J
                   DISPLAY WD-J with highlight AT VDU-REL.
           MOVE 41 TO VDU-P.
           PERFORM VARYING VDU-L FROM 3 BY 1 UNTIL VDU-L = 12
               DISPLAY "≥" AT VDU-LP.
       Z.  EXIT.
      *****************************************************************
       KARTANZ SECTION.
       A.  IF WH-NUM = 0 MOVE 0 TO WX-DATUM.
           CALL "CAUP" USING "1313012480" WH-CREG.
       B.  MOVE WX-DATUM TO KA-DATUM.
           MOVE DE-KTONR TO KA-ADNUM.
           MOVE 1201 TO VDU-LP.
           START KARTEIN KEY NOT < KA-KEY INVALID GO X.
           MOVE 0 TO WS.
       C.  MOVE SPACE TO KA-TXT.
           READ KARTEIN NEXT IGNORE LOCK AT END GO X.
           IF KA-ADNUM NOT = DE-KTONR GO X.
           ADD 1 TO VDU-L.
           MOVE 1 TO WX.
      *---------------------------------> abfragen ob noch alles geht <-
           IF WS < 9 GO F.
       D.  DISPLAY "<esc/#>= Ende, <tab>= weiterlisten, <Datum+ret>= Kar
      -        "teieintrag" AT 2401.
           MOVE 0 TO WH-WERT WH-NUM.
           CALL "CAUP" USING "0100016006" WH-CREG.
           IF ESC OR KIST GO Y.
           IF NOT RETA GO D.
           IF RET AND WH-WERT > 0 PERFORM ERFASS GO Z.
           IF WZ-DATUM NOT = 0 GO A.
           MOVE 1 TO WS.
       F.  PERFORM ZEIL.
           IF WX > 1 SUBTRACT 1 FROM VDU-L GO K.
       J.  MOVE KA-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE 1 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-LP.
           MOVE KA-TXT TO WH-TEXT.
           DISPLAY WR-TXT(1) with highlight AT VDU-LP.
           ADD 1 TO WS.
           MOVE 2 TO WX.
       K.  IF WR-TXT(WX) NOT = SPACE; ADD 1 TO VDU-L
           IF WS > 8 GO D ELSE
               ADD 1 TO WS
               MOVE 10 TO VDU-P
               DISPLAY WR-TXT(WX) with highlight AT VDU-LP
               PERFORM ZEIL.
           IF WX < 4 ADD 1 TO WX GO K.
           GO C.
       X.  ADD 1 TO VDU-L.
           PERFORM ZEIL.
           MOVE 16 TO VDU-P.
           DISPLAY "  ***** keine Daten mehr *****" AT VDU-LP.
       Y.  SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************************************
       ZEIL SECTION.
       A.  IF VDU-L > 21 SUBTRACT 1 FROM VDU-L
               SUBTRACT 100 FROM VDU-REL
               CALL "CAUP" USING "1413012280" WH-CREG GO A.
       Z.  EXIT.
      ******************************************************************
       ERFASS SECTION.
       A.  PERFORM ZEIL.
           MOVE 1 TO VDU-P.
           IF WZ-DATUM = 0 GO X.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO KA-DATUM.
           MOVE DE-KTONR TO KA-ADNUM.
       B.  MOVE VDU-LP TO VDU-REL.
           MOVE SPACE TO KA-TXT.
           READ KARTEIN IGNORE LOCK INVALID
               MOVE SPACE TO KA-TXT WH-TEXT GO F.
           ADD 9 TO VDU-LP.
           MOVE KA-TXT TO WH-TEXT.
           DISPLAY WR-TXT(1) with highlight AT VDU-LP.
           MOVE 2 TO WX.
       C.  IF WR-TXT(WX) NOT = SPACE ADD 1 TO VDU-L
               PERFORM ZEIL
               DISPLAY WR-TXT(WX) with highlight AT VDU-LP.
           IF WX < 4 ADD 1 TO WX GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE KA-TXT TO WH-TEXT.
           IF WR-TXT(4) NOT = SPACE SUBTRACT 1 FROM VDU-L GO I.
           IF WR-TXT(3) NOT = SPACE SUBTRACT 1 FROM VDU-L GO H.
           IF WR-TXT(2) NOT = SPACE SUBTRACT 1 FROM VDU-L GO G.
       F.  DISPLAY "<#>= Textende, <ret>= Zeilenende, <esc>= Abbruch"
               AT 2301.
           DISPLAY "kein Text = lîschen" AT 2401.
           MOVE WR-TXT(1) TO WT-TX.
           CALL "CAUP" USING "0200100170" WH-CREG.
           IF ESC GO L.
           MOVE WT-TX TO WR-TXT(1).
           MOVE WH-TEXT TO KA-TXT.
           IF KIST GO J.
           IF NOT RET GO F.
       G.  MOVE WR-TXT(2) TO WT-TX.
           ADD 1 TO VDU-L.
           PERFORM ZEIL.
           CALL "CAUP" USING "0200100170" WH-CREG.
           IF ESC GO L.
           MOVE WT-TX TO WR-TXT(2).
           IF WOLI OR AUF SUBTRACT 1 FROM VDU-L GO F.
           IF KIST GO J.
       H.  MOVE WR-TXT(3) TO WT-TX.
           ADD 1 TO VDU-L.
           PERFORM ZEIL.
           CALL "CAUP" USING "0200100170" WH-CREG.
           IF ESC GO L.
           MOVE WT-TX TO WR-TXT(3).
           IF WOLI OR AUF SUBTRACT 2 FROM VDU-L GO G.
           IF KIST GO J.
       I.  MOVE WR-TXT(4) TO WT-TX.
           ADD 1 TO VDU-L.
           PERFORM ZEIL.
           CALL "CAUP" USING "0200100170" WH-CREG.
           IF ESC GO L.
           MOVE WT-TX TO WR-TXT(4).
           IF WOLI OR AUF SUBTRACT 2 FROM VDU-L GO H.
       J.  MOVE WH-TEXT TO KA-TXT.
      *------------------------------------------------
           ADD 300 VDU-REL GIVING VDU-LP.
           IF WR-TXT(4) = SPACE SUBTRACT 1 FROM VDU-L
              IF WR-TXT(3) = SPACE SUBTRACT 1 FROM VDU-L
                 IF WR-TXT(2) = SPACE SUBTRACT 1 FROM VDU-L.
       K.  IF KA-TXT NOT = SPACE GO N.
           DELETE KARTEIN INVALID GO L.
       L.  MOVE VDU-REL TO VDU-LP.
           CALL "CAUP" USING "1300002480" WH-CREG.
           GO X.
       N.  MOVE KA-TXT TO WH-TEXT.
           PERFORM VARYING XX FROM 280 BY -1 UNTIL WR-XT(XX)
               NOT = SPACE CONTINUE.
           SET WH-KART TO XX.
           ADD 7 TO WH-KART.
           WRITE KA-SATZ INVALID REWRITE KA-SATZ.
           GO Z.
       X.  SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************************************
       REITAEND SECTION.
       A.  SUBTRACT 1 FROM WH-WERT GIVING WX.
           DIVIDE 10 INTO WX GIVING VDU-RL REMAINDER VDU-RP.
           COMPUTE VDU-RP = VDU-RP * 4 + 43.
           COMPUTE VDU-RL = VDU-RL * 2 + 5.
           MOVE WH-WERT TO WX.
           SUBTRACT WX FROM WH-WERT.
           MULTIPLY 100 by WH-WERT.
           MOVE WH-WERT TO WY.
           IF WY = 0 MOVE WH-DATUM TO WZ-DATUM
               MOVE WZ-JAHR TO WY.
           IF WY < 51 ADD 100 TO WY.
           SUBTRACT 50 FROM WY.
           MOVE WY TO DE-KARTEI(WX).
           IF DE-KARTEI(WX) = 0 DISPLAY "  " AT VDU-REL
               ELSE ADD DE-KARTEI(WX) 50 GIVING WS
                    DISPLAY WS with highlight AT VDU-REL.
       Z.  EXIT.
      **************************************** Matchcodeconvertierung *
       ALPHACONV SECTION.
       A.  MOVE WH-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV10" WH-CREG.
           MOVE WT-TX TO WH-MCODE WV-MCODE.
           MOVE WH-MCODE TO DE-MCODE.
       Z.  EXIT.
      ************************************************ Reconvertierung *
       RECONV SECTION.
       A.  MOVE WH-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV10" WH-CREG.
           MOVE WT-TX TO WH-MCODE WV-MCODE.
       Z.  EXIT.
      ********************************** Abfrage ob isch was getan hat *
       WARWAS SECTION.
       A.  MOVE 0 TO WH-OFT.
       B.  CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 1 GO Z.
       C.  IF WH-OFT < 999 ADD 1 TO WH-OFT GO B.
           CLOSE DEBITOR.
           OPEN I-O DEBITOR.
           DISPLAY "*C*" AT 0377.
       D.  MOVE 0 TO WH-OFT.
       E.  CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 1 GO Z.
       F.  IF WH-OFT < 3000 ADD 1 TO WH-OFT GO E.
           ACCEPT WZ-ZEIT FROM TIME.
           MOVE WZ-STUND TO VDU-STUND.
           MOVE WZ-MINUTE TO VDU-MINUTE.
           MOVE WZ-SEKUNDE TO VDU-SEKUNDE.
           DISPLAY VDU-ZEIT AT 0173.
           DISPLAY LOW-VALUE AT VDU-REL.
           GO D.
       Z.  EXIT.
      ************************** suchen v. gelîschten /freien Records *
       NUM-SUCHE SECTION.
       A.  MOVE 13 TO WX-TASTE.
           IF WH-NUM = 0 MOVE 12 TO WH-NUM.
           DISPLAY "<esc>= Abbruch" AT 2301.
       B.  MOVE WH-NUM TO DE-KTONR.
           IF DE-KTONR = 0 ADD 10 TO DE-KTONR.
       C.  READ DEBITOR IGNORE LOCK INVALID
                CALL "CAUP" USING "1320012480" WH-CREG GO X.
           DISPLAY DE-KTONR AT 2001 " " DE-BEZ with Size 35.
           ADD 10 TO WH-NUM.
           PERFORM KB-STAT.
           IF ESC GO Z.
           GO B.
       X.  IF WH-NUM > WK-NUM MOVE 0 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       ADRLOE SECTION.
       A.  MOVE SPACE TO WV-PLZL.
           MOVE "  Adressenlîschung" TO WK-GEB.
           PERFORM KOPF.
           DISPLAY "von Postleitzahl: " AT 0504.
       B.  DISPLAY "bis Postleitzahl: " AT 0604.
           DISPLAY "<ret-leer>= ab Anfang, <esc>= Ende" at 2301.
      *-----------------------------------------------> von-Eingaben <-
           MOVE 0 TO WH-WERT.
           MOVE SPACE TO WV-PLZL WT-TX.
           CALL "CAUP" USING "0205220108" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE DISPLAY "ab Anfang" with highlight AT 0522
           ELSE DISPLAY DE-PLZL with highlight AT 0522
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-VONPLZL.
      *-----------------------------------------------> bis-Eingaben <-
       C.  DISPLAY "<esc>= zurÅck, <ret>= Bildschirmausgabe, (<-)= Druck
      -        "erausgabe" AT 2301.
           DISPLAY "<ret-leer>=" AT 2401.
           IF WH-VONPLZL > SPACE DISPLAY "Wiederholung d. PLZL" AT 2412
               ELSE DISPLAY "bis Ende" AT 2412.
           MOVE WH-VONPLZL TO WT-TX.
           CALL "CAUP" USING "0206220108" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO C.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE MOVE 9999999 TO DE-PLZL
               DISPLAY "bis Ende" with highlight AT 0622
           ELSE
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-BISPLZL.
           MOVE WH-VONPLZL TO DE-PLZL.
           MOVE 0 TO DE-KTONR.
       E.  DISPLAY "<esc>= Abbruch, <tab>= Start der Lîschung"
               AT 2301.
           CALL "CAUP" USING "0023430000" WH-CREG.
           IF ESC GO Z.
           IF NOT TABL GO E.
           PERFORM UNTER.
           START DEBITOR KEY NOT < DE-PKEY INVALID PERFORM NO-REC GO A.
           MOVE 1 TO WY.
       F.  READ DEBITOR NEXT AT END GO Z.
           IF ZUGRIF GO F.
           IF DE-KTONR = 0 GO F.
           PERFORM KB-STAT
               IF ESC GO Z.
           IF DE-PLZL > WH-BISPLZL GO Z.
           DISPLAY WR-ADR(1) AT 2020.
           DELETE DEBITOR.
           PERFORM LOEKART.
           GO F.
       Z.  EXIT.
      ******************************************************************
       BUCHEN SECTION.
       A.  MOVE LOW-VALUE TO WT-ERFTAB.
           IF WK-S = 1;
               EVALUATE WL-CALL(1:6)
                   WHEN "GABAUF" MOVE 49 TO WH-PG
                                 MOVE "GABAUF  16AUFT" TO WT-TX
                                 MOVE WH-CREG TO WL-CREG GO Z
                   WHEN "GABREC" MOVE 49 TO WH-PG
                                 MOVE "GABRECH 15FAKT" TO WT-TX
                                 MOVE WH-CREG TO WL-CREG GO Z
                   WHEN "GABADR" MOVE 9 TO WH-PG GO Z.
           EVALUATE WL-CALL(1:6)
               WHEN "GABAUF"
               WHEN "GABREC" MOVE 1 TO WK-S
                             MOVE DE-KTONR TO WK-NUM WH-NUM
                             MOVE 1 TO WH-PG
                             PERFORM KOPF
                             MOVE 301 TO VDU-LP
                             PERFORM BILD
                             GO E
               WHEN "GABADR" MOVE 1 TO WH-PG.
           MOVE LOW-VALUE TO DE-SATZ WT-ERFTAB.
           MOVE 90000 TO DE-KTONR.
           START DEBITOR KEY < DE-KEY INVALID MOVE 12 TO WK-NUM GO C.
       B.  READ DEBITOR PREVIOUS IGNORE LOCK AT END
                MOVE 12 TO WK-NUM GO C.
           MOVE 0 TO WH-OFT.
           MOVE DE-KTONR TO WK-NUM.
           ADD 10 TO WK-NUM.
       C.  PERFORM KOPF.
           MOVE 301 TO VDU-LP.
           PERFORM BILD.
           DISPLAY "<ret>+alpha = suchen, <ret>+Nr.= Anlage od. énderung
      -        ", (<-)= letzte Adresse " AT 2301.
           DISPLAY "<#>= Freie suchen, <esc>= Ende, <Ende>= Pg.-Wahl, al
      -        "pah+<Einfg>= Str-Suche" AT 2401.
           MOVE 0 TO WM-MERKER WH-NUM.
           DIVIDE 10 INTO WK-NUM GIVING WD-KTO.
           DISPLAY WD-KTO AT 0304.
           MOVE 0327 TO VDU-REL.
           DISPLAY LOW-VALUE AT VDU-REL.
           PERFORM WARWAS.
       D.  CALL "CAUP" USING "0003279010" WH-CREG.
           MOVE 0 TO WH-OFT.
           IF ENTF PERFORM REORG GO D.
           IF WOLI MOVE WH-LAST TO WH-NUM GO E.
           IF ENDE GO Z.
           IF ESC MOVE 9 TO WH-PG GO Z.
           IF NOT RET and not WORE and not KIST and not EINF GO C.
      *-----------------------------------> WH-NUM = 0 : Suchen n. ALPHA
           IF FEHLER GO C.
           IF EINF MOVE WH-MCODE TO WT-TX
               PERFORM SUCH-ADR
               IF DE-KTONR = 0 GO A ELSE GO F.
           IF RET AND WH-NUM > 0 GO E.
           IF RET AND WH-MCODE NOT = SPACE PERFORM SUCH-ADR
               IF DE-KTONR = 0 GO A ELSE GO F.
      *--------------------------------------> WH-NUM = Nr. f. Aufruf <-
           IF KIST PERFORM NUM-SUCHE
               IF ESC OR FEHLER GO A.
       E.  IF WH-NUM > 0 MOVE WH-NUM TO WK-NUM
               ELSE MOVE WK-NUM TO WH-NUM.
           MOVE WK-NUM TO WH-LAST WD-KZ.
           IF WD-KZ NOT = 2 GO C.
           IF WH-NUM > 0 DIVIDE 10 INTO WH-LAST GIVING WD-KTO
              DISPLAY WD-KTO AT 0304.
           MOVE WH-NUM TO DE-KTONR.
       F.  READ DEBITOR IGNORE LOCK INVALID KEY MOVE 1 TO WM-MERKER
               INITIALIZE DE-SATZ
               MOVE WH-NUM TO DE-KTONR.
           PERFORM GETADR.
       G.  IF NOT ANLAGE MOVE 303 TO VDU-LP
               PERFORM EINBLENDEN
           ELSE PERFORM ADR-ANL
               IF ANLAGE AND ESC OR WX-TASTE = 98 GO A
               ELSE MOVE 303 TO VDU-LP
                    PERFORM EINBLENDEN.
           MOVE 1201 TO VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 80 AT 1201.
           DISPLAY "¡" AT 1241.
           DISPLAY "ƒ Datum ƒƒƒƒƒƒ T ƒ e ƒ x ƒ t " AT 1201.
           PERFORM LAST-KART.
       H.  ADD 1 TO VDU-L.
           PERFORM ZEIL.
       I.  DISPLAY "<esc>= Abbruch, <ret-leer>= speichern, <>= Adresse,
      -       " <tab+Datum>= anzeigen ab" AT 2301.
           DISPLAY "Gruppe= Nr,Jahr+<ret>-(z.B: 12,89), <ret>= Kartei, G
      -       "rp+<#>= lîschen, < >= Druck" AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1300010015000" WH-CREG.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0100016206" WH-CREG.
           IF AB PERFORM KARDRU GO I.
           IF ESC GO A.
           IF WOLI MOVE VDU-LP TO VDU-AB
               PERFORM ADR-ANL
               IF ESC MOVE 303 TO VDU-LP
                   PERFORM EINBLENDEN
               END-IF MOVE VDU-AB TO VDU-LP
                   IF WX-TASTE = 98 GO A ELSE GO I.
           IF KIST AND WH-NUM > 0 AND < 41 SET RET TO TRUE
               ADD WH-NUM 0,50 GIVING WH-WERT.
           IF WH-WERT < 41 AND > 0 PERFORM REITAEND GO I.
           IF TABL PERFORM KARTANZ GO H.
           IF EINF PERFORM ZUSAMMEN GO F.
           IF NOT RET GO I.
           IF WH-NUM > 0 PERFORM ERFASS GO H.
           REWRITE DE-SATZ.
           GO A.
       Z.  EXIT.
      ***************************************** letzte Kartei anzeigen *
       LAST-KART SECTION.
       A.  IF WH-DATUM < 800000 MOVE 19491231 TO KA-DATUM
                           else MOVE 19991232 TO KA-DATUM.
050103     MOVE 99999999 TO KA-DATUM.
           MOVE DE-KTONR TO KA-ADNUM.
           START KARTEIN KEY < KA-KEY INVALID GO Z.
       C.  MOVE SPACE TO KA-TXT.
           READ KARTEIN PREVIOUS IGNORE LOCK AT END GO Z.
           IF KA-ADNUM NOT = DE-KTONR GO Z.
           MOVE KA-DATUM TO WC-DATUM.
           MOVE 1301 TO VDU-LP.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-LP.
           MOVE KA-TXT TO WH-TEXT.
           DISPLAY WR-TXT(1) with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 4
               IF WR-TXT(WX) not = SPACE ADD 1 TO VDU-L
                   PERFORM ZEIL
                   DISPLAY WR-TXT(WX) with highlight AT VDU-LP.
       Z.  EXIT.
      ****************************************** Kartein zusammenlegen *
       ZUSAMMEN SECTION.
       A.  CALL "CAUP" USING "1313012480" WH-CREG.
           MOVE DE-KTONR TO WH-LAST.
           MOVE 1301 TO VDU-LP.
           PERFORM BILD.
           DISPLAY "zusammenlegen mit Nr." AT 2301.
           DISPLAY "<ret>+alpha = suchen, <ret>+Nr.= zu lîschende Kartei
      -        ", <esc>= Abbruch" AT 2401.
           CALL "CAUP" USING "0013276006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO A.
      *-----------------------------------> WH-NUM = 0 : Suchen n. ALPHA
           IF RET AND WH-MCODE NOT = SPACE AND WH-NUM = 0
               PERFORM SUCH-ADR
               IF DE-KTONR = 0 GO A else GO C.
      *--------------------------------------> WH-NUM = Nr. f. Aufruf <-
           IF WH-LAST = WH-NUM GO A.
           MOVE WH-NUM TO DE-KTONR.
       C.  READ DEBITOR INVALID KEY PERFORM NO-REC GO A.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 1303 TO VDU-LP.
           PERFORM EINBLENDEN.
       E.  DISPLAY "<esc>= Abbruch, <ret>= zusammenlegen und lîschen"
               AT 2301.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE DE-KARTEI(WX) TO WH-KARTEI(WX).
           DELETE DEBITOR.
           MOVE DE-KTONR TO KA-ADNUM DU-NUM.
           MOVE WH-LAST TO DE-KTONR.
       G.  READ DEBITOR INVALID KEY PERFORM NO-REC GO A.
           IF ZUGRIF PERFORM BESETZT GO G.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
              IF WH-KARTEI(WX) NOT = 0 AND WH-KARTEI(WX) > DE-KARTEI(WX)
                   MOVE WH-KARTEI(WX) TO DE-KARTEI(WX).
           REWRITE DE-SATZ.
      *--------------------------------------------> Karteieintragung <-
       I.  MOVE 0 TO KA-DATUM.
           DISPLAY " bitte warten " with reverse-video AT 2435.
       J.  START KARTEIN KEY > KA-KEY INVALID GO X.
       K.  MOVE SPACE TO KA-TXT.
           READ KARTEIN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF KA-ADNUM NOT = DU-NUM GO X.
           MOVE KA-KEY TO DATEI.
           DELETE KARTEIN.
           MOVE DE-KTONR TO KA-ADNUM.
       M.  WRITE KA-SATZ NOT INVALID MOVE DATEI TO KA-KEY GO J.
           ADD 1 TO KA-DATUM.
           MOVE KA-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE KA-DATUM TO WZ-DATUM
               MOVE 1 TO WZ-TAG
               ADD 1 TO WZ-MONAT
               IF WZ-MONAT = 12 ADD 1 TO WZ-JAHR
                   MOVE 1 TO WZ-MONAT.
           MOVE WZ-DATUM TO KA-DATUM.
           GO M.
       X.  CALL "CAUP" USING "1313012480" WH-CREG.
       Z.  EXIT.
      **************************************************** Karteidruck *
       KARDRU SECTION.
       A.  MOVE 1 TO WM-DRU.
           IF WM-DRU not = 1 DISPLAY "bitte Formular einlegen" AT 2401
               PERFORM WEITER.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
       C.  MOVE 0 TO KA-DATUM.
           MOVE DE-KTONR TO KA-ADNUM.
           START KARTEIN KEY NOT < KA-KEY INVALID GO X.
       D.  MOVE SPACE TO KA-TXT.
           READ KARTEIN NEXT IGNORE LOCK AT END GO X.
           IF KA-ADNUM NOT = DE-KTONR GO X.
           IF WZ-ZEILEN > 64 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 2 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-SATZ(6:).
           MOVE "KUNDENKARTEI" TO DRA-SATZ(40:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(80:8).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-SATZ(6:86).
           PERFORM DRUCK.
           MOVE "Kd-Nr.:" TO DRA-SATZ(40:).
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-SATZ(47:).
           PERFORM DRUCK.
           MOVE WT-TXT(DE-ANREDE + 1) TO DRA-SATZ(6:12).
           PERFORM DRUCK.
           MOVE WR-ADR(1) TO DRA-SATZ(6:25).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE WX TO WD-J
               COMPUTE WY = WX * 3 + 30
               MOVE WD-J TO DRA-SATZ(WY:2).
           PERFORM DRUCK.
           MOVE WR-ADR(2) TO DRA-SATZ(6:25).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               COMPUTE WY = WX * 3 + 30
               IF DE-KARTEI(WX) NOT = 0
                   ADD 50 DE-KARTEI(WX) GIVING WD-J
                   MOVE WD-J TO DRA-SATZ(WY:2).
           PERFORM DRUCK.
           PERFORM VARYING WX FROM 21 BY 1 UNTIL WX > 40
               MOVE WX TO WD-J
               COMPUTE WY = (WX - 20) * 3 + 30
               MOVE WD-J TO DRA-SATZ(WY:2).
           MOVE 2 TO WZ-SCHALT.
           MOVE WR-ADR(4) TO DRA-SATZ(6:25).
           PERFORM DRUCK.
           PERFORM VARYING WX FROM 21 BY 1 UNTIL WX > 40
               COMPUTE WY = (WX - 20) * 3 + 30
               IF DE-KARTEI(WX) NOT = 0
                   ADD 50 DE-KARTEI(WX) GIVING WD-J
                   MOVE WD-J TO DRA-SATZ(WY:2).
           MOVE WR-ADR(5) TO DRA-SATZ(6:25).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-SATZ(6:86).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  MOVE KA-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(6:).
           MOVE KA-TXT TO WH-TEXT.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4
               IF WR-TXT(WX) NOT = SPACE
               MOVE WR-TXT(WX) TO DRA-SATZ(16:)
               PERFORM DRUCK.
           GO D.
       X.  IF WM-OPEN = 1 PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       GETADR SECTION.
       A.  MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WT-ADR TO WT-BEZ.
       Z.  EXIT.
      ************************************* Anlage / énderung DEBITOR *
       ADR-ANL SECTION.
       A.  IF ANLAGE MOVE 0 TO DE-ANREDE DE-PLZL
              MOVE SPACE TO DE-BEZ
              PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 40
                  MOVE 0 TO DE-KARTEI(WY)
           ELSE PERFORM GETADR.
           MOVE 2401 TO VDU-REL.
           MOVE 0 TO WY.
       B.  MOVE WY TO WD-I.
           DISPLAY WD-I AT VDU-REL.
           ADD 1 TO VDU-REL.
           DISPLAY ":" AT VDU-REL.
           ADD 1 TO VDU-REL.
           DISPLAY WT-TXT(WY + 1) AT VDU-REL.
           ADD 9 TO VDU-REL.
           IF WY < 7 ADD 1 TO WY GO B.
       C.  DISPLAY "<esc> = Abbruch, <tab>= lîschen, 0: ohne Anrede"
               AT 2301.
           MOVE DE-ANREDE TO WH-WERT.
           CALL "CAUP" USING "0004211001" WH-CREG.
           IF ESC GO W.
           IF TABL PERFORM LOESCHEN
              IF WX-TASTE = 98 GO Z ELSE GO W.
           IF WOLI GO A.
           IF NOT RET GO B.
           IF WH-WERT > 7 GO B.
           MOVE WH-WERT TO DE-ANREDE WD-KZ.
           DISPLAY WD-KZ AT 0421.
           DISPLAY WT-TXT(DE-ANREDE + 1) with highlight AT 0408.
       D.  DISPLAY "<>= zurÅck, <ret-leer>= Wiederholung" AT 2301.
           PERFORM GETADR.
           CALL "CAUP" USING "0205080533" WH-CREG.
           IF WOLI GO B.
           IF NOT RET GO D.
           MOVE WT-BEZ TO DE-BEZ.
           PERFORM GETADR.
           PERFORM SUCH-PLZL.
           IF DE-PLZL = SPACE DISPLAY "Postleitzahl fehlerhaft" AT 2401
               PERFORM WEITER GO D.
       F.  DISPLAY "<>= zurÅck, <ret-leer>= Wiederholung" AT 2301.
           IF DE-REKTO = 0 MOVE DE-KTONR TO DE-REKTO.
           MOVE DE-REKTO TO WH-WERT.
           COMPUTE WD-KTO = DE-REKTO / 10.
           MOVE 1012 TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           CALL "CAUP" USING "0010125005" WH-CREG.
           IF WOLI GO D.
           IF NOT RET GO F.
           MOVE WH-WERT TO DE-REKTO.
           IF DE-REKTO = 0 MOVE DE-KTONR TO DE-REKTO.
           COMPUTE WD-KTO = DE-REKTO / 10.
           DISPLAY WD-KTO AT VDU-LP.
       G.  DISPLAY "<>= zurÅck, <ret-leer>= Wiederholung" AT 2301.
           MOVE DE-TEL TO WT-TX.
           CALL "CAUP" USING "0211080133" WH-CREG.
           IF WOLI GO D.
           IF NOT RET GO G.
           MOVE WT-TX TO DE-TEL.
           DISPLAY DE-TEL with highlight SIZE 33 AT VDU-LP.
       K.  DISPLAY "<ret>= speichern, <esc>= Abbruch" AT 2301.
           IF DE-MCODE = SPACE DISPLAY "Matchcode: <ret>= 1. Wort, <#>=
      -        "2. Wort" AT 2401.
           MOVE DE-MCODE TO WH-MCODE.
           PERFORM RECONV.
           MOVE WV-MCODE TO DE-MCODE WT-TX.
           CALL "CAUP" USING "0203270110" WH-CREG.
           IF DE-MCODE = SPACE PERFORM KUNBEZ.
           IF ESC GO W.
           MOVE WT-TX TO WH-MCODE.
           PERFORM ALPHACONV.
           IF NOT RET GO K.
      *-----------------------------------> Ende Anlage oder énderung <-
           IF NOT ANLAGE GO Z.
       R.  MOVE WK-NUM TO DE-KTONR.
           WRITE DE-SATZ INVALID KEY ADD 10 TO WK-NUM GO R.
           GO Z.
       W.  READ DEBITOR INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO W.
       Z.  EXIT.
      ***************************************  hole Adresse lt. PLZL *
       SUCH-PLZL SECTION.
       A.  MOVE SPACE TO WT-TX DE-PLZL.
           PERFORM VARYING WS FROM 5 BY -1 UNTIL WS < 2
               MOVE WR-ADR(WS) TO WH-MCODE
               IF WH-MCODE(1:4) IS NUMERIC OR WH-MCODE(2:4) IS NUMERIC
                 OR WH-MCODE(3:4) IS NUMERIC OR WH-MCODE(4:4) IS NUMERIC
                 MOVE WR-ADR(WS) TO WT-TX.
           IF WT-TX = SPACE GO Z.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 8
               IF WT-TX(WS:1) = SPACE CONTINUE
               MOVE WT-TX(1:WS) TO DE-PLZL.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, <ret-leer>/< >= weiterlisten,
      -        "< >= zurÅcklisten" AT 2301.
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
       A.  ADD 203 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-KZ
               DISPLAY WD-KZ WITH HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR.
           MOVE 3 TO WI.
       Z.  EXIT.
      *****************************************************************
       SUCH-ADR SECTION.
       A.  CALL "CAUP" USING "0707031176001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Namensabfrage " with highlight AT VDU-REL.
           PERFORM ALLANZ.
           IF EINF MOVE WT-TX TO DE-STR
              MOVE 25 TO WE-LG
              CALL "CBL_TOLOWER" USING DE-STR by value WE-LG
              START DEBITOR KEY NOT < DE-STR INVALID
                   MOVE 0 TO DE-KTONR WH-WERT GO X
              end-start
              GO E.
           PERFORM ALPHACONV.
           MOVE ZERO TO DE-KTONR.
           START DEBITOR KEY NOT < DE-MCODE INVALID PERFORM NO-REC
                MOVE 0 TO DE-KTONR WH-WERT GO X.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF DE-KTONR = 0 GO E.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF DE-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM ADR-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO DE-KTONR WH-WERT GO X.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO DE-KEY GO X.
           IF AB GO E.
       N.  READ DEBITOR PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF DE-KTONR = 0 GO N.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF DE-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM ADR-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       X.  CALL "CAUP" USING "08CLOFENST" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       ADR-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           ADD 9 TO VDU-REL.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           DISPLAY WT-TX with SIZE 50 highlight AT VDU-REL.
           ADD 51 TO VDU-REL.
           DISPLAY DE-PLZL AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE DE-KEY TO WT-KEY(WX).
       Z.  EXIT.
      ******************************************************************
       LOESCHEN SECTION.
       A.  IF ANLAGE GO C.
           DISPLAY "Codewort f. Lîschung: (<esc>= Abbruch) " AT 2401.
           CALL "CAUP" USING "0024406006" WH-CREG.
           IF ESC GO Z.
           IF WH-NUM NOT = DE-KTONR
               DISPLAY " nicht gelîscht," with BELL AT 2401
               PERFORM WEITER GO A.
       B.  DELETE DEBITOR INVALID KEY GO Z.
           PERFORM LOEKART.
       C.  MOVE 98 TO WX-TASTE.
       Z.  EXIT.
      *****************************************************************
       LOEKART SECTION.
       A.  MOVE 0 TO KA-DATUM.
           MOVE DE-KTONR TO KA-ADNUM.
           START KARTEIN KEY > KA-KEY INVALID GO Z.
       C.  READ KARTEIN NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF DE-KTONR NOT = KA-ADNUM GO Z.
           DISPLAY "-- bitte warten --" AT 2460.
           DELETE KARTEIN.
           GO C.
       Z.  EXIT.
      *****************************************************************
       BEG SECTION.
       A.  DISPLAY "<esc>= Ende, <ret-leer>= ab Anfang, Nr.+<ret>= ab Nu
      -        "mmer" AT 2301.
           DISPLAY "<ret>+alpha = suchen" AT 2401.
           DISPLAY "Kurzbez/Nr:" AT 0801.
           MOVE 0813 TO VDU-REL.
           DISPLAY LOW-VALUE AT VDU-REL.
           PERFORM WARWAS.
       D.  CALL "CAUP" USING "0008139010" WH-CREG.
           MOVE 0 TO WH-OFT.
           IF ESC OR WOLI GO Z.
           IF NOT RET GO A.
      *-----------------------------------> WH-NUM = 0 : Suchen n. ALPHA
           IF WH-MCODE NOT = SPACE; IF WH-NUM = 0 PERFORM SUCH-ADR
               IF DE-KTONR = 0 GO A
               ELSE MOVE DE-KTONR TO WH-NUM.
      *--------------------------------------> WH-NUM = Nr. f. Aufruf <-
       E.  DISPLAY "ab Nummer: " AT 0350.
           MOVE WH-NUM TO DE-KTONR WV-KTONR.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           IF DE-KTONR = 0 DISPLAY "ab Anfang" AT 0300
           ELSE DISPLAY WD-KTO with highlight AT 0300.
       F.  DISPLAY "Gruppe:      " AT 1001.
           DISPLAY "<esc>= Ende, <ret>= Gruppe, <>= zurÅck, <ret-leer>=
      -        " alle Kunden" AT 2301.
           DISPLAY "bei Gruppe 40 wird das akt. Jahr geprÅft!" AT 2401.
           CALL "CAUP" USING "0010092002" WH-CREG.
           IF ESC GO Z.
           IF WOLI CALL "CAUP" USING "1310012480000" WH-CREG GO A.
           IF NOT RET OR WH-NUM > 40 GO F.
           MOVE WH-NUM TO IX.
           IF IX = 0 DISPLAY "alle" with highlight AT 1009 GO Q
                ELSE DISPLAY IX with highlight AT 1009.
       H.  DISPLAY "von Jahr:    bis Jahr:   " AT 1201.
           DISPLAY "<esc>= Ende, <ret>= Jahreszahl" AT 2301.
           CALL "CAUP" USING "0012112002" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO H.
           MOVE WH-NUM TO WY WD-J.
           DISPLAY WD-J with highlight AT 1211.
           IF WY < 51 ADD 100 TO WY.
           SUBTRACT 50 FROM WY GIVING WV-VJ.
       J.  CALL "CAUP" USING "0012242002" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO H.
           MOVE WH-NUM TO WY WD-J.
           DISPLAY WD-J with highlight AT 1224.
           IF WY < 51 ADD 100 TO WY.
           SUBTRACT 50 FROM WY GIVING WV-BJ.
       Q.  DISPLAY "richtig?" with highlight AT 2301 "<esc>= falsch, <re
      -        "t>= ok" AT 2310.
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF ESC GO F.
           IF NOT RET GO Q.
           CALL "CAUP" USING "1311012480" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       ETIKETTEN SECTION.
       A.  MOVE "   Etikettendruck" TO WK-GEB.
           PERFORM KOPF.
           CALL "CAUP" USING "1303012480" WH-CREG.
           MOVE ZERO TO WS-ANZ-PLZ WS-ANZ-GES.
           MOVE SPACE TO WV-PLZL.
           MOVE WH-DATUM TO WZ-DATUM.
           MOVE WZ-JAHR TO WY.
           IF WZ-JAHR < 51 ADD 100 TO WY.
           SUBTRACT 50 FROM WY GIVING WS-JAHR.
           DISPLAY "1-, 2-, 3- oder 4-bahnig, 5- Export < >" AT 0305.
           DISPLAY "<ret>= Eingabe, <tab>= Liste, <esc>= Ende" AT 2401.
           CALL "CAUP" USING "0003421001" WH-CREG.
           IF ESC GO Y.
           IF TABL PERFORM LISTE GO Y.
           IF NOT RET GO A.
           IF WH-NUM = 0 OR WH-NUM > 5 GO A.
           MOVE WH-NUM TO WZ.
           DISPLAY "von Postleitzahl: " AT 0504.
       B.  DISPLAY "bis Postleitzahl: " AT 0604.
           DISPLAY "<ret-leer>= ab Anfang, <esc>= Ende" at 2301.
      *------------------------------------------------> von-Eingaben <-
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0205220108" WH-CREG.
           IF ESC GO Y.
           IF NOT RET GO B.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE DISPLAY "ab Anfang" with highlight AT 0522
           ELSE DISPLAY DE-PLZL with highlight AT 0522
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-VONPLZL.
      *-----------------------------------------------> bis-Eingaben <-
       C.  DISPLAY "<esc>= zurÅck, <ret-leer>=" AT 2301.
           IF WH-VONPLZL > SPACE DISPLAY "Wiederholung d. PLZL" AT 2300
               ELSE DISPLAY "bis Ende" AT 2300.
           MOVE WH-VONPLZL TO WT-TX.
           CALL "CAUP" USING "0206220108" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO C.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE MOVE 99999 TO DE-PLZL
               DISPLAY "bis Ende" with highlight AT 0622
           ELSE
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-BISPLZL.
           MOVE WH-VONPLZL TO DE-PLZL.
      *------------------ bei Absturz --------> Start ab Nummer/Namen <-
           PERFORM BEG.
           IF ESC GO Y.
           IF WOLI GO A.
           MOVE 1 TO WY.
           MOVE 0 TO WM-EX.
           PERFORM UNTER.
       E.  START DEBITOR KEY NOT < DE-PKEY INVALID PERFORM NO-REC GO A.
       F.  READ DEBITOR NEXT IGNORE LOCK AT END GO X.
           IF DE-KTONR = 0 GO F.
           PERFORM KB-STAT
               IF ESC GO X.
           IF IX = 0 GO G.
           IF IX = 40; IF WS-JAHR NOT = DE-KARTEI(40) GO F.
           IF DE-KARTEI(IX) = 0 GO F.
           IF DE-KARTEI(IX) < WV-VJ OR > WV-BJ GO F.
       G.  IF DE-KTONR < WV-KTONR MOVE WV-KTONR TO DE-KTONR GO E.
           IF PLZL AND DE-PLZL > WH-BISPLZL GO X.
           IF PLZL AND WV-PLZL = SPACE MOVE DE-PLZL TO WV-PLZL.
           IF PLZL AND WV-PLZL NOT = DE-PLZL PERFORM LOE-DRUCK
                MOVE DE-PLZL TO WV-PLZL
                PERFORM OB-DRUCKEN.
           PERFORM GETADR.
           DISPLAY WR-ADR(1) AT 2020.
           IF WZ = 5 PERFORM EXPORT
                     GO F.
           MOVE DE-KTONR TO WD-BNUM(WY).
           MOVE WR-ADR(1) TO WD-TX1(WY).
           MOVE WR-ADR(2) TO WV-MCODE.
           IF WV-MCODE(1:2) NOT = "T:" MOVE WR-ADR(2) TO WD-TX2(WY).
           MOVE WR-ADR(4) TO WD-TX3(WY).
           MOVE DE-ANREDE TO WD-ANR(WY).
           MOVE WR-ADR(5) TO WD-TX4(WY).
           ADD 1 TO WS-ANZ-PLZ.
           ADD 1 TO WS-ANZ-GES.
           PERFORM OB-DRUCKEN.
           GO F.
      *------------------------------------------------------> Dateiende
       X.  PERFORM LOE-DRUCK.
           MOVE WY TO WZ.
           PERFORM OB-DRUCKEN.
       Y.  IF WM-OPEN = 1 CLOSE DRUCKER
               MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      *************************************************** Exportlisten *
       EXPORT SECTION.
       A.  IF WR-ADR(1) = SPACE GO Z.
           IF WR-ADR(3) = SPACE MOVE WR-ADR(4) TO WR-ADR(3)
                                MOVE WR-ADR(5) TO WR-ADR(4)
                                MOVE WR-ADR(6) TO WR-ADR(5)
                                MOVE SPACE TO WR-ADR(6).
           IF WM-EX = 0 OPEN OUTPUT EXPODAT
                        MOVE 1 TO WM-EX
                        MOVE "Name;Zusatz;Strasse;Plzl;Ort;" TO EX-SATZ
                        WRITE EX-SATZ.
           MOVE WR-ADR(1) TO EX-SATZ.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 1
               OR EX-SATZ(WX:1) not = SPACE CONTINUE.
           ADD 1 TO WX.
           MOVE ";" TO EX-SATZ(WX:1).
           ADD 1 TO WX.
           MOVE WR-ADR(2) TO EX-SATZ(WX:).
           PERFORM VARYING WX FROM 50 BY -1 UNTIL WX = 1
               OR EX-SATZ(WX:1) not = SPACE CONTINUE.
           ADD 1 TO WX.
           MOVE ";" TO EX-SATZ(WX:1).
           ADD 1 TO WX.
           IF WR-ADR(3) = SPACE MOVE 4 TO WY
                           else MOVE 3 TO WY.
           MOVE WR-ADR(WY) TO EX-SATZ(WX:).
           PERFORM VARYING WX FROM 70 BY -1 UNTIL WX = 1
               OR EX-SATZ(WX:1) not = SPACE CONTINUE.
           ADD 1 TO WX.
           MOVE ";" TO EX-SATZ(WX:1).
           ADD 1 TO WX.
           MOVE WR-ADR(4) TO EX-SATZ(WX:).
           ADD 4 TO WX.
           MOVE ";" TO EX-SATZ(WX:1).
           ADD 1 TO WX.
           IF EX-SATZ(WX:1) = " " MOVE EX-SATZ(WX:) TO WT-TX
               MOVE WT-TX(2:) TO EX-SATZ(WX:).
           PERFORM VARYING WX FROM 90 BY -1 UNTIL WX = 1
               OR EX-SATZ(WX:1) not = SPACE CONTINUE.
           ADD 1 TO WX.
           MOVE ";" TO EX-SATZ(WX:1).
           WRITE EX-SATZ.
       Z.  EXIT.
      ******************************************************************
       LOE-DRUCK SECTION.
       A.  IF WS-ANZ-GES = 0 GO Z.
           MOVE 9 TO WD-ANR(WY).
           MOVE 0 TO WD-BNUM(WY).
           MOVE ALL "*" TO WD-TX1(WY) WD-TX2(WY).
           MOVE " = Anzahl insgesamt" TO WD-RTX3(WY).
           MOVE " = Summe Postleitz." TO WD-RTX4(WY).
           MOVE WS-ANZ-PLZ TO WD-ANZ-PLZ(WY).
           MOVE WV-PLZL TO WD-PLZL(WY).
           MOVE ZERO TO WS-ANZ-PLZ.
           MOVE WS-ANZ-GES TO WD-ANZ-GES(WY).
       Z.  EXIT.
      *************** WZ= Etikettenanzahl, WY= ZÑhler i. Druckpuffer *
       OB-DRUCKEN SECTION.
       A.  IF WY NOT = WZ ADD 1 TO WY GO Z.
           IF WS-ANZ-GES = 0 GO Z.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"0c00" TO WH-PX (2).
           PERFORM BEG-DRU.
           MOVE 1 TO WY.
       B.  IF WD-BNUM(WY) > 0 MOVE WD-BNUM(WY) TO DRA-BNUM(WY)
               ELSE MOVE ALL "*" TO DRA-TX(WY).
           IF WY NOT = WZ ADD 1 TO WY GO B.
           PERFORM DRUCK.
           MOVE 1 TO WY.
       C.  MOVE WD-ANR(WY) TO WX.
           IF WX > 0; IF WX < 9 MOVE WT-TXT(WX + 1) TO DRA-TX(WY)
               ELSE MOVE "***** Summenetikett *****" TO DRA-TX(WY).
           IF WY NOT = WZ ADD 1 TO WY GO C.
           PERFORM DRUCK.
           MOVE 1 TO WY.
       D.  MOVE WD-TX1(WY) TO DRA-TX(WY).
           MOVE SPACE TO WD-TX1(WY).
           IF WY NOT = WZ ADD 1 TO WY GO D.
           PERFORM DRUCK.
           MOVE 1 TO WY.
       E.  MOVE WD-TX2(WY) TO DRA-TX(WY).   
           MOVE SPACE TO WD-TX2(WY).
           IF WX = 9 MOVE WD-PLZL(WY) TO DRA-TX(WY)(10:10)
               MOVE "  " TO DRA-TX(WY)(8:2)
               MOVE SPACE TO WD-PLZL(WY).
           IF WY NOT = WZ ADD 1 TO WY GO E.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 1 TO WY.
       F.  MOVE WD-TX3(WY) TO DRA-TX(WY).
           MOVE SPACE TO WD-TX3(WY).
           IF WY NOT = WZ ADD 1 TO WY GO F.
           PERFORM DRUCK.
           MOVE 1 TO WY.
       G.  MOVE WD-TX4(WY) TO DRA-TX(WY).
           MOVE SPACE TO WD-TX4(WY).
           IF WY NOT = WZ ADD 1 TO WY GO G.
           PERFORM DRUCK.
           MOVE 1 TO WY.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************* Dupletten-Findung *
       DUPLETEN SECTION.
       A.  MOVE "   Duplettensuche" TO WK-GEB.
           PERFORM KOPF.
           DISPLAY "   Postleitzahl" AT 0501.
           DISPLAY "von:" AT 0517
                   "bis:" AT 0617.
           DISPLAY "<ret-leer>= ab Anfang, <esc>= Abbruch" AT 2301.
      *-----------------------------------------------> von-Eingaben <-
           MOVE 0 TO WH-WERT.
           MOVE SPACE TO WV-PLZL WT-TX.
           CALL "CAUP" USING "0205220108" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE DISPLAY "ab Anfang" with highlight AT 0522
           ELSE DISPLAY DE-PLZL with highlight AT 0522
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-VONPLZL.
      *-----------------------------------------------> bis-Eingaben <-
       C.  DISPLAY "<esc>= zurÅck, <ret>= Bildschirmausgabe, (<-)= Druck
      -        "erausgabe" AT 2301.
           DISPLAY "<ret-leer>=" AT 2401.
           IF WH-VONPLZL > SPACE DISPLAY "Wiederholung d. PLZL" AT 2412
               ELSE DISPLAY "bis Ende" AT 2412.
           MOVE WH-VONPLZL TO WT-TX.
           CALL "CAUP" USING "0206220108" WH-CREG.
           IF ESC GO A.
           IF RET MOVE 1 TO WH-ENTSCHEID
               MOVE 0 TO WX-TASTE
               ELSE MOVE 0 TO WH-ENTSCHEID.
           IF NOT RET GO C.
           MOVE WT-TX TO DE-PLZL.
           IF DE-PLZL = SPACE MOVE 99999 TO DE-PLZL
               DISPLAY "bis Ende" with highlight AT 0622
           ELSE
                DISPLAY DE-PLZL with highlight AT 0622.
           MOVE DE-PLZL TO WH-BISPLZL.
       D.  MOVE WH-VONPLZL TO DE-PLZL.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY NOT < DE-PKEY INVALID PERFORM NO-REC GO A.
           MOVE 1 TO WY.
           OPEN OUTPUT DUPLETE.
           CLOSE DUPLETE.
           OPEN I-O DUPLETE.
           IF ABILD PERFORM KOPF
               MOVE 301 TO VDU-LP
               PERFORM BILD
               MOVE 1101 TO VDU-LP
               PERFORM BILD.
           DISPLAY "<esc>= Abbruch" AT 2301.
       F.  READ DEBITOR NEXT IGNORE LOCK AT END GO X.
           IF DE-KTONR = 0 GO F.
           PERFORM KB-STAT
               IF ESC GO X.
           IF PLZL AND DE-PLZL > WH-BISPLZL GO X.
           IF PLZL AND WV-PLZL = SPACE MOVE DE-PLZL TO WV-PLZL.
           IF PLZL AND WV-PLZL NOT = DE-PLZL;
               IF NOT ABILD PERFORM DRUCK
                   MOVE DE-PLZL TO WV-PLZL.
           PERFORM GETADR.
           DISPLAY WR-ADR(1) with highlight AT 2020.
           MOVE DE-PLZL TO DU-PLZL.
           MOVE WR-ADR(4) TO DU-TX.
           MOVE DE-KTONR TO DU-NUM.
           MOVE WR-ADR(4) TO VDU-KUBEZ.
           MOVE SPACE TO WV-MCODE DU-NR.
           SET VX TO 25.
       G.  IF VDU-KZ(VX) = SPACE;
               IF VX NOT = 1 SET VX DOWN BY 1 GO G ELSE GO K.
           SET MX TO 3.
       H.  IF VDU-KZ(VX) = SPACE OR VDU-KZ(VX) = "." GO K.
           MOVE VDU-KZ(VX) TO WV-MC(MX).
           IF MX NOT = 1 AND VX NOT = 1 SET MX VX DOWN BY 1 GO H.
       K.  MOVE WV-MCODE TO DU-NR.
           MOVE 0 TO WX-TASTE.
           MOVE DU-SATZ TO WH-DUSATZ.
           WRITE DU-SATZ INVALID KEY PERFORM DUPLOS.
           IF WX-TASTE = 99 GO X.
           GO F.
       X.  IF WM-OPEN = 1 PERFORM END-DRU.
           CLOSE DUPLETE.
           OPEN OUTPUT DUPLETE.
           CLOSE DUPLETE.
       Z.  EXIT.
      ********************************************** Bildschirmausgabe *
       DUPLOS SECTION.
       D.  MOVE DE-SATZ TO WH-SATZ.
           MOVE 303 TO VDU-LP.
           IF ABILD PERFORM EINBLENDEN
               ELSE PERFORM DUPLIST.
       E.  READ DUPLETE. 
           IF ZUGRIF GO E.
           MOVE DU-NUM TO DE-KTONR.
       F.  READ DEBITOR INVALID KEY GO W.
           IF ZUGRIF GO F.
           MOVE 1103 TO VDU-LP.
           IF ABILD PERFORM EINBLENDEN
               ELSE PERFORM DUPLIST GO W.
           DISPLAY "           " AT 0350.
           DISPLAY "           " AT 1150.
           DISPLAY "0= weiter, 1= lîschen 1. Adresse, 2= lîschen 2. Adre
      -        "sse " AT 2301.
           DISPLAY "<esc>= Abbruch" AT 2401.
           CALL "CAUP" USING "0023570000" WH-CREG.
           IF ESC GO Z.
           IF WH-NUM = 1 GO K.
           IF WH-NUM NOT = 2 GO W.
      *----------------------------------------> lîschen 2. Adresse <-
       G.  DELETE DEBITOR INVALID KEY GO W.
           PERFORM LOEKART.
           IF ZUGRIF GO G.
           DISPLAY "gelîscht!" with highlight AT 1160.
       H.  DELETE DUPLETE INVALID KEY GO W.
           IF ZUGRIF GO H.
           MOVE WH-DUSATZ TO DU-SATZ.
       I.  WRITE DU-SATZ INVALID KEY GO W.
           GO W.
      *----------------------------------------> lîschen 1. Adresse <-
       K.  MOVE WH-SATZ TO DE-SATZ.
           DELETE DEBITOR INVALID KEY GO W.
           PERFORM LOEKART.
           IF ZUGRIF GO K.
           DISPLAY "gelîscht!" with highlight AT 0360.
       W.  MOVE WH-SATZ TO DE-SATZ.
           START DEBITOR KEY > DE-PKEY INVALID KEY MOVE 99 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       DUPLIST SECTION.
       A.  IF WM-OPEN = 0 OPEN OUTPUT DRUCKER
               MOVE 0 TO WZ-ZEILEN WZ-SEITE WZ-SCHALT
               MOVE 1 TO WM-OPEN.
           MOVE SPACE TO DRA-SATZ.
           IF WZ-ZEILEN > 66 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 MOVE "Duplettenliste" TO DRB-SATZ
               MOVE VDU-DATUM TO DRB-DATUM
               ADD 1 TO WZ-SEITE
               MOVE "Seite:" TO DRB-SEITX
               MOVE WZ-SEITE TO DRB-SEITE
               PERFORM DRUCK
               MOVE ALL "-" TO DRB-SATZ
               PERFORM DRUCK.
           MOVE DE-KTONR TO DRC-NUM.
           MOVE WR-ADR(1) TO DRC-TXA.
           MOVE WR-ADR(2) TO DRC-TXB.
           MOVE WR-ADR(4) TO DRC-TXC.
           MOVE WR-ADR(5) TO DRC-TXD.
           PERFORM DRUCK.
           GO Z.
       Z.  EXIT.
      ******************************************************************
       LISTE SECTION.
       A.  MOVE " L i s t u n g" TO WK-GEB.
           PERFORM KOPF.
           DISPLAY "<ret/#>= Start, <esc>= Abbruch <      >" AT 2301.
           DISPLAY "<#> statt <ret>= ab Nummer" AT 2401.
           CALL "CAUP" USING "0023336006" WH-CREG.
           IF ESC GO Z.
           IF KIST MOVE WH-WERT TO DE-KTONR.
           IF NOT RET and not KIST GO A.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"0c00" TO WH-PX (2).
           PERFORM BEG-DRU.
           MOVE LOW-VALUE TO DE-MCODE.
           IF KIST START DEBITOR KEY > DE-KEY INVALID KEY GO Z
                   END-START GO C.
           START DEBITOR KEY > DE-MCODE INVALID KEY GO Z.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END GO X.
           IF DE-KTONR = 0 GO C.
           MOVE SPACE TO DRA-SATZ.
           IF WZ-ZEILEN > 66 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 MOVE "Adressenliste" TO DRD-SATZ
               MOVE VDU-DATUM TO DRD-DATUM
               ADD 1 TO WZ-SEITE
               MOVE "Seite:" TO DRD-SEITX
               MOVE WZ-SEITE TO DRD-SEITE
               PERFORM DRUCK
               MOVE ALL "-" TO DRD-SATZ
               PERFORM DRUCK.
           MOVE DE-KTONR TO DRE-NUM.
           PERFORM GETADR.
           MOVE WR-ADR(1) TO DRE-TXA.
           MOVE WR-ADR(2) TO DRE-TXB.
           MOVE WR-ADR(4) TO DRE-TXC.
           MOVE WR-ADR(5) TO DRE-TXD.
           PERFORM DRUCK.
           GO C.
       X.  PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       ABSTELL SECTION.
       A.  SET BX TX TO 1.
           MOVE SPACE TO WT-BEZ.
       B.  MOVE WR-ADR(TX) TO WT-TX.
           SET TY TO 29.
           MOVE 1 TO WX.
       C.  IF WR-TX(TY) NOT = SPACE SET WX TO TY
               SET BX UP BY WX
               MOVE "#" TO WR-BEZ(BX)
               SET BX DOWN BY 1
               ADD 1 TO WX
               GO D.
           IF TY NOT = 1 SET TY DOWN BY 1 GO C.
           MOVE "#" TO WR-BEZ(BX).
           GO E.
       D.  MOVE WR-TX(TY) TO WR-BEZ(BX).
           IF TY NOT = 1 SET TY BX DOWN BY 1 GO D.
       E.  SET BX UP BY WX.
       F.  IF BX > 240 DISPLAY "Zuviele Zeichen eingegeben! " AT 2401
                PERFORM WEITER GO Z.
           IF TX < 5 SET TX UP BY 1 GO B.
       Z.  EXIT.
      ******************************************************************
       REORG SECTION.
       A.  OPEN INPUT DEBALT.
           IF WF-STATUS NOT = "00" GO Z.
           OPEN INPUT KARTALT.
           IF WF-STATUS NOT = "00" CLOSE DEBALT GO Z.
       B.  DISPLAY "<ret>= Start, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO B.
           DISPLAY " Bitte warten " with BLINK at 2330.
           MOVE 0 TO KOL-ADNUM KOL-DAT DA-KTONR.
           START DEBALT KEY NOT < DA-KEY INVALID GO X.
       C.  READ DEBALT NEXT AT END GO G.
           MOVE DA-SATZ TO DE-SATZ.
           DISPLAY DE-KTONR AT 1230.
           WRITE DE-SATZ INVALID GO C.
           GO C.
       G.  START KARTALT KEY NOT < KOL-KEY INVALID GO X.
           MOVE SPACE TO KOL-SATZ.
       I.  READ KARTALT NEXT IGNORE LOCK AT END GO X.
           MOVE KOL-SATZ TO KA-SATZ.
           DISPLAY KA-ADNUM AT 1330.
           WRITE KA-SATZ INVALID GO I.
           GO I.
       X.  CLOSE DEBALT KARTALT.
       Z.  EXIT.
