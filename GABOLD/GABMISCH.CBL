      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GABMISCH.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEDEB.CPY.
           SELECT KARTEIN  ASSIGN TO RANDOM "GABKART.ADI"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS KA-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT OLDKART  ASSIGN TO RANDOM "\ADR\ADRKART.ADI"
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS OL-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT ALDEBIT  ASSIGN TO "DEBITORE.FII"
                           ORGANIZATION INDEXED  ACCESS DYNAMIC
                           RECORD KEY AL-KEY
                           ALTERNATE RECORD KEY AL-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT ADRESSEN ASSIGN TO RANDOM "\ADR\ADRESSEN.ADI",
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS AD-KEY, 
                           ALTERNATE RECORD AD-PKEY,
                           ALTERNATE RECORD AD-BEZ DUPLICATES,
                           FILE STATUS IS WF-STATUS.
           SELECT PLZLDAT  ASSIGN TO RANDOM "\ADR\ADPLZL.ADI",
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS PL-KEY, 
                           FILE STATUS IS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
      *********************************************** Karteibewegungen *
       FD  KARTEIN     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 286 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  KA-SATZ.
           03  KA-KEY.
               05 KA-ADNUM             PIC 9(6)     COMP.
               05 KA-DATUM             PIC 9(6)     COMP.
           03  KA-TXT                  PIC X(280).
      *********************************************** Karteibewegungen *
       FD  OLDKART     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 286 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  OL-SATZ.
           03  OL-KEY.
               05 OL-ADNUM             PIC 9(6)     COMP.
               05 OL-DATUM             PIC 9(6)     COMP.
           03  OL-TXT                  PIC X(280).
      *****************************************************************
       FD  ALDEBIT      EXTERNAL       LABEL RECORDS STANDARD
                                       RECORD 256 CHARACTERS.
       01  AL-SATZ.
           03  AL-KEY.
               05 AL-FNR               PIC 99       COMP.
               05 AL-KTONR             PIC 9(6)     COMP.
           03  AL-MCODE                PIC X(8).
           03  AL-KTOART               PIC 9.
           03  ZEILE                   PIC 99.
           03  ZEILE-VJ                PIC 99.
           03  SEITE                   PIC 999      COMP.
           03  SEITE-VJ                PIC 999      COMP.
           03  T-BEW                OCCURS 3.
               05  BEW1                PIC 9(5)     COMP.
               05  BEWL                PIC 9(5)     COMP.
           03  OP1                     PIC 9(5)     COMP.
           03  OPL                     PIC 9(5)     COMP.
           03  AL-FWKZ                 PIC 9.
           03  AL-OPKZ                 PIC 9.
           03  UMTAB                   PIC S9(9)V99 OCCURS 10 COMP-3.
           03  AL-KOND                 PIC 9(1).
           03  AL-MAHNT                PIC 999      COMP-3.
           03  AL-MAHND                PIC 9(6)     COMP.
           03  AL-BEZ                  PIC X(110).
           03  AL-TEL                  PIC X(18).
           03  AL-SPERR                PIC 9(6)     COMP.
           03  AL-KETTD                PIC 9(5)     COMP.
           03  AL-REKTO                PIC 9(6)     COMP.
           03  AL-RAST                 PIC 9        COMP.
           03  AL-RAB                  PIC 99V9     COMP.
           03  AL-EAN                  PIC 9        COMP.
           03  AL-SUB                  PIC 9999     COMP.
      ******************************************************************
       FD  ADRESSEN     external       LABEL RECORD STANDARD
                                       RECORD 136 CHARACTERS.
       01  AD-SATZ.
           03  AD-KEY.
               05 AD-NUM               PIC 9(6)    COMP.
           03  AD-PKEY.
               05 AD-PLZL              PIC 9(8)    COMP.
               05 AD-PNUM              PIC 9(6)    COMP.
           03  AD-BEZ.
               05  AD-KUBEZ            PIC X(10).
           03  AD-ANREDE               PIC 99      COMP.
           03  AD-TEXT                 PIC X(25)         OCCURS 3.
           03  AD-KARTEI               PIC 99      COMP OCCURS 40.
      *--------------------------------------------------> WH-Key = 0 <-
       01  AD-KSATZ.
           03  FILLER                  PIC X(11).
           03  AD-FIRMA                PIC X(22).
           03  AD-ANR.
               05 AD-AN                PIC X(9)          OCCURS 7.
           03  FILLER                  PIC X(40).
      ******************************************************************
       FD  PLZLDAT     external        LABEL RECORD STANDARD
                                       RECORD 32 CHARACTERS.
       01  PL-SATZ.
           03  PL-KEY. 
               05  PL-PLZL             PIC 9(6)    COMP.
           03  PL-ORT                  PIC X(20).
           03  FILLER                  PIC X(5).
           03  PL-ALTER.
               05 PL-APLZL             PIC 9999.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRC-NUM                 PIC ZZZZZ9-.
           03  DRC-TX                  PIC X(110).
      *****************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WH-REG.
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
           03  WH-VONPLZL              PIC 9(5)    COMP-3.
           03  WH-BISPLZL              PIC 9(5)    COMP-3.
           03  WM-MERKER               PIC 9       COMP-3.
               88  ANLAGE     VALUE 1.
           03  WM-OPEN                 PIC 9       COMP-3.
           03  WI                      PIC 9       COMP.
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
           03  WD-ADNUM                PIC ZZZ.ZZ9.
           03  WV-PLZL                 PIC 9(6)  COMP VALUE ZERO.
           03  WM-PLZL                 PIC 9(8)  COMP VALUE ZERO.
           03  WH-PLZL                 PIC 99999      VALUE ZERO.
           03  VDU-REL                 PIC 9999       value zero.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  VDU-AB                  PIC 9999.
           03  VR REDEFINES VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  WX-PRNO                 PIC 99   COMP-X.
           03  WX-PRSTAT               PIC 99   COMP-X.
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
           03  WD-BELEG                PIC ZZZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-X                    PIC X.
           03  DATEI                   PIC X(12).
           03  WH-TEXT.
               05 WR-TXT               PIC X(70)    OCCURS 4.
           03  WR-XT REDEFINES WH-TEXT PIC X  OCCURS 280 INDEXED XX.
           03  WH-KARTEI               PIC 99        COMP  OCCURS 40.
           03  WH-KETTD                PIC 9(5)    VALUE ZERO.
           03  RED REDEFINES WH-KETTD.
               05  WH-ANREDE           PIC 9.
               05  WH-STATIS           PIC 9.
               05  WH-FAKART           PIC 9.
               05  WH-TOUR             PIC 99.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(10).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION USING WL-CALL WL-CREG.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ADRESSEN.
       A.  CALL "CADECL" USING "ADRESSEN.ADI" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON PLZLDAT.
       A.  CALL "CADECL" USING "ADPLZL.ADI  " WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ALDEBIT.
       A.  CALL "CADECL" USING "DEBITORE.FII" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "GABDEBIT.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON KARTEIN.
       A.  CALL "CADECL" USING "ADRKART.ADI " WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACE UPON CRT.
           MOVE WL-CREG TO WH-CREG.
           OPEN I-O DEBITOR.
           IF WF-STATUS = "00" GO W.
           DISPLAY " Zusammenlegung der Karteiadressen und Debitoren "
               with reverse-video AT 0115.
           PERFORM KARTADR.
      *    PERFORM DEBADR.
      *    PERFORM KARTZEIL.
       W.  CLOSE DEBITOR.
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
      ****************************** Druckerrckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularh”he *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT WZ-SEITE.
       C.  MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ************************************************* alte Debitoren *
       DEBADR SECTION.
       A.  OPEN INPUT ALDEBIT.
           IF WF-STATUS NOT = "00" GO Z.
           move x"0100" to wh-px(1).
           move x"0008" TO WH-PX(2).
           PERFORM BEG-DRU.
           MOVE 0 TO AL-KTONR.
           START ALDEBIT KEY NOT < AL-KEY INVALID GO X.
       C.  READ ALDEBIT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE AL-KTONR TO DE-KTONR.
           MOVE AL-MCODE TO DE-MCODE.
           MOVE AL-BEZ TO DE-BEZ.
           MOVE AL-TEL TO DE-TEL.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE 0 TO DE-KARTEI(WX).
           MOVE 0001400000024 TO DE-KOND.
           MOVE AL-KETTD TO WH-KETTD.
           MOVE WH-ANREDE TO DE-ANREDE.
      *---------------------------------------------> Adresszerlegung <-
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           IF WR-ADR(5)(1:4) NUMERIC MOVE WR-ADR(5)(1:4) TO DE-PLZL
           else IF WR-ADR(4)(1:4) NUMERIC
                    MOVE WR-ADR(4)(1:4) TO DE-PLZL.
           DISPLAY DE-KTONR AT 1201
           MOVE DE-KTONR TO DRC-NUM.
           WRITE DE-SATZ NOT INVALID GO C.
           MOVE DE-SATZ TO WT-BEZ.
           INSPECT de-bez replacing all "#" by ",".
           DISPLAY DE-BEZ with SIZE 80 at 1301.
           READ DEBITOR.
           INSPECT de-bez replacing all "#" by ",".
           DISPLAY de-bez with size 80 at 1401.
           MOVE DE-BEZ TO DRC-TX.
           PERFORM DRUCK.
           GO C.
      **-----------------------------------------> alte Adressendatei <-
       X.  CLOSE ALDEBIT.
       Z.  EXIT.
      ************************************************ Karteiadressen *
       KARTADR SECTION.
       F.  OPEN INPUT ADRESSEN.
           IF WF-STATUS NOT = "00" GO Z.
           OPEN I-O PLZLDAT.
           MOVE 0 TO AD-NUM.
           START ADRESSEN KEY NOT < AD-KEY INVALID GO X.
       H.  READ ADRESSEN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO H.
           INITIALIZE DE-SATZ.
           MOVE AD-PNUM TO DE-KTONR.
           IF DE-KTONR = 0 OR AD-NUM = 0 GO H.
           COMPUTE DE-KTONR = DE-KTONR * 10 + 2.
           MOVE AD-KUBEZ TO DE-MCODE.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE AD-KARTEI(WX) TO DE-KARTEI(WX).
           MOVE SPACE TO WT-BEZ WT-ADR.
           PERFORM MACH-PLZL.
           PERFORM HOL-ADR.
           MOVE SPACE TO WT-BEZ WT-ADR.
           MOVE WH-MCODE TO WR-ADR(5) DE-PLZL.
           PERFORM VARYING WY FROM 10 BY -1 UNTIL
               WR-ADR(5)(WY:1) NOT = SPACE OR WY = 1.
           IF WY NOT = 1 ADD 2 TO WY.
           MOVE PL-ORT TO WR-ADR(5)(WY:).
           MOVE AD-TEXT(1) TO WR-ADR(1).
           MOVE AD-TEXT(2) TO WR-ADR(2).
           MOVE AD-TEXT(3) TO WR-ADR(4).
           ADD -2 AD-ANREDE GIVING DE-ANREDE.
           IF AD-ANREDE < 2 MOVE 6 TO DE-ANREDE.
           PERFORM ABSTELLEN.
           MOVE WT-BEZ TO DE-BEZ.
           DISPLAY DE-KTONR AT 2020.
           WRITE DE-SATZ INVALID DISPLAY "bitte vormerken" AT 2401
               PERFORM WEITER.
           GO H.
       X.  CLOSE ADRESSEN.
       Z.  EXIT.
      *********************************** Key„nderung Karteieintragung *
       KARTZEIL SECTION.
       A.  OPEN I-O KARTEIN.
           OPEN INPUT OLDKART.
           IF WF-STATUS NOT = "00" GO X.
           MOVE 0 TO OL-ADNUM OL-DATUM.
           START OLDKART KEY NOT < OL-KEY INVALID GO X.
       C.  READ OLDKART NEXT AT END GO X.
           MOVE OL-SATZ TO KA-SATZ.
           SUBTRACT KA-ADNUM FROM 1000000 GIVING KA-ADNUM.
           COMPUTE KA-ADNUM = KA-ADNUM * 10 + 2.
           WRITE KA-SATZ.
           DISPLAY KA-ADNUM AT 1720 " " KA-DATUM.
           GO C.
       X.  CLOSE KARTEIN.
       Z.  EXIT.
      ***************************************  hole Adresse lt. PLZL *
       HOL-ADR SECTION.
       A.  IF AD-PLZL = PL-PLZL GO Z.
           MOVE AD-PLZL TO PL-PLZL.
       C.  READ PLZLDAT INVALID KEY MOVE SPACE TO PL-ORT GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           GO Z.
       E.  DISPLAY WH-MCODE with highlight AT 0910.
           DISPLAY "Ortsbezeichnung f. diese PLZL fehlt! Bitte anlegen"
               with BELL AT 2401.
           MOVE PL-ORT TO WT-TX.
           CALL "CAUP" USING "0209210120" WH-CREG.
           IF WOLI MOVE 0 TO PL-PLZL GO Z.
           IF NOT RET GO E.
           MOVE WT-TX TO PL-ORT.
           IF PL-ORT = SPACE GO E.
           DISPLAY PL-ORT with highlight AT 0921.
           IF AD-PLZL > 100 GO J.
       F.  DISPLAY "postal.:" AT 1006.
           DISPLAY "amtl. Plzl. bei verschiedenen Ortsnamen" AT 2301.
           MOVE PL-ALTER TO WH-WERT.
           CALL "CAUP" USING "0010154004" WH-CREG.
           IF WOLI GO E.
           IF NOT RET GO F.
           MOVE WH-WERT TO PL-APLZL.
           DISPLAY PL-APLZL with highlight AT 1015.
           IF PL-ALTER NOT NUMERIC GO F.
       J.  WRITE PL-SATZ INVALID KEY GO Z.
       Z.  EXIT.
      ********************************* bilde alpha-PLZL aus num.Plzl *
       MACH-PLZL SECTION.
       A.  move space to wt-tx.
           move ad-plzl to wh-wert wh-plzl.
           if wh-wert > 209999 move "YU-" to wt-tx(1:3)
               subtract 200000 from wh-wert giving wh-plzl
               move wh-plzl to wt-tx(4:5)
               move 8 to ws go x.
           if wh-wert > 9999 move "D-" to wt-tx(1:2)
               subtract 10000 from wh-wert giving wh-plzl
               move wh-plzl(2:4) to wt-tx(3:4)
      *        add 1 to vdu-rel
               move 6 to ws go x.
           if wh-plzl not = 0 move wh-plzl(2:4) to wt-tx(1:4)
      *        add 3 to vdu-rel
               move 4 to ws.
       x.  move wt-tx to wh-mcode.
       z.  exit.
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
      ***************************************  hole Adresse lt. PLZL *
      *HOL-ADR SECTION.
      *A.  IF AD-PLZL = PL-PLZL GO Z.
      *    MOVE AD-PLZL TO PL-PLZL.
      *C.  READ PLZLDAT INVALID KEY MOVE SPACE TO PL-ORT GO Z.
      *    IF ZUGRIF PERFORM BESETZT GO C.
      *    PERFORM VARYING WX FROM 10 BY -1 UNTIL WX = 1
      *        MOVE PL-ORT TO WT-TX(WX:).
      *Z.  EXIT.
