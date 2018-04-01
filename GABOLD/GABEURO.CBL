      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABEURO.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEART.CPY.
           COPY GABSEAUF.CPY.
       DATA DIVISION.
       FILE SECTION.
           COPY GABARTIK.CPY.
           COPY GABANBOT.CPY.
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
           03  WH-PX                   PIC XX               OCCURS 2.
           03  WT-KTONR                PIC 9(6)      COMP   OCCURS 15.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WK-BUKEY                PIC 9(6)      COMP.
           03  WH-SEC                  PIC 9(14)     COMP-X.
           03  WH-SYMT                 PIC 9999.
           03  WR-Y REDEFINES WH-SYMT.
               05 WH-SH                PIC 9.
               05 WH-UST               PIC 9.
               05 WH-OP                PIC 9.
               05 WH-DIV               PIC 9.
           03  WN-DEB                  PIC X(30).
       COPY GABEXT.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "GABISART.DAT" WH-CREG.
       DECL-Z SECTION.         USE AFTER ERROR PROCEDURE ON ZUSATZ.
       A.  CALL "CADECL" USING "GABISARZ.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON AUFTRAG.
       A.  CALL "CADECL" USING "GABISAUF.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON AUFKOPF.
       A.  CALL "CADECL" USING "GABIKOPF.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACES UPON CRT.
           CALL "CAUP" USING "9901012480000" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE "  Euro-Konvtg. " TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Artikel...:" AT 0520.
           DISPLAY "Auftr„ge..:" AT 0620.
           OPEN I-O ARTIKEL AUFTRAG AUFKOPF.
           MOVE LOW-VALUES TO AR-SATZ.
           START ARTIKEL KEY not < AR-KEY INVALID STOP RUN.
       C.  READ ARTIKEL NEXT AT END GO E.
           COMPUTE AR-PREIS rounded = AR-PREIS / 13,7603.
           COMPUTE AR-EKP rounded = AR-EKP / 13,7603.
           COMPUTE AR-VKP rounded = AR-VKP / 13,7603.
           COMPUTE AR-BVKP rounded = AR-BVKP / 13,7603.
           COMPUTE AR-FIXPR rounded = AR-FIXPR / 13,7603.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               COMPUTE AR-APREIS(WX) rounded = AR-APREIS(WX) / 13,7603.
           REWRITE AR-SATZ.
           DISPLAY AR-NUM AT 0532.
           GO C.
      *----------------------------------------------------> Auftr„ge <-
       E.  MOVE LOW-VALUE TO AK-SATZ.
           START AUFKOPF KEY not < AK-KEY INVALID STOP RUN.
       F.  READ AUFKOPF NEXT AT END GO K.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 17
               COMPUTE AK-SUM(WX) rounded = AK-SUM(WX) / 13,7603.
           REWRITE AK-SATZ.
           DISPLAY AK-NUM AT 0632.
           GO F.
       K.  MOVE LOW-VALUE TO AU-SATZ.
           START AUFTRAG KEY not < AU-KEY INVALID STOP RUN.
       L.  READ AUFTRAG NEXT AT END GO Z.
           COMPUTE AU-PREIS rounded = AU-PREIS / 13,7603.
           COMPUTE AU-EKP rounded = AU-EKP / 13,7603.
           COMPUTE AU-BET rounded = AU-BET / 13,7603.

           REWRITE AU-SATZ.
           DISPLAY AU-NUM AT 0632.
           GO L.
       Z.  STOP RUN.
