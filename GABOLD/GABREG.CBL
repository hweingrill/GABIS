      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GABREG.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEDEB.CPY.
           SELECT FDEBITOR ASSIGN TO WN-DEB
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY FDE-KEY
                           ALTERNATE KEY FDE-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
      ******************************************************************
       FD  FDEBITOR      external       LABEL RECORDS STANDARD.
       01  FDE-SATZ.
           03  FDE-KEY.
               05 FDE-KTONR             PIC 9(6)      COMP.
           03  FDE-MCODE                PIC X(8).
           03  FDE-KTOART               PIC 99        COMP.
           03  FDE-OPKZ                 PIC 99        COMP.
           03  FDE-ANREDE               PIC 99        COMP.
           03  FDE-SOLL                 PIC S9(9)V99  COMP-3.
           03  FDE-HABEN                PIC S9(9)V99  COMP-3.
           03  FDE-UMSATZ               PIC S9(9)V99  COMP-3.
           03  FDE-KOND                 PIC 9(13)     COMP.
           03  FDE-FWKZ                 PIC 99        COMP.
           03  FDE-MAHNT                PIC 999       COMP.
           03  FDE-MAHNDAT              PIC 9(6)      COMP.
           03  FDE-BEZ                  PIC X(110).
           03  FDE-TEL                  PIC X(35).
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
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FDEBITOR.
       A.  CALL "CADECL" USING "FIBUDEBI.DAT"  WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "DEBITORE.FII" WH-CREG.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WN-FKON TO WN-DEB.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WN-DEB(wx:1) = "\"
               CONTINUE.
           ADD 1 TO WX.
           MOVE "FIBUDEBI.DAT" TO WN-DEB(WX:).
           DISPLAY WN-DEB AT 2020.
           PERFORM REORG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       REORG SECTION.
       A.  OPEN I-O FDEBITOR.
           MOVE 0 TO FDE-KTONR.
           CALL "CAUP" USING "1303012580" WH-CREG.
           START FDEBITOR KEY NOT < FDE-KEY INVALID GO X.
       C.  READ FDEBITOR NEXT AT END GO X.
           IF ZUGRIF GO C.
           MOVE FDE-KTONR TO DE-KTONR.
       E.  READ DEBITOR NO LOCK INVALID GO C.
           IF ZUGRIF GO E.
           IF DE-MCODE = FDE-MCODE GO C.
           DISPLAY "Fakt: " AT 1501 DE-KTONR " "
               DE-BEZ(1:60) with highlight.
           DISPLAY "Fibu: " AT 1601 FDE-KTONR " "
               FDE-BEZ(1:60) with highlight.
           DISPLAY "<ret>= Åbernehmen, <#>= Åberspringen < >" AT 2301.
       G.  CALL "CAUP" USING "0023390000" WH-CREG.
           IF ESC GO X.
           IF KIST GO C.
           IF NOT RET GO G.
           MOVE DE-MCODE TO FDE-MCODE.
           REWRITE FDE-SATZ.
           GO C.
       X.  CLOSE FDEBITOR.
       Z.  EXIT PROGRAM.
