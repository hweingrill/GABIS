      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABKONV.
      *********************************************** Programmlogdatei *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY GABSEAUF.CPY.
           SELECT KARTEIN    ASSIGN TO "GABKART.DAT"
                             ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                             RECORD KEY IS KA-KEY,
                             FILE STATUS IS WF-STATUS.
           SELECT OAUFTRAG   ASSIGN TO "GABZ.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD IS OU-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT OAUFKOPF   ASSIGN TO "GABK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY IS OK-KEY
                             ALTERNATE RECORD OK-AKEY  DUPLICATES
                             ALTERNATE RECORD OK-KTONR DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT OKARTEIN   ASSIGN TO "GABKA.DAT"
                             ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                             RECORD KEY IS OKA-KEY,
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABANBOT.CPY.
      *********************************************** Karteibewegungen *
       FD  KARTEIN     external        RECORD IS VARYING IN SIZE
                                       FROM 7 TO 287 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  KA-SATZ.
           03  KA-KEY.
               05 KA-ADNUM             PIC 9(6)     COMP.
               05 KA-DATUM             PIC 9(8)     COMP.
           03  KA-TXT                  PIC X(280).
      *************************************************** Auftragskopf *
       FD  OAUFKOPF     EXTERNAL        LABEL RECORD STANDARD.
       01  OK-SATZ.
           03  OK-KEY.
               05 OK-NUM               PIC 9(8)      COMP.
           03  OK-AKEY                 PIC X(15).
           03  OK-BEZ                  PIC X(110).
           03  OK-DATUM                PIC 9(6)      COMP.
           03  OK-KTONR                PIC 9(6)      COMP.
           03  OK-REKTO                PIC 9(6)      COMP.
           03  OK-ANR                  PIC 99        COMP.
           03  OK-RAB                  PIC S99V9     COMP-3.
           03  OK-KOND                 PIC 9(13)     COMP.
           03  OK-TXKEY                PIC X(14).
           03  OK-VKF                  PIC 99        COMP.
           03  OK-RENUM                PIC 9(6)      COMP.
           03  OK-REDAT                PIC 9(6)      COMP.
           03  OK-SUMMEN.
               05 OK-SUM               PIC S9(7)V99  COMP-3
                                                 OCCURS 17 INDEXED AX.
           03  OK-AUF                  PIC S99V9     COMP-3.
      ******************************************** Auftragspositionen *
       FD  OAUFTRAG      EXTERNAL       LABEL RECORD STANDARD.
       01  OU-SATZ.
           03  OU-KEY.
               05 OU-NUM               PIC 9(8)      COMP.
               05 OU-KAP               PIC 99        COMP.
               05 OU-POS               PIC 9(4)      COMP.
               05 OU-TZ                PIC 99        COMP.
           03  OU-REST.
               05 OU-ART               PIC 99        COMP.
               05 OU-ARNUM             PIC 9(6)      COMP.
               05 OU-MEH               PIC 99        COMP.
               05 OU-ANZ               PIC S9(7)     COMP-3.
               05 OU-PREIS             PIC S9(7)V99  COMP-3.
               05 OU-EKP               PIC S9(7)V99  COMP-3.
               05 OU-BET               PIC S9(7)V99  COMP-3.
               05 OU-RAB               PIC S99V9     COMP-3.
               05 OU-UST               PIC 9         COMP.
               05 OU-MIN               PIC S999V9    COMP-3.
               05 OU-RAGRP             PIC 99        COMP.
               05 OU-MOGRP             PIC 99        COMP.
               05 OU-KAGRP             PIC 99        COMP.
               05 OU-GRP               PIC 99        COMP.
               05 OU-FOLGE             PIC 99        COMP.
               05 OU-DATUM             PIC XX.
           03  OU-TX.
               05 OU-BEZ               PIC X(30)   OCCURS 2.
      *********************************************** Karteibewegungen *
       FD  OKARTEIN     external       RECORD IS VARYING IN SIZE
                                       FROM 7 TO 286 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  OKA-SATZ.
           03  OKA-KEY.
               05 OKA-ADNUM             PIC 9(6)     COMP.
               05 OKA-DATUM             PIC 9(6)     COMP.
           03  OKA-TXT                  PIC X(280).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WN-STA                      PIC X(2)    COMP-5.
       01  WH-KART                     PIC 9(5).
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
      ******************************************************************
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM REORG.
      *    PERFORM KARTREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       REORG SECTION.
       A.  OPEN INPUT AUFKOPF.
           IF WF-STATUS = "00" CLOSE AUFKOPF GO Z.
           CALL "CAUP" USING "1301012580" WH-CREG.
           DISPLAY "Konvertierung fr 2000" AT 0330.
           CALL "CBL_COPY_FILE" USING "GABIKOPF.DAT " "GABK.DAT "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
           CALL "CBL_COPY_FILE" USING "GABIKOPF.IDX " "GABK.IDX "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
           CALL "CBL_COPY_FILE" USING "GABISAUF.DAT " "GABZ.DAT "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
           CALL "CBL_COPY_FILE" USING "GABISAUF.IDX " "GABZ.IDX "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
      *------------------------------------------------> Auftragskopf <-
           OPEN INPUT OAUFKOPF.
           DELETE FILE AUFKOPF.
           OPEN I-O AUFKOPF.
           MOVE LOW-VALUES TO OK-SATZ.
       C.  READ OAUFKOPF NEXT AT END GO G.
           MOVE OK-SATZ TO AK-SATZ.
           MOVE 0 TO AK-ART.
           WRITE AK-SATZ.
           DISPLAY AK-NUM AT 0530.
           GO C.
      *----------------------------------------------> Auftragszeilen <-
       G.  OPEN INPUT OAUFTRAG.
           DELETE FILE AUFTRAG.
           OPEN I-O AUFTRAG.
           MOVE LOW-VALUES TO OK-SATZ.
       I.  READ OAUFTRAG NEXT AT END GO X.
           MOVE OU-SATZ TO AU-SATZ.
           MOVE OU-TX TO AU-TX.
           MOVE 0 TO AU-DATUM.
           WRITE AU-SATZ.
           DISPLAY AU-NUM AT 0630.
           GO I.
       X.  CLOSE AUFTRAG AUFKOPF.
           CLOSE OAUFTRAG OAUFKOPF.
           DELETE FILE OAUFTRAG.
           DELETE FILE OAUFKOPF.
       Z.  EXIT.
      ******************************************************************
       KARTREG SECTION.
       A.  OPEN INPUT KARTEIN.
           IF WF-STATUS = "00" CLOSE KARTEIN GO Z.
           CALL "CAUP" USING "1301012580" WH-CREG.
           DISPLAY "Konvertierung fr 2000" AT 0330.
           CALL "CBL_COPY_FILE" USING "GABKART.DAT " "GABKA.DAT "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
           CALL "CBL_COPY_FILE" USING "GABKART.IDX " "GABKA.IDX "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
      *-------------------------------------------> Karteineintragung <-
           OPEN INPUT OKARTEIN.
           DELETE FILE KARTEIN.
           OPEN I-O KARTEIN.
           MOVE LOW-VALUES TO OK-SATZ.
       C.  READ OKARTEIN NEXT AT END GO X.
           MOVE OKA-ADNUM TO KA-ADNUM.
           ADD OKA-DATUM 19000000 GIVING KA-DATUM.
           MOVE OKA-TXT TO KA-TXT.
           ADD 1 TO WH-KART.
           WRITE KA-SATZ.
           DISPLAY KA-ADNUM AT 0730.
           GO C.
       X.  CLOSE KARTEIN.
           CLOSE OKARTEIN.
           DELETE FILE OKARTEIN.
       Z.  EXIT.
