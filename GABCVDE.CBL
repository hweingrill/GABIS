      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     GABCVDE.
      *********************************************** Programmlogdatei *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY GABSEDEB.CPY.
           SELECT ODEBITOR   ASSIGN TO "DEOLD.DAT"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY ODE-KEY
                             ALTERNATE RECORD KEY ODE-PKEY DUPLICATES
                             ALTERNATE RECORD KEY ODE-MCODE DUPLICATES
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY GABDEBIT.CPY.
      *****************************************************************
       FD  ODEBITOR      EXTERNAL       LABEL RECORDS STANDARD
                                        RECORD 256 CHARACTERS.
       01  ODE-SATZ.
           03  ODE-PKEY.
               05 ODE-PLZL              PIC X(8).
               05 ODE-KEY.
                  07 ODE-KTONR          PIC 9(6)     COMP.
           03  ODE-MCODE                PIC X(8).
           03  ODE-ANREDE               PIC 99       COMP.
           03  ODE-KOND                 PIC 9(13)    COMP.
           03  ODE-BEZ                  PIC X(110).
           03  ODE-TEL                  PIC X(35).
           03  ODE-REKTO                PIC 9(6)     COMP.
           03  ODE-KARTEI               PIC 99       COMP OCCURS 40.
           03  ODE-RES                  PIC 99       COMP.
           03  ODE-UST                  PIC 99       COMP.
           03  ODE-UID                  PIC X(15).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WN-STA                      PIC X(2)    COMP-5.
       01  WH-KART                     PIC 9(5).
       01  WX-LG                       PIC X(4)      COMP-5.
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
      ******************************************************************
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM REORG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       REORG SECTION.
       A.  OPEN INPUT DEBITOR.
           IF WF-STATUS = "00" CLOSE DEBITOR GO Z.
           CALL "CAUP" USING "1301012580" WH-CREG.
           DISPLAY "Konvertierung STRASSEN" AT 0330.
           CALL "CBL_COPY_FILE" USING "GABDEBIT.DAT " "DEOLD.DAT "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
           CALL "CBL_COPY_FILE" USING "GABDEBIT.IDX " "DEOLD.IDX "
                RETURNING WN-STA.
           IF WN-STA not = 0 DISPLAY "Kopieren fehlerhaft"
                                 with highlight AT 2020 stop run.
      *------------------------------------------------> Auftragskopf <-
           OPEN INPUT ODEBITOR.
           DELETE FILE DEBITOR.
           OPEN I-O DEBITOR.
           MOVE LOW-VALUES TO ODE-SATZ.
       C.  READ ODEBITOR NEXT AT END GO X.
           MOVE ODE-SATZ TO DE-SATZ.
      *--------------------------------------------> Adresse zerlegen <-
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(4) TO DE-STR.
           MOVE 25 TO WX-LG.
           CALL "CBL_TOLOWER" USING DE-STR by value WX-LG.
           WRITE DE-SATZ.
           DISPLAY DE-KTONR AT 0530 " " DE-STR.
           GO C.
       X.  CLOSE DEBITOR.
           CLOSE ODEBITOR.
           DELETE FILE ODEBITOR.
       Z.  EXIT.
