      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NESTCALL
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       GABIS.
      ******************************************************************
      *       GewerbeABrechnungs- u. InformationsSystem                *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WT-CALL                     PIC X(8).
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACES UPON CRT.
           CALL "CAUP" USING "9901012480000" WH-CREG.
       B.  CALL "GABVORL" USING "99VORLAUF" WH-CREG.
           CANCEL "GABVORL".
       C.  IF WH-PG = 99 GO B.
           MOVE WT-TX TO WT-CALL.
           IF WT-TX NOT = SPACE CALL WT-CALL USING WT-TX(9:15) WH-CREG
                                CANCEL WT-CALL.
           IF WH-PG = 49 GO C.
           MOVE 9 TO WH-PG.
           GO B.
       Z.  EXIT.
