      $SET LINKCOUNT "192" ANS85 NOBOUND AUTOLOCK NESTCALL NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        KART.
      ******************************************************************
      *  -- Adressenverwaltung -- direktes  Aufrufprogramm in ADR      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      *****************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       STEUER SECTION.
       A.  DISPLAY SPACE UPON CRT.
           CALL "CAUP" USING "990101248000000" WH-CREG.
           CALL "gabkart" USING "KART" WH-CREG.
           CALL "CAUP" USING "1301012480000" WH-CREG.
           DISPLAY " Programm beendet " AT 1231.
       Z.  STOP RUN.
