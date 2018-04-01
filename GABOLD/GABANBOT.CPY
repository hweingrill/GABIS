      *************************************************** Auftragskopf *
       FD  AUFKOPF     EXTERNAL        LABEL RECORD STANDARD.
       01  AK-SATZ.
           03  AK-KEY.
               05 AK-NUM               PIC 9(8)      COMP.
           03  AK-AKEY                 PIC X(15).
           03  AK-BEZ                  PIC X(110).
           03  AK-DATUM                PIC 9(6)      COMP.
           03  AK-KTONR                PIC 9(6)      COMP.
           03  AK-REKTO                PIC 9(6)      COMP.
           03  AK-ANR                  PIC 99        COMP.
           03  AK-RAB                  PIC S99V9     COMP-3.
           03  AK-KOND                 PIC 9(13)     COMP.
           03  AK-TXKEY                PIC X(14).
           03  AK-VKF                  PIC 99        COMP.
           03  AK-RENUM                PIC 9(6)      COMP.
           03  AK-REDAT                PIC 9(6)      COMP.
           03  AK-SUMMEN.
               05 AK-SUM               PIC S9(7)V99  COMP-3
                                                 OCCURS 17 INDEXED AX.
           03  AK-AUF                  PIC S99V9     COMP-3.
           03  AK-ART                  PIC 99        COMP.
           03  AK-UST                  PIC 99        COMP.
      ******************************************** Auftragspositionen *
       FD  AUFTRAG      EXTERNAL       LABEL RECORD STANDARD.
       01  AU-SATZ.
           03  AU-KEY.
               05 AU-NUM               PIC 9(8)      COMP.
               05 AU-KAP               PIC 99        COMP.
               05 AU-POS               PIC 9(4)      COMP.
               05 AU-TZ                PIC 99        COMP.
           03  AU-REST.
               05 AU-ART               PIC 99        COMP.
               05 AU-ARNUM             PIC 9(6)      COMP.
               05 AU-MEH               PIC 99        COMP.
               05 AU-ANZ               PIC S9(7)     COMP-3.
               05 AU-PREIS             PIC S9(7)V99  COMP-3.
               05 AU-EKP               PIC S9(7)V99  COMP-3.
               05 AU-BET               PIC S9(7)V99  COMP-3.
               05 AU-RAB               PIC S99V9     COMP-3.
               05 AU-UST               PIC 9         COMP.
               05 AU-MIN               PIC S999V9    COMP-3.
               05 AU-RAGRP             PIC 99        COMP.
               05 AU-MOGRP             PIC 99        COMP.
               05 AU-KAGRP             PIC 99        COMP.
               05 AU-GRP               PIC 99        COMP.
               05 AU-FOLGE             PIC 99        COMP.
               05 AU-DATUM             PIC 9(8)      COMP.
           03  AU-TX.
               05 AU-BEZ               PIC X(30)   OCCURS 2.
