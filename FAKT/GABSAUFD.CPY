      *************************************************** Auftragskopf *
       FD  SAUFKOPF     EXTERNAL       LABEL RECORD STANDARD.
       01  SAK-SATZ.
           03  SAK-KEY.
               05 SAK-NUM               PIC 9(8)      COMP.
           03  SAK-AKEY                 PIC X(15).
           03  SAK-BEZ                  PIC X(110).
           03  SAK-DATUM                PIC 9(6)      COMP.
           03  SAK-KTONR                PIC 9(6)      COMP.
           03  SAK-REKTO                PIC 9(6)      COMP.
           03  SAK-ANR                  PIC 99        COMP.
           03  SAK-RAB                  PIC S99V9     COMP-3.
           03  SAK-KOND                 PIC 9(13)     COMP.
           03  SAK-TXKEY                PIC X(14).
           03  SAK-VKF                  PIC 99        COMP.
           03  SAK-RENUM                PIC 9(6)      COMP.
           03  SAK-REDAT                PIC 9(6)      COMP.
           03  SAK-SUMMEN.
               05 SAK-SUM               PIC S9(7)V99  COMP-3
                                                 OCCURS 17 INDEXED SAX.
           03  SAK-AUF                  PIC S99V9     COMP-3.
      ******************************************** Auftragspositionen *
       FD  SAUFTRAG      EXTERNAL      LABEL RECORD STANDARD.
       01  SAU-SATZ.
           03  SAU-KEY.
               05 SAU-NUM               PIC 9(8)      COMP.
               05 SAU-KAP               PIC 99        COMP.
               05 SAU-POS               PIC 9(4)      COMP.
               05 SAU-TZ                PIC 99        COMP.
           03  SAU-REST.
               05 SAU-ART               PIC 99        COMP.
               05 SAU-ARNUM             PIC 9(6)      COMP.
               05 SAU-MEH               PIC 99        COMP.
               05 SAU-ANZ               PIC S9(7)     COMP-3.
               05 SAU-PREIS             PIC S9(7)V99  COMP-3.
               05 SAU-EKP               PIC S9(7)V99  COMP-3.
               05 SAU-BET               PIC S9(7)V99  COMP-3.
               05 SAU-RAB               PIC S99V9     COMP-3.
               05 SAU-UST               PIC 9         COMP.
               05 SAU-MIN               PIC S999V9    COMP-3.
               05 SAU-RAGRP             PIC 99        COMP.
               05 SAU-MOGRP             PIC 99        COMP.
               05 SAU-KAGRP             PIC 99        COMP.
               05 SAU-GRP               PIC 99        COMP.
               05 SAU-FOLGE             PIC 99        COMP.
               05 FILLER               PIC XX.
           03  SAU-TX.
               05 SAU-BEZ               PIC X(30)   OCCURS 2.
