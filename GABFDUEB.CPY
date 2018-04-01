      *************************************************** Auftragskopf *
       FD  SAUFKOPF     EXTERNAL        LABEL RECORD STANDARD.
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
           03  SAK-ART                  PIC 99        COMP.
           03  SAK-UST                  PIC 99        COMP.
      ******************************************** Auftragspositionen *
       FD  SAUFTRAG      EXTERNAL       LABEL RECORD STANDARD.
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
               05 SAU-DATUM             PIC 9(8)      COMP.
           03  SAU-TX.
               05 SAU-BEZ               PIC X(30)   OCCURS 2.
      *********************************** Daten aus externer Erfassung *
       FD  SDEBITOR      EXTERNAL      LABEL RECORDS STANDARD
                                       RECORD 256 CHARACTERS.
       01  SD-SATZ.
           03  SD-PKEY.
               05 SD-PLZL              PIC X(8).
               05 SD-KEY.
                  07 SD-KTONR          PIC 9(6)     COMP.
           03  SD-MCODE                PIC X(8).
           03  SD-ANREDE               PIC 99       COMP.
           03  SD-KOND                 PIC 9(13)    COMP.
           03  SD-BEZ                  PIC X(110).
           03  SD-TEL                  PIC X(35).
           03  SD-REKTO                PIC 9(6)     COMP.
           03  SD-KARTEI               PIC 99       COMP OCCURS 40.
           03  SD-RES                  PIC 99       COMP.     *> šb-Kz.
           03  SD-UST                  PIC 99       COMP.
           03  SD-UID                  PIC X(15).
      *-------------------> Adressen f. ANGEBOT/FAKT. + KARTEI + FIBU <-
      *
      *    GABKUNDE.DAT = allgem. Adressenkartei f. Faktg. / Kartei
      *
      *    šberleitung in FIBU erfolgt grunds„tzlich bei Erstellung
      *         einer Rechnung (Adresse, Mcode Tel.) in einer
      *         eigenen šberleitungsdatei FIBUEBKD.DAT
      *-----------------------------------------------------------------
      *********************************** Daten aus externer Erfassung *
       FD  SARTIKEL     external       LABEL RECORD STANDARD
                                       RECORD 284 CHARACTERS.
       01  SR-SATZ.
           03  SR-BKEY.
               05 SR-KAGRP             PIC 99         COMP.
               05 SR-KEY.
                  07 SR-NUM            PIC 9(6)       COMP.
           03  SR-AKEY                 PIC X(14).
           03  SR-BEZA                 PIC X(30).
           03  SR-BEZB                 PIC X(30).
           03  SR-URNR                 PIC X(10).
           03  SR-HERST                PIC X(10).
           03  SR-GRP                  PIC 99         COMP.
           03  SR-RAGRP                PIC 99         COMP.
           03  SR-MOGRP                PIC 99         COMP.
           03  SR-MEH                  PIC 99         COMP.
           03  SR-UST                  PIC 99         COMP.
           03  SR-MIN                  PIC S999V9     COMP-3.
      *------------------> Preise & Rabatt des aktuellen Lieferanten <-
           03  SR-TAB.
               05 SR-LIEF              PIC X(5).
               05 SR-DATUM             PIC 9(6)       COMP.
               05 SR-PREIS             PIC S9(7)V99   COMP-3.
               05 SR-RAB               PIC S999V9     COMP-3.
               05 SR-ZRAB              PIC S99V9      COMP-3.
               05 SR-AUF               PIC S999V99    COMP-3.
               05 SR-EKP               PIC S9(7)V99   COMP-3.
               05 SR-VKP               PIC S9(7)V99   COMP-3.
           03  SR-BVKP                 PIC S9(7)V99   COMP-3.
           03  SR-FIXPR                PIC S9(7)V99   COMP-3.
           03  SR-PRTAB       OCCURS 5  INDEXED SPY.
               05 SR-ALIEF             PIC X(5).
               05 SR-ADAT              PIC 9(6)       COMP.
               05 SR-APREIS            PIC S9(7)V99   COMP-3.
               05 SR-ARAB              PIC S99V9      COMP-3.
               05 SR-AZRAB             PIC S99V9      COMP-3.
               05 SR-AAUF              PIC 999        COMP-3.
               05 SR-SYM               PIC 99         COMP.
           03  SR-FOLGE                PIC 9          COMP.
           03  SR-MET                  PIC 99         COMP.
           03  SR-DGW                  PIC 99V99      COMP.
           03  SR-CUBAS                PIC 99V99      COMP.
           03  SR-MIND                 PIC S9(5)V99   COMP-3.
           03  SR-REGAL                PIC XXXX.
           03  SR-STAND                PIC S9(7)V99   COMP-3.
