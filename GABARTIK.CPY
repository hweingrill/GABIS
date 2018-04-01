      *****************************************************************
       FD  ARTIKEL     external        LABEL RECORD STANDARD
                                       RECORD 284 CHARACTERS.
       01  AR-SATZ.
           03  AR-BKEY.
               05 AR-KAGRP             PIC 99         COMP.
               05 AR-KEY.
                  07 AR-NUM            PIC 9(6)       COMP.
           03  AR-AKEY                 PIC X(14).
           03  AR-BEZA                 PIC X(30).
           03  AR-BEZB                 PIC X(30).
           03  AR-URNR                 PIC X(10).
           03  AR-HERST                PIC X(10).
           03  AR-GRP                  PIC 99         COMP.
           03  AR-RAGRP                PIC 99         COMP.
           03  AR-MOGRP                PIC 99         COMP.
           03  AR-MEH                  PIC 99         COMP.
           03  AR-UST                  PIC 99         COMP.
           03  AR-MIN                  PIC S999V9     COMP-3.
      *------------------> Preise & Rabatt des aktuellen Lieferanten <-
           03  AR-TAB.
               05 AR-LIEF              PIC X(5).
               05 AR-DATUM             PIC 9(6)       COMP.
               05 AR-PREIS             PIC S9(7)V99   COMP-3.
               05 AR-RAB               PIC S999V9     COMP-3.
               05 AR-ZRAB              PIC S99V9      COMP-3.
               05 AR-AUF               PIC S999V99    COMP-3.
               05 AR-EKP               PIC S9(7)V99   COMP-3.
               05 AR-VKP               PIC S9(7)V99   COMP-3.
           03  AR-BVKP                 PIC S9(7)V99   COMP-3.
           03  AR-FIXPR                PIC S9(7)V99   COMP-3.
           03  AR-PRTAB       OCCURS 5  INDEXED PY.
               05 AR-ALIEF             PIC X(5).
               05 AR-ADAT              PIC 9(6)       COMP.
               05 AR-APREIS            PIC S9(7)V99   COMP-3.
               05 AR-ARAB              PIC S99V9      COMP-3.
               05 AR-AZRAB             PIC S99V9      COMP-3.
               05 AR-AAUF              PIC 999        COMP-3.
               05 AR-SYM               PIC 99         COMP.
           03  AR-FOLGE                PIC 9          COMP.
           03  AR-MET                  PIC 99         COMP.
           03  AR-DGW                  PIC 99V99      COMP.
           03  AR-CUBAS                PIC 99V99      COMP.
           03  AR-MIND                 PIC S9(5)V99   COMP-3.
           03  AR-REGAL                PIC XXXX.
           03  AR-STAND                PIC S9(7)V99   COMP-3.
      **************************************************** Zusatztexte *
       FD  ZUSATZ       external       LABEL RECORD STANDARD.
       01  ZU-SATZ.
           03  ZU-KEY.
               05 ZU-NUM               PIC 9(6)       COMP.
           03  ZU-TX.
               05 ZU-BEZ               PIC X(40)   OCCURS 15.

