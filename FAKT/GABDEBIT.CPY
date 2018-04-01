      *****************************************************************
       FD  DEBITOR      EXTERNAL       LABEL RECORDS STANDARD
                                       RECORD 256 CHARACTERS.
       01  DE-SATZ.
           03  DE-PKEY.
               05 DE-PLZL              PIC X(8).
               05 DE-KEY.
                  07 DE-KTONR          PIC 9(6)     COMP.
           03  DE-MCODE                PIC X(8).
           03  DE-ANREDE               PIC 99       COMP.
           03  DE-KOND                 PIC 9(13)    COMP.
           03  DE-BEZ                  PIC X(110).
           03  DE-TEL                  PIC X(35).
           03  DE-REKTO                PIC 9(6)     COMP.
           03  DE-KARTEI               PIC 99       COMP OCCURS 40.
           03  DE-RES                  PIC 99       COMP.
           03  DE-UST                  PIC 99       COMP.
           03  DE-UID                  PIC X(15).
      *-------------------> Adressen f. ANGEBOT/FAKT. + KARTEI + FIBU <-
      *
      *    GABKUNDE.DAT = allgem. Adressenkartei f. Faktg. / Kartei
      *
      *    šberleitung in FIBU erfolgt grunds„tzlich bei Erstellung
      *         einer Rechnung (Adresse, Mcode Tel.) in einer
      *         eigenen šberleitungsdatei FIBUEBKD.DAT
      *-----------------------------------------------------------------
