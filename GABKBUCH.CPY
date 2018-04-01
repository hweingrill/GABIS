      *************************** Buchsaetze f. Fibuueberleitung FAKT *
       FD  BUCHSATZ     EXTERNAL       RECORD 86 CHARACTERS
                                       LABEL RECORD STANDARD.
       01  BU-SATZ.
           03  FILLER                  PIC XX.
           03  BU-FN                   PIC 999        COMP-3.
           03  BU-KTO                  PIC 9(6)       COMP-3.
           03  BU-BUD                  PIC 9(5)       COMP-3.
           03  BU-RED                  PIC 9(6)       COMP-3.
           03  BU-SY                   PIC XX.
           03  BU-GK                   PIC 9(6)       COMP-3.
           03  BU-REN                  PIC 9(5)       COMP-3.
           03  BU-ZK                   PIC 9.
           03  BU-ZT                   PIC 999        COMP-3.
           03  BU-Z                    PIC 9.
           03  BU-RF                   PIC 9(5)       COMP-3.
           03  BU-U1                   PIC 9          COMP-3.
           03  BU-U2                   PIC 9          COMP-3.
           03  BU-MW                   PIC S9(7)V99   COMP-3.
           03  BU-B1                   PIC S9(9)V99   COMP-3.
           03  BU-B2                   PIC S9(9)V99   COMP-3.
           03  BU-SH                   PIC 9          COMP-3.
           03  BU-BET                  PIC S9(9)V99   COMP-3.
           03  BU-KST                  PIC 9(5)       COMP-3.
           03  BU-TX                   PIC X(10).
           03  BU-FWK                  PIC 9.
           03  BU-FKU                  PIC S9999V9999 COMP-3.
           03  BU-FWB                  PIC S9(11)V99  COMP-3.
           03  FILLER                  PIC XXX.
      ************************************ Unternehmerstammdaten FIBU *
       FD  KENNSATZ      EXTERNAL        RECORD 128 CHARACTERS
                                       LABEL RECORD STANDARD.
       01  KE-SATZ.
           03  KE-KEY.
               05 KE-FNR               PIC 9(3) COMP-3.
               05 KE-RKA               PIC 9(5) COMP-3.
           03  KE-INHALT.
               05 FILLER               PIC 9(5) COMP-3.
               05 KE-LREC              PIC 9(5) COMP-3.
               05 FILLER               PIC X(115).
      *-----> SATZ 6-Zahlungskonsitionen DEBITOREN fuer Firma 1 - 999 <-
           03  K06-SATZ REDEFINES KE-INHALT.
               05 KE-TABK.
                  07 FILLER            PIC 9(13) OCCURS 9 COMP-3.
               05 FILLER               PIC X(58).
           03  FILLER                  PIC XX.
