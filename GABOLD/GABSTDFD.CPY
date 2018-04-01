      *************************************************** Auftragskopf *
       FD  STUNDEN       external       LABEL RECORD STANDARD.
       01  GS-SATZ.
           03  GS-KEY.
               05 GS-NUM                PIC 9(3)      COMP.
               05 GS-AUF                PIC 9(8)      COMP.
           03  GS-DATUM                 PIC 9(8)      COMP.
           03  GS-ANZ                   PIC S999V99   COMP.
