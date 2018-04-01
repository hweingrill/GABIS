      ********************************************** Artikelstatistik *
       FD  STATISTIK    EXTERNAL       RECORD 108 CHARACTERS
                                       LABEL RECORD STANDARD.
       01  ST-SATZ.
           03  ST-KEY.
               05 ST-ARNUM             PIC 9(6)       COMP.
           03  ST-JAHR                 PIC 99         COMP-X.
           03  ST-MENGEN               PIC S9(7)      COMP-3
                                            OCCURS 24 INDEXED SY.
           03  ST-REST                 PIC X(8).
