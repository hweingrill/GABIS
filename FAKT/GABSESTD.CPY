           SELECT STUNDEN    ASSIGN TO "GABISSTD.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD IS GS-KEY
                             ALTERNATE RECORD GS-AKEY =
                                              GS-NUM GS-DATUM GS-AUF
                             FILE STATUS IS WF-STATUS.
