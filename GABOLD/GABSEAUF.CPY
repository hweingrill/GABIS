           SELECT AUFTRAG    ASSIGN TO "GABISAUF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD IS AU-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT AUFKOPF    ASSIGN TO "GABIKOPF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY IS AK-KEY
                             ALTERNATE RECORD AK-AKEY  DUPLICATES
                             ALTERNATE RECORD AK-KTONR DUPLICATES
                             ALTERNATE RECORD AK-AUFART =
                                       AK-ART AK-DATUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
