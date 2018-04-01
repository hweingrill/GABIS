           SELECT ARTIKEL    ASSIGN TO "GABISART.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AR-KEY
                             ALTERNATE RECORD AR-AKEY DUPLICATES
                             ALTERNATE RECORD AR-BKEY DUPLICATES
                             ALTERNATE RECORD AR-CKEY =
                                              AR-RAGRP AR-NUM
                             FILE STATUS IS WF-STATUS.
           SELECT ZUSATZ     ASSIGN TO "GABISARZ.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ZU-KEY
                             FILE STATUS IS WF-STATUS.
