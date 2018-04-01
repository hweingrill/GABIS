           SELECT SAUFTRAG   ASSIGN TO "R:GABISAUF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD IS SAU-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT SAUFKOPF   ASSIGN TO "R:GABIKOPF.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY IS SAK-KEY
                             ALTERNATE RECORD SAK-AKEY  DUPLICATES
                             ALTERNATE RECORD SAK-KTONR DUPLICATES
                             ALTERNATE RECORD SAK-AUFART =
                                       SAK-ART SAK-DATUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT SDEBITOR   ASSIGN TO "R:GABDEBIT.DAT"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY SD-KEY
                             ALTERNATE RECORD KEY SD-PKEY DUPLICATES
                             ALTERNATE RECORD KEY SD-MCODE DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT SARTIKEL   ASSIGN TO "R:GABISART.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SR-KEY
                             ALTERNATE RECORD SR-AKEY DUPLICATES
                             ALTERNATE RECORD SR-BKEY DUPLICATES
                             ALTERNATE RECORD SR-CKEY =
                                              SR-RAGRP SR-NUM
                             FILE STATUS IS WF-STATUS.
