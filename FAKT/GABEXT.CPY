       01  WH-GABEXT      external.
           03  WH-TXKEY                PIC X(20).
           03  WK-TXKEY                PIC 9(5).
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
           03  WN-FKON                 PIC X(35).
           03  WN-BUEB                 PIC X(35).
           03  WE-ARSYM                PIC 99        COMP.
           03  WE-GSSYM                PIC 99        COMP.
           03  WE-TRSYM                PIC 99        COMP.
           03  WE-SRSYM                PIC 99        COMP.
           03  WH-DRUNAM               PIC X(15).
           03  WH-ABLNAM               PIC X(15).
           03  WH-MODAT                PIC 9(8)      COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WM-KOPF                 PIC 99        COMP.
           03  WM-WO                   PIC 99        COMP.
           03  WE-PARAM       OCCURS 10.
               05 WE-DRU               PIC 99      COMP.
               05 WE-KOPF              PIC 99      COMP.
               05 WE-WO                PIC 99      COMP.
           03  WE-STG                  PIC X(40)   OCCURS 12.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
