---------- gabanz.cbl
[472]           IF ESC OR KIST MOVE 0 TO AK-NUM GO Z.
[488]           MOVE 0 TO AK-NUM.
[493]           MOVE AK-NUM TO WD-NUM.

---------- gabauf.cbl
[271]       A.  MOVE 999999 TO AK-NUM.
[272]           START AUFKOPF KEY < AK-NUM INVALID GO X.
[276]           DIVIDE 10000 INTO AK-NUM GIVING WC-TAG.
[277]           IF WC-TAG < WZ-JAHR MULTIPLY WZ-JAHR BY 10000 GIVING AK-NUM.
[278]           ADD 1 TO AK-NUM.
[280]       X.  MOVE 0 TO AK-NUM.
[288]           MOVE AK-NUM TO AU-NUM.
[335]       L.  IF AU-NUM NOT = AK-NUM GO M.
[356]           MOVE AK-NUM TO AU-NUM.
[383]           MOVE AK-NUM TO AU-NUM.
[388]           IF AU-KAP = WH-KAP AND AK-NUM = AU-NUM
[673]           MOVE AK-NUM TO AU-NUM.
[677]           IF AK-NUM NOT = AU-NUM GO R.
[774]               END-IF MOVE WH-NUM TO AK-NUM GO D.
[778]       D.  IF AK-NUM = 0 GO B.
[782]           MOVE AK-NUM TO WD-AUF WH-NUM.
[789]           MOVE WH-NUM TO AK-NUM.
[846]           MOVE AK-NUM TO AU-NUM.
[867]       T.  IF AU-NUM NOT = AK-NUM or AU-KAP NOT = WH-KAP
[869]           MOVE AK-NUM TO AU-NUM.
[1070]           IF AU-NUM NOT = AK-NUM OR AU-POS NOT = WV-POS
[1100]           MOVE AK-NUM TO AU-NUM.
[1170]           IF AU-NUM NOT = AK-NUM OR AU-KAP NOT = WH-KAP GO Q.
[1189]           MOVE AK-NUM TO AU-NUM.
[1199]           IF AU-NUM NOT = AK-NUM GO Z.
[1339]           IF AU-NUM NOT = AK-NUM OR AU-KAP NOT = WV-KAP GO X.

---------- gabrech.cbl
[402]           IF AU-NUM NOT = AK-NUM GO X.

---------- gabdru.cbl
[313]           MOVE AK-NUM TO AU-NUM.
[350]           IF AU-NUM NOT = AK-NUM MOVE 0 TO WV-KAP GO F.
[362]           IF AU-NUM NOT = AK-NUM GO W.
[367]           IF AU-NUM NOT = AK-NUM GO X.
[408]           IF AU-NUM NOT = AK-NUM GO W.
[577]           MOVE AK-NUM TO DRB-RENUM.

---------- gablist.cbl
[466]           MOVE 0 TO AK-NUM.
[467]           START AUFKOPF KEY > AK-NUM INVALID KEY GO Z.
[488]       E.  MOVE AK-NUM TO DRB-NUM.
