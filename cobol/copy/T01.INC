            02  T01.
              03  T01-SKYYM.
                04  T01-VALUE   PIC X(7).
              03  T01-B06-STATE   PIC S9(9)   BINARY.
              03  T01-B07-STATE   PIC S9(9)   BINARY.
              03  T01-B08-STATE   PIC S9(9)   BINARY.
              03  T01-B09-STATE   PIC S9(9)   BINARY.
              03  T01-B10-STATE   PIC S9(9)   BINARY.
              03  T01-B15-STATE   PIC S9(9)   BINARY.
              03  T01-B16-STATE   PIC S9(9)   BINARY.
              03  T01-B19-STATE   PIC S9(9)   BINARY.
              03  T01-B20-STATE   PIC S9(9)   BINARY.
              03  T01-B21-STATE   PIC S9(9)   BINARY.
              03  T01-B22-STATE   PIC S9(9)   BINARY.
              03  T01-SRYYM-STATE   PIC S9(9)   BINARY.
              03  T01-SRYYM-VALUE   PIC X(20).
              03  T01-SKYYMD.
                04  T01-VALUE   PIC X(10).
              03  T01-HKNJANUM.
                04  T01-VALUE   PIC X(8).
              03  T01-HKNJANUM1-STATE   PIC S9(9)   BINARY.
              03  T01-HKNJANUM1.
                04  T01-VALUE   PIC X(8).
              03  T01-PRTKBN-COMBO.
                04  T01-PRTKBN-STATE PIC S9(9)   BINARY.
                04  T01-PRTKBN.
                  05  T01-VALUE    PIC X(32).
                04  T01-PRTKBN-COUNT PIC S9(9)   BINARY.
                04  T01-PRTKBN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
              03  T01-DATA-COMBO.
                04  T01-DATA-STATE PIC S9(9)   BINARY.
                04  T01-DATA.
                  05  T01-1VALUE    PIC X(38).
                04  T01-DATA-COUNT PIC S9(9)   BINARY.
                04  T01-DATA-ITEM
                        PIC X(38)   OCCURS  6 TIMES.
              03  T01-NYUGAIKBN-COMBO.
                04  T01-NYUGAIKBN-STATE PIC S9(9)   BINARY.
                04  T01-NYUGAIKBN.
                  05  T01-VALUE    PIC X(14).
                04  T01-NYUGAIKBN-COUNT PIC S9(9)   BINARY.
                04  T01-NYUGAIKBN-ITEM
                        PIC X(14)   OCCURS  3 TIMES.
              03  T01-DATAKBN4-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN4-VALUE   PIC X.
              03  T01-DATAKBN3-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN3-VALUE   PIC X.
              03  T01-DATAKBN6-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN6-VALUE   PIC X.
              03  T01-DATAKBN7-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN7-VALUE   PIC X.
              03  T01-HKNJAMEI.
                04  T01-VALUE   PIC X(50).
              03  T01-HKNJAMEI1.
                04  T01-VALUE   PIC X(50).
              03  T01-CHK11-STATE   PIC S9(9)   BINARY.
              03  T01-CHK11-VALUE   PIC X.
              03  T01-CHK12-STATE   PIC S9(9)   BINARY.
              03  T01-CHK12-VALUE   PIC X.
              03  T01-CHK21-STATE   PIC S9(9)   BINARY.
              03  T01-CHK21-VALUE   PIC X.
              03  T01-CHK22-STATE   PIC S9(9)   BINARY.
              03  T01-CHK22-VALUE   PIC X.
              03  T01-CHK31-STATE   PIC S9(9)   BINARY.
              03  T01-CHK31-VALUE   PIC X.
              03  T01-CHK32-STATE   PIC S9(9)   BINARY.
              03  T01-CHK32-VALUE   PIC X.
              03  T01-DATAKBN5-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN5-VALUE   PIC X.
              03  T01-DATAKBN1-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN1-VALUE   PIC X.
              03  T01-DATAKBN2-STATE   PIC S9(9)   BINARY.
              03  T01-DATAKBN2-VALUE   PIC X.
              03  T01-PRTKBN1-STYLE   PIC X(20).
              03  T01-PRTKBN1-VALUE   PIC X(4).
              03  T01-PRTKBN2-STYLE   PIC X(20).
              03  T01-PRTKBN2-VALUE   PIC X(4).
              03  T01-PRTKBN3-STYLE   PIC X(20).
              03  T01-PRTKBN3-VALUE   PIC X(4).
              03  T01-PRTKBN4-STYLE   PIC X(20).
              03  T01-PRTKBN4-VALUE   PIC X(4).
              03  T01-PRTKBN5-STYLE   PIC X(20).
              03  T01-PRTKBN5-VALUE   PIC X(4).
              03  T01-TITLE-STYLE     PIC X(20).
              03  T01-TITLE-VALUE     PIC X(110).
              03  T01-TITLE1-STYLE    PIC X(20).
              03  T01-TITLE1-VALUE    PIC X(90).
