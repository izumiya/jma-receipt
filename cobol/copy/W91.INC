        02  W91.
            03  W91-TITLE   PIC X(256).
            03  W91-POPUP-SUMMARY   PIC X(256).
            03  W91-POPUP-BODY  PIC X(1024).
            03  W91-POPUP-ICON  PIC X(256).
            03  W91-POPUP-TIMEOUT   PIC S9(9)   BINARY.
            03  W91-FIXED1.
              04  W91-FILEDIR.
                05  W91-VALUE   PIC X(200).
              04  W91-COOPBASE-COMB.
                05  W91-COOPBASE.
                  07  W91-VALUE   PIC X(20).
                05  W91-COOPBASE-COUNT   PIC S9(9)   BINARY.
                05  W91-COOPBASE-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
