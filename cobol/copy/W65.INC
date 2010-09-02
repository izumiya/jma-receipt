       02  W65.
      *     02  W65-FIXED6.
              03  W65-B06-STATE            PIC S9(9)   BINARY.
              03  W65-B07-STATE            PIC S9(9)   BINARY.
              03  W65-B08-STATE            PIC S9(9)   BINARY.
              03  W65-KJNYMD               PIC X(10).
              03  W65-SELNUM1              PIC X(50).
              03  W65-KOHNUM-G.
                04  W65-KOHNUM             PIC X(3).
                04  W65-KOHNUM-STATE       PIC S9(9)   BINARY.
              03  W65-KOHNAME-G.
                04  W65-KOHNAME            PIC X(50).
                04  W65-KOHNAME-STATE       PIC S9(9)   BINARY.
              03  W65-CMB-JNISSUKBN.
                04  W65-JNISSUKBN          PIC X(80).
                04  W65-JNISSUKBN-COUNT    PIC S9(9)   BINARY.
                04  W65-JNISSUKBN-ITEM     PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-JNISSUTAIKBN.
                04  W65-JNISSUTAIKBN       PIC X(30).
                04  W65-JNISSUTAIKBN-STATE PIC S9(9)   BINARY.
                04  W65-JNISSUTAIKBN-COUNT PIC S9(9)   BINARY.
                04  W65-JNISSUTAIKBN-ITEM  PIC X(30)   OCCURS  10 TIMES.
                04  W65-JNISSUTAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-TOTALTENKBN.
                04  W65-TOTALTENKBN        PIC X(80).
                04  W65-TOTALTENKBN-COUNT  PIC S9(9)   BINARY.
                04  W65-TOTALTENKBN-ITEM   PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-TOTALTENTAIKBN.
                04  W65-TOTALTENTAIKBN     PIC X(30).
                04  W65-TOTALTENTAIKBN-STATE PIC S9(9)   BINARY.
                04  W65-TOTALTENTAIKBN-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-TOTALTENTAIKBN-ITEM
                                           PIC X(30)   OCCURS  10 TIMES.
                04  W65-TOTALTENTAIKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  W65-CMB-SYUKEIKBN.
                04  W65-SYUKEIKBN             PIC X(80).
                04  W65-SYUKEIKBN-COUNT  PIC S9(9)   BINARY.
                04  W65-SYUKEIKBN-ITEM   PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-SYUKEITAIKBN.
                04  W65-SYUKEITAIKBN             PIC X(30).
                04  W65-SYUKEITAIKBN-STATE  PIC S9(9)  BINARY.
                04  W65-SYUKEITAIKBN-COUNT  PIC S9(9)  BINARY.
                04  W65-SYUKEITAIKBN-ITEM   PIC X(30)  OCCURS  10 TIMES.
                04  W65-SYUKEITAIKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  W65-CMB-FTNMONEYKBNG.
                04  W65-FTNMONEYKBNG       PIC X(80).
                04  W65-FTNMONEYKBNG-COUNT PIC S9(9)   BINARY.
                04  W65-FTNMONEYKBNG-ITEM  PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-FTNMONEYTAIKBNG.
                04  W65-FTNMONEYTAIKBNG    PIC X(30).
                04  W65-FTNMONEYTAIKBNG-STATE
                                           PIC S9(9)   BINARY.
                04  W65-FTNMONEYTAIKBNG-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-FTNMONEYTAIKBNG-ITEM
                                           PIC X(30)   OCCURS  10 TIMES.
                04  W65-FTNMONEYTAIKBNG-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-FTNMONEYKBNN.
                04  W65-FTNMONEYKBNN       PIC X(80).
                04  W65-FTNMONEYKBNN-COUNT PIC S9(9)   BINARY.
                04  W65-FTNMONEYKBNN-ITEM  PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-FTNMONEYTAIKBNN.
                04  W65-FTNMONEYTAIKBNN    PIC X(30).
                04  W65-FTNMONEYTAIKBNN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-FTNMONEYTAIKBNN-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-FTNMONEYTAIKBNN-ITEM
                                           PIC X(30)   OCCURS  10 TIMES.
                04  W65-FTNMONEYTAIKBNN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-FTNZEROKBNG.
                04  W65-FTNZEROKBNG        PIC X(80).
                04  W65-FTNZEROKBNG-COUNT  PIC S9(9)   BINARY.
                04  W65-FTNZEROKBNG-ITEM   PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-FTNZEROTAIKBNG.
                04  W65-FTNZEROTAIKBNG   PIC X(30).
                04  W65-FTNZEROTAIKBNG-STATE
                                           PIC S9(9)   BINARY.
                04  W65-FTNZEROTAIKBNG-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-FTNZEROTAIKBNG-ITEM
                                           PIC X(30)   OCCURS  10 TIMES.
                04  W65-FTNZEROTAIKBNG-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-FTNZEROKBNN.
                04  W65-FTNZEROKBNN        PIC X(80).
                04  W65-FTNZEROKBNN-COUNT  PIC S9(9)   BINARY.
                04  W65-FTNZEROKBNN-ITEM   PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-FTNZEROTAIKBNN.
                04  W65-FTNZEROTAIKBNN     PIC X(30).
                04  W65-FTNZEROTAIKBNN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-FTNZEROTAIKBNN-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-FTNZEROTAIKBNN-ITEM
                                           PIC X(30)   OCCURS  10 TIMES.
                04  W65-FTNZEROTAIKBNN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-SKJKBN.
                04  W65-SKJKBN             PIC X(80).
                04  W65-SKJKBN-COUNT       PIC S9(9)   BINARY.
                04  W65-SKJKBN-ITEM        PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-SKJTAIKBN.
                04  W65-SKJTAIKBN          PIC X(30).
                04  W65-SKJTAIKBN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-SKJTAIKBN-COUNT    PIC S9(9)   BINARY.
                04  W65-SKJTAIKBN-ITEM     PIC X(30)   OCCURS  10 TIMES.
                04  W65-SKJTAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-OCRKBN.
                04  W65-OCRKBN             PIC X(80).
                04  W65-OCRKBN-COUNT       PIC S9(9)   BINARY.
                04  W65-OCRKBN-ITEM        PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-OCRTAIKBN.
                04  W65-OCRTAIKBN          PIC X(30).
                04  W65-OCRTAIKBN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-OCRTAIKBN-COUNT    PIC S9(9)   BINARY.
                04  W65-OCRTAIKBN-ITEM     PIC X(30)   OCCURS  10 TIMES.
                04  W65-OCRTAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-SAIKEIKBN.
                04  W65-SAIKEIKBN         PIC X(80).
                04  W65-SAIKEIKBN-STATE   PIC S9(9)   BINARY.
                04  W65-SAIKEIKBN-COUNT   PIC S9(9)   BINARY.
                04  W65-SAIKEIKBN-ITEM    PIC X(80)   OCCURS  10 TIMES.
                04  W65-SAIKEIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-SAIKEITAIKBN.
                04  W65-SAIKEITAIKBN      PIC X(30).
                04  W65-SAIKEITAIKBN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-SAIKEITAIKBN-COUNT
                                           PIC S9(9)   BINARY.
                04  W65-SAIKEITAIKBN-ITEM PIC X(30)   OCCURS  10 TIMES.
                04  W65-SAIKEITAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-JYUHENKBN.
                04  W65-JYUHENKBN          PIC X(80).
                04  W65-JYUHENKBN-COUNT    PIC S9(9)   BINARY.
                04  W65-JYUHENKBN-ITEM     PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-JYUHENTAIKBN.
                04  W65-JYUHENTAIKBN       PIC X(30).
                04  W65-JYUHENTAIKBN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-JYUHENTAIKBN-COUNT PIC S9(9)   BINARY.
                04  W65-JYUHENTAIKBN-ITEM  PIC X(30)   OCCURS  10 TIMES.
                04  W65-JYUHENTAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-CMB-JGNFTNKBN.
                04  W65-JGNFTNKBN          PIC X(80).
                04  W65-JGNFTNKBN-COUNT    PIC S9(9)   BINARY.
                04  W65-JGNFTNKBN-ITEM     PIC X(80)   OCCURS  10 TIMES.
              03  W65-CMB-JGNFTNTAIKBN.
                04  W65-JGNFTNTAIKBN       PIC X(30).
                04  W65-JGNFTNTAIKBN-STATE
                                           PIC S9(9)   BINARY.
                04  W65-JGNFTNTAIKBN-COUNT PIC S9(9)   BINARY.
                04  W65-JGNFTNTAIKBN-ITEM  PIC X(30)   OCCURS  10 TIMES.
                04  W65-JGNFTNTAIKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  W65-YUKOSTYMD-G.
                04  W65-YUKOSTYMD          PIC X(10).
                04  W65-YUKOSTYMD-STATE    PIC S9(9)   BINARY.
              03  W65-YUKOEDYMD-G.
                04  W65-YUKOEDYMD          PIC X(10).
                04  W65-YUKOEDYMD-STATE    PIC S9(9)   BINARY.
              03  W65-SELNUM2-G.
                04  W65-SELNUM2            PIC S9(2).
                04  W65-SELNUM2-STATE      PIC S9(9)   BINARY.
              03  W65-LBLMODE-G.
                04  W65-LBLMODE-STYLE      PIC X(20).
                04  W65-LBLMODE            PIC X(40).
              03  W65-SCRW28.
                04  W65-KOHLST.
                  05  W65-KOHLST-COUNT     PIC S9(9)   BINARY.
                  05  W65-KOHLST-ROW       PIC S9(9)   BINARY.
                  05  W65-KOHLST-ROWATTR   PIC S9(9)   BINARY.
                  05  W65-KOHLST-ITEM      OCCURS  100 TIMES.
                    06  W65-TKNO           PIC X(4).
                    06  W65-TKKOHNUM       PIC X(3).
                    06  W65-TKKOHNAME      PIC X(100).
                    06  W65-TKYUKSTYMD     PIC X(10).
                    06  W65-TKYUKEDYMD     PIC X(10).
                  05  W65-KOHLST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  W65-SCRW29.
                04  W65-RIREKILST.
                  05  W65-RIREKILST-COUNT   PIC S9(9)   BINARY.
                  05  W65-RIREKILST-ROW     PIC S9(9)   BINARY.
                  05  W65-RIREKILST-ROWATTR PIC S9(9)   BINARY.
                  05  W65-RIREKILST-ITEM    OCCURS  20 TIMES.
                    06  W65-TRNO            PIC X(3).
                    06  W65-TRKIKAN         PIC X(30).
                  05  W65-RIREKILST-SELECT  PIC X   OCCURS  20 TIMES.
