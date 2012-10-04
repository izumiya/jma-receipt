        02  X04.
      *     02  X04-FIXED2.
              03  X04-B03-STATE PIC S9(9)   BINARY.
              03  X04-B04-STATE PIC S9(9)   BINARY.
              03  X04-B05-G.
                  05  X04-B05-LABEL PIC X(10).
                  05  X04-B05-STATE PIC S9(9)   BINARY.
              03  X04-B06-LABEL PIC X(10).
              03  X04-B09-STATE PIC S9(9)   BINARY.
              03  X04-B10-STATE PIC S9(9)   BINARY.
              03  X04-LBLREIGAIYKZ-G.
                04  X04-LBLREIGAIYKZ-STYLE  PIC X(20).
                04  X04-LBLREIGAIYKZ        PIC X(30).
              03  X04-BTN-ALLINS-STATE PIC S9(9)   BINARY.
              03  X04-CHKKBN.
                04  X04-VALUE   PIC X(50).
              03  X04-SRYCD.
                04  X04-VALUE   PIC X(9).
              03  X04-STYUKYMD.
                04  X04-VALUE   PIC X(9).
              03  X04-EDYUKYMD.
                04  X04-VALUE   PIC X(9).
              03  X04-NAME.
                04  X04-VALUE   PIC X(100).
              03  X04-CMB-NYUGAIKBN.
                04  X04-NYUGAIKBN.
                  05  X04-VALUE   PIC X(22).
                04  X04-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-NYUGAIKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
              03  X04-SELNUM.
                04  X04-VALUE   PIC X(50).
              03  X04-BYOSTATUS.
                04  X04-VALUE   PIC X(10).
              03  X04-BYOMEICD-G   OCCURS 6.
                04  X04-BYOMEICD   PIC X(20).
                04  X04-BYOMEICD-STATE   PIC S9(9)   BINARY.
              03  X04-BYOMEI.
                04  X04-VALUE   PIC X(400).
              03  X04-BYOMEI-STATE   PIC S9(9)   BINARY.
              03  X04-CMB-JYKNKBN.
                04  X04-JYKNKBN.
                  05  X04-VALUE   PIC X(12).
                04  X04-JYKNKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-JYKNKBN-ITEM
                        PIC X(12)   OCCURS  5 TIMES.
              03  X04-LBLUTAGAIKBN      PIC X(10).
              03  X04-CMB-UTAGAIKBN.
                04  X04-UTAGAIKBN       PIC X(32).
                04  X04-UTAGAIKBN-STATE PIC S9(9)   BINARY.
                04  X04-UTAGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-UTAGAIKBN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
                04  X04-UTAGAIKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  X04-LBLSYSKANRI-G.
                04  X04-LBLSYSKANRI-STYLE
                                      PIC X(20).
                04  X04-LBLSYSKANRI   PIC X(30).
              03  X04-SEL-BYOMEI.
                04  X04-VALUE   PIC X(100).
              03  X04-CMB-SRCBYOKBN.
                04  X04-SRCBYOKBN.
                  05  X04-VALUE   PIC X(22).
                04  X04-SRCBYOKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-SRCBYOKBN-ITEM
                        PIC X(22)   OCCURS  5 TIMES.
              03  X04-CMB-SRTKBN.
                04  X04-SRTKBN.
                  05  X04-VALUE   PIC X(23).
                04  X04-SRTKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-SRTKBN-ITEM
                        PIC X(23)   OCCURS  50 TIMES.
              03  X04-CMB-SRCJYKNKBN.
                04  X04-SRCJYKNKBN.
                  05  X04-VALUE   PIC X(12).
                04  X04-SRCJYKNKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-SRCJYKNKBN-ITEM
                        PIC X(12)   OCCURS  5 TIMES.
              03  X04-CMB-SRCUTAGAIKBN.
                04  X04-SRCUTAGAIKBN.
                  05  X04-VALUE   PIC X(32).
                04  X04-SRCUTAGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  X04-SRCUTAGAIKBN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
              03  X04-LBLGRPID    PIC X(8).
              03  X04-CMB-GRPID.
                04  X04-GRPID.
                  05  X04-VALUE   PIC X(10).
                04  X04-GRPID-STATE PIC S9(9)   BINARY.
                04  X04-GRPID-COUNT   PIC S9(9)   BINARY.
                04  X04-GRPID-ITEM
                        PIC X(10)   OCCURS  26 TIMES.
                04  X04-GRPID-ITEM-STATE PIC S9(9)   BINARY.
              03  X04-SCROLLEDWINDOW1.
                04  X04-BYOLST.
                  05  X04-BYOLST-COUNT   PIC S9(9)   BINARY.
                  05  X04-BYOLST-ROW PIC S9(9)   BINARY.
                  05  X04-BYOLST-ROWATTR PIC S9(9)   BINARY.
                  05  X04-BYOLST-ITEM        OCCURS  200 TIMES.
                    06  X04-TNO       PIC X(5).
                    06  X04-TMARK     PIC X(2).
                    06  X04-TBYOMEI   PIC X(100).
                    06  X04-TJYKN     PIC X(4).
                    06  X04-TUTAGAI   PIC X(4).
                  05  X04-BYOLST-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  X04-SCROLLEDWINDOW3.
                04  X04-SRCBYOLST.
                  05  X04-LBLSRCBYOCD      PIC X(10).
                  05  X04-SRCBYOLST-COUNT  PIC S9(9)   BINARY.
                  05  X04-SRCBYOLST-ROW    PIC S9(9)   BINARY.
                  05  X04-SRCBYOLST-ROWATTR    PIC S9(9)   BINARY.
                  05  X04-SRCBYOLST-ITEM   OCCURS  50 TIMES.
                    06  X04-TSRCNO         PIC X(3).
                    06  X04-TSRCBYOMEI     PIC X(100).
                    06  X04-TSRCCD         PIC X(12).
                  05  X04-SRCBYOLST-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  X04-SCROLLEDWINDOW2.
                04  X04-CDLST.
                  05  X04-CDLST-COUNT   PIC S9(9)   BINARY.
                  05  X04-CDLST-ROW     PIC S9(9)   BINARY.
                  05  X04-CDLST-ROWATTR PIC S9(9)   BINARY.
                  05  X04-CDLST-ITEM    OCCURS  150 TIMES.
                    06  X04-TCDNO       PIC X(3).
                    06  X04-TCDBYOCD    PIC X(7).
                    06  X04-TCDBYOMEI   PIC X(40).
                  05  X04-CDLST-SELECT
                                        PIC X   OCCURS  150 TIMES.
