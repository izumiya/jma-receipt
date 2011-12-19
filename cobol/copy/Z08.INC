        02  Z08.
     *****  02  Z08-FIXED1.
              03  Z08-SRYCDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-YUKOSTYMDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-YUKOEDYMDH.
                04  Z08-VALUE   PIC X(9).
              03  Z08-NAMEH.
                04  Z08-VALUE   PIC X(100).
              03  Z08-NAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-KANANAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-YUKOSTYMD.
                04  Z08-VALUE   PIC X(10).
              03  Z08-YUKOEDYMD.
                04  Z08-VALUE   PIC X(10).
              03  Z08-FORMALNAME.
                04  Z08-VALUE   PIC X(200).
              03  Z08-IDOKANREN.
                04  Z08-VALUE   PIC X(9).
              03  Z08-IDOKANRENNAME.
                04  Z08-VALUE   PIC X(100).
              03  Z08-IDOKIKAN.
                04  Z08-IDOKIKAN-STYLE   PIC X(20).
                04  Z08-IDOKIKAN-VALUE   PIC X(30).
              03  Z08-COPYCD.
                04  Z08-VALUE   PIC X(9).
              03  Z08-TEN.
                04  Z08-VALUE   PIC S9(8)V9(2).
              03  Z08-TANICD-COMB.
                04  Z08-TANICD.
                  05  Z08-VALUE   PIC X(30).
                04  Z08-TANICD-COUNT   PIC S9(9)   BINARY.
                04  Z08-TANICD-ITEM
                        PIC X(30)   OCCURS  200 TIMES.
              03  Z08-MSGLBEL.
                04  Z08-VALUE   PIC X(100).
