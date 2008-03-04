            02  C10.
              03  C10-TENKICOMBO.
                04  C10-TENKIKBN.
                  05  C10-VALUE   PIC X(10).
                04  C10-TENKIKBN-COUNT   PIC S9(9)   BINARY.
                04  C10-TENKIKBN-ITEM    PIC X(10)    OCCURS  6 TIMES.
              03  C10-TENKIYMD.
                04  C10-VALUE   PIC X(10).
              03  C10-UTAGAIFLGCOMBO.
                04  C10-UTAGAIFLG.
                  05  C10-VALUE   PIC X(14).
                04  C10-UTAGAIFLG-COUNT   PIC S9(9)   BINARY.
                04  C10-UTAGAIFLG-ITEM    PIC X(14)   OCCURS  4 TIMES.
              03  C10-MANSEIFLGCOMBO.
                04  C10-MANSEIFLG.
                  05  C10-VALUE   PIC X(46).
                04  C10-MANSEIFLG-COUNT   PIC S9(9)   BINARY.
                04  C10-MANSEIFLG-ITEM    PIC X(46)   OCCURS  6 TIMES.
              03  C10-HKNCOMBICOMBO.
                04  C10-HKNCOMBI.
                  05  C10-VALUE   PIC X(40).
                04  C10-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  C10-HKNCOMBI-ITEM    PIC X(40)   OCCURS  101 TIMES.
              03  C10-NYUGAIKBNCOMBO.
                04  C10-NYUGAIKBN.
                  05  C10-VALUE   PIC X(8).
                04  C10-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  C10-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  3 TIMES.
              03  C10-SYUBYOFLGCOMBO.
                04  C10-SYUBYOFLG.
                  05  C10-VALUE   PIC X(14).
                04  C10-SYUBYOFLG-COUNT   PIC S9(9)   BINARY.
                04  C10-SYUBYOFLG-ITEM    PIC X(14)   OCCURS  2 TIMES.
              03  C10-SRYYMD.
                04  C10-VALUE   PIC X(10).
