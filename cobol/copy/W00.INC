      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  W00.
              03  W00-KBN   PIC X(24).
              03  W00-NAME   PIC X(100).
              03  W00-HOSPKBNCOMB.
                04  W00-HOSPKBN   PIC X(10).
                04  W00-HOSPKBN-COUNT   PIC S9(9)   BINARY.
                04  W00-HOSPKBN-ITEM    PIC X(10)   OCCURS  2 TIMES.
              03  W00-SCROLLEDWINDOW1.
                04  W00-LIST.
                  05  W00-LIST-STATE   PIC S9(9)   BINARY.
                  05  W00-LIST-STYLE   PIC X(20).
                  05  W00-LIST-TROW    PIC S9(9)   BINARY.
                  05  W00-LIST-TROWATTR    PIC S9(9)   BINARY.
                  05  W00-LIST-TCOLUMN PIC S9(9)   BINARY.
                  05  W00-LIST-TVALUE  PIC X(256).
                  05  W00-LIST-ROWDATA     OCCURS  31 TIMES.
                    06  W00-BANGO-G.
                      07  W00-BANGO  PIC X(6).
                      07  W00-BANGO-FGCOLOR PIC X(20).
                      07  W00-BANGO-BGCOLOR PIC X(20).
                    06  W00-MEISYO-G.
                      07  W00-MEISYO    PIC X(30).
                      07  W00-MEISYO-FGCOLOR PIC X(20).
                      07  W00-MEISYO-BGCOLOR PIC X(20).
                    06  W00-ATAI-G.
                      07  W00-ATAI    PIC X(40).
                      07  W00-ATAI-FGCOLOR PIC X(20).
                      07  W00-ATAI-BGCOLOR PIC X(20).
                    06  W00-YUKOSTYMD-G.
                      07  W00-YUKOSTYMD    PIC X(9).
                      07  W00-YUKOSTYMD-FGCOLOR PIC X(20).
                      07  W00-YUKOSTYMD-BGCOLOR PIC X(20).
                    06  W00-YUKOEDYMD-G.
                      07  W00-YUKOEDYMD    PIC X(9).
                      07  W00-YUKOEDYMD-FGCOLOR PIC X(20).
                      07  W00-YUKOEDYMD-BGCOLOR PIC X(20).

