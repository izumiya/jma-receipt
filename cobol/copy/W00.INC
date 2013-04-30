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
              03  W00-B03-STATE        PIC S9(9)   BINARY.
              03  W00-B04-STATE        PIC S9(9)   BINARY.
              03  W00-B06-STATE        PIC S9(9)   BINARY.
              03  W00-B07-STATE        PIC S9(9)   BINARY.
              03  W00-B12-STATE        PIC S9(9)   BINARY.
              03  W00-LIST.
                  05  W00-LIST-COUNT       PIC S9(9)   BINARY.
                  05  W00-LIST-ROW         PIC S9(9)   BINARY.
                  05  W00-LIST-ROWATTR     PIC S9(9)   BINARY.
                  05  W00-LIST-ITEM        OCCURS  30.
                      07  W00-LIST-NO          PIC X(02).
                      07  W00-LIST-NAME        PIC X(40).
                      07  W00-LIST-ATAI        PIC X(40).
                      07  W00-LIST-YUKOSTYMD   PIC X(09).
                      07  W00-LIST-YUKOEDYMD   PIC X(09).
                  05  W00-LIST-SELECT      PIC X       OCCURS  30.
              03  W00-SELNUM           PIC 9(02).
              03  W00-KANRICD          PIC X(04).
              03  W00-KBNCD            PIC X(08).
              03  W00-ITEMCD           PIC 9(05).
              03  W00-NAME             PIC X(40).
              03  W00-YUKOSTYMD        PIC X(09).
              03  W00-YUKOEDYMD        PIC X(09).
              03  W00-ATAI-COMBO.
                  05  W00-ATAI             PIC X(40).
                  05  W00-ATAI-COUNT       PIC S9(9)   BINARY.
                  05  W00-ATAI-ITEM        PIC X(40)   OCCURS  50.
