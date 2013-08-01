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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
         02  M01.
            03  M01-TITLE           PIC X(120).
            03  M01-BGCOLORS.
              04  M01-VALUE   PIC X(20).
            03  M01-FIXED1.
              04  M01-B08-LABEL     PIC X(10).
              04  M01-STATE-G.
                05  M01-G11-STATE   PIC S9(09) BINARY.
                05  M01-G12-STATE   PIC S9(09) BINARY.
                05  M01-G13-STATE   PIC S9(09) BINARY.
                05  M01-G14-STATE   PIC S9(09) BINARY.
                05  M01-G21-STATE   PIC S9(09) BINARY.
                05  M01-G22-STATE   PIC S9(09) BINARY.
                05  M01-G23-STATE   PIC S9(09) BINARY.
                05  M01-G24-STATE   PIC S9(09) BINARY.
                05  M01-G41-STATE   PIC S9(09) BINARY.
                05  M01-G42-STATE   PIC S9(09) BINARY.
                05  M01-G43-STATE   PIC S9(09) BINARY.
                05  M01-G44-STATE   PIC S9(09) BINARY.
                05  M01-G51-STATE   PIC S9(09) BINARY.
                05  M01-G52-STATE   PIC S9(09) BINARY.
                05  M01-G71-STATE   PIC S9(09) BINARY.
                05  M01-G82-STATE   PIC S9(09) BINARY.
                05  M01-G91-STATE   PIC S9(09) BINARY.
                05  M01-G92-STATE   PIC S9(09) BINARY.
              04  M01-STATE-R       REDEFINES  M01-STATE-G.
                05  M01-STATE-O     PIC S9(09) BINARY  OCCURS  18.
              04  M01-SELNUM  PIC 9(2).
