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
        02  I07.
      *      02  I07-FIXED6.
              03  I07-PTNUM                PIC X(20).
              03  I07-NAME                 PIC X(60).
              03  I07-KANANAME             PIC X(60).
              03  I07-BIRTHDAY-MOM         PIC X(9).
              03  I07-AGE                  PIC X(8).
              03  I07-NYUINYMD             PIC X(10).
              03  I07-TAIINYMD             PIC X(10).
              03  I07-SYAKOK               PIC X(4).
              03  I07-HONKZK               PIC X(4).
              03  I07-HKNJANUM             PIC X(8).
              03  I07-KIGO                 PIC X(80).
              03  I07-NUM                  PIC X(80).
              03  I07-CMB-RIYOUKBN.
                04  I07-RIYOUKBN           PIC X(22).
                04  I07-RIYOUKBN-COUNT     PIC S9(9)   BINARY.
                04  I07-RIYOUKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
              03  I07-SKYYM                PIC X(10).
              03  I07-SKYYM-STATE          PIC S9(9)   BINARY.
              03  I07-CMB-TEISYUTUBIKBN.
                04  I07-TEISYUTUBIKBN       PIC X(22).
                04  I07-TEISYUTUBIKBN-STATE PIC S9(9)   BINARY.
                04  I07-TEISYUTUBIKBN-COUNT PIC S9(9)   BINARY.
                04  I07-TEISYUTUBIKBN-ITEM  PIC X(22)   OCCURS  5 TIMES.
                04  I07-TEISYUTUBIKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  I07-CMB-SKYKBN.
                04  I07-SKYKBN             PIC X(22).
                04  I07-SKYKBN-STATE       PIC S9(9)   BINARY.
                04  I07-SKYKBN-COUNT       PIC S9(9)   BINARY.
                04  I07-SKYKBN-ITEM        PIC X(22)   OCCURS  5 TIMES.
                04  I07-SKYKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  I07-CMB-BUNBENKBN.
                04  I07-BUNBENKBN          PIC X(22).
                04  I07-BUNBENKBN-COUNT    PIC S9(9)   BINARY.
                04  I07-BUNBENKBN-ITEM     PIC X(22)   OCCURS  5 TIMES.
              03  I07-CMB-TEISYUTUSAKI.
                04  I07-TEISYUTUSAKI       PIC X(22).
                04  I07-TEISYUTUSAKI-STATE PIC S9(9)   BINARY.
                04  I07-TEISYUTUSAKI-COUNT PIC S9(9)   BINARY.
                04  I07-TEISYUTUSAKI-ITEM  PIC X(22)   OCCURS  5 TIMES.
                04  I07-TEISYUTUSAKI-ITEM-STATE  PIC S9(9)   BINARY.
              03  I07-ZAIWEEK              PIC S9(2).
              03  I07-BIRTHDAY-BABY        PIC X(10).
              03  I07-CMB-SIZANKBN.
                04  I07-SIZANKBN           PIC X(22).
                04  I07-SIZANKBN-COUNT     PIC S9(9)   BINARY.
                04  I07-SIZANKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
              03  I07-BIRTHCNT             PIC S9(1).
              03  I07-NYUIN-DAY            PIC S9(3).
              03  I07-CMB-SANKAKBN.
                04  I07-SANKAKBN           PIC X(22).
                04  I07-SANKAKBN-STATE     PIC S9(9)   BINARY.
                04  I07-SANKAKBN-COUNT     PIC S9(9)   BINARY.
                04  I07-SANKAKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
                04  I07-SANKAKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  I07-NYU-MONEY            PIC S9(7).
              03  I07-RMSGK-MONEY          PIC S9(7).
              03  I07-KAIJO-MONEY          PIC S9(7).
              03  I07-BUNBEN-MONEY         PIC S9(7).
              03  I07-SHIN-MONEY           PIC S9(7).
              03  I07-KNS-MONEY            PIC S9(7).
              03  I07-SYC-MONEY            PIC S9(7).
              03  I07-SNK-MONEY            PIC S9(7).
              03  I07-ETC-MONEY            PIC S9(7).
              03  I07-ICBFTN-MONEY         PIC S9(7).
              03  I07-NINPUFTN-MONEY       PIC S9(7).
              03  I07-DAIRI-MONEY          PIC S9(7).
              03  I07-BIKOU                PIC X(60).
              03  I07-CMB-MEIPRTKBN.
                04  I07-MEIPRTKBN          PIC X(22).
                04  I07-MEIPRTKBN-COUNT    PIC S9(9)   BINARY.
                04  I07-MEIPRTKBN-ITEM     PIC X(22)   OCCURS  5 TIMES.
              03  I07-LBLINFO01-OCC        OCCURS  4 TIMES.
                04  I07-LBLINFO            PIC X(100).
                04  I07-LBLINFO-STYLE      PIC X(20).
              03  I07-LBLHEADER            PIC X(20).
              03  I07-LBLHEADER-STYLE      PIC X(20).

