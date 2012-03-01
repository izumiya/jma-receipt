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
        02  W20.
            03  W20-FIXED6.
              04  W20-B02-STATE            PIC S9(9)   BINARY.
              04  W20-B05-STATE            PIC S9(9)   BINARY.
              04  W20-B06-STATE            PIC S9(9)   BINARY.
              04  W20-B07-STATE            PIC S9(9)   BINARY.
              04  W20-B08-STATE            PIC S9(9)   BINARY.
              04  W20-B09-G.
                05  W20-B09                PIC X(04).
                05  W20-B09-STATE          PIC S9(9)   BINARY.
              04  W20-B10-G.
                05  W20-B10                PIC X(10).
                05  W20-B10-STATE          PIC S9(9)   BINARY.
              04  W20-B12-STATE            PIC S9(9)   BINARY.
              04  W20-KJNYMD               PIC X(10).
              04  W20-BTUNUM               PIC S9(2).
      *
              04  W20-NAME-G.
                05  W20-NAME               PIC X(20).
                05  W20-NAME-STATE         PIC S9(9)   BINARY.
      *
              04  W20-TANNAME-G.
                05  W20-TANNAME            PIC X(10).
                05  W20-TANNAME-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-TOKUTEINYUIN.
      *
                05  W20-TOKUTEINYUIN       PIC X(43).
                05  W20-TOKUTEINYUIN-STATE
                                           PIC S9(9)   BINARY.
      *
                05  W20-TOKUTEINYUIN-CNT
                                           PIC S9(9)   BINARY.
                05  W20-TOKUTEINYUIN-ITM
                                           PIC X(43)   OCCURS  30.
                05  W20-TOKUTEINYUIN-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-CMB-SRYKA.
      *
                05  W20-SRYKA             PIC X(23).
                05  W20-SRYKA-STATE       PIC S9(9)   BINARY.
      *
                05  W20-SRYKA-CNT         PIC S9(9)   BINARY.
                05  W20-SRYKA-ITM         PIC X(23)   OCCURS  99.
                05  W20-SRYKA-ITM-STATE   PIC S9(9)   BINARY.
      *
              04  W20-CMB-SBT.
      *
                05  W20-SBT                PIC X(23).
                05  W20-SBT-STATE          PIC S9(9)   BINARY.
      *
                05  W20-SBT-CNT            PIC S9(9)   BINARY.
                05  W20-SBT-ITM            PIC X(23)   OCCURS  15.
                05  W20-SBT-ITM-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-KHNRYO.
      *
                05  W20-KHNRYO             PIC X(63).
                05  W20-KHNRYO-STATE       PIC S9(9)   BINARY.
                05  W20-KHNRYO-CNT         PIC S9(9)   BINARY.
                05  W20-KHNRYO-ITM         PIC X(63)   OCCURS  50.
                05  W20-KHNRYO-ITM-STATE   PIC S9(9)   BINARY.
      *
      *
              04  W20-KHNTEN-G.
                05  W20-KHNTEN               PIC S9(6).
                05  W20-KHNTEN-STATE         PIC S9(9)   BINARY.
      *
              04  W20-CMB-RECEDSPKBN.
      *
                05  W20-RECEDSPKBN         PIC X(13).
                05  W20-RECEDSPKBN-STATE
                                           PIC S9(9)   BINARY.
      *
                05  W20-RECEDSPKBN-CNT     PIC S9(9)   BINARY.
                05  W20-RECEDSPKBN-ITM     PIC X(13)   OCCURS  10.
                05  W20-RECEDSPKBN-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-SELNUM               PIC S9(2).
      *
              04  W20-LBLKNGHJO            PIC X(36).
              04  W20-CMB-KNGHJO.
      *
                05  W20-KNGHJO             PIC X(41).
                05  W20-KNGHJO-STATE       PIC S9(9)   BINARY.
      *
                05  W20-KNGHJO-CNT         PIC S9(9)   BINARY.
                05  W20-KNGHJO-ITM         PIC X(41)   OCCURS  20.
                05  W20-KNGHJO-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-LBLKSN-G.
                05  W20-LBLKSN             PIC X(36)
                                           OCCURS      16.
      *
              04  W20-CMB-KSN-G.
                05  W20-CMB-KSN-OCC        OCCURS      16.
      *
                  06  W20-KSN              PIC X(30).
                  06  W20-KSN-STATE        PIC S9(9)   BINARY.
      *
                  06  W20-KSN-CNT          PIC S9(9)   BINARY.
                  06  W20-KSN-ITM          PIC X(30)   OCCURS  10.
                  06  W20-KSN-ITM-STATE    PIC S9(9)   BINARY.
              04  W20-YUKOSTYMD            PIC X(10).
              04  W20-YUKOSTYMD-STATE      PIC S9(9)   BINARY.
      *
              04  W20-YUKOEDYMD            PIC X(10).
              04  W20-YUKOEDYMD-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-EDKBN.
      *
                05  W20-EDKBN              PIC X(8).
                05  W20-EDKBN-STATE        PIC S9(9)   BINARY.
                05  W20-EDKBN-CNT          PIC S9(9)   BINARY.
                05  W20-EDKBN-ITM          PIC X(8)    OCCURS  2.
                05  W20-EDKBN-ITM-STATE    PIC S9(9)   BINARY.
      *
              04  W20-SCROLLEDWINDOW28.
                05  W20-BTULST-TBL.
                  06  W20-BTULST-CNT       PIC S9(9)   BINARY.
                  06  W20-BTULST-ROW       PIC S9(9)   BINARY.
                  06  W20-BTULST-ROWATTR   PIC S9(9)   BINARY.
                  06  W20-BTULST-OCC       OCCURS  50.
                    07  W20-TBTUNUM        PIC X(04).
                    07  W20-TNAME          PIC X(20).
                    07  W20-TSBT           PIC X(20).
                    07  W20-TTOKUTEINYUIN  PIC X(24).
                    07  W20-TSRYKA         PIC X(20).
                    07  W20-TKHNRYO        PIC X(30).
                    07  W20-TKHNTEN        PIC X(12).
                    07  W20-TNINZU         PIC X(10).
                    07  W20-TYUKOSTYMD     PIC X(09).
                    07  W20-TYUKOEDYMD     PIC X(09).
                  06  W20-BTULST-SEL       PIC X   OCCURS  50.
              04  W20-SCROLLEDWINDOW29.
                05  W20-RIREKILST-TBL.
                  06  W20-RIREKILST-CNT    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-ROW    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-ROWATTR    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-OCC    OCCURS  20.
                    07  W20-TSELNUM        PIC X(04).
                    07  W20-TRIREKI        PIC X(22).
                  06  W20-RIREKILST-SEL    PIC X   OCCURS  20.
