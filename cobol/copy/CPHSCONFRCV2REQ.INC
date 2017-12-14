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
      ******************************************************************
      *    ＡＰＩリクエスト室料差額更新
      *    COPY (CPHSCONFRCV2REQ.INC)
      ******************************************************************
        01  XML-HSCONFRCREQ.
            03  HSCONFRCREQ-CONTEXT            PIC S9(9)   BINARY.
            03  HSCONFRCREQ-OBJECT             PIC X(8).
            03  HSCONFRCREQ-MODE               PIC S9(9)   BINARY.
            03  HSCONFRCREQ-RECORDNAME         PIC X(128).
      *
            03  HSCONFRCREQ-AREA.
                05  HSCONFRCREQ-REQNUM         PIC X(02).
                05  HSCONFRCREQ-BASEYMD        PIC X(10).
                05  HSCONFRCREQ-OCC            OCCURS   99.
                    07  HSCONFRCREQ-TKBNCD     PIC X(02).
                    07  HSCONFRCREQ-TSAGAKU    PIC X(07).
                    07  HSCONFRCREQ-TSTYUKYMD  PIC X(10).
                    07  HSCONFRCREQ-TEDYUKYMD  PIC X(10).
                    07  HSCONFRCREQ-TFORCE     PIC X(5).
